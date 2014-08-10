%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2014 12:36 PM
%%%-------------------------------------------------------------------
-module(id3_tag_writer).
-author("aardvocate").

-behaviour(gen_server).

%% API
-export([start_link/0, writeV2/3, syncV2/0, set_file/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("erlp3header.hrl").

%%%===================================================================
%%% Gen Server
%%%===================================================================

start_link() ->
  process_flag(trap_exit, true),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
  {ok, #id3V2{}}.

handle_call({set_file, File}, _From, State) ->
  {reply, ok, State#id3V2{file = File}};

handle_call({write, NewTagVal, _Version, TagID}, _From, State) ->
  Tags = State#id3V2.tags,
  case State#id3V2.file of
    undefined ->
      Reply = {error, "Call set_file/1 before calling write"},
      {reply, Reply, State};
    [] ->
      Reply = {error, "Call set_file/1 before calling write"},
      {reply, Reply, State};
    _ ->
      Tag = #tag{tag_id = TagID, value = NewTagVal},
      NewState = State#id3V2{tags = [Tag | Tags]},
      {reply, ok, NewState}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({sync, v2},  #id3V2{tags = [#tag{tag_id = _TagID, value = _Value} | _Tags], file = File} = State) ->
  TempFile = File ++ ".tmp",
  file:delete(TempFile),
  State2 = write_to_file(v2, State, []),
  {noreply, State2};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Public functions
%%%===================================================================

writeV2(Version, NewTagVal, TagID) ->
  case gen_server:call(?SERVER, {write, NewTagVal, Version, TagID}) of
    {error, Reason} ->
      Reason;
    _ ->
      ok
  end.

syncV2() ->
  ok = gen_server:cast(?SERVER, {sync, v2}).

set_file(File) ->
  gen_server:call(?SERVER, {set_file, File}).


%%=======================================================================
%%% Private Functions
%%=======================================================================

write_to_file(v2, #id3V2{tags = [#tag{tag_id = TagID, value = Value} | Tags], file = File}, WrittenTags) ->
  State = #id3V2{tags = Tags, file = File},
  TempFile = File ++ ".tmp",
  {ok, TempFileHandle} = file:open(TempFile, [append, binary, raw]),
  write_frame(TempFileHandle, Value, TagID),
  file:close(TempFileHandle),
  write_to_file(v2, State, [TagID | WrittenTags]);

write_to_file(_, #id3V2{tags = [], file = File} = State, WrittenTags) ->
  TempFile = File ++ ".tmp",
  {ok, TempFileHandle} = file:open(TempFile, [append, binary, raw]),
  {ok, AllData} = file:read_file(File),
  {ok, [{idv1tag, _IDv1TagResult}, {idv2tag, [{header, V2Header}, {extended_header, _extHeader}, {tags, V2Tags}]}]} = id3_tag_reader:read_tag(File),
  {_, MP3Data} = split_binary(AllData, (10 + proplists:get_value(size, V2Header))),

  case V2Tags of
    ok ->
      its_v24;
    _ ->
      Keys = proplists:get_keys(V2Tags),
      write_all_frames(TempFileHandle, Keys, V2Tags, WrittenTags)
  end,

  TagsSize = filelib:file_size(TempFile),
  file:write(TempFileHandle, MP3Data),
  {ok, Data} = file:read_file(TempFile),
  file:close(TempFileHandle),
  file:delete(TempFile),
  {ok, TempFileHandle2} = file:open(TempFile, [append, binary, raw]),
  {_, MajV, MinV} = proplists:get_value(version, V2Header),
  HeaderFlags = proplists:get_value(flags, V2Header),
  erlog:info("HeaderFlags: ~p~n", [HeaderFlags]),
  Unsync = erlp3_utils:boolean_atom_to_code(proplists:get_value(unsync, HeaderFlags)),
  Extended = erlp3_utils:boolean_atom_to_code(proplists:get_value(extended, HeaderFlags)),
  Experimental = erlp3_utils:boolean_atom_to_code(proplists:get_value(experimental, HeaderFlags)),
  <<S1:7/integer, S2:7/integer, S3:7/integer, S4:7/integer>> = <<TagsSize:28/integer>>,
  HeaderBin = <<"ID3", MajV:8/integer, MinV:8/integer, Unsync:1/integer, Extended:1/integer, Experimental:1/integer, 0:5/integer, 0:1/integer, S1:7/integer, 0:1/integer, S2:7/integer, 0:1/integer, S3:7/integer, 0:1/integer, S4:7/integer>>,
  file:write(TempFileHandle2, HeaderBin),
  file:write(TempFileHandle2, Data),
  file:close(TempFileHandle2),
  file:copy(TempFile, File),
  file:delete(TempFile),
  State.

write_frame_header(TempFileHandle, TagValue, TagID) ->
  Flags = proplists:get_value(flags, TagValue),
  TagAlterPreservation = erlp3_utils:reverse_boolean_atom_to_code(proplists:get_value(tag_alter_preservation, Flags)),
  FileAlterPreservation = erlp3_utils:reverse_boolean_atom_to_code(proplists:get_value(file_alter_preservation, Flags)),
  ReadOnly = erlp3_utils:boolean_atom_to_code(proplists:get_value(read_only, Flags)),
  Compression = erlp3_utils:boolean_atom_to_code(proplists:get_value(compression, Flags)),
  Encryption = erlp3_utils:boolean_atom_to_code(proplists:get_value(encryption, Flags)),
  GroupingID = erlp3_utils:boolean_atom_to_code(proplists:get_value(grouping_identity, Flags)),
  FlagsBin = <<TagAlterPreservation:1/integer, FileAlterPreservation:1/integer, ReadOnly:1/integer, 0:5/integer, Compression:1/integer, Encryption:1/integer, GroupingID:1/integer, 0:5/integer>>,
  Size = proplists:get_value(size, TagValue),

  file:write(TempFileHandle, list_to_binary(TagID)),
  <<A:8/integer, B:8/integer, C:8/integer, D:8/integer>> = <<Size:32/integer>>,
  file:write(TempFileHandle, <<A:8/integer, B:8/integer, C:8/integer, D:8/integer>>),
  file:write(TempFileHandle, FlagsBin).

write_frame(TempFileHandle, {apic, TagValue}, TagID) ->
  erlog:info("Writing Frame: ~p~n", [TagID]),
  write_frame_header(TempFileHandle, TagValue, TagID),
  Encoding = proplists:get_value(encoding, TagValue),
  MimeType = proplists:get_value(mime_type, TagValue),
  PicType = proplists:get_value(picture_type, TagValue),
  Desc = proplists:get_value(description, TagValue),
  PicData = proplists:get_value(picture_data, TagValue),
  file:write(TempFileHandle, binary:encode_unsigned(Encoding)),
  file:write(TempFileHandle, list_to_binary(MimeType)),
  file:write(TempFileHandle, erlp3_utils:pic_type_atom_to_code(PicType)),
  file:write(TempFileHandle, list_to_binary(Desc)),
  file:write(TempFileHandle, PicData),
  erlog:info("Writing Frame: ~p Successful~n", [TagID]),
  ok;

write_frame(TempFileHandle, {_TagIDLowerCase, TagValue}, [H | _Rest] = TagID) when H =:= 84 ->
  erlog:info("Writing Frame: ~p~n", [TagID]),
  write_frame_header(TempFileHandle, TagValue, TagID),
  Encoding = proplists:get_value(encoding, TagValue),
  TextString = proplists:get_value(textstring, TagValue),
  file:write(TempFileHandle, binary:encode_unsigned(Encoding)),
  file:write(TempFileHandle, list_to_binary(TextString)),
  ok;

write_frame(TempFileHandle, {uslt, TagValue}, TagID) ->
  erlog:info("Writing Frame: ~p~n", [TagID]),
  write_frame_header(TempFileHandle, TagValue, TagID),
  Encoding = proplists:get_value(encoding, TagValue),
  Language = proplists:get_value(language, TagValue),
  ContentDesc = proplists:get_value(content_descriptor, TagValue),
  Lyrics = proplists:get_value(lyrics, TagValue),
  file:write(TempFileHandle, binary:encode_unsigned(Encoding)),
  file:write(TempFileHandle, list_to_binary(Language)),
  file:write(TempFileHandle, list_to_binary(ContentDesc)),
  file:write(TempFileHandle, list_to_binary(Lyrics)),
  ok;

write_frame(TempFileHandle, {comm, TagValue}, TagID) ->
  erlog:info("Writing Frame: ~p~n", [TagID]),
  write_frame_header(TempFileHandle, TagValue, TagID),
  Encoding = proplists:get_value(encoding, TagValue),
  Language = proplists:get_value(language, TagValue),
  ContentDesc = proplists:get_value(short_description, TagValue),
  Comment = proplists:get_value(comment, TagValue),
  file:write(TempFileHandle, binary:encode_unsigned(Encoding)),
  file:write(TempFileHandle, list_to_binary(Language)),
  file:write(TempFileHandle, list_to_binary(ContentDesc)),
  file:write(TempFileHandle, list_to_binary(Comment)),
  ok;

write_frame(TempFileHandle, {pic, TagValue}, TagID) ->
  write_frame(TempFileHandle, {apic, TagValue}, TagID);

write_frame(TempFileHandle, {ult, TagValue}, TagID) ->
  write_frame(TempFileHandle, {uslt, TagValue}, TagID);

write_frame(TempFileHandle, {com, TagValue}, TagID) ->
  write_frame(TempFileHandle, {comm, TagValue}, TagID);

write_frame(_TempFileHandle, _TagValue, _TagID) ->
  ok.

write_all_frames(TempFileHandle, [Key | Keys], V2Tags, WrittenTags) ->
  UpperCaseKey = string:to_upper(atom_to_list(Key)),
  case lists:member(UpperCaseKey, WrittenTags) of
    true ->
      write_all_frames(TempFileHandle, Keys, V2Tags, WrittenTags);
    _ ->
      write_frame(TempFileHandle, proplists:get_value(Key, V2Tags), string:to_upper(atom_to_list(Key))),
      write_all_frames(TempFileHandle, Keys, V2Tags, WrittenTags)
  end;

write_all_frames(_TempFileHandle, [], _V2Tags, _WrittenTags) ->
  ok.