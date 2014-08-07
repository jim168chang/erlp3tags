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

handle_call({sync, v2}, _From, State) ->
  State2 = write_to_file(v2, State),
  {reply, ok, State2};

handle_call({write, NewTagVal, ?ID3V2_3, TagID}, _From, State) ->
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

writeV2(?ID3V2_3 = Version, NewTagVal, TagID) ->
  case gen_server:call(?SERVER, {write, NewTagVal, Version, TagID}) of
    {error, Reason} ->
      Reason;
    _ ->
      ok
  end;

writeV2(?ID3V2_4, _NewTagVal, _TagID) ->
  not_yet_implemented.

syncV2() ->
  gen_server:call(?SERVER, {sync, v2}).

set_file(File) ->
  gen_server:call(?SERVER, {set_file, File}).


%%=======================================================================
%%% Private Functions
%%=======================================================================

write_to_file(v2, #id3V2{tags = [#tag{tag_id = TagID, value = Value} | Tags], file = File}) ->
  io:format("Writing Tag: ~p~n", [TagID]),
  State2 = #id3V2{tags = Tags, file = File},
  {ok, [{idv1tag, _IDv1TagResult}, {idv2tag, [{header, V2Header}, {extended_header, _extHeader}, {tags, V2Tags}]}]} = id3_tag_reader:read_tag(File),
  TempFile = File ++ ".tmp",
  {ok, TempFileHandle} = file:open(TempFile, [write, binary, raw]),
  {ok, AllData} = file:read_file(File),
  {_, MP3Data} = split_binary(AllData, (10 + proplists:get_value(size, V2Header))),

  if
    length(TagID) =:= 4 ->
      write_frame(TempFileHandle, Value, TagID),
      Keys = proplists:get_keys(V2Tags),
      write_all_frames(TempFileHandle, TagID, Keys, V2Tags),
      TagsSize = filelib:file_size(TempFile),
      file:write(TempFileHandle, MP3Data),
      {ok, Data} = file:read_file(TempFile),
      file:close(TempFileHandle),
      file:delete(TempFile),
      {ok, TempFileHandle2} = file:open(TempFile, [write, binary, raw]),

      {_, MajV, MinV} = proplists:get_value(version, V2Header),
      HeaderFlags = proplists:get_value(flags, V2Header),
      Unsync = utils:boolean_atom_to_code(proplists:get_value(unsync, HeaderFlags)),
      Extended = utils:boolean_atom_to_code(proplists:get_value(extended, HeaderFlags)),
      Experimental = utils:boolean_atom_to_code(proplists:get_value(experimental, HeaderFlags)),
      <<S1:7/integer, S2:7/integer, S3:7/integer, S4:7/integer>> = <<TagsSize:28/integer>>,
      HeaderBin = <<"ID3", MajV:8/integer, MinV:8/integer, Unsync:1/integer, Extended:1/integer, Experimental:1/integer, 0:5/integer, 0:1/integer, S1:7/integer, 0:1/integer, S2:7/integer, 0:1/integer, S3:7/integer, 0:1/integer, S4:7/integer>>,
      file:write(TempFileHandle2, HeaderBin),
      file:write(TempFileHandle2, Data),
      file:close(TempFileHandle2),
      write_to_file(v2, State2);
    true ->
      do3
  end;

write_to_file(_, State) ->
  State.

write_frame_header(TagID, TempFileHandle, TagValue) ->
  Flags = proplists:get_value(flags, TagValue),
  TagAlterPreservation = utils:reverse_boolean_atom_to_code(proplists:get_value(tag_alter_preservation, Flags)),
  FileAlterPreservation = utils:reverse_boolean_atom_to_code(proplists:get_value(file_alter_preservation, Flags)),
  ReadOnly = utils:boolean_atom_to_code(proplists:get_value(read_only, Flags)),
  Compression = utils:boolean_atom_to_code(proplists:get_value(compression, Flags)),
  Encryption = utils:boolean_atom_to_code(proplists:get_value(encryption, Flags)),
  GroupingID = utils:boolean_atom_to_code(proplists:get_value(grouping_identity, Flags)),
  FlagsBin = <<TagAlterPreservation:1/integer, FileAlterPreservation:1/integer, ReadOnly:1/integer, 0:5/integer, Compression:1/integer, Encryption:1/integer, GroupingID:1/integer, 0:5/integer>>,
  Size = proplists:get_value(size, TagValue),

  file:write(TempFileHandle, list_to_binary(TagID)),
  <<A:8/integer, B:8/integer, C:8/integer, D:8/integer>> = <<Size:32/integer>>,
  file:write(TempFileHandle, <<A:8/integer, B:8/integer, C:8/integer, D:8/integer>>),
  file:write(TempFileHandle, FlagsBin).

write_frame(TempFileHandle, {apic, TagValue}, TagID) ->
  write_frame_header(TagID, TempFileHandle, TagValue),
  Encoding = proplists:get_value(encoding, TagValue),
  MimeType = proplists:get_value(mime_type, TagValue),
  PicType = proplists:get_value(picture_type, TagValue),
  Desc = proplists:get_value(description, TagValue),
  PicData = proplists:get_value(picture_data, TagValue),
  file:write(TempFileHandle, binary:encode_unsigned(Encoding)),
  file:write(TempFileHandle, list_to_binary(MimeType ++ "\0")),
  file:write(TempFileHandle, utils:pic_type_atom_to_code(PicType)),
  file:write(TempFileHandle, list_to_binary(Desc ++ "\0")),
  file:write(TempFileHandle, PicData),
  ok;

write_frame(TempFileHandle, {_TagIDLowerCase, TagValue}, [H | _Rest] = TagID) when H =:= 84 ->
  write_frame_header(TagID, TempFileHandle, TagValue),
  Encoding = proplists:get_value(encoding, TagValue),
  TextString = proplists:get_value(textstring, TagValue),
  file:write(TempFileHandle, binary:encode_unsigned(Encoding)),
  file:write(TempFileHandle, list_to_binary(TextString ++ "\0")),
  ok;

write_frame(TempFileHandle, {pic, TagValue}, TagID) ->
  write_frame(TempFileHandle, {apic, TagValue}, TagID);


write_frame(_TempFileHandle, _TagValue, _TagID) ->
  ok.

write_all_frames(TempFileHandle, TagID, [Key | Keys], V2Tags) ->
  UpperCaseKey = string:to_upper(atom_to_list(Key)),
  if
    UpperCaseKey =:= TagID ->
      write_all_frames(TempFileHandle, TagID, Keys, V2Tags);
    true ->
      write_frame(TempFileHandle, proplists:get_value(Key, V2Tags), string:to_upper(atom_to_list(Key))),
      write_all_frames(TempFileHandle, TagID, Keys, V2Tags)
  end;

write_all_frames(_TempFileHandle, _TagID, [], _V2Tags) ->
  ok.