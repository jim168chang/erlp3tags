%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2014 2:23 PM
%%%-------------------------------------------------------------------
-module(erlp3tags_tests).
-author("aardvocate").

-include("../include/erlp3header.hrl").

%% API
-export([tests/0]).

test_read_header(FileHandle) ->
  Expected = {ok,[{version,{2,2,0}},
    {flags,[{unsync,0},{extended,0},{experimental,0},{footer,0}]},
    {size,85754}]},
  {ok, FirstTen} = file:pread(FileHandle, 0, 10),
  Actual = id3_tag_reader:read_v2_header(FirstTen),
  Expected = Actual,
  Expected2 = {error, invalid_v2_header_bytes},
  {ok, FirstFour} = file:pread(FileHandle, 0, 4),
  Actual2 = id3_tag_reader:read_v2_header(FirstFour),
  Expected2 = Actual2,
  erlog:info("Testing id3_tag_reader:read_header/1 - passed ~n").

test_synch_safe() ->
  Expected = {ok, 85754},
  Actual = utils:synch_safe(<<0, 5, 29, 122>>),
  Expected = Actual,
  Expected2 = {error, invalid_size_bytes},
  Actual2 = utils:synch_safe(<<5, 29, 122>>),
  Expected2 = Actual2,
  erlog:info("Testing id3_tag_reader:synch_safe/1 - passed~n").

test_parse_v22_frame_bin() ->
  ActualBUF = v22_reader:parse_frame_bin(<<"BUF">>, 8, <<12, 13, 14, 1, 1, 2, 3, 4>>),
  ExpectedBUF = {buf, [{buffer_size, 789774}, {embedded_info, true}, {next_flag_offset, 16909060}]},
  ActualBUF = ExpectedBUF,

  ActualCNT = v22_reader:parse_frame_bin(<<"CNT">>, 5, <<0, 1, 2, 3, 4>>),
  ExpectedCNT = {cnt, 16909060},
  ActualCNT = ExpectedCNT,

  ExpectedCOM = v22_reader:parse_frame_bin(<<"COM">>, 42, <<1, $e, $n, $g, "Short Comment", 0, "This is the actual comment", " ", 0>>),
  ActualCOM = {com, [
    {language, "eng"},
    {short_description, "Short Comment"},
    {comment, "This is the actual comment"}
  ]},
  ActualCOM = ExpectedCOM,

  ActualCRA = v22_reader:parse_frame_bin(<<"CRA">>, 10, <<"http://mailto:segun@ratendate.com", 32, $a, $b, $c, 0, 0, 33, 0, 44, 1,2,3,4,5,6,7,8,9,10>>),
  ExpectedCRA = {cra, [
    {owner_id, "http://mailto:segun@ratendate.com abc"},
    {preview_start, 33},
    {preview_length, 44},
    {encryption_info, <<1,2,3,4,5,6,7,8,9,10>>}
  ]},
  ActualCRA = ExpectedCRA,

  erlog:info("Testing v22_reader:parse_frame_bin/3 - passed~n").


tests() ->
  erlog:start(),
  erlog:load_config_file("conf/erlog.conf"),
  erlog:info("~n~n---------------Starting Tests---------------~n~n"),
  File = filename:join("misc", "mi_one_six.mp3"),
  {ok, S} = file:open(File, [read, binary, raw]),
  id3_tag_reader:read_tag(File),
  test_read_header(S),
  test_synch_safe(),
  test_parse_v22_frame_bin(),
  erlog:info("~n~n---------------Tests Finished---------------~n~n"),
  ok.