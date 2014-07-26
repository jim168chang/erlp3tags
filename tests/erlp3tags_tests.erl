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
  Expected = {ok, [{version, {2, 3, 0}},
    {flags, [{unsync, 0}, {extended, 0}, {experimental, 0}, {footer, 0}]},
    {size, 35250}]},
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

test_text_header_to_atom() ->
  Expected = tal,
  Actual = utils:text_header_to_atom("TAL"),
  Expected = Actual,
  erlog:info("Testing utils:text_header_to_atom/1 - passed~n").

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

  ActualCRA = v22_reader:parse_frame_bin(<<"CRA">>, 10, <<"http://mailto:segun@ratendate.com", 32, $a, $b, $c, 0, 0, 33, 0, 44, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedCRA = {cra, [
    {owner_id, "http://mailto:segun@ratendate.com abc"},
    {preview_start, 33},
    {preview_length, 44},
    {encryption_info, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}
  ]},
  ActualCRA = ExpectedCRA,

  ActualCRM = v22_reader:parse_frame_bin(<<"CRM">>, 10, <<"http://mailto:segun@ratendate.com", 0, "no reason", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedCRM = {crm, [
    {owner_id, "http://mailto:segun@ratendate.com"},
    {content_explanation, "no reason"},
    {encrypted_data, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}
  ]},
  ActualCRM = ExpectedCRM,

  ActualETC = v22_reader:parse_frame_bin(<<"ETC">>, 10, <<2, 16#A, 0, 0, 0, 33>>),
  ExpectedETC = {etc, [
    {time_stamp_format, milliseconds},
    {event, etc_variation},
    {timestamp, 33}
  ]},
  ActualETC = ExpectedETC,

  ActualEQU = v22_reader:parse_frame_bin(<<"EQU">>, 10, <<16, 255, 128, 0, 4>>),
  ExpectedEQU = {equ, [
    {adjustment_bits, 16},
    {inc_or_dec, increment},
    {frequency, 32640},
    {adjustment, 4}
  ]},
  ActualEQU = ExpectedEQU,

  ActualGEO = v22_reader:parse_frame_bin(<<"GEO">>, 10, <<1, "text/xhtml", 0, "hello.xml", 0, "Hello World In XML", 0, 44, 55, 66, 77, 88, 99, 100, 33, 22, 11>>),
  ExpectedGEO = {geo, [
    {encoding, 1},
    {mime_type, "text/xhtml"},
    {filename, "hello.xml"},
    {content_description, "Hello World In XML"},
    {encapsulated_object, <<44, 55, 66, 77, 88, 99, 100, 33, 22, 11>>}
  ]},
  ActualGEO = ExpectedGEO,

  ActualIPL = v22_reader:parse_frame_bin(<<"IPL">>, 10, <<0, "Director", 0, "Moses Monday", 0, "Producer", 0, "Wyte Tag", 0, "Creative Director", 0, "Aaron", 0>>),
  ExpectedIPL = {ipl, [
    {ip, {involvement, "Director"}, {involvee, "Moses Monday"}},
    {ip, {involvement, "Producer"}, {involvee, "Wyte Tag"}},
    {ip, {involvement, "Creative Director"}, {involvee, "Aaron"}}
  ]},
  ActualIPL = ExpectedIPL,

  ActualLNK = v22_reader:parse_frame_bin(<<"LNK">>, 10, <<"TOS", "http://nalyrics.com.ng", 0>>),
  ExpectedLNK = {lnk, [
    {frame_identifier, "TOS"},
    {url, "http://nalyrics.com.ng"},
    {additional_id_data, []}
  ]},
  ActualLNK = ExpectedLNK,

  ActualLNK2 = v22_reader:parse_frame_bin(<<"LNK">>, 10, <<"TOS", "http://nalyrics.com.ng", 0, "olu ", 0, "is ", 0, "a ", 0, "boy", 0>>),
  ExpectedLNK2 = {lnk, [
    {frame_identifier, "TOS"},
    {url, "http://nalyrics.com.ng"},
    {additional_id_data, ["olu", "is", "a", "boy"]}
  ]},
  ActualLNK2 = ExpectedLNK2,

  ActualMLL = v22_reader:parse_frame_bin(<<"MLL">>, 10, <<2, 3, 1, 2, 3, 3, 2, 1, 2, 1>>),
  ExpectedMLL = {mll, [
    {frames_between_reference, 515},
    {bytes_between_reference, 66051},
    {milliseconds_between_reference, 197121},
    {bit_for_bytes_deviation, 2},
    {bits_for_milliseconds_deviation, 1}
  ]},
  ActualMLL = ExpectedMLL,

  ActualPIC = v22_reader:parse_frame_bin(<<"PIC">>, 128, <<1, "PNG", 2, "Wande Coal In Allow Me To Kiss Your Hand", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedPIC = {pic, [
    {encoding, 1},
    {image_format, "PNG"},
    {picture_type, 2},
    {description, "Wande Coal In Allow Me To Kiss Your Hand"},
    {picture_data, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}
  ]},
  ActualPIC = ExpectedPIC,

  ActualPOP = v22_reader:parse_frame_bin(<<"POP">>, 10, <<"akintayo.segun@gmail.com", 0, 254, 0, 1, 2, 3, 4>>),
  ExpectedPOP = {pop, [
    {email_to_user, "akintayo.segun@gmail.com"},
    {rating, 254},
    {counter, 16909060}
  ]},
  ActualPOP = ExpectedPOP,

  ActualRVA = v22_reader:parse_frame_bin(<<"RVA">>, 10, <<16, 16, 0, 4, 0, 8, 0, 2, 0, 6>>),
  ExpectedRVA = {rva, [
    {inc_or_dec_right, 1},
    {inc_or_dec_left, 0},
    {bits_used_for_volume, 16},
    {relative_volume_change_right, 4},
    {relative_voilume_change_left, 8},
    {peak_volume_right, 2},
    {peak_volume_left, 6}
  ]},
  ActualRVA = ExpectedRVA,

  ActualSLT = v22_reader:parse_frame_bin(<<"SLT">>, 10, <<1, "ENG", 2, 1,
        "Strang", 0, 1, 0,  "ers",  0, 1, 1,  " in", 0, 2, 0,  " the", 0, 2, 1, " night", 0, 2, 2, 0>>),
  ExpectedSLT = {slt, [
    {encoding, 1},
    {language, "ENG"},
    {timestamp_format, utils:time_format_code_to_atom(2)},
    {content_type, utils:slt_content_type_code_to_atom(1)},
    {content_descriptor, [83,116,114,97,110,103,0,1,0,101,114,115,0, 1,1,32,105,110,0,2,0,32,116,104,101,0,2,1, 32,110,105,103,104,116,0,2,2]}
  ]},

  ActualSLT = ExpectedSLT,

  erlog:info("Testing v22_reader:parse_frame_bin/3 - passed~n").

tests() ->
  erlog:start(),
  erlog:load_config_file("conf/erlog.conf"),
  erlog:info("~n---------------Starting Tests---------------~n"),
  File = filename:join("misc", "mi_one_six.mp3"),
  {ok, S} = file:open(File, [read, binary, raw]),
  id3_tag_reader:read_tag(File),
  test_read_header(S),
  test_synch_safe(),
  test_parse_v22_frame_bin(),
  test_text_header_to_atom(),
  erlog:info("~n---------------Tests Finished---------------~n"),

  id3_tag_reader:read_tag(filename:join("misc", "mi_one_six_real.mp3")),
  ok.