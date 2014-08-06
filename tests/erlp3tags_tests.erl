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
    {flags, [{unsync, false}, {extended, false}, {experimental, false}]},
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
  Actual = utils:header_to_atom("TAL"),
  Expected = Actual,
  erlog:info("Testing utils:text_header_to_atom/1 - passed~n").

test_parse_v23_frame_bin() ->
  <<A, B, C, I, J, K>> = <<1, 0, 1, 0, 1, 0>>,

  Flags = {flags, [
    {tag_alter_preservation, utils:reverse_boolean_code_to_atom(A)},
    {file_alter_preservation, utils:reverse_boolean_code_to_atom(B)},
    {read_only, utils:boolean_code_to_atom(C)},
    {compression, utils:boolean_code_to_atom(I)},
    {encryption, utils:boolean_code_to_atom(J)},
    {grouping_identity, utils:boolean_code_to_atom(K)}
  ]},
  ActualAENC = v23_reader:parse_frame_bin(<<"AENC">>, 10, Flags, <<"Akintayo Olusegun", 0, 0, 3, 0, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedAENC = {aenc, [
    {size, 10}, Flags | v22_reader:parse_cra_content(<<"Akintayo Olusegun", 0, 0, 3, 0, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>)
  ]},
  ActualAENC = ExpectedAENC,

  ActualAPIC = v23_reader:parse_frame_bin(<<"APIC">>, 10, Flags, <<1, "PNG", 0, 3, "This is a test pic", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  %%io:format("Actual: ~p~n", [ActualAPIC]),
  ExpectedAPIC = {apic,
    [
      {size, 10},
      Flags,
      {encoding, 1},
      {mime_type, "PNG"},
      {picture_type, cover_front},
      {description, "This is a test pic"},
      {picture_data, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}
    ]
  },
  ActualAPIC = ExpectedAPIC,

  ActualCOMM = v23_reader:parse_frame_bin(<<"COMM">>, 10, Flags, <<0, "SPA", "Short Desc", 0, 0, "Hello World Comment">>),

  ExpectedCOMM = {comm, [
    {size, 10},
    Flags | v22_reader:parse_com_content(<<0, "SPA", "Short Desc", 0, 0, "Hello World Comment">>)
  ]},
  ActualCOMM = ExpectedCOMM,

  ActualCOMR = v23_reader:parse_frame_bin(<<"COMR">>, 10, Flags, <<1, "234.56", 0, "20140810", "http://nalyrics.com.ng", 0, 3, "Akintayo Olusegun", 0, 0, 0, 0, "This Is A Test Description", 0, 0, "image/jpeg", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedCOMR = {comr, [
    {size, 10},
    Flags,
    {encoding, 1},
    {price_string, "234.56"},
    {valid_until, "20140810"},
    {contact_url, "http://nalyrics.com.ng"},
    {recieved_as, file_over_intenet},
    {name_of_seller, "Akintayo Olusegun"},
    {description, "This Is A Test Description"},
    {picture_mime_type, "image/jpeg"},
    {seller_logo, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}
  ]},
  ActualCOMR = ExpectedCOMR,

  ActualENCR = v23_reader:parse_frame_bin(<<"ENCR">>, 10, Flags, <<"i@j.k", 0, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedENCR = {encr, [
    {size, 10},
    Flags,
    {owner_identifier, "i@j.k"},
    {method_symbol, 1},
    {encryption_data, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}]},
  ActualENCR = ExpectedENCR,

  ActualGRID = v23_reader:parse_frame_bin(<<"GRID">>, 10, Flags, <<"Segun Ak.", 0, 8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedGRID = {grid, [
    {size, 10},
    Flags,
    {owner_identifier, "Segun Ak."},
    {group_symbol, 8},
    {group_dependent_data, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}
  ]},
  ActualGRID = ExpectedGRID,

  ActualEQUA = v23_reader:parse_frame_bin(<<"EQUA">>, 10, Flags, <<16, 255, 128, 0, 4>>),
  %io:format("Actual: ~p~n", [ActualEQUA]),
  ExpectedEQUA = {equa, [
    {size, 10},
    Flags,
    {adjustment_bits, 16},
    {inc_or_dec, increment},
    {frequency, 32640},
    {adjustment, 4}
  ]},
  ActualEQUA = ExpectedEQUA,

  ActualETCO = v23_reader:parse_frame_bin(<<"ETCO">>, 10, Flags, <<2, 6, 0, 0, 0, 4>>),
  ExpectedETCO = {etco, [
    {size, 10},
    Flags | v22_reader:parse_etc_content(<<2, 6, 0, 0, 0, 4>>)
  ]},
  ActualETCO = ExpectedETCO,

  ActualGEOB = v23_reader:parse_frame_bin(<<"GEOB">>, 10, Flags, <<1, "image/png", 0, "filename.png", 0, 0, "describe it", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedGEOB = {geob, [
    {size, 10},
    Flags | v22_reader:parse_geo_content(<<1, "image/png", 0, "filename.png", 0, 0, "describe it", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>)
  ]},
  ActualGEOB = ExpectedGEOB,

  ActualIPLS = v23_reader:parse_frame_bin(<<"IPLS">>, 10, Flags, <<0, "Director", 0, "Moses Monday", 0, "Producer", 0, "Wyte Tag", 0, "Creative Director", 0, "Aaron", 0>>),
  ExpectedIPLS = {ipls, [
    {size, 10},
    Flags | v22_reader:parse_ipl_content(<<0, "Director", 0, "Moses Monday", 0, "Producer", 0, "Wyte Tag", 0, "Creative Director", 0, "Aaron", 0>>)
  ]},
  ActualIPLS = ExpectedIPLS,

  ActualLINK = v23_reader:parse_frame_bin(<<"LINK">>, 10, Flags, <<"IPL", "http://nalyrics.com.ng", 0, "Additional Data", 0, "Additional Data2", 0, "Additional Data 3", 0>>),
  ExpectedLINK = {link, [
    {size, 10},
    Flags | v22_reader:parse_lnk_content(<<"IPL", "http://nalyrics.com.ng", 0, "Additional Data", 0, "Additional Data2", 0, "Additional Data 3", 0>>)
  ]},
  ActualLINK = ExpectedLINK,

  ActualMLLT = v23_reader:parse_frame_bin(<<"MLLT">>, 10, Flags, <<2, 3, 1, 2, 3, 3, 2, 1, 8, 16, 10, 0, 88>>),
  ExpectedMLLT = {mllt, [
    {size, 10},
    Flags,
    {frames_between_reference, 515},
    {bytes_between_reference, 66051},
    {milliseconds_between_reference, 197121},
    {bit_for_bytes_deviation, 8},
    {bits_for_milliseconds_deviation, 16},
    {deviation_in_bytes, 10},
    {deviation_in_milliseconds, 88}
  ]},
  ActualMLLT = ExpectedMLLT,

  ActualOWNE = v23_reader:parse_frame_bin(<<"OWNE">>, 10, Flags, <<1, "234.56", 0, "20140810", "Akintayo Segun", 0>>),
  ExpectedOWNE = {owne, [
    {size, 10},
    Flags,
    {encoding, 1},
    {price_payed, "234.56"},
    {date_of_purchase, "20140810"},
    {seller, "Akintayo Segun"}
  ]},
  ActualOWNE = ExpectedOWNE,

  ActualPCNT = v23_reader:parse_frame_bin(<<"PCNT">>, 5, Flags, <<0, 1, 2, 3, 4>>),
  ExpectedPCNT = {pcnt, [
    {size, 5},
    Flags,
    {counter, 16909060}
  ]},
  ActualPCNT = ExpectedPCNT,

  ActualPOPM = v23_reader:parse_frame_bin(<<"POPM">>, 10, Flags, <<"segun@ratendate.com", 0, 23, 0, 0, 0, 32>>),
  ExpectedPOPM = {popm, [
    {size, 10},
    Flags,
    {email_to_user, "segun@ratendate.com"},
    {rating, 23},
    {counter, 32}
  ]},
  ActualPOPM = ExpectedPOPM,

  ActualPOSS = v23_reader:parse_frame_bin(<<"POSS">>, 10, Flags, <<1, 1, 2, 3, 4>>),
  ExpectedPOSS = {poss, [
    {size, 10},
    Flags,
    {timestamp_format, mpeg_frames},
    {position, 16909060}
  ]},
  ActualPOSS = ExpectedPOSS,

  ActualRBUF = v23_reader:parse_frame_bin(<<"RBUF">>, 10, Flags, <<0, 0, 1, 1, 0, 0, 0, 2>>),
  ExpectedRBUF = {rbuf, [
    {size, 10},
    Flags,
    {buffer_size, 1},
    {embedded_info, true},
    {next_flag_offset, 2}
  ]},
  ActualRBUF = ExpectedRBUF,

  ActualRBUF2 = v23_reader:parse_frame_bin(<<"RBUF">>, 10, Flags, <<0, 0, 1, 0>>),
  ExpectedRBUF2 = {rbuf, [
    {size, 10},
    Flags,
    {buffer_size, 1},
    {embedded_info, false}
  ]},
  ActualRBUF2 = ExpectedRBUF2,

  ActualRVAD = v23_reader:parse_frame_bin(<<"RVAD">>, 10, Flags, <<3, 16, 0, 4, 0, 8, 0, 2, 0, 6>>),
  ExpectedRVAD = {rvad, [
    {size, 10},
    Flags,
    {inc_or_dec_right, 1},
    {inc_or_dec_left, 1},
    {bits_used_for_volume, 16},
    {relative_volume_change_right, 4},
    {relative_voilume_change_left, 8},
    {peak_volume_right, 2},
    {peak_volume_left, 6}
  ]},
  ActualRVAD = ExpectedRVAD,

  ActualRVAD2 = v23_reader:parse_frame_bin(<<"RVAD">>, 10, Flags, <<7, 16, 0, 4, 0, 8, 0, 2, 0, 6, 0, 2, 0, 4, 0, 1, 0, 2>>),
  ExpectedRVAD2 = {rvad, [
    {size, 10},
    Flags,
    {inc_or_dec_right, 1},
    {inc_or_dec_left, 1},
    {inc_or_dec_right_back, 1},
    {inc_or_dec_left_back, 0},
    {bits_used_for_volume, 16},
    {relative_volume_change_right, 4},
    {relative_voilume_change_left, 8},
    {peak_volume_right, 2},
    {peak_volume_left, 6},
    {relative_volume_change_right_back, 2},
    {relative_voilume_change_left_back, 4},
    {peak_volume_right_back, 1},
    {peak_volume_left_back, 2}
  ]},
  ActualRVAD2 = ExpectedRVAD2,

  ActualRVAD3 = v23_reader:parse_frame_bin(<<"RVAD">>, 10, Flags, <<7, 16, 0, 4, 0, 8, 0, 2, 0, 6, 0, 2, 0, 4, 0, 1, 0, 2, 1, 3, 2, 3>>),
  ExpectedRVAD3 = {rvad, [
    {size, 10},
    Flags,
    {inc_or_dec_right, 1},
    {inc_or_dec_left, 1},
    {inc_or_dec_right_back, 1},
    {inc_or_dec_left_back, 0},
    {bits_used_for_volume, 16},
    {relative_volume_change_right, 4},
    {relative_voilume_change_left, 8},
    {peak_volume_right, 2},
    {peak_volume_left, 6},
    {relative_volume_change_right_back, 2},
    {relative_voilume_change_left_back, 4},
    {peak_volume_right_back, 1},
    {peak_volume_left_back, 2},
    {relative_volume_change_center, 259},
    {peak_volume_center, 515}
  ]},
  ActualRVAD3 = ExpectedRVAD3,

  ActualRVAD4 = v23_reader:parse_frame_bin(<<"RVAD">>, 10, Flags, <<7, 16, 0, 4, 0, 8, 0, 2, 0, 6, 0, 2, 0, 4, 0, 1, 0, 2, 1, 3, 2, 3, 1, 3, 2, 3>>),
  ExpectedRVAD4 = {rvad, [
    {size, 10},
    Flags,
    {inc_or_dec_right, 1},
    {inc_or_dec_left, 1},
    {inc_or_dec_right_back, 1},
    {inc_or_dec_left_back, 0},
    {bits_used_for_volume, 16},
    {relative_volume_change_right, 4},
    {relative_voilume_change_left, 8},
    {peak_volume_right, 2},
    {peak_volume_left, 6},
    {relative_volume_change_right_back, 2},
    {relative_voilume_change_left_back, 4},
    {peak_volume_right_back, 1},
    {peak_volume_left_back, 2},
    {relative_volume_change_center, 259},
    {peak_volume_center, 515},
    {relative_volume_change_bass, 259},
    {peak_volume_bass, 515}
  ]},
  ActualRVAD4 = ExpectedRVAD4,

  ActualSYLT = v23_reader:parse_frame_bin(<<"SYLT">>, 10, Flags, <<1, "ENG", 2, 1,
  "Strang", 0, 1, 0, "ers", 0, 1, 1, " in", 0, 2, 0, " the", 0, 2, 1, " night", 0, 2, 2, 0>>),
  ExpectedSYLT = {sylt, [
    {size, 10},
    Flags,
    {encoding, 1},
    {language, "ENG"},
    {timestamp_format, utils:time_format_code_to_atom(2)},
    {content_type, utils:slt_content_type_code_to_atom(1)},
    {content_descriptor, [83, 116, 114, 97, 110, 103, 0, 1, 0, 101, 114, 115, 0, 1, 1, 32, 105, 110, 0, 2, 0, 32, 116, 104, 101, 0, 2, 1, 32, 110, 105, 103, 104, 116, 0, 2, 2]}
  ]},
  ActualSYLT = ExpectedSYLT,

  ActualSYTC = v23_reader:parse_frame_bin(<<"SYTC">>, 10, Flags, <<1, 1,2,3,4,5,6,7,8,9,0>>),
  ExpectedSYTC = {sytc, [
    {size, 10},
    Flags,
    {timestamp_format, mpeg_frames},
    {tempo_data, <<1,2,3,4,5,6,7,8,9,0>>}
  ]},
  ActualSYTC = ExpectedSYTC,

  ActualUFID = v23_reader:parse_frame_bin(<<"UFID">>, 10, Flags, <<"The Don", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9>>),
  ExpectedUFID = {ufid, [
    {size, 10},
    Flags,
    {owner_identifier, "The Don"},
    {identifier, <<1, 2, 3, 4, 5, 6, 7, 8, 9>>}
  ]},
  ActualUFID = ExpectedUFID,

  ActualUSER = v23_reader:parse_frame_bin(<<"USER">>, 10, Flags, <<1, "ENG", "This Is The TOR", 0>>),
  ExpectedUSER = {user, [
    {size, 10},
    Flags,
    {encoding, 1},
    {language, "ENG"},
    {terms_of_use, "This Is The TOR"}
  ]},
  ActualUSER = ExpectedUSER,


  ActualUSLT = v23_reader:parse_frame_bin(<<"USLT">>, 10, Flags, <<0, "SPA", "This Is A Lyrics Description", 0, "this is the lyrics">>),
  ExpectedUSLT = {uslt, [
    {size, 10},
    Flags,
    {encoding, 0},
    {language, "SPA"},
    {content_descriptor, "This Is A Lyrics Description"},
    {lyrics, "this is the lyrics"}
  ]},
  ActualUSLT  = ExpectedUSLT,

  ActualTALB = v23_reader:parse_frame_bin(<<"TALB">>, 10, Flags, <<0, "Hello TALB">>),
  ExpectedTALB = {talb, [
    {size, 10},
    Flags,
    {encoding, 0},
    {textstring, "Hello TALB"}
  ]},
  ActualTALB = ExpectedTALB,

  ActualWORS = v23_reader:parse_frame_bin(<<"WORS">>, 10, Flags, <<"http://nalyricsradio.com.ng">>),
  ExpectedWORS = {wors, [
    {size, 10},
    Flags,
    {url, "http://nalyricsradio.com.ng"}
  ]},
  ActualWORS = ExpectedWORS,

  %io:format("Actual: ~p~n", [ActualUSLT]),
  erlog:info("Testing v23_reader:parse_frame_bin/1 - passed~n").

test_parse_v22_frame_bin() ->
  ActualBUF = v22_reader:parse_frame_bin(<<"BUF">>, 8, <<12, 13, 14, 1, 1, 2, 3, 4>>),
  ExpectedBUF = {buf, [{size, 8}, {buffer_size, 789774}, {embedded_info, true}, {next_flag_offset, 16909060}]},
  ActualBUF = ExpectedBUF,

  ActualCNT = v22_reader:parse_frame_bin(<<"CNT">>, 5, <<0, 1, 2, 3, 4>>),
  ExpectedCNT = {cnt, [{size, 5}, {counter, 16909060}]},
  ActualCNT = ExpectedCNT,

  ExpectedCOM = v22_reader:parse_frame_bin(<<"COM">>, 42, <<1, $e, $n, $g, "Short Comment", 0, "This is the actual comment", " ", 0>>),
  ActualCOM = {com, [
    {size, 42},
    {language, "eng"},
    {short_description, "Short Comment"},
    {comment, "This is the actual comment"}
  ]},
  ActualCOM = ExpectedCOM,

  ActualCRA = v22_reader:parse_frame_bin(<<"CRA">>, 10, <<"http://mailto:segun@ratendate.com", 32, $a, $b, $c, 0, 1, 33, 0, 44, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedCRA = {cra, [
    {size, 10},
    {owner_id, "http://mailto:segun@ratendate.com abc"},
    {preview_start, 289},
    {preview_length, 44},
    {encryption_info, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}
  ]},
  ActualCRA = ExpectedCRA,

  ActualCRM = v22_reader:parse_frame_bin(<<"CRM">>, 10, <<"http://mailto:segun@ratendate.com", 0, "no reason", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedCRM = {crm, [
    {size, 10},
    {owner_id, "http://mailto:segun@ratendate.com"},
    {content_explanation, "no reason"},
    {encrypted_data, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}
  ]},
  ActualCRM = ExpectedCRM,

  ActualETC = v22_reader:parse_frame_bin(<<"ETC">>, 10, <<2, 16#A, 0, 0, 0, 33>>),
  ExpectedETC = {etc, [
    {size, 10},
    {time_stamp_format, milliseconds},
    {event, etc_variation},
    {timestamp, 33}
  ]},
  ActualETC = ExpectedETC,

  ActualEQU = v22_reader:parse_frame_bin(<<"EQU">>, 10, <<16, 255, 128, 0, 4>>),
  ExpectedEQU = {equ, [
    {size, 10},
    {adjustment_bits, 16},
    {inc_or_dec, increment},
    {frequency, 32640},
    {adjustment, 4}
  ]},
  ActualEQU = ExpectedEQU,

  ActualGEO = v22_reader:parse_frame_bin(<<"GEO">>, 10, <<1, "text/xhtml", 0, "hello.xml", 0, "Hello World In XML", 0, 44, 55, 66, 77, 88, 99, 100, 33, 22, 11>>),
  ExpectedGEO = {geo, [
    {size, 10},
    {encoding, 1},
    {mime_type, "text/xhtml"},
    {filename, "hello.xml"},
    {content_description, "Hello World In XML"},
    {encapsulated_object, <<44, 55, 66, 77, 88, 99, 100, 33, 22, 11>>}
  ]},
  ActualGEO = ExpectedGEO,

  ActualIPL = v22_reader:parse_frame_bin(<<"IPL">>, 10, <<0, "Director", 0, "Moses Monday", 0, "Producer", 0, "Wyte Tag", 0, "Creative Director", 0, "Aaron", 0>>),
  ExpectedIPL = {ipl, [
    {size, 10},
    {encoding, 0},
    {involvements, [
      {involvement, "Director"}, {involvee, "Moses Monday"},
      {involvement, "Producer"}, {involvee, "Wyte Tag"},
      {involvement, "Creative Director"}, {involvee, "Aaron"}
    ]}
  ]},
  ActualIPL = ExpectedIPL,

  ActualLNK = v22_reader:parse_frame_bin(<<"LNK">>, 10, <<"TOS", "http://nalyrics.com.ng", 0>>),
  ExpectedLNK = {lnk, [
    {size, 10},
    {frame_identifier, "TOS"},
    {url, "http://nalyrics.com.ng"},
    {additional_id_data, []}
  ]},
  ActualLNK = ExpectedLNK,

  ActualLNK2 = v22_reader:parse_frame_bin(<<"LNK">>, 10, <<"TOS", "http://nalyrics.com.ng", 0, "olu ", 0, "is ", 0, "a ", 0, "boy", 0>>),
  ExpectedLNK2 = {lnk, [
    {size, 10},
    {frame_identifier, "TOS"},
    {url, "http://nalyrics.com.ng"},
    {additional_id_data, ["olu", "is", "a", "boy"]}
  ]},
  ActualLNK2 = ExpectedLNK2,

  ActualMLL = v22_reader:parse_frame_bin(<<"MLL">>, 10, <<2, 3, 1, 2, 3, 3, 2, 1, 8, 16, 10, 0, 88>>),
  ExpectedMLL = {mll, [
    {size, 10},
    {frames_between_reference, 515},
    {bytes_between_reference, 66051},
    {milliseconds_between_reference, 197121},
    {bit_for_bytes_deviation, 8},
    {bits_for_milliseconds_deviation, 16},
    {deviation_in_bytes, 10},
    {deviation_in_milliseconds, 88}
  ]},
  ActualMLL = ExpectedMLL,

  ActualPIC = v22_reader:parse_frame_bin(<<"PIC">>, 128, <<1, "PNG", 2, "Wande Coal In Allow Me To Kiss Your Hand", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
  ExpectedPIC = {pic, [
    {size, 128},
    {encoding, 1},
    {image_format, "PNG"},
    {picture_type, other_file_icon},
    {description, "Wande Coal In Allow Me To Kiss Your Hand"},
    {picture_data, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>}
  ]},
  ActualPIC = ExpectedPIC,

  ActualPOP = v22_reader:parse_frame_bin(<<"POP">>, 10, <<"akintayo.segun@gmail.com", 0, 254, 0, 1, 2, 3, 4>>),
  ExpectedPOP = {pop, [
    {size, 10},
    {email_to_user, "akintayo.segun@gmail.com"},
    {rating, 254},
    {counter, 16909060}
  ]},
  ActualPOP = ExpectedPOP,

  ActualRVA = v22_reader:parse_frame_bin(<<"RVA">>, 10, <<3, 16, 0, 4, 0, 8, 0, 2, 0, 6>>),
  ExpectedRVA = {rva, [
    {size, 10},
    {inc_or_dec_right, 1},
    {inc_or_dec_left, 1},
    {bits_used_for_volume, 16},
    {relative_volume_change_right, 4},
    {relative_voilume_change_left, 8},
    {peak_volume_right, 2},
    {peak_volume_left, 6}
  ]},
  ActualRVA = ExpectedRVA,

  ActualSLT = v22_reader:parse_frame_bin(<<"SLT">>, 10, <<1, "ENG", 2, 1,
  "Strang", 0, 1, 0, "ers", 0, 1, 1, " in", 0, 2, 0, " the", 0, 2, 1, " night", 0, 2, 2, 0>>),
  ExpectedSLT = {slt, [
    {size, 10},
    {encoding, 1},
    {language, "ENG"},
    {timestamp_format, utils:time_format_code_to_atom(2)},
    {content_type, utils:slt_content_type_code_to_atom(1)},
    {content_descriptor, [83, 116, 114, 97, 110, 103, 0, 1, 0, 101, 114, 115, 0, 1, 1, 32, 105, 110, 0, 2, 0, 32, 116, 104, 101, 0, 2, 1, 32, 110, 105, 103, 104, 116, 0, 2, 2]}
  ]},
  ActualSLT = ExpectedSLT,

  ActualTEN = v22_reader:parse_frame_bin(<<"TEN">>, 10, <<0, "Naija", 0>>),
  ExpectedTen = {ten, [
    {size, 10},
    {encoding, 0},
    {textstring, "Naija"}
  ]},
  ActualTEN = ExpectedTen,

  ActualUFI = v22_reader:parse_frame_bin(<<"UFI">>, 10, <<"The Don", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9>>),
  ExpectedUFI = {ufi, [
    {size, 10},
    {owner_identifier, "The Don"},
    {identifier, <<1, 2, 3, 4, 5, 6, 7, 8, 9>>}
  ]},
  ActualUFI = ExpectedUFI,

  ActualULT = v22_reader:parse_frame_bin(<<"ULT">>, 10, <<0, "SPA", "This Is A Content Descriptor", 0, "This Is A Lyrics">>),
  %%io:format("RESULT: ~p~n", [ActualULT]),
  ExpectedULT = {ult, [
    {size, 10},
    {encoding, 0},
    {language, "SPA"},
    {content_descriptor, "This Is A Content Descriptor"},
    {lyrics_text, "This Is A Lyrics"}
  ]},
  ActualULT = ExpectedULT,

  ActualWAF = v22_reader:parse_frame_bin(<<"WAF">>, 10, <<"http://nalyrics.com.ng">>),
  ExpectedWAF = {waf, [
    {size, 10},
    {url, "http://nalyrics.com.ng"}
  ]},
  ActualWAF = ExpectedWAF,

  erlog:info("Testing v22_reader:parse_frame_bin/3 - passed~n").

test_find_v23_frame(FileHandle) ->
  {ok, FirstTen} = file:read(FileHandle, 10),
  {ok, Header} = id3_tag_reader:read_v2_header(FirstTen),
  {ok, {_ExtendedHeader, ID3Data}} = id3_tag_reader:read_v2_ex_header(FileHandle, Header),
  _Version = proplists:get_value(version, Header),
  Actual = v23_reader:find_v23_frame(<<"TYER">>, ID3Data),
  Expected = {ok, found},
  Actual = Expected,
  erlog:info("Testing v23_reader:find_v23_frame/3 - passed~n").

test_write_pic() ->
  erlp3tags:start(),
  {ok, PicData} = file:read_file("misc/pic.jpg"),
  TagValue = {apic, [
    {size, filelib:file_size("misc/pic.jpg")},
    {flags, [
      {tag_alter_preservation,true},
      {file_alter_preservation,true},
      {read_only,false},
      {compression,false},
      {encryption,false},
      {grouping_identity,false}
    ]},
    {encoding, 0},
    {mime_type,"image/jpeg"},
    {picture_type,other},
    {description,"Edited JPEG"},
    {picture_data, PicData}
  ]},

  id3_tag_writer:writeV2(v23, TagValue, "APIC", "misc/sgc.mp3"),
  id3_tag_writer:syncV2().


tests() ->
  erlog:start(),
  erlog:load_config_file("conf/erlog.conf"),
  erlog:info("~n---------------Starting Tests---------------~n"),
  test_write_pic(),
  File = filename:join("misc", "mi_one_six.mp3"),
  {ok, S} = file:open(File, [read, binary, raw]),
  test_find_v23_frame(S),
  id3_tag_reader:read_tag(File),
  test_read_header(S),
  test_synch_safe(),
  test_parse_v22_frame_bin(),
  test_parse_v23_frame_bin(),
  test_text_header_to_atom(),
  erlog:info("~n---------------Tests Finished---------------~n"),

  erlog:error("Result: ~p~n", [id3_tag_reader:read_tag(filename:join("misc", "sgc.mp3"))]),
  ok.