%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2014 1:48 AM
%%%-------------------------------------------------------------------
-module(v22_reader).
-author("aardvocate").

%% API
-export([read_v22/2, parse_frame_bin/3]).
-include("erlp3header.hrl").

read_v22(FileHandle, Header) ->
  {ok, ID3Data} = file:read(FileHandle, proplists:get_value(size, Header)),
  Result = read_v22_frame(ID3Data, []),
  Result.

read_v22_frame(<<FrameID:3/binary, Size:24/integer, Rest/binary>>, Frames) ->
  {FrameContent, ID3Data} = split_binary(Rest, Size),
  Frame = parse_frame_bin(FrameID, Size, FrameContent),
  read_v22_frame(ID3Data, [Frame | Frames]);

read_v22_frame(_, Frames) ->
  lists:reverse([Frame || Frame <- Frames, Frame =/= not_yet_implemented]).

parse_frame_bin(<<"BUF">>, _Size, <<BufferSize:24/integer, 1:8/integer, NextFlagOffset:32/integer>>) ->
  {buf, [{buffer_size, BufferSize}, {embedded_info, true}, {next_flag_offset, NextFlagOffset}]};

parse_frame_bin(<<"BUF">>, _Size, <<BufferSize:24/integer, 0:8/integer>>) ->
  {buf, [{buffer_size, BufferSize}, {embedded_info, false}]};

parse_frame_bin(<<"CNT">>, Size, FrameContent) ->
  Bits = Size * 8,
  <<PlayCount:Bits/integer>> = FrameContent,
  {cnt, PlayCount};

parse_frame_bin(<<"COM">>, _Size, FrameContent) ->
  {com, parse_com_content(FrameContent)};

parse_frame_bin(<<"CRA">>, _Size, FrameContent) ->
  {cra, parse_cra_content(FrameContent)};

parse_frame_bin(<<"CRM">>, _Size, FrameContent) ->
  {crm, parse_crm_content(FrameContent)};

parse_frame_bin(<<"ETC">>, _Size, FrameContent) ->
  {etc, parse_etc_content(FrameContent)};

parse_frame_bin(<<"EQU">>, _Size, FrameContent) ->
  {equ, parse_equ_content(FrameContent)};

parse_frame_bin(<<"GEO">>, _Size, FrameContent) ->
  {geo, parse_geo_content(FrameContent)};

parse_frame_bin(<<"IPL">>, _Size, FrameContent) ->
  {ipl, parse_ipl_content(FrameContent)};

parse_frame_bin(<<"LNK">>, _Size, FrameContent) ->
  {lnk, parse_lnk_content(FrameContent)};

parse_frame_bin(<<"MCI">>, _Size, <<TOC/binary>>) ->
  {mci, [{table_of_contents, TOC}]};

parse_frame_bin(<<"MLL">>, _Size, <<FBR:16/integer, BBR:24/integer, MBR:24/integer, BBD:8/integer, BMD:8/integer>>) ->
  {mll, [
    {frames_between_reference, FBR},
    {bytes_between_reference, BBR},
    {milliseconds_between_reference, MBR},
    {bit_for_bytes_deviation, BBD},
    {bits_for_milliseconds_deviation, BMD}
  ]};

parse_frame_bin(<<"POP">>, _Size, FrameContent) ->
  {pop, parse_pop_content(FrameContent)};

parse_frame_bin(<<"PIC">>, _Size, FrameContent) ->
  {pic, parse_pic_content(FrameContent)};

parse_frame_bin(<<"REV">>, _Size, <<RL:16/integer, RR:16/integer,
RBL:8/integer, RBR:8/integer, RFLTL:8/integer, RFLTR:8/integer,
RFRTR:8/integer, RFRTL:8/integer, PLTR:8/integer, PRTL:8/integer>>) ->
  {rev, [
    {reveb_left, RL},
    {reverb_right, RR},
    {reverb_bounces_left, RBL},
    {reverb_bounces_right, RBR},
    {reverb_feedback_left_to_left, RFLTL},
    {reverb_feedback_left_to_right, RFLTR},
    {reverb_feedback_right_to_right, RFRTR},
    {reverb_feedback_right_to_left, RFRTL},
    {premix_left_to_right, PLTR},
    {premix_right_to_left, PRTL}
  ]};

parse_frame_bin(<<"RVA">>, _Size, <<IncR:4/integer, IncL:4/integer, Len:8/integer, RVCR:Len/integer, RVCL:Len/integer, PVR:Len/integer, PVL:Len/integer>>) ->
  {rva, [
    {inc_or_dec_right, IncR},
    {inc_or_dec_left, IncL},
    {bits_used_for_volume, Len},
    {relative_volume_change_right, RVCR},
    {relative_voilume_change_left, RVCL},
    {peak_volume_right, PVR},
    {peak_volume_left, PVL}
  ]};

parse_frame_bin(<<"SLT">>, _Size, <<Enc:8/integer, Language:3/binary, TimeStampFormat:8/integer, ContentType:8/integer, Rest/binary>>) ->
  {slt, [
    {encoding, Enc},
    {language, utils:decode_string(Language)},
    {timestamp_format, utils:time_format_code_to_atom(TimeStampFormat)},
    {content_type, utils:slt_content_type_code_to_atom(ContentType)},
    {content_descriptor, utils:decode_string(Enc, Rest)}
  ]};

parse_frame_bin(<<"STC">>, _Size, <<TimeStampFormat:8/integer, TempoData/binary>>) ->
  {stc, [
    {timestamp_format, utils:time_format_code_to_atom(TimeStampFormat)},
    {tempo_data, TempoData}
  ]};

parse_frame_bin(<<$T, _T2:1/binary, _T3:1/binary>> = Header, _Size, <<Enc:8/integer, Rest/binary>>) ->
  TextData = case utils:get_null_terminated_string_from_frame(Rest) of
    {Data, _Rem} ->
      Data;
    _ ->
      Rest
  end,
  {utils:header_to_atom(utils:decode_string(Header)), [
    {encoding, Enc},
    {textstring, utils:decode_string(Enc, TextData)}
  ]};

parse_frame_bin(<<$W, _W2:1/binary, _W3:1/binary>> = Header, _Size, <<URL/binary>>) ->
  {utils:header_to_atom(utils:decode_string(Header)), [
    {url, utils:decode_string(URL)}
  ]};

parse_frame_bin(<<"UFI">>, _Size, FrameContent) ->
  {ufi, parse_ufi_content(FrameContent)};

parse_frame_bin(<<"ULT">>, _Size, <<Enc:8/integer, Lang:3/binary, Rest/binary>>) ->
  case utils:get_null_terminated_string_from_frame(Rest) of
    {ContentDesc, Lyrics} ->
      {ult, [
        {encoding, Enc},
        {language, utils:decode_string(Enc, Lang)},
        {content_descriptor, utils:decode_string(Enc, ContentDesc)},
        {lyrics_text, utils:decode_string(Enc, Lyrics)}
      ]};
    _ ->
      invalid_bytes_detected
  end;

parse_frame_bin(_Header, _Size, _FrameContent) ->
  not_yet_implemented.

parse_ufi_content(FrameContent) ->
  case utils:get_null_terminated_string_from_frame(FrameContent) of
    {OwnerID, Identitifer} ->
      [
        {owner_identifier, utils:decode_string(OwnerID)},
        {identifier, Identitifer}
      ]
  end.

parse_pop_content(FrameContent) ->
  case utils:get_null_terminated_string_from_frame(FrameContent) of
    {Email, Rest} ->
      <<Rating:8/integer, CounterBin/binary>> = Rest,
      Len = length(binary_to_list(CounterBin)) * 8,
      <<Counter:Len/integer>> = CounterBin,
      [
        {email_to_user, utils:decode_string(Email)},
        {rating, Rating},
        {counter, Counter}
      ];
    _ ->
      invalid_bytes_detected
  end.

parse_pic_content(<<Enc:8/integer, ImageFormat:3/binary, PicType:8/integer, Rest/binary>>) ->
  case utils:get_null_terminated_string_from_frame(Rest) of
    {Description, PictureData} ->
      [
        {encoding, Enc},
        {image_format, utils:decode_string(ImageFormat)},
        {picture_type, PicType},
        {description, utils:decode_string(Enc, Description)},
        {picture_data, PictureData}
      ];
    _ ->
      invalid_bytes_detected
  end.

parse_lnk_content(<<LNKEDFrame:3/binary, Rest/binary>>) ->
  LinkedFrameID = utils:decode_string(LNKEDFrame),
  case utils:get_null_terminated_string_from_frame(Rest) of
    {URL, RestAfterURL} ->
      [
        {frame_identifier, LinkedFrameID},
        {url, utils:decode_string(URL)},
        {additional_id_data, get_additional_id_data(RestAfterURL, [])}
      ];
    _ ->
      invalid_bytes_detected
  end.

get_additional_id_data(LinkedContent, Acc) ->
  case utils:get_null_terminated_string_from_frame(LinkedContent) of
    {IDData, Rest} ->
      Acc2 = [utils:decode_string(IDData) | Acc],
      get_additional_id_data(Rest, Acc2);
    _ ->
      lists:reverse(Acc)
  end.

parse_ipl_content(<<Encoding:8/integer, Involvements/binary>>) ->
  get_ipls(Encoding, Involvements, []).

get_ipls(Encoding, Involvements, Acc) ->
  case utils:get_null_terminated_string_from_frame(Involvements) of
    {Involvement, RestAfterInvolvement} ->
      case utils:get_null_terminated_string_from_frame(RestAfterInvolvement) of
        {Involvee, Rest} ->
          Acc2 = [{ip, {involvement, utils:decode_string(Encoding, Involvement)}, {involvee, utils:decode_string(Encoding, Involvee)}} | Acc],
          get_ipls(Encoding, Rest, Acc2);
        _ ->
          invalid_bytes_detected
      end;
    _ ->
      lists:reverse(Acc)
  end.

parse_geo_content(<<Encoding:8/integer, Rest/binary>>) ->
  case utils:get_null_terminated_string_from_frame(Rest) of
    {MimeType, RestAfterMimeType} ->
      case utils:get_null_terminated_string_from_frame(RestAfterMimeType) of
        {Filename, RestAfterFilename} ->
          case utils:get_null_terminated_string_from_frame(RestAfterFilename) of
            {ContentDesc, EncapsulatedObject} ->
              [
                {encoding, Encoding},
                {mime_type, utils:decode_string(0, MimeType)},
                {filename, utils:decode_string(0, Filename)},
                {content_description, utils:decode_string(Encoding, ContentDesc)},
                {encapsulated_object, EncapsulatedObject}
              ];
            _ ->
              invalid_bytes_detected
          end;
        _ ->
          invalid_bytes_detected
      end;
    _ ->
      invalid_bytes_detected
  end.

parse_equ_content(<<AdjBits:8/integer, IncOrDec:1, Frequency:15, Rest/binary>>) ->
  <<Adjustment:AdjBits/integer>> = Rest,
  [
    {adjustment_bits, AdjBits},
    {inc_or_dec, utils:equ_inc_dec_code_to_atom(IncOrDec)},
    {frequency, Frequency},
    {adjustment, Adjustment}
  ].

parse_etc_content(<<TimeStampFormat:8/integer, EventCode:8/integer, TimeStamp:32/integer>>) ->
  [
    {time_stamp_format, utils:time_format_code_to_atom(TimeStampFormat)},
    {event, utils:etc_event_code_to_atom(EventCode)},
    {timestamp, TimeStamp}
  ].

parse_crm_content(FrameContent) ->
  case utils:get_null_terminated_string_from_frame(FrameContent) of
    {OwnerID, Rem} ->
      case utils:get_null_terminated_string_from_frame(Rem) of
        {ConExp, EncryptedData} ->
          [
            {owner_id, utils:decode_string(OwnerID)},
            {content_explanation, utils:decode_string(ConExp)},
            {encrypted_data, EncryptedData}
          ];
        _ ->
          invalid_bytes_detected
      end;
    _ ->
      invalid_bytes_detected
  end.

parse_cra_content(FrameContent) ->
  case utils:get_null_terminated_string_from_frame(FrameContent) of
    {OwnerID, Rest} ->
      <<PreviewStart:16/integer, PreviewLength:16/integer, EncryptionInfo/binary>> = Rest,
      [
        {owner_id, utils:decode_string(OwnerID)},
        {preview_start, PreviewStart},
        {preview_length, PreviewLength},
        {encryption_info, EncryptionInfo}
      ];
    _ ->
      invalid_bytes_detected
  end.

parse_com_content(<<Enc:8/integer, Language:3/binary, Rest/binary>>) ->
  case utils:get_null_terminated_string_from_frame(Rest) of
    {ShortDesc, Comment} ->
      [
        {language, utils:decode_string(Enc, Language)},
        {short_description, utils:decode_string(Enc, ShortDesc)},
        {comment, utils:decode_string(Enc, Comment)}
      ];
    _ ->
      invalid_bytes_detected
  end.