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
  read_v22_frame(ID3Data, []).

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

parse_frame_bin(_Header, _Size, _FrameContent) ->
  not_yet_implemented.

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
    {time_stamp_format, utils:etc_time_format_code_to_atom(TimeStampFormat)},
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