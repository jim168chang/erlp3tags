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

parse_frame_bin(_Header, _Size, _FrameContent) ->
  not_yet_implemented.

parse_cra_content(FrameContent) ->
  case binary:match(FrameContent, <<0>>) of
    {Start, _Len} ->
      {OwnerID, Rest} = split_binary(FrameContent, Start),
      <<0:8/integer, PreviewStart:16/integer, PreviewLength:16/integer, EncryptionInfo/binary>> = Rest,
      [
        {owner_id, utils:decode_string(OwnerID)},
        {preview_start, PreviewStart},
        {preview_length, PreviewLength},
        {encryption_info, EncryptionInfo}
      ];
    nomatch ->
      invalid_bytes_detected
  end.

parse_com_content(<<Enc:8/integer, Language:3/binary, Rest/binary>>) ->
  case binary:match(Rest, <<0>>) of
    {Start, _Len} ->
      {ShortDesc, C} = split_binary(Rest, Start),
      <<0:8/integer, Comment/binary>> = C,
      [
        {language, utils:decode_string(Enc, Language)},
        {short_description, utils:decode_string(Enc, ShortDesc)},
        {comment, utils:decode_string(Enc, Comment)}
      ];
    nomatch ->
      invalid_bytes_detected
  end.