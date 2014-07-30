%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jul 2014 11:43 PM
%%%-------------------------------------------------------------------
-module(v24_reader).
-author("aardvocate").

%% API

-export([read_v24/2, parse_frame_bin/4]).

read_v24(FileHandle, Header) ->
  {ok, ID3Data} = file:read(FileHandle, proplists:get_value(size, Header)),
  Result = read_v24_frame(ID3Data, []),
  Result.

read_v24_frame(<<FrameID:4/binary, S1:8/integer, S2:8/integer, S3:8/integer, S4:8/integer,
                  0:1/integer, A:1/integer, B:1/integer, C:1/integer, _Rem1:4,
                  0:1/integer, H:1/integer, 0:2/integer, K:1/integer, M:1/integer, N:1/integer, P:1/integer,
                  Rest/binary>>, Frames) when FrameID =/= <<0, 0, 0, 0>> ->
  Flags = {flags, [
    {tag_alter_preservation, utils:reverse_boolean_code_to_atom(A)},
    {file_alter_preservation, utils:reverse_boolean_code_to_atom(B)},
    {read_only, utils:boolean_code_to_atom(C)},
    {grouping_identity, utils:boolean_code_to_atom(H)},
    {compression, utils:boolean_code_to_atom(K)},
    {encryption, utils:boolean_code_to_atom(M)},
    {unsynchronization, utils:boolean_code_to_atom(N)},
    {data_length_indication, utils:boolean_code_to_atom(P)}
  ]},

  {ok, Size} = utils:synch_safe(<<S1, S2, S3, S4>>),

  {FrameContent, ID3Data} = split_binary(Rest, Size),
  Frame = parse_frame_bin(FrameID, Size, Flags, FrameContent),
  read_v24_frame(ID3Data, [Frame | Frames]);

read_v24_frame(_, Frames) ->
  lists:reverse([Frame || Frame <- Frames, Frame =/= not_yet_implemented]).

parse_frame_bin(_FID, _Size, _Flags, _FrameContent) ->
  not_yet_implemented.