%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2014 1:33 PM
%%%-------------------------------------------------------------------
-module(utils).
-author("aardvocate").

%% API
-compile(export_all).


to_bool(I) ->
  case I of
    1 -> true;
    _ -> false
  end.

from_bool(B) ->
  case B of
    true -> 1;
    _ -> 0
  end.

synch_safe(<<0:1/integer, S1:7/integer, 0:1/integer, S2:7/integer, 0:1/integer, S3:7/integer, 0:1/integer, S4:7/integer>>) ->
  <<Size:28/integer>> = <<S1:7/integer, S2:7/integer,S3:7/integer, S4:7/integer>>,
  {ok, Size};

synch_safe(_) ->
  {error, invalid_size_bytes}.

trim(Bin) ->
  X = list_to_binary(trim_blanks(binary_to_list(Bin))),
  X.

trim_blanks(X) ->
  lists:reverse(skip_blanks_and_zeros(lists:reverse(X))).

skip_blanks_and_zeros([32 | T]) ->
  skip_blanks_and_zeros(T);

skip_blanks_and_zeros([0 | T]) ->
  skip_blanks_and_zeros(T);

skip_blanks_and_zeros(X) ->
  X.

decode_string(BinString) ->
  unicode:characters_to_list(trim(BinString), latin1).

decode_string(1, BinString) ->
  unicode:characters_to_list(trim(BinString), unicode);

decode_string(0, BinString) ->
  unicode:characters_to_list(trim(BinString), latin1).