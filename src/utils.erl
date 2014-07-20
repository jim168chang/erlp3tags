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
-include("erlp3header.hrl").

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

get_null_terminated_string_from_frame(FrameContent) ->
  case binary:match(FrameContent, <<0>>) of
    {Start, _Len} ->
      {NTS, Rest} = split_binary(FrameContent, Start),
      <<0:8/integer, Rem/binary>> = Rest,
      {NTS, Rem};
    nomatch ->
      invalid_bytes_detected
  end.

etc_event_code_to_atom(Code) ->
  case Code of
    1 -> etc_end_initial_silence;
    2 -> etc_intro_start;
    3 -> etc_main_part_start;
    4 -> etc_outro_start;
    5 -> etc_outro_end;
    6 -> etc_verse_begin;
    7 -> etc_refrain_begin;
    8 -> etc_interlude;
    9 -> etc_theme_start;
    16#A -> etc_variation;
    16#B -> etc_key_change;
    16#C -> etc_time_change;
    16#D -> etc_unwanted_noise;
    16#FD -> etc_audio_end;
    16#FE -> etc_audio_file_end;
    _ -> etc_user_defined
  end.

etc_event_atom_to_code(Atom) ->
  case Atom of
    etc_end_initial_silence -> ?ETC_END_INITIAL_SILENCE;
    etc_intro_start -> ?ETC_INTRO_START;
    etc_main_part_start -> ?ETC_MAIN_PART_START;
    etc_outro_start -> ?ETC_OUTRO_START;
    etc_outro_end -> ?ETC_OUTRO_END;
    etc_verse_begin -> ?ETC_VERSE_BEGIN;
    etc_refrain_begin -> ?ETC_REFRAIN_BEGIN;
    etc_interlude -> ?ETC_INTERLUDE;
    etc_theme_start -> ?ETC_THEME_START;
    etc_variation -> ?ETC_VARIATION;
    etc_key_change -> ?ETC_KEY_CHANGE;
    etc_time_change -> ?ETC_TIME_CHANGE;
    etc_unwanted_noise -> ?ETC_UNWANTED_NOISE;
    etc_audio_end -> ?ETC_AUDIO_END;
    etc_audio_file_end -> ?ETC_AUDIO_FILE_END
  end.

etc_time_format_code_to_atom(Code) ->
  case Code of
    1 -> mpeg_frames;
    2 -> milliseconds
  end.

etc_time_format_atom_to_code(Atom) ->
  case Atom of
    mpeg_frames -> ?ETC_MPEG_FRAMES;
    milliseconds -> ?ETC_MILLISECONDS
  end.

equ_inc_dec_code_to_atom(Code) ->
  case Code of
    1 -> increment;
    0 -> decrement
  end.

equ_inc_dec_atom_to_code(Atom) ->
  case Atom of
    increment -> 1;
    decrement -> 0
  end.