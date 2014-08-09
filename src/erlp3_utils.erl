%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2014 1:33 PM
%%%-------------------------------------------------------------------
-module(erlp3_utils).
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
  RemoveLeading = skip_blanks_and_zeros(X),
  lists:reverse(skip_blanks_and_zeros(lists:reverse(RemoveLeading))).

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

get_null_terminated_string_from_frame_skip_zeros(FrameContent) ->
  case binary:match(FrameContent, <<0>>) of
    {Start, _Len} ->
      {NTS, Rest} = split_binary(FrameContent, Start),
      {NTS, list_to_binary(skip_blanks_and_zeros(binary_to_list(Rest)))};
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

time_format_code_to_atom(Code) ->
  case Code of
    1 -> mpeg_frames;
    2 -> milliseconds
  end.

time_format_atom_to_code(Atom) ->
  case Atom of
    mpeg_frames -> ?MPEG_FRAMES_TIME_FORMAT;
    milliseconds -> ?MILLISECONDS_TIME_FORMAT
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

slt_content_type_code_to_atom(Code) ->
  case Code of
    0 -> other;
    1 -> lyrics;
    2 -> text_transcription;
    3 -> movement;
    4 -> events;
    5 -> chord
  end.

slt_content_type_atom_to_code(Atom) ->
  case Atom of
    other -> ?SLT_CON_TYPE_OTHER;
    lyrics -> ?SLT_CON_TYPE_LYRICS;
    text_transcription -> ?SLT_CON_TYPE_TEXT_TRANSCRIPTION;
    movement -> ?SLT_CON_TYPE_MOVEMENT;
    events -> ?SLT_CON_TYPE_EVENTS;
    chord -> ?SLT_CON_TYPE_CHORD
  end.

header_to_atom(Header) ->
  LowerCase = string:to_lower(Header),
  list_to_atom(LowerCase).

reverse_boolean_code_to_atom(Code) ->
  case Code of
    1 -> false;
    _ -> true
  end.

reverse_boolean_atom_to_code(Atom) ->
  case Atom of
    false -> 1;
    true -> 0
  end.

boolean_code_to_atom(Code) ->
  case Code of
    0 -> false;
    _ -> true
  end.

boolean_atom_to_code(Atom) ->
  case Atom of
    false -> 0;
    true -> 1
  end.

pic_type_code_to_atom(Code) ->
  case Code of
    ?PIC_TYPE_ARTIST -> artist;
    ?PIC_TYPE_BAND -> band_orchestra;
    ?PIC_TYPE_BAND_LOGO_TYPE -> band_logo_type;
    ?PIC_TYPE_BCF -> bright_colored_fish;
    ?PIC_TYPE_COMPOSER -> composer;
    ?PIC_TYPE_CONDUCTOR -> conductor;
    ?PIC_TYPE_COVER_BACK -> cover_back;
    ?PIC_TYPE_COVER_FRONT -> cover_front;
    ?PIC_TYPE_DURING_PERFORMANCE -> during_performance;
    ?PIC_TYPE_DURING_RECORDING -> during_recording;
    ?PIC_TYPE_FILE_ICON -> file_icon;
    ?PIC_TYPE_ILLUSTRATION -> illustration;
    ?PIC_TYPE_LEAD_ARTIST -> lead_artist;
    ?PIC_TYPE_LEAFLET_PAGE -> leaflet_page;
    ?PIC_TYPE_LYRICIST -> lyricist;
    ?PIC_TYPE_MEDIA -> media;
    ?PIC_TYPE_MOVIE -> movie;
    ?PIC_TYPE_OTHER -> other;
    ?PIC_TYPE_OTHER_FILE_ICON -> other_file_icon;
    ?PIC_TYPE_PUBLISHER_LOGO_TYPE -> publisher_logo_type;
    ?PIC_TYPE_RECORDING_LOCATION -> recording_location
  end.

pic_type_atom_to_code(Atom) ->
  case Atom of
    artist -> ?PIC_TYPE_ARTIST;
    band_orchestra -> ?PIC_TYPE_BAND;
    bright_colored_fish -> ?PIC_TYPE_BCF;
    composer -> ?PIC_TYPE_COMPOSER;
    conductor -> ?PIC_TYPE_CONDUCTOR;
    cover_back -> ?PIC_TYPE_COVER_BACK;
    cover_front -> ?PIC_TYPE_COVER_FRONT;
    during_performance -> ?PIC_TYPE_DURING_PERFORMANCE;
    during_recording -> ?PIC_TYPE_DURING_RECORDING;
    file_icon -> ?PIC_TYPE_FILE_ICON;
    illustration -> ?PIC_TYPE_ILLUSTRATION;
    lead_artist -> ?PIC_TYPE_LEAD_ARTIST;
    leaflet_page -> ?PIC_TYPE_LEAFLET_PAGE;
    lyricist -> ?PIC_TYPE_LYRICIST;
    media -> ?PIC_TYPE_MEDIA;
    movie -> ?PIC_TYPE_MOVIE;
    other -> ?PIC_TYPE_OTHER;
    other_file_icon -> ?PIC_TYPE_OTHER_FILE_ICON;
    publisher_logo_type -> ?PIC_TYPE_PUBLISHER_LOGO_TYPE;
    recording_location -> ?PIC_TYPE_RECORDING_LOCATION
  end.

recieved_as_code_to_atom(Code) ->
  case Code of
    ?RCVD_AS_COMPRESSED_AUDIO_ON_CD -> compressed_audio_on_cd;
    ?RCVD_AS_FILE_OVER_INTERNET -> file_over_intenet;
    ?RCVD_AS_MUSIC_ON_OTHER_MEDIA -> music_on_other_media;
    ?RCVD_AS_NON_MUSICAL_MERCHANDISE -> non_musical_merchandise;
    ?RCVD_AS_NOTE_SHEETS -> note_sheets;
    ?RCVD_AS_NOTE_SHEETS_IN_A_BOOK -> note_sheets_in_a_book;
    ?RCVD_AS_OTHER -> other;
    ?RCVD_AS_STANDARD_CD_ALBUM -> standard_cd_album;
    ?RCVD_AS_STREAM_OVER_INTERNET -> stream_over_internet
  end.

recieved_as_atom_to_code(Atom) ->
  case Atom of
    compressed_audio_on_cd -> ?RCVD_AS_COMPRESSED_AUDIO_ON_CD;
    file_over_internet -> ?RCVD_AS_FILE_OVER_INTERNET;
    music_on_other_media -> ?RCVD_AS_MUSIC_ON_OTHER_MEDIA;
    non_musical_merchandise -> ?RCVD_AS_NON_MUSICAL_MERCHANDISE;
    note_sheets -> ?RCVD_AS_NOTE_SHEETS;
    note_sheets_in_a_book -> ?RCVD_AS_NOTE_SHEETS_IN_A_BOOK;
    other -> ?RCVD_AS_OTHER;
    standard_cd_album -> ?RCVD_AS_STANDARD_CD_ALBUM;
    stream_over_internet -> ?RCVD_AS_STREAM_OVER_INTERNET
  end.