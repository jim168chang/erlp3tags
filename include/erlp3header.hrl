%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2014 1:22 PM
%%%-------------------------------------------------------------------
-author("aardvocate").

-define(ID3Size, 128).

-record(id3v1, {tag, title, artist, album, year, comment, genre}).
-record(id3v1_1, {tag, title, artist, album, year, comment, track, genre}).

-define(ETC_END_INITIAL_SILENCE, 1).
-define(ETC_INTRO_START, 2).
-define(ETC_MAIN_PART_START, 3).
-define(ETC_OUTRO_START, 4).
-define(ETC_OUTRO_END, 5).
-define(ETC_VERSE_BEGIN, 6).
-define(ETC_REFRAIN_BEGIN, 7).
-define(ETC_INTERLUDE, 8).
-define(ETC_THEME_START, 9).
-define(ETC_VARIATION, 16#A).
-define(ETC_KEY_CHANGE, 16#B).
-define(ETC_TIME_CHANGE, 16#C).
-define(ETC_UNWANTED_NOISE, 16#D).
-define(ETC_AUDIO_END, 16#FD).
-define(ETC_AUDIO_FILE_END, 16#FE).

-define(ETC_MPEG_FRAMES, 1).
-define(ETC_MILLISECONDS, 2).

-define(EQU_INCREMENT, 1).
-define(EQU_DECREMENT, 2).