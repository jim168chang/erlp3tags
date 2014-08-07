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

-record(tag, {tag_id, value}).
-record(id3V2, {tags = [], file}).

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

-define(MPEG_FRAMES_TIME_FORMAT, 1).
-define(MILLISECONDS_TIME_FORMAT, 2).

-define(EQU_INCREMENT, 1).
-define(EQU_DECREMENT, 2).

-define(SLT_CON_TYPE_OTHER, 0).
-define(SLT_CON_TYPE_LYRICS, 1).
-define(SLT_CON_TYPE_TEXT_TRANSCRIPTION, 2).
-define(SLT_CON_TYPE_MOVEMENT, 3).
-define(SLT_CON_TYPE_EVENTS, 4).
-define(SLT_CON_TYPE_CHORD, 5).

-define(PIC_TYPE_OTHER, 0).
-define(PIC_TYPE_FILE_ICON, 1).
-define(PIC_TYPE_OTHER_FILE_ICON, 2).
-define(PIC_TYPE_COVER_FRONT, 3).
-define(PIC_TYPE_COVER_BACK, 4).
-define(PIC_TYPE_LEAFLET_PAGE, 5).
-define(PIC_TYPE_MEDIA, 6).
-define(PIC_TYPE_LEAD_ARTIST, 7).
-define(PIC_TYPE_ARTIST, 8).
-define(PIC_TYPE_CONDUCTOR, 9).
-define(PIC_TYPE_BAND, 16#A).
-define(PIC_TYPE_COMPOSER, 16#B).
-define(PIC_TYPE_LYRICIST, 16#C).
-define(PIC_TYPE_RECORDING_LOCATION, 16#D).
-define(PIC_TYPE_DURING_RECORDING, 16#E).
-define(PIC_TYPE_DURING_PERFORMANCE, 16#F).
-define(PIC_TYPE_MOVIE, 16#10).
-define(PIC_TYPE_BCF, 16#11).
-define(PIC_TYPE_ILLUSTRATION, 16#12).
-define(PIC_TYPE_BAND_LOGO_TYPE, 16#13).
-define(PIC_TYPE_PUBLISHER_LOGO_TYPE, 16#14).

-define(RCVD_AS_OTHER, 0).
-define(RCVD_AS_STANDARD_CD_ALBUM, 1).
-define(RCVD_AS_COMPRESSED_AUDIO_ON_CD, 2).
-define(RCVD_AS_FILE_OVER_INTERNET, 3).
-define(RCVD_AS_STREAM_OVER_INTERNET, 4).
-define(RCVD_AS_NOTE_SHEETS, 5).
-define(RCVD_AS_NOTE_SHEETS_IN_A_BOOK, 6).
-define(RCVD_AS_MUSIC_ON_OTHER_MEDIA, 7).
-define(RCVD_AS_NON_MUSICAL_MERCHANDISE, 8).

-define(ID3V1, v1).
-define(ID3V1_1, v11).
-define(ID3V2, v2).
-define(ID3V2_3, v23).
-define(ID3V2_4, v24).