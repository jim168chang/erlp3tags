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
