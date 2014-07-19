%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2014 2:11 PM
%%%-------------------------------------------------------------------
-module(id3_tag_reader).
-author("aardvocate").

%% API
-export([read_tag/1, read_v2_header/1, synch_safe/1]).

-include("erlp3header.hrl").

read_tag(File) ->
  case file:open(File, [read, binary, raw]) of
    {ok, S} ->
      Size = filelib:file_size(File),

      erlog:info("Checking for V2"),
      {ok, IDv2Tag} = file:pread(S, 0, 3),
      case IDv2Tag of
        <<"ID3">> ->
          erlog:info("Found ID3v2"),
          parse_v2_tag(S);
        _ ->
          erlog:info("ID3v2 Not Found ~p", [IDv2Tag])
      end,

      erlog:info("Checking for V1"),
      Start = Size - 128,
      End = Start + 3,
      {ok, ID3V1Tag} = file:pread(S, Start, End),
      case ID3V1Tag of
        <<"TAG">> ->
          erlog:info("Found ID3v1"),
        parse_v1_tag(S);
        _ ->
          erlog:info("ID3v1 Not Found")
      end,
      %%Result = parse_v1_tag(ID3Tags),
      file:close(S);
    _Error ->
      error
  end.

parse_v2_tag(Filehandle) ->
  {ok, FirstTen} = file:pread(Filehandle, 0, 10),
  _Header = read_v2_header(FirstTen),
  ok.

read_v2_header(<<"ID3", MajV:8/integer, MinV:8/integer, A:1/integer, B:1/integer, C:1/integer, D:1/integer, _UnusedFlags:4, S1:8/integer, S2:8/integer, S3:8/integer, S4:8/integer>>) ->
  {Unsync, Extended, Experimental, Footer} = {A, B, C, D},
  {ok, Size} = synch_safe(<<S1, S2, S3, S4>>),

  {ok,
    [
      {version, {2, MajV, MinV}},
      {flags, [{unsync, Unsync}, {extended, Extended}, {experimental, Experimental}, {footer, Footer}]},
      {size, Size}
    ]
  };

read_v2_header(_) ->
  {error, invalid_v2_header_bytes}.

synch_safe(<<0:1/integer, S1:7/integer, 0:1/integer, S2:7/integer, 0:1/integer, S3:7/integer, 0:1/integer, S4:7/integer>>) ->
  <<Size:28/integer>> = <<S1:7/integer, S2:7/integer,S3:7/integer, S4:7/integer>>,
  {ok, Size};

synch_safe(_) ->
  {error, invalid_size_bytes}.

parse_v1_tag(<<$T, $A, $G, Title:30/binary, Artist:30/binary, Album:30/binary, Year:4/binary, Comment:28/binary,  0:1, Track:1, Genre:1>> = _Result) ->
  {ok, "ID3v1.1", #id3v1_1{tag = "ID3v1.1", title = Title, artist = Artist, album = Album, year = Year, comment = Comment, track = Track, genre = Genre}};


parse_v1_tag(<<$T, $A, $G, Title:30/binary, Artist:30/binary, Album:30/binary, Year:4/binary, Comment:30/binary, Genre:8>>) ->
  {ok, "ID3v1", #id3v1{tag = "ID3v1", title = Title, artist = Artist, album = Album, year = Year, comment = Comment, genre = Genre}}.
