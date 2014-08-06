%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2014 1:14 PM
%%%-------------------------------------------------------------------
-module(erlp3tags).
-author("aardvocate").

%% API
-export([start/0]).


start() ->
  application:start(erlog),
  application:start(erlp3tags).
