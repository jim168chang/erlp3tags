
-module(erlp3tags_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  RestartStategy = one_for_one,
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 1000,
  Type = worker,

  WriterServerChildDef = {"ID3 Writer Server", {id3_tag_writer, start_link, []}, Restart, Shutdown, Type, [id3_tag_writer]},

  {ok, {SupFlags, [WriterServerChildDef]}}.

