%%%-------------------------------------------------------------------
%% @doc unit7 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit7_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([{'_', % Set Host to any
    [ {"/pong"          , pong_handler, []}
    ]}]),
  persistent_term:put(dispatch, Dispatch),
  cowboy:start_clear(http,
   [{port, 3030}],
   #{env => #{dispatch => {persistent_term, dispatch}}}
  ),
  unit7_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http).

%% internal functions
