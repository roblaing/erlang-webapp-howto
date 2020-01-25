%%%-------------------------------------------------------------------
%% @doc unit1 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit1_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([{'_', % Set Host to any
    [ {"/"             , cowboy_static, {priv_file, unit1, "index.html"}}
    , {"/images/[...]" , cowboy_static, {priv_dir,  unit1, "images"}}
    , {"/styles/[...]" , cowboy_static, {priv_dir,  unit1, "styles"}}
    , {"/scripts/[...]", cowboy_static, {priv_dir,  unit1, "scripts"}}
    ]}]),
  persistent_term:put(unit1_routes, Dispatch),
  cowboy:start_clear(unit1_http_listener,
   [{port, 3030}],
   #{env => #{dispatch => {persistent_term, unit1_routes}}}
  ),
  unit1_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(unit1_http_listener).

%% internal functions
