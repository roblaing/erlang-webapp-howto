%%%-------------------------------------------------------------------
%% @doc unit3 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit3_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([{'_', % Set Host to any
    [ {"/"             , cowboy_static, {priv_file, unit3, "index.html"}}
    , {"/images/[...]" , cowboy_static, {priv_dir,  unit3, "images"}}
    , {"/styles/[...]" , cowboy_static, {priv_dir,  unit3, "styles"}}
    , {"/scripts/[...]", cowboy_static, {priv_dir,  unit3, "scripts"}}
    , {"/arts"         , arts_handler, []}
    ]}]),
  persistent_term:put(unit3_routes, Dispatch),
  cowboy:start_clear(unit3_http_listener,
   [{port, 3030}],
   #{env => #{dispatch => {persistent_term, unit3_routes}}}
  ),
  unit3_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(unit3_http_listener).

%% internal functions
