%%%-------------------------------------------------------------------
%% @doc unit2 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit2_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([{'_', % Set Host to any
    [ {"/"             , cowboy_static, {priv_file, unit2, "index.html"}}
    , {"/images/[...]" , cowboy_static, {priv_dir,  unit2, "images"}}
    , {"/styles/[...]" , cowboy_static, {priv_dir,  unit2, "styles"}}
    , {"/scripts/[...]", cowboy_static, {priv_dir,  unit2, "scripts"}}
    , {"/form"         , form_handler, ["","","","","",""]}
    , {"/welcome/:name", welcome_handler, []}
    , {"/[...]"        , filenotfound_handler, []}
    ]}]),
  persistent_term:put(unit2_routes, Dispatch),
  cowboy:start_clear(unit2_http_listener,
   [{port, 3030}],
   #{env => #{dispatch => {persistent_term, unit2_routes}}}
  ),
  unit2_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(unit2_http_listener).

%% internal functions
