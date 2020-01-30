%%%-------------------------------------------------------------------
%% @doc unit5 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit5_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile(
   [{'_', 
     [ {"/"              , cowboy_static, {priv_file, unit5, "index.html"}}
     , {"/weather"       , weather_handler, []}
     , {"/styles/[...]"  , cowboy_static, {priv_dir,  unit5, "styles"}}
     , {"/scripts/[...]" , cowboy_static, {priv_dir,  unit5, "scripts"}}
     ] 
    }
   ]
  ),
  persistent_term:put(unit4_routes, Dispatch),
  cowboy:start_clear(unit4_http_listener,
   [{port, 3030}],
   #{env => #{dispatch => {persistent_term, unit4_routes}}}
  ),
  unit4_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(unit4_http_listener).


%% internal functions
