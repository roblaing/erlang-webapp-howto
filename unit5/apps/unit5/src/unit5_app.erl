%%%-------------------------------------------------------------------
%% @doc unit5 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit5_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ets:new(weather_table, [public, named_table]),
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
  persistent_term:put(unit5_routes, Dispatch),
  cowboy:start_clear(unit5_http_listener,
   [{port, 3030}],
   #{env => #{dispatch => {persistent_term, unit5_routes}}}
  ),
  webutil:get_json(),
  unit5_sup:start_link().

stop(_State) ->
  ets:delete(weather_table),  
  ok = cowboy:stop_listener(unit5_http_listener).


%% internal functions
