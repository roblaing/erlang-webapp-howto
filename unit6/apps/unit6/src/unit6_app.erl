%%%-------------------------------------------------------------------
%% @doc unit6 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit6_app).
-behaviour(application).
-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
  ets:new(uuids, [public, named_table]),
  Dispatch = cowboy_router:compile(
   [{'_', 
     [ {"/"              , welcome_or_login_handler, []}
     , {"/login"         , login_handler, []}
     , {"/signup"        , signup_handler, []}
     , {"/welcome/:name" , welcome_handler, []}
     , {"/logout"        , logout_handler, []}
     , {"/styles/[...]"  , cowboy_static, {priv_dir,  unit6, "styles"}}
     , {"/scripts/[...]" , cowboy_static, {priv_dir,  unit6, "scripts"}}
     , {"/[...]"         , filenotfound_handler, []}
     ]
    }
   ]
  ),
  persistent_term:put(unit6_routes, Dispatch),
  cowboy:start_clear(unit6_http_listener,
   [{port, 3030}],
   #{env => #{dispatch => {persistent_term, unit6_routes}}}
  ),
  unit6_sup:start_link().

stop(_State) ->
  ets:delete(uuids), 
  ok = cowboy:stop_listener(unit6_http_listener).

%% internal functions
