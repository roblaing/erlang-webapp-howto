%%%-------------------------------------------------------------------
%% @doc unit4 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit4_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile(
   [{'_', 
     [ {"/"              , welcome_or_login_handler, []}
     , {"/login"         , login_handler, []}
     , {"/signup"        , signup_handler, []}
     , {"/welcome/:name" , welcome_handler, []}
     , {"/logout"        , cowboy_static, {priv_file, unit4, "logout.html"}}
     , {"/styles/[...]"  , cowboy_static, {priv_dir,  unit4, "styles"}}
     , {"/scripts/[...]" , cowboy_static, {priv_dir,  unit4, "scripts"}}
     , {"/[...]"         , filenotfound_handler, []}
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
