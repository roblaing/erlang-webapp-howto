%%%-------------------------------------------------------------------
%% @doc unit8 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit8_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ets:new(uuids, [public, named_table]),
  Dispatch = cowboy_router:compile([{'_', % Set Host to any
    [ {"/"              , cowboy_static, {priv_file, unit8, "front.html"}}
    , {"/login"         , cowboy_static, {priv_file, unit8, "login_form.html"}}
    , {"/logout"        , cowboy_static, {priv_file, unit8, "logout.html"}}
    , {"/signup"        , cowboy_static, {priv_file, unit8, "signup_form.html"}}
    , {"/images/[...]"  , cowboy_static, {priv_dir,  unit8, "images"}}
    , {"/styles/[...]"  , cowboy_static, {priv_dir,  unit8, "styles"}}
    , {"/scripts/[...]" , cowboy_static, {priv_dir,  unit8, "scripts"}}
    , {"/blog"          , blog_handler, []}
    , {"/:post_id"      , permalink_handler, []}
    ]}]),
  persistent_term:put(dispatch, Dispatch),
  cowboy:start_clear(http_listener,
   [{port, 3030}],
   #{env => #{dispatch => {persistent_term, dispatch}}}
 ),
 unit8_sup:start_link().

stop(_State) ->
  ets:delete(uuids), 
  ok = cowboy:stop_listener(http_listener).

%% internal functions
