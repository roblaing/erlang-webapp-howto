-module(logout_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  {ok, Content} = file:read_file(code:priv_dir(unit6) ++ "/logout.html"),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/html; charset=UTF-8">>},
    Content,
    Req0
  ),
  {ok, Req, State};

init(Req0=#{method := <<"POST">>}, State) ->
  {ok, [{JString, true}], _} = cowboy_req:read_urlencoded_body(Req0),
  Proplist = jsx:decode(JString),
  webutil:delete_uid(Proplist),
  Json = jsx:encode([{<<"result">>, true}]),
  % io:format("~p~n", [Json]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"application/json; charset=UTF-8">>},
    Json,
    Req0
    ),
  {ok, Req, State}.


