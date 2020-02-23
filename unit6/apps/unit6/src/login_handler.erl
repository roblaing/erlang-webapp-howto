-module(login_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  {ok, Content} = file:read_file(code:priv_dir(unit6) ++ "/login_form.html"),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0),
  {ok, Req, State};

init(Req0=#{method := <<"POST">>}, State) ->
  {ok, [{JString, true}], _} = cowboy_req:read_urlencoded_body(Req0),
  Proplist = jsx:decode(JString),
  Json = jsx:encode([{<<"uuid">>, webutil:getuuid(Proplist)}]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"application/json; charset=UTF-8">>},
    Json,
    Req0
    ),
  {ok, Req, State}.

