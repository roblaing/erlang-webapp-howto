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
  Hash = proplists:get_value(<<"user_id">>, Proplist),
  #{num_rows := Rows} = pgo:query("SELECT name FROM users WHERE id = $1::text", [webutil:create_hash(Hash)]),
  case Rows of
    0 -> Content = jsx:encode([{<<"logged_in">>, false}]);
    1 -> Content = jsx:encode([{<<"logged_in">>, true}])
  end,
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"application/json; charset=UTF-8">>},
    Content,
    Req0
    ),
  {ok, Req, State}.

