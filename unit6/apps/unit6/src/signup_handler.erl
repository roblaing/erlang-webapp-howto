-module(signup_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  {ok, Content} = file:read_file(code:priv_dir(unit6) ++ "/signup_form.html"),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0),
  {ok, Req, State};

init(Req0=#{method := <<"POST">>}, State) ->
  {ok, [{JString, true}], _} = cowboy_req:read_urlencoded_body(Req0),
  Proplist = jsx:decode(JString),
  Name = proplists:get_value(<<"username">>, Proplist),
  #{num_rows := Rows} = pgo:query("SELECT name FROM users WHERE name=$1::text", [Name]),
  case Rows of
    0 -> Hash = proplists:get_value(<<"user_id">>, Proplist),
         Email = proplists:get_value(<<"email">>, Proplist),
         pgo:query("INSERT INTO users (id, name, email) VALUES ($1::text, $2::text, $3::text)", 
           [webutil:create_hash(Hash), Name, Email]),
         Json = jsx:encode([{<<"uuid">>, webutil:getuuid(Proplist)}]);                
    1 -> Json = jsx:encode([{<<"uuid">>, false}])
  end,
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"application/json; charset=UTF-8">>},
    Json,
    Req0
  ),
  {ok, Req, State}.


