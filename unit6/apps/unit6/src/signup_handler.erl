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
  EscapedName = string:replace(Name, "'", "''", all),
  Query1 = io_lib:format("SELECT name FROM users WHERE name='~s'", [EscapedName]),
  PGMap = pgo:query(Query1),
  #{num_rows := Rows} = PGMap,
  case Rows of
    0 -> Content = jsx:encode([{<<"logged_in">>, true}]),
         Hash = proplists:get_value(<<"user_id">>, Proplist),
         Id = webutil:create_hash(Hash),
         Email = proplists:get_value(<<"email">>, Proplist),
         EscapedEmail = string:replace(Email, "'", "''", all),
         Query2 = io_lib:format("INSERT INTO users (id, name, email) VALUES ('~s', '~s', '~s')", 
           [Id, EscapedName, EscapedEmail]),
         pgo:query(Query2);                  
    1 -> Content = jsx:encode([{<<"logged_in">>, false}])
  end,
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"application/json; charset=UTF-8">>},
    Content,
    Req0
  ),
  {ok, Req, State}.


