-module(signup_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  Content = webutil:template(code:priv_dir(unit4) ++ "/signup_form.html", ["","",""]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0),
  {ok, Req, State};

init(Req0=#{method := <<"POST">>}, State) ->
  {ok, PostVals, _} = cowboy_req:read_urlencoded_body(Req0),
  Name = proplists:get_value(<<"username">>, PostVals),
  Email = proplists:get_value(<<"email">>, PostVals),
  EscapedName = string:replace(Name, "'", "''", all),
  Query = io_lib:format("SELECT name FROM users WHERE name='~s'", [EscapedName]),
  QueryMap = pgo:query(Query),
  case maps:get(num_rows, QueryMap) of
    0 -> add_user(Req0, Name, Email),
         Req = cowboy_req:reply(303, 
           #{<<"location">> => list_to_binary(io_lib:format("/welcome/~s", [Name]))}, Req0);  
    1 -> Content = webutil:template(code:priv_dir(unit4) ++ "/signup_form.html", 
                   webutil:html_escape([Name, "Sorry, that name is already taken. Please pick another.", Email])),
         Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0)
  end,
  {ok, Req, State}.

-spec add_user(Req :: cowboy_req:req(), Name :: string(), Email :: string()) -> ok | {error, nocookie}.
%% @doc Insert a row into users tables. Assumes the name entry has already been screened for duplicates.
add_user(Req, Name, Email) ->
  #{user_id := Hash} = cowboy_req:match_cookies([{user_id, [], false}], Req),
  if
    Hash =:= false -> {error, nocookie};
    true ->
      Id = webutil:create_hash(Hash),
      EscapedName = string:replace(Name, "'", "''", all),
      EscapedEmail = string:replace(Email, "'", "''", all),
      Query = io_lib:format("INSERT INTO users (id, name, email) VALUES ('~s', '~s', '~s')", 
        [Id, EscapedName, EscapedEmail]),
      pgo:query(Query)
  end.

