-module(login_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  Content = webutil:template(code:priv_dir(unit4) ++ "/login_form.html", ["",""]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0),
  {ok, Req, State};

init(Req0=#{method := <<"POST">>}, State) ->
  case webutil:logged_in(Req0) of
    false ->
      {ok, PostVals, _} = cowboy_req:read_urlencoded_body(Req0),
      Name = proplists:get_value(<<"username">>, PostVals),
      Content = webutil:template(code:priv_dir(unit4) ++ "/login_form.html", 
        [Name,"Your user name or password is incorrect"]),
      Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0);
    Name ->
      Req = cowboy_req:reply(303, 
        #{<<"location">> => list_to_binary(io_lib:format("/welcome/~s", [Name]))}, Req0)
  end,
  {ok, Req, State}.

