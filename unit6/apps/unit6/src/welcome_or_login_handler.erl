-module(welcome_or_login_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  case webutil:logged_in(Req0) of
    false -> 
      Req = cowboy_req:reply(303, 
        #{<<"location">> => <<"/login">>}, Req0);
    Name ->
      Req = cowboy_req:reply(303, 
        #{<<"location">> => list_to_binary(io_lib:format("/welcome/~s", [Name]))}, Req0)
  end,
  {ok, Req, State}.

