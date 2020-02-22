-module(filenotfound_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  Req = cowboy_req:reply(404,
    #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
     },
    <<"<h1>404 Page Not Found Error</h1>">>,
    Req0
  ),
  {ok, Req, State}.

