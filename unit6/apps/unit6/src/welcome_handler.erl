-module(welcome_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  #{bindings := #{name := Name}} = Req0,
  {ok, Content} = file:read_file(code:priv_dir(unit6) ++ "/welcome.html"),  
  Req = cowboy_req:reply(200,
    #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
     },
    Content,
    Req0
  ),
  {ok, Req, State}.


