-module(welcome_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  #{bindings := #{name := Name}} = Req0,
  Content = webutil:template(code:priv_dir(unit4) ++ "/welcome.html", 
    webutil:html_escape([Name, Name, Name])),  
  Req = cowboy_req:reply(200,
    #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
     },
    Content,
    Req0
  ),
  {ok, Req, State}.


