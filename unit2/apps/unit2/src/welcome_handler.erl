-module(welcome_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  Name = maps:get(name, maps:get(bindings, Req0)),
  Content = template(code:priv_dir(unit2) ++ "/welcome.html", [Name, Name]),  
  Req = cowboy_req:reply(200,
    #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
     },
    Content,
    Req0
  ),
  {ok, Req, State}.

template(FileName, ArgList) ->
  {ok, Binary} = file:read_file(FileName),
  list_to_binary(io_lib:format(Binary, ArgList)).

