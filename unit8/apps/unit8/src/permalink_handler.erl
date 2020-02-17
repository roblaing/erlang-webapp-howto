-module(permalink_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  #{bindings := #{post_id := Id}} = Req0,
  #{rows := [{Title, Created, Art}]} = 
    pgo:query("SELECT title, created, art FROM arts WHERE id = $1::integer", [binary_to_integer(Id)]),
  Content = template(code:priv_dir(unit8) ++ "/permalink.html", [Title, Title, Created, Art]),  
  Req = cowboy_req:reply(200,
    #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
     },
    Content,
    Req0
  ),
  {ok, Req, State}.

-spec template(FileName :: file:filename(), ArgList :: [string()]) -> Html :: binary().
%% @doc Reads an html file from its complete path name, and inserts strings without escaping `<' or `>'.
template(FileName, ArgList) ->
  {ok, Binary} = file:read_file(FileName),
  io_lib:format(Binary, ArgList).

