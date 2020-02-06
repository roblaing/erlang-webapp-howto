-module(arts_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  #{rows := Rows} = pgo:query("SELECT title, art FROM arts ORDER BY created DESC"),
  Content = webutil:template(code:priv_dir(unit3) ++ "/arts.html",
    ["", "", "", "", title_art(Rows)]),
  Req = cowboy_req:reply(200,
    #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
     },
    Content,
    Req0
  ),
  {ok, Req, State};

init(Req0=#{method := <<"POST">>}, State) ->
  {ok, PostVals, _} = cowboy_req:read_urlencoded_body(Req0),
  Title = proplists:get_value(<<"title">>, PostVals),
  if 
    byte_size(Title) > 0 -> TitleError = "";
    true -> TitleError = "Oops, no title!"
  end,
  Art = proplists:get_value(<<"art">>, PostVals),
  if 
    byte_size(Art) > 0 -> ArtError = "";
    true -> ArtError = "Oops, no art!"
  end,
  if
    TitleError =:= "", ArtError =:= "" ->
      pgo:query("INSERT INTO arts (title, art) VALUES ($1::text, $2::text)", [Title, Art]),
      Req = cowboy_req:reply(303, #{<<"location">> => <<"/arts">>}, Req0),
      {ok, Req, State};      
    true ->
     #{rows := Rows} = pgo:query("SELECT title, art FROM arts ORDER BY created DESC"),
     Content = webutil:template(code:priv_dir(unit3) ++ "/arts.html", 
       [Title, TitleError, Art, ArtError, title_art(Rows)]
      ),
      Req = cowboy_req:reply(200,
        #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
         },
        Content,
        Req0
      ),
      {ok, Req, State}    
  end.

-spec title_art([{Title::string(), Art::string()}]) -> Html::string().
%% @doc Convert the title and art entries read from the arts table into HTML.
title_art(Rows) ->
  lists:foldl(fun({Title, Art}, Html) ->
    Html ++ io_lib:format("
    <h2>~s</h2>
    <pre>
~s
    </pre>
", webutil:html_escape([Title, Art])) end, "", Rows).



