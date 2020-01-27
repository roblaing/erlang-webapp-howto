-module(arts_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  Rows = maps:get(rows, pgo:query("SELECT title, art FROM arts ORDER BY created DESC")),
  Content = template(code:priv_dir(unit3) ++ "/arts.html",
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
      EscapedTitle = string:replace(Title, "'", "''", all),
      EscapedArt = string:replace(Art, "'", "''", all),
      Query = io_lib:format("INSERT INTO arts (title, art) VALUES ('~s', '~s')", [EscapedTitle, EscapedArt]),
      pgo:query(Query),
      Req = cowboy_req:reply(303, #{<<"location">> => <<"/arts">>}, Req0),
      {ok, Req, State};      
    true ->
     Rows = maps:get(rows, pgo:query("SELECT title, art FROM arts ORDER BY created DESC")),
     Content = template(code:priv_dir(unit3) ++ "/arts.html", 
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
", html_escape([Title, Art])) end, "", Rows).

html_escape(ArgList) ->
  lists:map(fun(Html) -> 
      string:replace(string:replace(Html, "<", "&lt;", all), ">", "&gt;", all)
    end, 
    ArgList).

template(FileName, ArgList) ->
  {ok, Binary} = file:read_file(FileName),
  list_to_binary(io_lib:format(Binary, ArgList)).


