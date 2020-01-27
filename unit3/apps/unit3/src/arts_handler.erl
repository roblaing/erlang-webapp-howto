-module(arts_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  Rows = maps:get(rows, pgo:query("SELECT title, art FROM arts ORDER BY created DESC")),
  Content = template(code:priv_dir(unit3) ++ "/arts.html",
    ["", "", "", "", title_art(Rows, "")]),
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
       [Title, TitleError, Art, ArtError, title_art(Rows, "")]
      ),
      Req = cowboy_req:reply(200,
        #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
         },
        Content,
        Req0
      ),
      {ok, Req, State}    
  end.

-spec title_art([{Title::string(), Art::string()}], Html0::string()) -> Html1::string().
%% @doc Convert the title and art entries read from the arts table into HTML.
title_art([], Html) -> Html;
title_art([{Title, Art}|Tail], Html0) ->
    Html1 = io_lib:format("
~s
    <h2>~s</h2>
    <pre>
~s
    </pre>
", [Html0, Title, Art]),
    title_art(Tail, Html1).

template(FileName, ArgList) ->
  {ok, Binary} = file:read_file(FileName),
  list_to_binary(io_lib:format(Binary, ArgList)).


