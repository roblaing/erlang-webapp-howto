-module(signup_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  Content = webutil:template(code:priv_dir(unit4) ++ "/signup_form.html", ["","",""]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0),
  {ok, Req, State};

init(Req0=#{method := <<"POST">>}, State) ->
  {ok, PostVals, _} = cowboy_req:read_urlencoded_body(Req0),
  Name = proplists:get_value(<<"username">>, PostVals),
  Email = proplists:get_value(<<"email">>, PostVals),
  #{user_id := Hash} = cowboy_req:match_cookies([{user_id, [], false}], Req0),
  if
    Hash =:= false -> 
      Content = webutil:template(code:priv_dir(unit4) ++ "/signup_form.html", 
      webutil:html_escape([Name, "Oops, no cookie!", Email])),
      Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0);
    true ->  
      Query = pgo:query("INSERT INTO users (id, name, email) VALUES ($1::text, $2::text, $3::text)", 
               [webutil:create_hash(Hash), Name, Email]),
      case Query of
        {error, _} -> 
          Content = webutil:template(code:priv_dir(unit4) ++ "/signup_form.html", 
          webutil:html_escape([Name, "Sorry, that name is already taken. Please pick another.", Email])),
          Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0);
        _  -> 
          Req = cowboy_req:reply(303, #{<<"location">> => list_to_binary(io_lib:format("/welcome/~s", [Name]))}, Req0)
      end
  end,
  {ok, Req, State}.     

