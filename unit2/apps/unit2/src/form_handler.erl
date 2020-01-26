-module(form_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  Content = template("/home/roblaing/tuts/erlang/cowboy_eg/apps/cowboy_eg/priv/form.html", State),
  Req = cowboy_req:reply(200,
    #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
     },
    Content,
    Req0
  ),
  {ok, Req, State};

init(Req0=#{method := <<"POST">>}, State) ->
  {ok, PostVals, _} = cowboy_req:read_urlencoded_body(Req0),
  Name = proplists:get_value(<<"user_name">>, PostVals),
  if 
    byte_size(Name) > 0 -> NameError = '';
    true -> NameError = "Please enter your name"
  end,
  Email = proplists:get_value(<<"user_mail">>, PostVals),
  if
    byte_size(Email) > 0 -> EmailError = '';
    true -> EmailError = "Please enter your email"
  end,
  Message = proplists:get_value(<<"user_message">>, PostVals),
  if
    byte_size(Message) > 0 -> MessageError = '';
    true -> MessageError = "Please enter a message"
  end,
  if 
    NameError =:= '', EmailError =:= '', MessageError =:= '' ->
      Req = cowboy_req:reply(303,
        #{ <<"location">> => list_to_binary("/welcome/" ++ io_lib:format("~s", [Name]))
         },
        Req0
      ),
      {ok, Req, State};
	  true ->
      Content = template("/home/roblaing/tuts/erlang/cowboy_eg/apps/cowboy_eg/priv/form.html", 
       [ binary_to_atom(Name, utf8)
       , NameError
       , binary_to_atom(Email, utf8)
       , EmailError
       , binary_to_atom(Message, utf8)
       , MessageError
       ]
      ),
      Req = cowboy_req:reply(200,
        #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
         },
        Content,
        Req0
      ),
      {ok, Req, State}
	end.

template(FileName, ArgList) ->
  {ok, Binary} = file:read_file(FileName),
  list_to_binary(io_lib:format(Binary, ArgList)).

