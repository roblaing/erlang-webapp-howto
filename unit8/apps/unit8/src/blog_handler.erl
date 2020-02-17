-module(blog_handler).
-behaviour(cowboy_websocket).

-export([ init/2
        , websocket_handle/2
        , websocket_info/2
        ]).

init(Req, State) ->
  {cowboy_websocket, Req, State}.

% Responds to Frames from client
% InFrame    :: ping | pong | {text | binary | ping | pong, binary()}
websocket_handle({text, Json}, State) ->
  Proplist = jsx:decode(Json),
  case proplists:get_value(<<"page">>, Proplist, false) of
    <<"login">> ->
      delete_uid(Proplist),
      Uuid = getuuid(Proplist),
      RetMsg = jsx:encode([{<<"uuid">>, Uuid}]),
      {[{text, RetMsg}], State, hibernate};
    <<"signup">> ->
      Name = proplists:get_value(<<"username">>, Proplist),
      #{num_rows := Rows} = pgo:query("SELECT name FROM users WHERE name=$1::text", [Name]),
      case Rows of
       0 -> delete_uid(Proplist),
            Id = create_hash(proplists:get_value(<<"user_id">>, Proplist)),
            Email = proplists:get_value(<<"email">>, Proplist),
            pgo:query("INSERT INTO users (id, name, email) VALUES ($1::text, $2::text, $3::text)", 
              [Id, Name, Email]),         
            Uuid = getuuid(Proplist),
            RetMsg = jsx:encode([{<<"uuid">>, Uuid}]);
       1 -> RetMsg = jsx:encode([{<<"uuid">>, false}])
      end,
      {[{text, RetMsg}], State, hibernate};          
    <<"logout">> ->
      delete_uid(Proplist),
      {[], State, hibernate};
    <<"front">> ->
      Logged = proplists:get_value(<<"uuid">>, Proplist, false),
      case ets:lookup(uuids, Logged) of
        []                -> Uuid = false;
        [{Logged, _Name}] -> Uuid = Logged
      end,
      Posts = getposts(),
      RetMsg = jsx:encode([{<<"uuid">>, Uuid}, {<<"posts">>, Posts}]),
      {[{text, RetMsg}], State, hibernate};
    <<"newpost">> ->
      Uuid = proplists:get_value(<<"uuid">>, Proplist, false),
      Title = proplists:get_value(<<"title">>, Proplist),
      Art = proplists:get_value(<<"art">>, Proplist),      
      pgo:query("INSERT INTO arts (title, art) VALUES ($1::text, $2::text)", 
        [Title, Art]),
      RetMsg = jsx:encode([{<<"uuid">>, Uuid}]),
      {[{text, RetMsg}], State, hibernate}
  end.

% This has to be here, but isn't used.
websocket_info(_Info, State) ->
  {[], State, hibernate}.

-spec create_hash(Input::binary()) -> Hexdigest :: string().
%% @doc Rehash the hexdigest read from browser cookie and return as a new hexdigest.
create_hash(Binary) ->
  Salt = "Some very long randomly generated string",
  <<I:256>> = crypto:mac(hmac, sha256, Salt, Binary),
  string:lowercase(integer_to_binary(I, 16)).

-spec logged_in(Proplist :: [{Uuid::bitstring(), Name::bitstring()}]) -> Name::bitstring() | false.
%% @doc Check if uuid is false, and if not that it's a valid ID.
logged_in(Proplist) ->
  Bin = proplists:get_value(<<"uuid">>, Proplist, false),
  case Bin of
    false -> false;
    Uuid  -> case ets:lookup(uuids, Uuid) of
               []             -> false;
               [{Uuid, Name}] -> ets:lookup(uuids, Uuid),
                                 Name
             end
  end.

-spec delete_uid(Proplist::[{Uuid::bitstring(), Name::bitstring()}]) -> true.
%% @doc Call before new login or signup to avoid cluttering ETS with uuid/name pairs which have been overwitten in browsers.
delete_uid(Proplist) ->
  Logged = logged_in(Proplist),
  case Logged of
    false -> true;
    _Name -> Uuid = proplists:get_value(<<"uuid">>, Proplist),
             ets:delete(uuids, Uuid)
  end.

-spec getuuid(Proplist::[{Uuid::bitstring(), Name::bitstring()}]) -> Uuid::bitstring() | false.
%% @doc Return a unique hashkey if the supplied user_id is valid, else false.
getuuid(Proplist) ->
  case proplists:get_value(<<"user_id">>, Proplist, false) of
    false -> false;
    Hash  ->
      #{rows := Rows} = pgo:query("SELECT name FROM users WHERE id=$1::text", [create_hash(Hash)]),
      case Rows of
        [] -> false;
        [{Name}] -> String = io_lib:format("~p~p~p", [erlang:system_time(millisecond), make_ref(), node()]),
                    <<I:128>> = crypto:hash(md5, String),
                    Uuid = integer_to_binary(I, 16),
                    Unique = ets:insert_new(uuids, {Uuid, Name}),
                    case Unique of
                      true -> Uuid;
                      false -> getuuid(Proplist)
                    end,
                    Uuid
      end
  end.

getposts() ->
  #{rows := Rows} = pgo:query("SELECT id, title, art, created FROM arts ORDER BY created DESC LIMIT 10"),
  lists:map(fun({Id, Title, Art, Created}) -> 
              [{<<"id">>, Id}, {<<"title">>, Title}, {<<"art">>, Art}, {<<"created">>, Created}] 
            end, Rows). 

