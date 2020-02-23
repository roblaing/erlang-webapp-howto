-module(webutil).

-export([ create_hash/1
        , logged_in/1
        , delete_uid/1
        , getuuid/1
        ]).

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
        [{Name}] -> delete_uid(Proplist),
                    String = io_lib:format("~p~p~p", [erlang:system_time(millisecond), make_ref(), node()]),
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

