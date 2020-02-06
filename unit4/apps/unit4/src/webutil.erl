-module(webutil).

-export([ template/2
        , html_escape/1
        , logged_in/1
        , create_hash/1
        ]).

-spec template(FileName :: file:filename(), ArgList :: [string()]) -> Html :: binary().
%% @doc Reads an html file from its complete path name, and inserts strings without escaping `<' or `>'.
template(FileName, ArgList) ->
  {ok, Binary} = file:read_file(FileName),
  io_lib:format(Binary, ArgList).

-spec html_escape(ArgList :: [string()]) -> EscapedList :: [string()].
%% @doc Makes input text "safe" by replacing `<' with `&lt;' and `>' with `&gt;'.
html_escape(ArgList) ->
  lists:map(fun(Html) -> 
              string:replace(string:replace(Html, "<", "&lt;", all), ">", "&gt;", all)
            end, 
            ArgList
).

-spec create_hash(Input::binary()) -> Hexdigest :: string().
%% @doc Rehash the hexdigest read from browser cookie and return as a new hexdigest.
create_hash(Binary) ->
  Salt = "Some very long randomly generated string",
  <<I:256>> = crypto:mac(hmac, sha256, Salt, Binary),
  string:lowercase(integer_to_binary(I, 16)).

-spec logged_in(Req :: cowboy_req:req()) -> Name::binary() | false.
%% @doc if the user is logged in, return their name, else return false.
logged_in(Req) ->
  #{user_id := Hash} = cowboy_req:match_cookies([{user_id, [], false}], Req),
  if
    Hash =:= false -> false;
    true ->
      #{num_rows := NumRows, rows := Rows} = 
        pgo:query("SELECT name FROM users WHERE id=$1::text", [create_hash(Hash)]),
      case NumRows of
        0 -> false;
        1 -> [{Name}] = Rows,
             Name
      end
  end.


