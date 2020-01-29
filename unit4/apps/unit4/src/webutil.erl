-module(webutil).

-export([ template/2
        , html_escape/1
        , logged_in/1
        , add_user/3
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
  Bin = crypto:mac(hmac, sha256, Salt, Binary),
  [begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= Bin].

-spec logged_in(Req :: cowboy_req:req()) -> Name::binary() | false.
%% @doc if the user is logged in, return their name, else return false.
logged_in(Req) ->
  #{user_id := Hash} = cowboy_req:match_cookies([{user_id, [], false}], Req),
  if
    Hash =:= false -> false;
    true ->
      Query = io_lib:format("SELECT name FROM users WHERE id='~s'", [create_hash(Hash)]),
      QueryMap = pgo:query(Query),
      case maps:get(num_rows, QueryMap) of
        0 -> false;
        1 -> [{Name}] = maps:get(rows, QueryMap), 
             Name
      end
  end.

-spec add_user(Req :: cowboy_req:req(), Name :: string(), Email :: string()) -> ok | {error, nocookie}.
%% @doc Insert a row into users tables. Assumes the name entry has already been screened for duplicates.
add_user(Req, Name, Email) ->
  #{user_id := Hash} = cowboy_req:match_cookies([{user_id, [], false}], Req),
  if
    Hash =:= false -> {error, nocookie};
    true ->
      Id = create_hash(Hash),
      EscapedName = string:replace(Name, "'", "''", all),
      EscapedEmail = string:replace(Email, "'", "''", all),
      Query = io_lib:format("INSERT INTO users (id, name, email) VALUES ('~s', '~s', '~s')", 
        [Id, EscapedName, EscapedEmail]),
      pgo:query(Query)
  end.


