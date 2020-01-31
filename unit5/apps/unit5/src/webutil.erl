-module(webutil).

-export([ template/2
        , html_escape/1
        , logged_in/1
        , add_user/3
        , get_json/0
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

-spec get_json() -> ok.
%% @doc Fetches a Json string from the web, converts it to a proplist and inserts it into an ETS table.
get_json() ->
  {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = 
  httpc:request(
    "https://samples.openweathermap.org/data/2.5/weather?q=London,uk&appid=b6907d289e10d714a6e88b30761fae22"),
  Json = list_to_binary(Body),
  Proplist = jsx:decode(Json),
  proplist_to_ets(weather_table, Proplist).

proplist_to_ets(_TabId,[]) -> ok;
proplist_to_ets(TabId, [{Key, Value}|Proplist]) ->
  ets:insert(TabId, {Key, Value}),
  proplist_to_ets(TabId, Proplist).




