%% @doc Helper functions for my simple templating system.
-module(webutil).
-export([ template/2
        , html_escape/1
        ]).

-spec template(FileName :: file:filename(), ArgList :: [string()]) -> Html :: binary().
%% @doc Reads an html file from its complete path name, and inserts strings without escaping `<' or `>'.
template(FileName, ArgList) ->
  {ok, Binary} = file:read_file(FileName),
  list_to_binary(io_lib:format(Binary, ArgList)).

-spec html_escape(ArgList :: [string()]) -> EscapedList :: [string()].
%% @doc Makes input text "safe" by replacing `<' with `&lt;' and `>' with `&gt;'.
html_escape(ArgList) ->
  lists:map(fun(Html) -> 
              string:replace(string:replace(Html, "<", "&lt;", all), ">", "&gt;", all)
            end, 
            ArgList
).

