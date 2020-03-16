%% @doc Helper functions for my simple templating system.
-module(webutil).
-export([template/2]).

-spec template(FileName :: file:filename(), ArgList :: [string()]) -> Html :: chars().
%% @doc Reads an html file from its complete path name, and inserts strings without escaping `<' or `>'.
template(FileName, ArgList) ->
  {ok, Binary} = file:read_file(FileName),
  io_lib:format(Binary, ArgList).

