-module(weather_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  [{<<"name">>, Name}] = ets:lookup(weather_table, <<"name">>),
  [{<<"dt">>, Dt}] = ets:lookup(weather_table, <<"dt">>),
  {ok, Date} = tempo:format(<<"%A, %e %B %Y">>, {unix, Dt}),
  [{<<"sys">>, Sys}] = ets:lookup(weather_table, <<"sys">>),
  SunriseDt = proplists:get_value(<<"sunrise">>, Sys),
  {ok, Sunrise} = tempo:format(<<"%l:%M">>, {unix, SunriseDt}),
  SunsetDt = proplists:get_value(<<"sunset">>, Sys),
  {ok, Sunset} = tempo:format(<<"%l:%M">>, {unix, SunsetDt}),
  [{<<"main">>, Main}] = ets:lookup(weather_table, <<"main">>),
  Temp = float_to_list(proplists:get_value(<<"temp">>, Main) - 273.15,[{decimals,2}]),
  Min = float_to_list(proplists:get_value(<<"temp_min">>, Main) - 273.15,[{decimals,2}]),
  Max = float_to_list(proplists:get_value(<<"temp_max">>, Main) - 273.15,[{decimals,2}]),
  Pressure = io_lib:format("~p", [proplists:get_value(<<"pressure">>, Main)]),
  Content = webutil:template(code:priv_dir(unit5) ++ "/weather.html", webutil:html_escape(
    [ Name
    , Date
    , Sunrise
    , Sunset
    , Temp
    , Min
    , Max
    , Pressure
    ])),
  Req = cowboy_req:reply(200,
    #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
     },
    Content,
    Req0
  ),
  {ok, Req, State}.


