<h1>Unit 5: Web Services</h1>

This tutorial goes into the common task of getting data from another website &mdash; usually supplied as Json, but
sometimes XML &mdash; and then parsing it and rendering it on our site.

I'm using <a href="https://openweathermap.org">openweathermap.org</a> which offers free accounts, but this step can
be skipped by using its test URL as I'll do here.

For this we need a web client, which the standard OTP application <a href="http://erlang.org/doc/apps/inets/http_client.html">inets</a>
includes as <a href="http://erlang.org/doc/apps/inets/http_client.html">httpc</a>. 
To access `https://...` sites, httpc needs Erlang's 
secure socket layer <a href="https://erlang.org/doc/man/ssl.html">ssl</a> application. 

We also need a Json parser &mdash; which means adding a third-party application to the dependency list &mdash; and 
an XML parser which comes included.

This means we need to edit 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit5/apps/unit5/src/unit5.app.src">
apps/unit5/src/unit5.app.src</a> to include several new additions to the applications list.

```erlang
  {applications,
   [ kernel
   , stdlib
   , inets
   , ssl
   , cowboy
   , pgo
   , jsx
   , xmerl
   , tempo
   ]},
```

Adding inets to the above list automatically creates an httpc process when the `unit5` application is started. 
(One of the things that makes inet's httpd application confusing is it isn't started unless you make edits to explicitly include it.)

Erlang's standard library doesn't include a Json parser, so we need to pick one of the many third-party libraries available.
I'm going with <a href="https://hex.pm/packages/jsx">jsx</a> which is easy to add as dependency because 
<a href="https://hex.pm/">https://hex.pm/</a> is the default depot for OTP applications, so we don't need extra 
information on how to get them from their github or wherever homes.

The standard library also doesn't include a way to convert the conventional way of representing time as data &mdash; 
the number of seconds since the start of 1970 &mdash; to a human
readable string. For this I'm going to use a third-party application <a href="https://github.com/selectel/tempo">tempo</a>.

The edits required in <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit5/rebar.config">rebar.config</a> are:

```erlang
{deps, [ {cowboy, {git, "https://github.com/ninenines/cowboy.git", {branch, "master"}}}
       , {pgo, {git, "https://github.com/tsloughter/pgo.git", {branch, "master"}}}
       , jsx
       , {tempo, {git, "https://github.com/selectel/tempo", {branch, "master"}}}
       ]
}.
```

<h3>Caching</h3>

OpenWeather asks users not to bombard its servers with constant requests, as could happen if `httpc:request(URL)`
was placed in the web page handler of a busy site.

Ideally we need a type of cron job that gets fresh data with at least 10 minute gaps as requested by OpenWeather and cached 
somewhere for handlers to read rather than constantly hitting the data supplier's servers.

To be run as cron jobs, these data fetchers should possibly be made a separate application with their own run script.
But for now I'll write helper functions get_json and get_xml in my webutil module which will be called once when the application starts.
I intend to explore caching options further in Unit 6. The ASCIIChan project in 
<a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit3">Unit 3</a>, for instance, could probably be improved
by getting the handler to access the list of art from a cache instead of making an SQL request every time, and updating the cache
whenever fresh art is added to the database.

In this tutorial I'm introducing Erlang Term Storage, <a href="https://erlang.org/doc/man/ets.html">ets</a>,
to cache the data downloaded from OpenWeather so it can be rendered any number of times without needing
another hit on the service provider.

<h3>Json</h3>

From the erl command line, if you run 
```erlang
inets:start().  % Done automatically if inets added to the list of applications in resource file.
ssl:start().    % I only discovered I needed this after getting an error for leaving it out.
{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = 
httpc:request("https://samples.openweathermap.org/data/2.5/weather?q=London,uk&appid=b6907d289e10d714a6e88b30761fae22"),
io:format("Body: ~p~n", [Body]).
```
This will show `Body` to be a string containing Json, so lots of escaped double quotes inside the bounding double quotes,
which can be gotten rid of in the output with `io:format("Body: ~s~n", [Body])`. I find the `~p` option handy when exploring so as to
show if the returned data is a string, binary, atom, map...

Using <a href="https://jsonlint.com/">https://jsonlint.com</a> to neaten up the indentation, the returned Json looks like:

```json
{
	"coord": {
		"lon": -0.13,
		"lat": 51.51
	},
	"weather": [{
		"id": 300,
		"main": "Drizzle",
		"description": "light intensity drizzle",
		"icon": "09d"
	}],
	"base": "stations",
	"main": {
		"temp": 280.32,
		"pressure": 1012,
		"humidity": 81,
		"temp_min": 279.15,
		"temp_max": 281.15
	},
	"visibility": 10000,
	"wind": {
		"speed": 4.1,
		"deg": 80
	},
	"clouds": {
		"all": 90
	},
	"dt": 1485789600,
	"sys": {
		"type": 1,
		"id": 5091,
		"message": 0.0103,
		"country": "GB",
		"sunrise": 1485762037,
		"sunset": 1485794875
	},
	"id": 2643743,
	"name": "London",
	"cod": 200
}
```

`jsx:decode(Json)` barfs if the input text isn't binary, so `Json = list_to_binary(Body)` is needed to produce
this <a href="https://erlang.org/doc/man/proplists.html">proplist</a>:

```erlang
[{<<"coord">>,[{<<"lon">>,-0.13},{<<"lat">>,51.51}]},
 {<<"weather">>,
  [[{<<"id">>,300},
    {<<"main">>,<<"Drizzle">>},
    {<<"description">>,<<"light intensity drizzle">>},
    {<<"icon">>,<<"09d">>}]]},
 {<<"base">>,<<"stations">>},
 {<<"main">>,
  [{<<"temp">>,280.32},
   {<<"pressure">>,1012},
   {<<"humidity">>,81},
   {<<"temp_min">>,279.15},
   {<<"temp_max">>,281.15}]},
 {<<"visibility">>,10000},
 {<<"wind">>,[{<<"speed">>,4.1},{<<"deg">>,80}]},
 {<<"clouds">>,[{<<"all">>,90}]},
 {<<"dt">>,1485789600},
 {<<"sys">>,
  [{<<"type">>,1},
   {<<"id">>,5091},
   {<<"message">>,0.0103},
   {<<"country">>,<<"GB">>},
   {<<"sunrise">>,1485762037},
   {<<"sunset">>,1485794875}]},
 {<<"id">>,2643743},
 {<<"name">>,<<"London">>},
 {<<"cod">>,200}]
```

That the above data is in a proplist is handy since {Key, Value} tuples are what ETS data is stored as.
The <a href="https://erlang.org/doc/man/ets.html#insert-2">ets:insert(Tab, {Key, Value}) -> true</a>
function is an exception to Erlang's immutable variable rule. If `Key` already exists, its old value gets
overwritten with the new value, making it ideal to regularly update our weather data.

We can insert the above proplist into an ETS table with this recursive function:

```erlang
proplist_to_ets(TabId, []) -> ok;
proplist_to_ets(TabId, [{Key, Value}|Proplist]) ->
  ets:insert(TabId, {Key, Value}),
  proplist_to_ets(TabId, Proplist).
```

Before I can use the table refered to as TabId, I need to have called 
<a href="https://erlang.org/doc/man/ets.html#new-2">ets:new(Name, Options) -> tid() | atom()</a>,
which I'm going to do first thing in my application's start/2 function.

The default `type` option `set` is what I think I want (it implies no duplicate keys). 

I'm guessing the default `protection` option needs to be changed to `public` so that my function can update
the values.

The `named_table` option means whatever name I give my ETS table gets 
<a href="http://erlang.org/doc/man/erlang.html#register-2">registered</a>, ie its process ID becomes a global
constant that, say, `get_json()` and `get_xml()`, can be on a first name basis with.

A tuple in my application resource file I havent used yet is `{registered, []}`, so I had better insert my table name
(I'm picking `weather_table`) to this list.

The start of the start function in my apps/unit5/src/unit5_app.erl file now looks like this:

```erlang
start(_StartType, _StartArgs) ->
  ets:new(weather_table, [public, named_table]),
  ...
```
For the sake of good housekeeping, I'm going to expand my stop function to:

```erlang
stop(_State) ->
  ets:delete(weather_table),
  ok = cowboy:stop_listener(my_http_listener).
```

And now my hander can extract keys from the ETS weather_table like so: 

```erlang
  [{<<"name">>, Name}] = ets:lookup(weather_table, <<"name">>)
```

<h4>Date formating</h4>

This Json objects includes a "dt" key with a value of 1485789600 &mdash; the number of seconds to that particular date 
since 1970 &mdash; which converting to something human readable is exasperating in most programing languages, and in
Erlang more than most.

I eventually discovered the magic incantation in tempo was 
`{ok, Date} = tempo:format(iso8601, {unix, 1485789600})` which produced `<<"2017-01-30T15:20:00Z">>`, coincidently
exactly two years from when I was writing this.

Alternatively using `rfc1123` instead of `iso8601` produces `<<"Mon, 30 Jan 2017 15:20:00 GMT">>`, and
`rfc2822` produces `<<"Mon, 30 Jan 2017 15:20:00 +0000">>`.

The tempo application is a wrapper for C's <a href="https://linux.die.net/man/3/strftime">strftime</a> function, so you can
customise as in `<<"%A, %Y-%d-%m">>` which produces `<<"Monday, 2017-30-01">>`.

Long story short, if you live in London, you'll need to get a proper `appid=...` value by registering 
instead of using the test data to get a current weather forecast.

<h2>Routing</h2>

We need to modify the <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit5/apps/unit5/src/unit5_app.erl">
apps/unit5/src/unit5_app.erl</a> where `unit5` should be substitued with whatever you are calling your project, to include the 
following new route in the list:

```erlang
     ...
     , {"/weather"       , weather_handler, []}
     ...
```

<h3>XML</h3>

If you want XML instead of Json, Open Weather provides the `mode=xml` option in the URL's query string:
```
httpc:request("https://samples.openweathermap.org/data/2.5/weather?q=London,uk&mode=xml&appid=b6907d289e10d714a6e88b30761fae22").
```
The body of this request is again a string, but containing XML, which pretty printed looks like this:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<current>
   <city id="2643743" name="London">
      <coord lon="-0.13" lat="51.51" />
      <country>GB</country>
      <sun rise="2017-01-30T07:40:36" set="2017-01-30T16:47:56" />
   </city>
   <temperature value="280.15" min="278.15" max="281.15" unit="kelvin" />
   <humidity value="81" unit="%" />
   <pressure value="1012" unit="hPa" />
   <wind>
      <speed value="4.6" name="Gentle Breeze" />
      <gusts />
      <direction value="90" code="E" name="East" />
   </wind>
   <clouds value="90" name="overcast clouds" />
   <visibility value="10000" />
   <precipitation mode="no" />
   <weather number="701" value="mist" icon="50d" />
   <lastupdate value="2017-01-30T15:50:00" />
</current>
```

I got as far with the xmerl library as puting this output through

```erlang]
...
XML = xmerl_scan:string(Body),
Content = io_lib:format("~p~n", [XML]),
...
```
which just turned this verbose input into even more verbose, and to me unusable, output. I thought replicating the above Json example
with XML would be easy, but no such luck, so if you have to use XML and Erlang, good luck.



