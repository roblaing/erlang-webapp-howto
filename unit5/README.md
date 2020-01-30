<h1>Unit 5: Web Services</h1>

For this we need a web client, which the standard library's <a href="http://erlang.org/doc/apps/inets/http_client.html">inets</a>
includes as <a href="http://erlang.org/doc/apps/inets/http_client.html">httpc</a>. 
to access `https://...` sites, httpc needs Erlang's 
secure socket layer <a href="https://erlang.org/doc/man/ssl.html">ssl</a> application. 

We also need a Json parser &mdash; which means adding a third-party application to the dependency list &mdash; and 
an XML parser which is included.

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
   ]},
```

Adding inets to the above list automatically creates an httpc process when the `unit5` application is started. 
(One of the things that makes inet's httpd application confusing is it isn't started unless you make edits to explicitly include it.)

Since the example data supplier I'm using in this tutorial has an `https://...` address, we also need Erlang's `ssl` application
added.

Erlang's standard library doesn't include a Json parser, so we need to pick one of the many third-party libraries available.
I'm going with <a href="https://hex.pm/packages/jsx">jsx</a> which is easy to add as dependency because applications in 
Elixer's <a href="https://hex.pm/">https://hex.pm/</a> depo don't need accompanying URL's to their github or whatever homes.

So we need a small edit in <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit5/rebar.config">rebar.config</a>:

```erlang
{deps, [ {cowboy, {git, "https://github.com/ninenines/cowboy.git", {branch, "master"}}}
       , {pgo, {git, "https://github.com/tsloughter/pgo.git", {branch, "master"}}}
       , jsx  
       ]
}.
```

<h2>Reading and parsing Json and XML</h2>

This tutorial goes into the common task of getting data from another website &mdash; usually supplied as Json, but
sometimes XML &mdash; and then parsing it and rendering it on our site.

I'm using <a href="https://openweathermap.org">openweathermap.org</a> which offers free accounts which your don't need
if using its test URL which requires no registration to use.

From the erl command line, if you run 
```erlang
inets:start().  % Done automatically if inets added to the list of applications in resource file.
ssl:start().    % Only discovered I needed this after getting an error for leaving it out.
{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = 
httpc:request("https://samples.openweathermap.org/data/2.5/weather?q=London,uk&appid=b6907d289e10d714a6e88b30761fae22"),
io:format("Body: ~p~n", [Body]).
```
This will show `Body` to be a string containing Json, so lots of escaped double quotes inside the bounding double quotes,
which can be gotten rid of in the output with `io:format("Body: ~s~n", [Body])`. I find the `~p` option handy when exploring so as to
show if the returned data is a string, binary, atom, map...

Using <a href="https://jsonlint.com/">https://jsonlint.com</a> to neaten up the indentation results in:

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

If you live in London, note the "dt" (datetime) value of 1485789600 in the Json returned equates to 2017 January 30, 
so if you want a live, current weather report, you'll need to get a propper `appid=...` value by registering 
instead of using the test data.

If your prefer XML to Json, Open Weather provides the `mode=xml` option in the URL's query string:
```
inets:start().  % Done automatically if inets added to the list of applications in resource file.
ssl:start().    % Only discovered I needed this after getting an error for leaving it out.
{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = 
httpc:request("https://samples.openweathermap.org/data/2.5/forecast?q=London,us&mode=xml&appid=b6907d289e10d714a6e88b30761fae22"),
io:format("Body: ~s~n", [Body]).
```
The body of this request is again a string, but containing XML, which for some reason is far more verbose than the
Json version because it includes dozens of `<time ...>` elements which I've only included the first of.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<weatherdata>
   <location>
      <name>London</name>
      <type />
      <country>US</country>
      <timezone />
      <location altitude="0" latitude="39.8865" longitude="-83.4483" geobase="geonames" geobaseid="4517009" />
   </location>
   <credit />
   <meta>
      <lastupdate />
      <calctime>0.0028</calctime>
      <nextupdate />
   </meta>
   <sun rise="2017-03-03T12:03:03" set="2017-03-03T23:28:37" />
   <forecast>
      <time from="2017-03-03T06:00:00" to="2017-03-03T09:00:00">
         <symbol number="600" name="light snow" var="13n" />
         <precipitation unit="3h" value="0.03125" type="snow" />
         <windDirection deg="303.004" code="WNW" name="West-northwest" />
         <windSpeed mps="2.29" name="Light breeze" />
         <temperature unit="kelvin" value="269.91" min="269.91" max="270.877" />
         <pressure unit="hPa" value="1005.61" />
         <humidity value="93" unit="%" />
         <clouds value="scattered clouds" all="32" unit="%" />
      </time>
   </forecast>
</weatherdata>
```      

OpenWeather requests that users don't constantly request data from its servers, as would happen if the `httpc:request(URL)`
was placed in the web page handler everytime a visitor pointed their browser to its corresponding URL.

Ideally we need a type of `cron` job that gets fresh data with at least 10 minute gaps as requested by OpenWeather and cached 
somewhere for handlers to read rather than constantly hitting the data supplier's servers. I'm going to use
Erlang's <a href="https://erlang.org/doc/man/ets.html">ets</a> module to cache the Json and XML file.

To be run as cron jobs, these data fetchers should possibly be separate applications with their own run scripts.
But for now I'll write helper functions get_json and get_xml in my webutil module which will be called once when the application starts.
I intend to explore caching options further in Unit 6. The ASCIIChan project in 
<a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit3">Unit 3</a>, for instance, could probably be improved
by getting the handler to access the list of art from a cache instead of making an SQL request every time, and updating the cache
whenever fresh art is added to the database.

<h2>Routing</h2>

We need to modify the <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit5/apps/unit5/src/unit5_app.erl">
apps/unit5/src/unit5_app.erl</a> where `unit5` should be substitued with whatever you are calling your project, to include the 
following new route in the list:

```erlang
     ...
     , {"/weather"       , weather_handler, []}
     ...
```


