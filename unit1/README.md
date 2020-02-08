<h1>Unit 1: Basic, static page server</h1>

*Robert Laing*

If you haven't already, run `rebar3 new release <myproject>` where <em>unit1</em> is used as the placeholder for `<myproject>` here
to get the following subdirectories and skeleton files:

```
unit1/
├── .gitignore    
├── rebar.config
├── LICENSE
├── README.md
├── apps/
│   └──unit1/
│      └── src/
│          ├── unit1_app.erl
│          ├── unit1_sup.erl
│          └── unit1.app.src
└── config/
    ├── sys.config
    └── vm.args
```

I'm assuming you are in the `unit1/` project root directory when running `rebar3 release` to build the application
in the remainder of this tutorial. But first we need to edit some of the skeleton files

<h2>1. Add cowboy as a dependency in rebar3.config</h2>

Looking at the 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit1/rebar.config">rebar.config</a> 
file, which is in the project root directory, you'll see it has four curly bracketed data structures
of which the second one, `{deps, []}`, is the one we want to edit to:

```erlang
{deps, [ {cowboy, {git, "https://github.com/ninenines/cowboy.git", {branch, "master"}}}
       ]
}.
```

This simple edit will prompt rebar3 to download, build and install Cowboy (if it hasn't already) when you invoke `rebar3 release`.

<h3>Quick digression into Erlang's data types</h3>

Erlang's curly bracketed <a href="https://erlang.org/doc/reference_manual/data_types.html#tuple">tuples</a> 
and square bracketed <a href="https://erlang.org/doc/reference_manual/data_types.html#list">lists</a>
look similar to Json's objects and arrays, and they essentially serve the same purpose. 

In Json, the above would look something like


```json
{
	"deps": [{
		"cowboy": {
			"git": "https://github.com/ninenines/cowboy.git",
			"branch": "master"
		}
	}]
}
```

But there are subtle differences between Erlang's data conventions and Json which are bound to trip up novices, 
even those like me with some experience with Prolog.

<dl>
  <dt><a href="https://erlang.org/doc/reference_manual/data_types.html#atom">Atoms</a>
  <dd><p>Non-Prolog programming languages tend to call atoms <em>literals</em> &mdash; <em>deps, cowboy, git</em>, and <em>branch</em> 
      are all atoms in the above example. These must either start with a lower case
      letter or be enclosed in single quotes. In Prologish languages, 
      <a href="http://erlang.org/doc/reference_manual/expressions.html#variables">variables</a> 
      always start with upper case letters or underscores, so names that aren't variables need to start with lower case letters or be within 
      single quotes to indicate they are not variables. </p>
      <p>A gotcha in Json is anything except a <em>number, true, false</em>, or <em>null</em> has to be double quoted,
      and single quotes are only used within strings. In Erlang, the difference between single and double quotes is 
      very significant &mdash; especially since Erlang does not have strings <em>per se</em> which I'll elaborate on in Unit 2.</p>
  </dd>
  <dt><a href="https://erlang.org/doc/man/proplists.html">Proplists</a>
  <dd><p>The rebar.config file consists of something we'll see a lot of in Erlang, <code>{key, value}</code> tuples. The value of the
      <code>{deps, [{cowboy, Value}]}</code> follows a pattern that is very common, lists of 
      <code>[{key1, value1}, {key2, value2}, ...]</code> to
      store key-value pairs. This makes Erlang's proplists akin
      to Json's <code>{"key1": value1, "key2": value2, ...}</code> objects, though when Erlang square bracketed lists are not used for 
      <code>{key, value}</code> tuples, they are pretty much identical to Json arrays.
      Erlang's <a href="https://erlang.org/doc/man/proplists.html#get_value-2">proplist:get_value(Key, List)</a> can be used
      to reference values in proplists.</p>
      <p>Erlang offers an alternative to proplists, <a href="https://erlang.org/doc/man/maps.html">maps</a> which use
      <code>#{key1 => value1, key2 => value2,...}</code> syntax which I generally prefer. Proplists can be converted to
      maps with <a href="https://erlang.org/doc/man/maps.html#from_list-1">maps:from_list(List) -> Map</a>
      and back with <a href="https://erlang.org/doc/man/maps.html#to_list-1">maps:to_list(Map) -> [{Key, Value}]</a>.</p> 
      <p>Another alternative compound data structure
      is <a href="https://erlang.org/doc/reference_manual/records.html">records</a></p>
  </dd>
</dl>

Don't forget to end blocks of expressions with a full stop. 
The rebar.config file uses four separate `{key, value}` tuples instead of placing
them in a list, and each one ends with an individual full stop. Even as a relatively experienced Prolog programmer, I often
get error messages for forgetting to end code blocks with a full stop.

<h3>Style tip: use leading commas</h3>

A habit I picked up from reading SQL code was to start, rather than end, elements in lists with their separating commas.
This means, rather than write `[{key1, value1}, {key2, value2}, ...]`, write

```
[ {key1, value1}
, {key2, value2}
...
]
```

This becomes especially handy when value1 is a nested proplist within a proplist, in turn containing deeper nested proplists.
Even though my text editor (gedit) highlights matching parenthesis, I still find stacking them like this makes it a lot easier
to balance square and curly brackets.

```
[ { key1
  , value1
  }
, { key2
  , value2
  }
...
]
```

<h2>2. Add cowboy to the list of applications to start in apps/unit1/src/unit1.app.src</h2>

The third element in the `{application, Application, Proplist}.` tuple found in the 
<a href="https://erlang.org/doc/design_principles/applications.html#application-resource-file">
application resource file</a>, which rebar3 creates a skeleton of in 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit1/apps/unit1/src/unit1.app.src">
apps/unit1/src/unit1.app.src</a>
is a proplist. The `{key, value}` tuple we need to edit has the key <code>applications</code> followed by a list
which always contains `[kernel, stdlib]` to provide Erlang's standard builtin functions (BIFs to their friends).

To include cowboy, it needs to be added to this list.

```
  {applications,
   [ kernel,
   , stdlib
   , cowboy
   ]},
```

A thing that tripped me was if you run Erlang's REPL invoked by `erl` at a command line, it seems you can use any command
in the libraries that OTP installs. Later in these tutorials I tried to use `http_uri:encode(URI)`, and found that while it worked
from the REPL command line, calling it from my code caused my application to crash. The reason is it's part of the inets application which I only
add to the above list in Unit 5. So nothing but the functions from applications you explicitly add to the applications list are
available in your application, despite what tutorials running from a REPL command line may make you think.

After adding some modules in Unit 2 and successfully using the run script to start and stop the server,
I recalled OTP tutorials saying you need to add module names to a tuple in the application resource file
which I'd forgotten to do. The skeleton provided by rebar3 is an empty list.

```
{modules, []},
```

But checking the `_build/default/lib/unit1/ebin/unit1.app` created by invoking `rebar3 release`, I discovered
the build tool had written `{modules,[unit1_app,unit1_sup]}`, so you don't have to worry about it.

<h3>Serving index.html</h3>

The first thing we typically want to do with any web application framework is get it to load an index.html file with related stylesheets,
graphics and Javascript files from `http://localhost:<portnumber>`, which can be surprisingly difficult with many of them.

Cowboy does this fairly painlessly, though you need to skip to
<a href="https://ninenines.eu/docs/en/cowboy/2.7/guide/static_files/">Chapter 11</a>
of its users guide and ignore the opening paragraphs advising you not to use it to serve static files.

Before getting into coding routers and handlers, lets create our index.html and associated files. I assume you already have some,
and if not, suggest working through Mozilla's 
<a href="https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web">beginner's tutorial</a>. I'm going to make a subdirectly
`priv/` as a sibling of the `src/` subdirectory in `apps/unit1/`, and then create `images/`, `styles/`, and `scripts/` folders as suggested by
<a href="https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web/Dealing_with_files">Dealing with files</a> in the Mozilla
tutorial.

After creating some content, my tree now looks something like this:

```
unit1/
├── .gitignore    
├── rebar.config
├── LICENSE
├── README.md
├── apps/
│   └──unit1/
│      ├── priv/
│      │   ├── index.html
│      │   ├── images/
│      │   │   ├── dove.png
│      │   │   ├── favorite-1.jpg
│      │   │   └── favorite-4.jpg
│      │   ├── scripts/
│      │   │   └── validator.js
│      │   └── styles/
│      │       └── style.css
│      └── src/
│          ├── unit1_app.erl
│          ├── unit1_sup.erl
│          └── unit1.app.src
└── config/
    ├── sys.config
    └── vm.args
```

<h2>3. Flesh out apps/unit1/src/unit1_app.erl</h2>

Erlang and Cowboy very much follows the Rails' <em>convention over configuration</em> school, which makes coding something
of a <em>paint by numbers</em> job. Opening `apps/unit1/src/unit1_app.erl` shows this skeleton:

```erlang
%%%-------------------------------------------------------------------
%% @doc unit1 public API
%% @end
%%%-------------------------------------------------------------------

-module(unit1_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    unit1_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
```

The first line of code after comments in every Erlang module is `-module(Modulename).` where Modulename is an atom matching the filename
without the .erl suffix.

The `-export([Function1/N,...]).` allows the functions in the module to be called as `Modulename:Function1(Arg1, ..., ArgN)` by other modules.
Functions not included in the export list are private to that specific module.

All we typically want to do with web servers is start and stop them, which Erlang along with rebar3 makes very easy with a script 
&mdash; which will be called <em>unit1</em> since that is what I called this project &mdash; which we'll get to soon.

<h3>Module:start/2</h3>

There is rather sketchy documentation for the <a href="http://erlang.org/doc/apps/kernel/application.html#Module:start-2">start/2</a>
function required by modules which have selected `-behaviour(application)`. The leading underscores &mdash; as in `_StartType`
and `_StartArgs` &mdash; to the two arguments in the start function
follow a Prolog convention of telling the compiler to ignore arguments which are not used in the body of the function. This happens
in Prologish languages because the same `function_name(Arg1, Arg2, ...)` can be used to handle different cases, and some of these cases
may not need arguments which others do.

But in the case of the required two arity start function for applications, I can't figure out from the documentation I've seen so far
what the purpose of these ignored arguments is.

One of the attractions of Erlang is it creates a supervisor tree to automate recoveries from crashes, so 
`unit1_sup:start_link().` which calls the start_link/0 function in unit1_sup.erl needs to remain the final
<em>return</em> statement.

The basic structure for unit1_app:start/2 I've taken from Cowboy's 
<a href="https://ninenines.eu/docs/en/cowboy/2.7/guide/getting_started/">getting started</a> tutorial.

This involves the following two steps:

 1. Setting up the routes using <a href="https://ninenines.eu/docs/en/cowboy/2.7/manual/cowboy_router.compile/">cowboy_router:compile/3</a>
 2. Launching a "listening loop" with <a href="https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy.start_clear/">cowboy:start_clear/3</a>
    for an http server or <a href="https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy.start_tls/">
    cowboy:start_tls/3</a> for an https server.
 
<h3>Routing</h3>

Since I've placed my html content in the recommended
<a href="https://erlang.org/doc/design_principles/applications.html#directory-structure">directory structure</a> 
`priv/` subdirectory, I can use cowboy's <code>priv_file</code> 
and <code>priv_dir</code> atoms to find them:

```erlang
start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([{'_', % Set Host to any
    [ {"/"              , cowboy_static, {priv_file, unit1, "index.html"}}
    , {"/images/[...]"  , cowboy_static, {priv_dir,  unit1, "images"}}
    , {"/styles/[...]"  , cowboy_static, {priv_dir,  unit1, "styles"}}
    , {"/scripts/[...]" , cowboy_static, {priv_dir,  unit1, "scripts"}}
    ]}]),
  ...
```

If I want to keep my content in, say, `/var/www`, my routers would look like:

```erlang
start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([{'_', % Set Host to any
    [ {"/"             , cowboy_static, {file, "/var/www/index.html"}}
    , {"/images/[...]" , cowboy_static, {dir,  "/var/www/images"}}
    , {"/styles/[...]" , cowboy_static, {dir,  "/var/www/styles"}}
    , {"/scripts/[...]", cowboy_static, {dir,  "/var/www/scripts"}}
    ]}]),
  ...
```

A tip suggested in the <a href="https://ninenines.eu/docs/en/cowboy/2.7/guide/routing/">routing</a> chapter
of the Cowboy User Guide is to store `Dispatch` as a <a href="https://erlang.org/doc/man/persistent_term.html">
persistent_term</a>, refering to it later as `unit1_routes` (or whatever name makes sense to you) instead of `Dispatch`.

```erlang
  persistent_term:put(unit1_routes, Dispatch),
``` 

<h3>Listening</h3>

Here we link our application to a port number. For some reason (some long forgotten tutorial) I got into the habit of using
3030 instead of the more conventional 8080. I assume one should be able to pick whatever port number as an argument passed 
by the start script to the unused StartArgs, but I don't know how, so am leaving it hardwired in the code for now.

As mentioned above, Cowboy gives you a choice of an http listener
<a href="https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy.start_clear/">cowboy:start_clear/3</a>
or and https listener <a href="https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy.start_tls/">
cowboy:start_tls/3</a>.

```erlang
start(_StartType, _StartArgs) ->
  ...
  persistent_term:put(unit1_routes, Dispatch),
  cowboy:start_clear(unit1_http_listener,
   [{port, 3030}],
   #{env => #{dispatch => {persistent_term, unit1_routes}}}
  ),
  unit1_sup:start_link().
```

<h2>Module:stop/1</h2>

The documentation for <a href="http://erlang.org/doc/apps/kernel/application.html#Module:stop-1">Module:stop/1</a>
provides few clues.

The <a href="https://ninenines.eu/docs/en/cowboy/2.7/guide/listeners/">listener</a> section of the Cowboy's User Guide
suggests a small change to the default skeleton.

```
stop(_State) ->
  ok = cowboy:stop_listener(unit1_http_listener).
```

<h2>4. Build</h2>

This simply requires running ```rebar3 release``` in the project root directory.

The first time you run this might take some time as it needs to download and build Cowboy.

Once it has finished, a new tree called `_build` will have appeared in the document root, which
has a lot of branches and files in it.

<h2>5. Start and stop the server</h2>

The key file we want is `_build/default/rel/unit1/bin/unit1` which is the script we use to run our
new webserver.

Invoking it without any arguments will bring up a list of options:

```bash
./_build/default/rel/unit1/bin/unit1
Usage: unit1 {start|start_boot <file>|foreground|stop|restart|reboot|pid|ping|console|console_clean|
              console_boot <file>|attach|remote_console|upgrade|downgrade|install|uninstall|versions|
              escript|rpc|rpcterms|eval|status|undefined}
```

While debugging fresh code, the safest option is probably:

```
./_build/default/rel/unit1/bin/unit1 console
```

This launches the http daemon and leaves you at the erl REPL command line to see any error messages or warnings that appear when
you point your browser to `http://localhost:3030` (assuming you haven't picked a different port number).

Once working, use

```
./_build/default/rel/unit1/bin/unit1 start
```

and 

```
./_build/default/rel/unit1/bin/unit1 stop
```

To start and stop the web server.

Once the project is developed, we want to
<a href="https://adoptingerlang.org/docs/production/releases/#building-a-production-release">build a production release</a> putting
this script somewhere $PATH can find it, but I haven't got that far yet.

<h3>A quick rant about documentation systems</h3>

One of the ironies I've noted learning various programing languages is the worst documented part of most of them tends to be their documentation system, creating a vicious cycle of poor documentation.

Conspiciuously absent in the tree `rebar3 new release unit1` creates is a sister directory to `src/` called `doc/` which should include a file
called `overview.edoc` which becomes the home page when you use Erlang's [EDoc](http://erlang.org/doc/apps/edoc/chapter.html) package.

On the other hand, the command `rebar3 edoc` makes generating Erlang's documentation a lot easier than figuring out the commands without
this third-part application, and rebar3's release skeleton includes a github friendly README.md file, which is not entirely compatible 
with EDoc's markup language &mdash; leading me to curse all the way through translating one to the other for this document.

Vital things associated with Erlang's documentation are its <a href="https://erlang.org/doc/reference_manual/typespec.html">
Types and Function Specifications</a> which also tie into Erlang's <a href="http://erlang.org/doc/man/dialyzer.html">dialyzer</a> system,
which I'm guessing was inspired by Lesley Lamport's <a href="https://lamport.azurewebsites.net/tla/tla.html">TLA+</a>.

When dealing with this kind of programming where inputs tend to be convoluted compound data, and the results cause the side-effect of something
appearing on a web browser, carefully documenting these inputs and outputs is even more important than usual &mdash; and Erlang has a rich
set of tools to do so, but I don't know how to use them when the input arguments are simply ignored and the function's result comes from a badly
documented auxiliary function.

Next &mdash; Unit 2: <a href ="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit2">Forms with validation and redirection</a>.


