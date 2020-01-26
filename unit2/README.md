<h1>Unit2: Forms with validation and redirection</h2>

Step1 is run `rebar3 new release unit2` and add cowboy as a dependency in 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit2/rebar.config">rebar.config</a> 
and to the applications list in 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit2/apps/unit2/src/unit2.app.src">apps/unit2/src/unit2.app.src</a>
as described in <a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit1">Unit 1</a>.

<h2>Templating</h2>

We now move from static to dynamic content, which involves string substitutions &mdash; something with lots of gotchas in Erlang which I'll
warn about here.

Generating dynamic HTML has spawned many templating systems, and Erlang programers have a wide choice.
<a href="https://github.com/mojombo/mustache.erl">Mustache</a>,
<a href="https://github.com/erlydtl/erlydtl">Django Template Language</a>,
and 
<a href="https://github.com/filippo/sgte">StringTemplate</a> are some I found with a quick Google search.

If you are familiar with one of these templating systems, simply add it with cowboy as a dependency. I don't use templates much, so am
just going to use this two-line function as my templating system:

```erlang
template(FileName, ArgList) ->
  {ok, Binary} = file:read_file(FileName),
  list_to_binary(io_lib:format(Binary, ArgList)).
```

The file loaded from FileName is a standard HTML file with <code>~s</code> written wherever I want to create a <em>hole</em>
to be filled by substituting a string from ArgList. The strings in ArgList have to be exactly in the order of <code>~s</code> and 
the length of ArgList has to match the number of tilde esses.

For Prolog programers, Erlang's <a href="https://erlang.org/doc/man/io_lib.html#format-2">format/2</a> function will be familiar,
and for C-family programers, it's nearly identical to the <code>printf</code> function except it uses tildes instead of percentage signs to mark
what in a string is to be substituted.

Here is an <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit2/apps/unit2/priv/form.html">html form</a> using my 
templating method based on Mozilla's <a href="https://developer.mozilla.org/en-US/docs/Learn/Forms/Your_first_form">
Your first form</a> tutorial.

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Your first HTML form, styled</title>
    <link rel="stylesheet" href="/styles/form.css">
  </head>
  <body>
    <form method="post">
      <ul>
        <li>
          <label for="name">Name:</label>
          <input type="text" id="name" name="user_name" value="~s"/>
          <span class="error">~s</span>
        </li>
        <li>
          <label for="mail">E-mail:</label>
          <input type="email" id="mail" name="user_mail" value="~s"/>
          <span class="error">~s</span>
        </li>
        <li>
          <label for="msg">Message:</label>
          <textarea id="msg" name="user_message">~s</textarea>
          <span class="error">~s</span>
        </li>
        <li class="button">
          <button type="submit">Send your message</button>
        </li>
      </ul>
    </form>
  </body>
</html>
```

<h3>Quick warning about Erlang's text traps</h3>

I discovered I needed to use <code>~s</code> rather than the more familiar (to Prolog programers) <code>~p</code> else the double quotes
surrounding the string would remain in the rendered HTML. If I used <code>~w</code>, Erlang would convert "Hello World" to 
[72,101,108,108,111,32,87,111,114,108,100]. The reason is that unlike <em>modern</em> programming languages (including versions
of Prolog like SWI Prolog), Erlang does not consider text a type in its own right, but one of three things:

  1. Double quoted "Hello World" is seen as a list of character codes. Since it's a list, you can concatenate these using Erlang's
     "Hello " ++ "World" notation.
  2. Single quoted 'Hello World' is an atom, and cannot be concatenated unless converted by <code>atom_to_list('Hello World')</code>
     first.
  3. Chevroned <<"Hello World">> is a <a href="https://erlang.org/doc/reference_manual/data_types.html#bit-strings-and-binaries">binary</a>.
     This is the format that <a href="https://erlang.org/doc/man/file.html#read_file-1">file:read_file("form.hml")</a> returns the HTML in
     form.html as, and also how Cowboy sends and receives http headers and bodies, which is apparently more efficient than traditional text.

While text handling functions such as io_lib:format(Template, [Arg1, Arg2, Arg3, ...]) hide that Erlang has no string type <em>per se</em> 
by allowing Template and each argument to be any of these three types, I hit snags when returning "POST" data in binary format to be re-rendered
for incorrect forms. For instance, blank strings were rendered as <<>>. To fix this, I used `binary_to_atom(Name, utf8)` rather than their
original format when doing string substitution.

Long story short, text processing with Erlang requires keeping in mind which of the three string types is suitable when. I only learnt the hard way
that my two-line template function had to convert the list returned by format into a binary for Cowboy.

<h2>Adding routes</h2>

<code>unit2_app.erl</code> is nearly identical to
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit1/apps/unit1/src/unit1_app.erl">unit1_app.erl</a>
except we add two new routes conforming to the pattern <code>{PathMatch, Handler, InitialState}</code>:

```erlang
     [ ...
     , {"/form", form_handler, ['','','','','','']}
     , {"/welcome/:name", welcome_handler, []}
     ]
```

Next we need to create two new modules in <code>apps/unit2/src</code> called 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit2/apps/unit2/src/form_handler.erl">form_handler.erl</a> and 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit2/apps/unit2/src/welcome_handler.erl">welcome_handler.erl</a>.

A drawback of using rebar3 instead of Cowboy-aligned erlang.mk is there is no `make new t=cowboy.http n=hello_handler` to create a skeleton
file.

The key things are a Cowboy handler needs to have <code>-behavior(cowboy_handler).</code> and
<code>-export([init/2]).</code> (which whould be explained in the
<a href="https://ninenines.eu/docs/en/cowboy/2.2/guide/handlers/">handlers</a> section of its User Guide, but
seems to be missing):

```erlang
-module(foo_handler).
-behavior(cowboy_handler).

-export([init/2]).
```

A skeleton init function looks like:

```erlang
init(Req0, State) ->
  Req = cowboy_req:reply(200,
    #{ <<"content-type">> => <<"text/html; charset=UTF-8">>
     },
    <<"Hello World">>,
    Req0
  ),
  {ok, Req, State}.
```

<h3>Form logic</h3>

Typically with a web form you want to validate the user input, and if it is correct, redirect the user to a <em>success page</em>
&mdash; with URL <code>http://localhost:3030/welcome/John Smith</code> or whatever port number and user name &mdash;
but if there are missing or wrong answers, back to the form with hints on what needs to be done, and the original inputs held
so as not to force the exasperated user to start from scratch.

This pretty basic stuff, which many web application frameworks make impossible, (the inets library which comes with OTP for instance).

Luckily, Cowboy makes this all fairly easy. It also has a nifty <em>permalink</em> very similar to Google app engine (which is the one
I'm most familiar with from doing the Udacity web development course given by Reddid founder Steve Huffman a few years ago, which
I've borrowed a bit from for these tutorials.

<h4>Welcome page</h4>

I'm starting at the <em>end</em> page since it's simpler than the form handler, and illustrates permalinks.

```erlang
#{bindings => #{name => <<"John Smith">>},
  body_length => 0,cert => undefined,has_body => false,
  headers =>
      #{<<"accept">> =>
            <<"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>,
        <<"accept-encoding">> => <<"gzip, deflate">>,
        <<"accept-language">> => <<"en-US,en;q=0.5">>,
        <<"connection">> => <<"keep-alive">>,
        <<"host">> => <<"localhost:3030">>,
        <<"upgrade-insecure-requests">> => <<"1">>,
        <<"user-agent">> =>
            <<"Mozilla/5.0 (X11; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/60.0">>},
  host => <<"localhost">>,host_info => undefined,method => <<"GET">>,
  path => <<"/welcome/John%20Smith">>,path_info => undefined,
  peer => {{127,0,0,1},48270},
  pid => <0.531.0>,port => 3030,qs => <<>>,ref => my_http_listener,
  scheme => <<"http">>,
  sock => {{127,0,0,1},3030},
  streamid => 2,version => 'HTTP/1.1'}
```



