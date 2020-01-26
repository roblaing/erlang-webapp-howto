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

The file loaded from FileName is a standard HTML file with <code>~s</code> written wherever I want to create a <em>hole</e>
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

Next we need to create two new modules in <code>apps/unit2/src</code> called form_handler.erl and welcome_handler.erl.

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


