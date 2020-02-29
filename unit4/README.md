<h1>Unit 4: User authentication</h1>

*Robert Laing*

If you are starting this project afresh instead of continuing from Unit 3, make the necessary edits to
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/rebar.config">rebar.config</a>,
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/config/sys.config">config/sys.config</a>, and the
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit1/src/unit4.app.src">application resource file</a>.

It also requires the following table in your database:

```sql
CREATE TABLE IF NOT EXISTS users 
  ( id     TEXT PRIMARY KEY
  , name   TEXT UNIQUE NOT NULL
  , email  TEXT
  );
```

Note there is no field for password in this table because passwords should never be seen let alone stored in plain text by web servers — an elementary part of online security, yet stories keep appearing of big corporations storing passwords in plain text to get stolen by criminals.

The idea of this series of tutorials is to lead to a fully fledged blog. When you arrive at Facebook or Twitter,
you don't want to have to log in every time. So when you point your browser to `http://localhost:3030`  
and are already logged in (ie your cookie is set and correct) it should redirect you to your personal home page (which is just the same
welcome page as in <a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit2">Unit 2</a> at this stage).

If you are not logged in, it should redirect you to a login form. This needs to provide you with an option of signing up
if you don't already have an account.

You don't want to remain logged in on a public computer, so a logout option is needed.
This can be done with a static 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/priv/logout.html">logout.html</a> page 
which clears the cookie and redirects back to the login page.

<h2>Hashing</h2>

We avoid the password ever leaving the browser by using a hash created by Javascript's 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest">SubtleCrypto.digest()</a>
on the user's browser created from their login name, password, and <em>salt</em> we store in a hidden field in our HTML.
This creates a gibberish string (which bad guys will be unable to recreate the password from) which is stored as a cookie
in the browser. The server can then use this cookie to authenticate that the person has entered their correct username and password
without ever seeing the password.

This means the user's password never leaves their browser for bad guys to intercept <em>en route</em> to our server.
But we can't simply store this hash as the id in our database because if anyone breaks in and copies it, they could set it as
a cookie on their browser to masquerade as the user. So the hash created by Javascript in the browser needs to get rehashed
using functions in OTP's <a href="https://erlang.org/doc/man/crypto.html">crypto</a> library.

The important thing is that the same combination of login and password must create the same unique gibberish for the server
to authenticate the user with. 

As with my SWI Prolog version of this tutorial, I'm going to use John Smith as the username and Password1 as the password.

Once this user is registered and logged in, the browser cookie should be

```js
user_id=aca87862328161c8d5cc6b95d29c04401df3d4496001ba54748fed7719834a0c
```

and the database id should be

```
21b07bc6c590b4b826d8786b837c859e740d9d1a1e9cbfdfcc3c05c299f5f62d
```

Both are strings containing hexadecimals. The Javascript functions `hexString(buffer)` and `digestMessage(message)` to create
the cookie are in my 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/priv/scripts/getid.js">
getid.js</a> file and based on Mozilla's
<a href="https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest">examples</a>.

This was my first, painful, encounter with Javascript promises, which I'll elaborate on at the end of this tutorial

The options listed for Erlang's <a href="https://erlang.org/doc/man/crypto.html#mac-4">
mac(Type, SubType, Key, Data) -> Mac | descriptive_error()</a> are exhaustive, but I couldn't find a builtin
Erlang function to translate the <em>Mac</em> binary (called a <em>digest</em> by hashing libraries) into a hexadecimal string.

I initially used a list comprehension found on <a href="https://stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex">
stackoverflow</a>, but pgo (and other modules) author Tristan Sloughter gave me this more elegant solution using
<a href="http://erlang.org/doc/man/erlang.html#integer_to_binary-2">integer_to_binary(Integer, Base) -> binary()</a>:

```erlang
create_hash(Binary) ->
  Salt = "Some very long randomly generated string",
  <<I:256>> = crypto:mac(hmac, sha256, Salt, Binary),
  string:lowercase(integer_to_binary(I, 16)).
```

Note I had to convert the hexstring to lower case to match the existing data since integer_to_binary/2 writes <em>A..F</em>
instead of <em>a..f</em>. I'm not sure whether upper or lower case is more correct for hexadecimals, but it's a snag to 
watch out for when using them as database IDs.

<h3>Bit syntax</h3>

I hadn't encountered something like Erlang's <<I:256>> before, and found an explanation in
<a href="http://erlang.org/doc/reference_manual/expressions.html#bit-syntax-expressions">Expressions</a> section of the
official documentation. Besides the colon followed by a 2<sup>n</sup> value to say `<<Binary>>` should be chopped up into a list
`<<Value1, Value2, Value3,...>>` of 2<sup>n</sup> sized values, there's also the `Value1/Type`notation, as in 
`<< "That's what she said! ", Msg/binary >>` which I encountered in Unit 7.

<h2>Routing</h2>

My routing section in <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/src/unit4_app.erl">
apps/unit4/src/unit4_app.erl</a> looks like this:

```erlang
  Dispatch = cowboy_router:compile(
   [{'_', 
     [ {"/"              , welcome_or_login_handler, []}
     , {"/login"         , login_handler, []}
     , {"/signup"        , signup_handler, []}
     , {"/welcome/:name" , welcome_handler, []}
     , {"/logout"        , cowboy_static, {priv_file, unit4, "logout.html"}}
     , {"/styles/[...]"  , cowboy_static, {priv_dir,  unit4, "styles"}}
     , {"/scripts/[...]" , cowboy_static, {priv_dir,  unit4, "scripts"}}
     , {"/[...]"         , filenotfound_handler, []}
     ] 
    }
   ]
  ),
```

<h3>Reading cookies</h3>

Back to the <a href="https://ninenines.eu/docs/en/cowboy/2.2/guide/req/">Req</a> Erlang 
<a href="https://erlang.org/doc/man/maps.html">map</a> that cowboy handlers' init/2 receive
as their first argument.

```erlang
#{headers => #{...
  <<"cookie">> =>
    <<"user_id=aca87862328161c8d5cc6b95d29c04401df3d4496001ba54748fed7719834a0c">>,
  ...}
}
```

Checking if a user is logged in requires a three step series of checks:

 1. Is there a `<<"cookie">>` key in the headers map? If no cookie is set, the user is not logged in.
 2. Even if there is a cookie string, it could be junk put there by advertisers or browser plugins.
    The semicolon separated text list of key1=value1; key2=value2;... 
    needs to be parsed to find if there is a "user_id=". If there's no such key, the user is not logged in.
 3. Even if there is a "user_id=Hash", the Hash may not translate into a valid Id in the database, in which
    case the Hash was created from a wrong user name or password, so the user is not be logged in.

I initially wrote some extremely convoluted code to work through these steps (learning a bit about
Erlang's string:find/2 and string:split2 which I'm sure will come in handy later) but mercifully found
<a href="https://ninenines.eu/docs/en/cowboy/2.4/manual/cowboy_req.match_cookies/">cowboy_req:match_cookies/3</a>
which handled all this with

```erlang
-spec logged_in(Req :: cowboy_req:req()) -> Name::binary() | false.
%% @doc if the user is logged in, return their name, else return false.
logged_in(Req) ->
  #{user_id := Hash} = cowboy_req:match_cookies([{user_id, [], false}], Req),
  if
    Hash =:= false -> false;
    true ->
      #{num_rows := NumRows, rows := Rows} = 
        pgo:query("SELECT name FROM users WHERE id=$1::text", [create_hash(Hash)]),
      case NumRows of
        0 -> false;
        1 -> [{Name}] = Rows,
             Name
      end
  end.
```

<h2>welcome_or_login_handler</h2>

Once I had the webutil:logged_in/1 function working, writing the 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/src/welcome_or_login_handler.erl">
welcome_or_login_handler.erl</a> was relatively straighforward:

```erlang
-module(welcome_or_login_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  case webutil:logged_in(Req0) of
    false -> 
      Req = cowboy_req:reply(303, 
        #{<<"location">> => <<"/login">>}, Req0);
    Name ->
      Req = cowboy_req:reply(303, 
        #{<<"location">> => list_to_binary(io_lib:format("/welcome/~s", [Name]))}, Req0)
  end,
  {ok, Req, State}.
```

Notice Name appears in the URL as "/welcome/John Smith" instead of "/welcome/John+Smith" which the browser doesn't mind.

Trying to be pedantic and figuring out how to URI encode in Erlang turned into a wild goose chase. There is a
function <a href="http://erlang.org/doc/man/http_uri.html#encode-1">http_uri:encode/1</a> but it's not available
because inets isn't included in the application list which I'm not using because inets doesn't support redirections which
I use a lot. Cowboy which I'm using instead of inets doesn't seem to have a URI encode function, so I didn't bother.

<h2>login_handler</h2>

My <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/src/login_handler.erl">login_handler.erl</a>
code is only slightly more elaborate than the welcome_or_login_handler.erl above because it needs to handle rendering a new form when 
it receives a "GET" request and then either redraw with an error message for "POST" or redirect to the welcome page.

```erlang
-module(login_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
  Content = webutil:template(code:priv_dir(unit4) ++ "/login_form.html", ["",""]),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0),
  {ok, Req, State};

init(Req0=#{method := <<"POST">>}, State) ->
  case webutil:logged_in(Req0) of
    false ->
      {ok, PostVals, _} = cowboy_req:read_urlencoded_body(Req0),
      Name = proplists:get_value(<<"username">>, PostVals),
      Content = webutil:template(code:priv_dir(unit4) ++ "/login_form.html", 
        [Name,"Your user name or password is incorrect"]),
      Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0);
    Name ->
      Req = cowboy_req:reply(303, 
        #{<<"location">> => list_to_binary(io_lib:format("/welcome/~s", [Name]))}, Req0)
  end,
  {ok, Req, State}.
```
I had to restrain myself from over elaborating by redirecting to the welcome page if a user was already logged in.

Note this is a lot simpler than the form in <a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit2">Unit 2</a>
because I've kept the input validation in the browser

```html
<input type="text" id="username" name="username" value="~s" required minlength="6">
```

to prevent submission of usernames or passwords shorter than six characters. But I still need a `onsubmit="return validateLoginForm()"`
call to set the cookie. What I also do in this Javascript function is blank the password and salt values that get passed to the server.

<h2>signup_handler</h2>

Like Facebook and Twitter, user names have to be unique (though I haven't bothered to tell someone signing up as John Smith that he has to be
johnsmith6982).

A classic blunder I made in the first version of this was to check if a username was already taken before doing the insert, requiring
two database hits when one would do. Cutting out unnecessary hits on the database is generally the first place to start optimising web applications.

The command

```erlang
Query = pgo:query("INSERT INTO users (id, name, email) VALUES ($1::text, $2::text, $3::text)", 
  [Id, Name, Email]),
```
will either return

```erlang
#{command => insert,num_rows => 1,rows => []}
```

or something like

```erlang
{error,{pgsql_error,#{code => <<"23505">>,constraint => <<"users_name_key">>,
                      detail => <<"Key (name)=(John Smith) already exists.">>,
                      file => <<"nbtinsert.c">>,line => <<"434">>,
                      message =>
                          <<"duplicate key value violates unique constraint \"users_name_key\"">>,
                      routine => <<"_bt_check_unique">>,
                      schema => <<"public">>,severity => <<"ERROR">>,
                      table => <<"users">>,
                      {unknown,86} => <<"ERROR">>}}}
```

So I can rely on the constraint I set when I created my user table and let PostgreSQL check usernames are unique like this:

```erlang
...
  Query = pgo:query("INSERT INTO users (id, name, email) VALUES ($1::text, $2::text, $3::text)", 
            [webutil:create_hash(Hash), Name, Email]),
  case Query of
    {error, _} -> 
      Content = webutil:template(code:priv_dir(unit4) ++ "/signup_form.html", 
        webutil:html_escape([Name, "Sorry, that name is already taken. Please pick another.", Email])),
      Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=UTF-8">>}, Content, Req0);
    _ -> 
      Req = cowboy_req:reply(303, #{<<"location">> => list_to_binary(io_lib:format("/welcome/~s", [Name]))}, Req0)
  end,
  {ok, Req, State}.     
```

This style of programming encouraged by Erlang of using "guards" stands on the shoulders of giants such as Edsger Dijkstra's
<a href="http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.97&rep=rep1&type=pdf">Guarded Command Language</a> which cited
Tony Hoare's <a href="https://www.cs.cmu.edu/~crary/819-f09/Hoare69.pdf">axiomatic basis for computer programming</a>. I've found
learning to think in terms of guards rather than traditional <em>if-then</em> statements took some practice.

My actual code in <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/src/signup_handler.erl">signup_handler.erl</a>
is a bit more convoluted because I also guard against the cookie not being set, an intermittent bug which plagued my earlier attempts
until I figured out how to use Javascript promises as explained next.

Again, I'm relying on the browser to check that the user name and password are at least six characters long, and that the password
entered a second time in the verify field matches.

<h2>Javascript and asynchronous programming</h2>

To grasp the horror of Javascript <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises">promises</a>,
go to a console (F12 in Firefox) and enter

```javascript
crypto.subtle.digest("SHA-256", new TextEncoder().encode("My login and password with a salt"));
```

Besides the liturgy of having to create a 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder">TextEncoder</a> 
object instead of just inputting the text, there's the unhelpful response of 

```javascript
Promise { <state>: "pending" }
```

Getting the promised pending <em>promise</em> as so:

```javascript
crypto.subtle.digest("SHA-256", new TextEncoder().encode("My login and password with a salt"))
.then((hash) => console.log(hash));
```

produces the equally unhelpful:

```javascript
ArrayBuffer { byteLength: 32 }
```

requiring further liturgy to actually get something useable...

<a href="https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest">SubtleCrypto.digest()</a> was
my first encounter with what Javascript terms
<a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises">promises</a>.

Something that tripped me up with my first attempt at
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/priv/scripts/getid.js">getid.js</a> 
was a race condition whereby the function sometime produced a hashstring, othertimes `{<state>: "pending"}`. Whether the cookie
had been set by the time the server received the browser's http header response was completely random.

I partly blame the hashing examples given by Mozilla &mdash; which I admittedly blindly cut 'n pasted for my first version of this &mdash;
for misleading me down the frustrating path of using 
<a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function">
async function() {...}</a> combined with <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/await">
await</a>.

A lot of learning concurrent programming involves unlearning the bad, sequential, habits most of us have grown up with writing
C-family code. I instinctively went for pausing while the promise finished using `await`, which works, but is not the efficient
or ellegant way to handle asynchronous programming in Javascript.

My original, wrong, blocking, way to write my function was:

```javascript
async function write_cookie() {
  const text = document.getElementById("username").value +
               document.getElementById("salt").value +
               document.getElementById("password").value;
  const digestValue = await digestMessage(text);
  const hex = await hexString(digestValue);
  document.cookie = "user_id=" + hex;
```

Incidently, you can only use `await` within functions prefixed with `async`, and then when these are called from other functions
they return `{<state>: "pending"}` unless the call invoking them was also prefixed with `await`, 
which requires the function the invoking line is housed in to have also been prefixed with `async`... and so on to 
<a href="https://www.freecodecamp.org/news/avoiding-the-async-await-hell-c77a0fb71c4c/">async/await hell</a>.

Mercifully, I <em>then</em> discovered <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then">then()</a>:

```javascript
async function write_cookie() {
  const text = document.getElementById("username").value +
               document.getElementById("salt").value +
               document.getElementById("password").value;
  digestMessage(text)
    .then((digestValue) => hexString(digestValue))
    .then((hex) => {document.cookie = "user_id=" + hex});
```

I spent many frustrating hours trying to get asynchronous functions to return things. For instance, I
tried to have a function called getId(), return the hash instead of writing it as a cookie, only to 
discover `getId().then((id) => {document.cookie = "user_id=" + hex})` wrote `{<state>: "pending"}` as the cookie.

It seems with Javascript promises, you have to keep the old <em>Fleetwood Mac</em> song in mind and never
break the chain. 

We go deeper into Javascript's promises in Unit 6 which introduces `fetch`.

Next Unit 5 &mdash; <a href ="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit5">Web Services</a>. 


