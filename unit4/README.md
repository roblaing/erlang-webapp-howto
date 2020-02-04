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

I've placed the Javascript functions which both the login and signup forms will use to create cookies in
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/priv/scripts/userid_cookie.js">
userid_cookie.js</a>.

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
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/priv/scripts/login.js">
apps/unit4/priv/scripts/login.js</a> file and based on Mozilla's
<a href="https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest">examples</a>.

The options listed for Erlang's <a href="https://erlang.org/doc/man/crypto.html#mac-4">
mac(Type, SubType, Key, Data) -> Mac | descriptive_error()</a> are exhaustive, but I couldn't find a builtin
Erlang function to translate the <em>Mac</em> binary (called a <em>digest</em> by hashing libraries) into a hexadecimal string.

Luckily a Google search revealed someone on <a href="https://stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex">
stackoverflow</a> had written a nifty list comprehension for me to cut 'n paste.

I put the function below in my `webutil` module file and added it to the export list.

```erlang
-spec create_hash(Input::binary()) -> Hexdigest :: string().
%% @doc Rehash the hexdigest read from browser cookie and return as a new hexdigest.
create_hash(Binary) ->
  Salt = "Some very long randomly generated string",
  Bin = crypto:mac(hmac, sha256, Salt, Binary),
  [begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= Bin].
```

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
      Query = io_lib:format("SELECT name FROM users WHERE id='~s'", [create_hash(Hash)]),
      QueryMap = pgo:query(Query),
      case maps:get(num_rows, QueryMap) of
        0 -> false;
        1 -> [{Name}] = maps:get(rows, QueryMap), 
             Name
      end
  end.
```

I don't have to bother escaping single quotes since the string inserted into the SQL query
is generated by create_hash/1, so any malicious SQL-injection attack read from a cookie would have gotten turned into
a string whose characters are limited to numbers `0` to `9` and letters `a` to `f`.

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

Again, I'm relying on the browser to check that the user name and password are at least six characters long, and that the password
entered a second time in the verify field matches.

<h3>Asynchronous race conditions, and Javascript's async/await hell</h3>

In the course of redoing this exercise in Unit 6, using Ajax instead of the 

```html
<form name="signup" onsubmit="return validateSignupForm()" method="POST">
...
</form>
```

method used here, I tripped over my first asynchronous race condition trap.

I assumed that when the signup_handler's init/2 received a "POST" request, the cookie would be present
and correct in the `Req0` map. This turned out to be a bad assumption, since the 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/priv/scripts/userid_cookie.js">
userid_cookie.js</a> script would sometimes have completed writing the "user_id=..." cookie by the time the form
responded, sometimes not.

The reason is the Javascript crypto library's `digest` along with the function to convert the digest to a hexstring both
return what Javascript terms a
<a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises">promise</a>.

```js
async function hexString(buffer) {
  const byteArray = new Uint8Array(buffer);
  const hexCodes = [...byteArray].map(value => {
    const hexCode = value.toString(16);
    const paddedHexCode = hexCode.padStart(2, "0");
    return paddedHexCode;
  });
  return hexCodes.join("");
}

async function digestMessage(message) {
  const encoder = new TextEncoder();
  const data = encoder.encode(message);
  return window.crypto.subtle.digest("SHA-256", data);
}

async function writeCookie() {
  const text = document.getElementById("username").value +
               document.getElementById("salt").value +
               document.getElementById("password").value;
  const digestValue = await digestMessage(text);
  const hex = await hexString(digestValue);
  document.cookie = "user_id=" + hex;
  return hex;
}
```

My initial guess was I'd just need to prefix my
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/priv/scripts/signup.js">
`function validateSignupForm() {...}`</a> with `async` and put `await` before `writeCookie()` inside the
statements block.

No such luck. All that achieved was getting the lines to blank the password and salt values ignored so that they were sent to the server.
I'll hopefully have more experience with Javascript's promises by the end of Unit 6, which introduces another promise, `fetch`.

Next Unit 5 &mdash; <a href ="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit5">Web Services</a>. 


