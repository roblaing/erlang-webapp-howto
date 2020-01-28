<h1>Unit 4: User authentication</h1>

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

We achieve this by using a hash created by Javascript's 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest">SubtleCrypto.digest()</a>
on the user's browser created from their login name, password, and <em>salt</em> we store in a hidden field in our HTML and which the server
reads as a cookie.

This means the user's password never leaves their browser for bad guys to intercept <em>en route</em> to our server.
We don't simply store this hash as the id in our database because if anyone breaks in and copies it, they could set it as
a cookie on their browser to masquerade as the user. So the hash created by Javascript in the browser needs to get rehashed
using functions in OTP's <a href="https://erlang.org/doc/man/crypto.html">crypto</a> library.

The important thing is that the same combination of login and password must create the same unique gibberish for the server
to authenticate the user with. 

I've used the following to be compatible with the <a href="https://github.com/roblaing/swipl-webapp-howto/tree/master/unit4">
SWI Prolog tutorial</a> I'm redoing here in Erlang. I need to return a hexadecimal value, and found the way to do this with a
list comprehension in this 
<a href="https://stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex">facebook post</a>.

```erlang
create_hash(Binary) ->
  Salt = "Some very long randomly generated string",
  Bin = crypto:mac(hmac, sha256, Salt, Binary),
  [begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= Bin].
```

The options listed for <a href="https://erlang.org/doc/man/crypto.html#mac-4">
mac(Type, SubType, Key, Data) -> Mac | descriptive_error()</a> are exhaustive, and my
knowledge of cryptology is very rudimentary. (I have no desire to be a Bitcoin miner 
or create my own cryptocurrency, so find it a fairly boring topic).

As with my SWI Prolog version, I'm going to use John Smith as the username and Password1 as the password.

Once this user is registered and logged in, the browser cookie should be

```js
user_id=aca87862328161c8d5cc6b95d29c04401df3d4496001ba54748fed7719834a0c
```

and the database id should be

```
21b07bc6c590b4b826d8786b837c859e740d9d1a1e9cbfdfcc3c05c299f5f62d
```

<h2>Routing</h2>

The idea of this series of tutorials is to lead to a fully fledged blog. When you arrive at Facebook or Twitter,
you don't want to have to log in every time. So when you point your browser to `http://localhost:3030`  
and are already logged in (ie your cookie is set and correct) it should redirect you to your personal home page (which is just the same
welcome page as in <a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit2">Unit 2</a> at this stage).

If you are not logged in, it should redirect you to a login form. This needs to provide you with an option of signing up
if you don't already have an account.

If you share a computer and account with other people, you may not want to remain logged in, so a logout option is needed.
This can be done with a static 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/priv/logout.html">logout.html</a> page 
which clears the cookie, but then the server needs to redirect you back to the login screen.


My routing section in <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit4/apps/unit4/src/unit4_app.erl">
apps/unit4/src/unit4_app.erl</a> looks like this:

```erlang
  Dispatch = cowboy_router:compile(
   [{'_', 
     [ {"/"              , welcome_or_login, []}
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
<a href="https://erlang.org/doc/man/maps.html">map</a> that cowboy handler's init/2 receive
as their first argument.

```erlang
#{headers => #{...
  <<"cookie">> =>
    <<"user_id=aca87862328161c8d5cc6b95d29c04401df3d4496001ba54748fed7719834a0c">>,
  ...}
}
```

Checking if a user is logged in requires a three step series of checks:

 1. Is there a <<"cookie">> key in the headers map? If no cookie is set, the user is not logged in.
 2. Even if there is a cookie string, it could be junk put their by advertisers and browser plugins.
    If no "user_id=" in the cookie string, the user is not logged in.
 3. Even if there is a "user_id=Hash", the Hash may not match an Id in the database, in which
    case the user is not logged in.

To work through this fairly convoluted logic, I've written the following auxiliary function which
either returns a string with the user's name from the database or false.

```erlang

logged_in(Req0) ->
  HeadersMap = maps:get(headers, Req0),
  AreCookiesSet = maps:is_key(<<"cookie">>, HeadersMap),
  if 
    AreCookiesSet ->
      CookieString = binary:bin_to_list(maps:get(<<"cookie">>, HeadersMap)),
      IdStart = string:find(CookieString, "user_id="),
      IdEnd = string:split(IdStart, ";"),
    true -> io:format("Not logged in~n")
  end,
  ...
```
