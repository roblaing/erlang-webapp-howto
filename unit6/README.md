<h1>Unit 6: Ajax</h1>

In this unit I'm going to refactor the login system from <a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit4">Unit4</a>
to use what is generically called <a href="https://developer.mozilla.org/en-US/docs/Web/Guide/AJAX">Ajax</a>. From a user's perspective,
it will make no difference &mdash; making this exercise look a bit dull.

But in terms of what I want to do with Erlang in the longer term, this is a vital lesson.

<h2>Back to basics</h2>

To understand how combining Ajax with Erlang makes the Javascript code more, uhm, Erlangish, 
lets step back to the introductory
<a href="https://erlang.org/doc/getting_started/conc_prog.html#message-passing">ping pong</a> example
in the <a href="https://erlang.org/doc/getting_started/users_guide.html">Getting Started</a> section of the
official Erlang documentation.

In this basic example, `ping` or `pong` (depending on whose turn it is) sends a message to the other using
Erlang's `Pid ! Message` syntax &mdash; which is syntactic sugar for `erlang:send(Pid, Message)`, which I personally
find more readable.

Both `ping` and `pong` have listening loops to react to the messages they receive which are similar to 
Erlang's case statements:

```erlang
...
receive
  message_type1     -> <action for message_type1>;
  message_type2     -> <action for message_type2>;
  ...
  message_typeN     -> <action for message_typeN>
  after Miliseconds -> <action for timeout error> 
end.
```

In the previous five units I've skipped the above simple example and dived straight into 
larger applications and their
<a href="https://erlang.org/doc/design_principles/des_princ.html#behaviours">behaviours</a>
which hide all this.

I've never seen Erlang's `!` and `receive` statements outside of introductory tutorials, so don't see much point
in covering them much. But it's important to remember Erlang is all about asynchronous messaging between nodes.

With Ajax, we can think of the browser as a node with a listening loop. This unit involves more rewriting of Javascript than Erlang.

<h2>Call and response with Javascript</h2>

I've returned to refactor this unit after redoing my user authentication system a third time in 
<a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit8">Unit 8</a> with websocket &mdash; a technology
Cowboy supports &mdash; during which I improved my Javascript, leading to a realisation that instead of using cookies 
I could store whatever key-value pairs I needed in 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API">Web Storage</a> on the browser and ETS on the server,
joined by a unique ID replaced every login session to give it little value to hackers.

Ways to do <em>call</em> and <em>response</em> with a Javascript browser and a whatever-language server include the original 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest">XMLHttpRequest</a>, 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API">fetch</a> which I'm using here,
<a href="https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API">websocket</a> which I use in
<a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit7">Unit 7</a> to communicate between Erlang and Prolog and then again in
<a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit8">Unit 8</a> to communicate between Erlang and Javascript, and
there's also <a href="https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events">Server-sent events</a> and
<a href="https://www.ibm.com/developerworks/library/wa-reverseajax1/index.html">Comet</a> which I'm unfamiliar with.

I did <em>fetch</em> in this unit, and then <em>websocket</em> in the last unit. In retrospect I should have done them
the other way round &mdash; websocket is actually easier than fetch, and I had several "aha!" moments doing the last unit
which I've now had to come back and implement here.

Something that frustrates me about Javascript &mdash; and I'm guessing many others &mdash; is it's a language designed by committee,
cluttered with synonymous ways of doing anything. Googling the "best" way leads to a swamp of conflicting advice, 
usually advocating using JQuery, TypeScript, Angular, React... whatever library is top of the pops that week.

I'm sticking to a minimal <em>patois</em> of plain vanilla Javascript to keep things manageable, using
the examples in <a href="https://developer.mozilla.org/en-US/">MDN</a> as my reference.

<h2>Writing listening loops in Javascript</h2>

In my first iteration of this exercise I wrote my Javascript as an
<a href="https://developer.mozilla.org/en-US/docs/Glossary/IIFE">Immediately Invoked Function Expression</a> 
(IIFE to its friends) under the misconception the listener had to be wrapped in `(function() {...})();`
syntax which I personally find ugly and confusing.

Fortunatley, one of the side-effects of learning Erlang and its emphasis on thinking <em>concurrently</em> instead of <em>sequentially</em>
is viewing <em>event listeners</em> as the basic building blocks of programs rather than thinking in objects or functions. So I no longer
need IIFEs.

Javascript makes the Erlangish style of programing easy with
<a href="https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener">
EventTarget.addEventListener()</a> as in:

```javascript
<target>.addEventListener("EventType", (event) => {
  <response to event goes here...> 
});
```

To access the `<target>` when it's an `element`, as opposed to `window` or `document`, I'm going to standardise on
`document.querySelector("CSS Selector")` which involves refreshing my memory of when to use dots, hashes, square brackets,
and greater than signs in <a href="https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors">CSS selectors</a>.

Both <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit6/apps/unit6/priv/scripts/login.js">login.js</a> and
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit6/apps/unit6/priv/scripts/signup.js">signup.js</a> have 
entry points which look like this:

```javascript
document.querySelector("#loginButton").addEventListener("click", (event) => {
  ...
}).
```

A more elaborate example with lots of user manipulatable objects in the web page reacting to mouse hovers etc besides clicks would
simply involve lots of stanzas following the same basic pattern.

<h3>Changes to html</h3>

Since we are no longer using the browser's default <em>form</em> behaviour as in Unit 4, the opening `<form ...>` and closing `</form>` 
tags have to be removed (which messes up the CSS styling copied from Mozilla, but such is life on the interwebs).

Telling the <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button">
button</a>  
to send its click event to our event listener requires editing `type="submit"` to `type="button"`.

<h2>Fetch</h2>

Javascript <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises">promises</a>, 
first encountered in Unit 4's password hashing exercise, appear again here with
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch">fetch(url, Object)</a>.

```javascript
fetch("/login",
     { method: "POST"
     , headers: {"Content-Type": "application/json"}
     , body: JSON.stringify({"hash": hash})
     }
)
```
A huge number of parameters besides <em>method, headers</em>, and <em>body</em> are listed at
<a href="https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/fetch">Mozilla's reference page</a>.

Getting the response from the server to the browser involves chaining a then as in

```javascript
fetch(url, Object)
.then((response) => response.json())
.then((msg)) => {
  window.localStorage.setItem("uuid", msg.uuid);  
})
.catch((error) => {
  window.alert('Error:', error);
});
```
<h2>Messaging between Erlang and Javascript</h2>

Translation is needed to make the Erlang messages understandable to Javascript and vice versa, which is easily done
via Json. <a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit5">Unit5</a> introduced
`jsx:decode(Json) -> Erlang`, and now this unit will expand that to `jsx:encode(Erlang) -> Json` for two-way 
communication between browser and server.

Javascript in turn has handy
<a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify">
JSON.stringify(Javascript) -> Json</a>
and <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse">
JSON.parse(Json) -> Javascript</a>
counterparts to make messaging with an Erlang server easy.

In this example, the message from the server is always a succinct boolean `{"logged_in": true}` or `{"logged_in": false}`.

Instead of <em>303</em> redirects from the server, this is now done in the browser using Javascript's 
`document.location.replace(URL)` function, (which means a key reason I used Cowboy instead of the builtin inets httpd
module has gone).

If there are problems with the form, the existing values automatically remain where they are and error messages can be inserted
by the browser using `document.getElementById('error_id').textContent = "Error message"`, so I no longer need to use templating
on the server to make small changes and then schlep the whole web page back.

This means I can remove the `~s` placeholders in the HTML file.

In terms of messages from the browser to the server, the difference isn't very big. I'm still sending the data from the browser
in the body of a "POST" httpd request.

Whereas in Unit 4 I used

```erlang
 {ok, PostVals, _} = cowboy_req:read_urlencoded_body(Req0),
```
to get the `key1=value1;key2=value2;...` "POST" data, I now use

```erlang
  {ok, [{JString, true}], _} = cowboy_req:read_urlencoded_body(Req0),
  Proplist = jsx:decode(JString),
```
Having more control of what data gets posted from the browser, however, is nice. I no longer have to blank
the password, salt and verify keys, and Javascript's `async-await` construct works.

<h3>User Authentication with websocket and Web Storage</h3>

Instead of writing the user's ID hash as a cookie, I can turn to another of the myriad of web APIs
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API">Web Storage</a> which lets me
store key/value pairs temporarily during a session with
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage">Window.sessionStorage</a>
or permanently with
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage">Window.localStorage</a>.

Web Storage differs from cookies in that the key/value pairs are not sent to the server with each request, which
helps with <em>information hiding</em> in various senses of the word. 

I still need to send the hash created by the browser from the user's login name and password to verify with its "rehash" on the server,
but there's no longer any need to store it. Instead, the server can respond with a disposable session ID which it
remembers as an ETS key to temporarily store whatever is needed in the database (just the user name in this example), so no more
database hits are required for authentication until the user logs out and back in again, when a new session ID is created, foiling
anyone masquerading by somehow stealing someone else's cookie.

My method of creating a unique ID is hopefully overkill: I create a string by concatenating
<a href="https://erlang.org/doc/man/erlang.html#system_time-1">erlang:system_time(millisecond)</a> to get a precise instant in time, 
<a href="http://erlang.org/doc/man/erlang.html#make_ref-0">make_ref()</a> to get a "reference unique among connected nodes", and
<a href="http://erlang.org/doc/man/erlang.html#node-0">node()</a> in case some other node created the same make_ref/0
at precisely the same instant.

This string is then hashed with

```erlang
...
<<I:128>> = crypto:hash(md5, String),
Uuid = integer_to_binary(I, 16),
...
```
Furthermore, I check this key isn't already in ETS, and get a different key if it is.

The client and server just pass this uuid back and forth, with no need to wire already known data such as the user name &mdash; unless
the server wants the browser to say its user name to verify the uuid is not a lucky guess by a hacker.



Next &mdash; Unit 7: [Interlanguage communication via websocket](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit7)

