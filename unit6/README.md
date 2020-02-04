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

<h2>Writing listening loops in Javascript</h2>

The first big difference between login.js from Unit4 to this unit is it now contains an
<a href="https://developer.mozilla.org/en-US/docs/Glossary/IIFE">Immediately Invoked Function Expression</a> 
(IIFE to its friends).

In this simple example, our IIFE is only listening out for clicks on the single button in the HTML form, but in a more complicated
example it could be waiting and reacting to any number of keyboard, mouse, joystick... events sent to it by whatever the
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction">Document Object Model (DOM)</a>
contains.

I've made the button's id "ajaxButton" as in Mozilla's <a href="https://developer.mozilla.org/en-US/docs/Web/Guide/AJAX/Getting_Started">
Getting Started</a> tutorial.

Getting our script to listen to this event simply involves the line:

```js
document.getElementById("ajaxButton").onclick = function() {...} 
```

<h3>Changes to login_form.html</h3>

Telling the <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button"><code><button></code></a>  
to send its click event to our listening loop instead of the form element as before in Unit4 requires
editing `type="submit"` to `type="button"`.

The button also needs an id so the Javascript code can reference it.

Since we are no longer using the browser's form functions as in Unit 4, the opening `<form ...>` and closing `</form>` 
parent tags could be removed, though I've kept them (removing the no longer used attributed in the opening form tag).
so as not to have to change the style sheet I took from the Mozilla tutorial.

<h2>Changes to login.js</h2>

Ajax used to be synonymous with Javascript's
<a href="https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest">XMLHttpRequest</a> object,
but relatively recently a more succinct alternative, a
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch">fetch</a> promise appeared which I'm going to use.

In some ways `XMLHttpRequest` is more readable since it puts things in auxiliary functions whereas
`fetch(...)` <a href="https://en.wikipedia.org/wiki/Agglutination">agglutinates</a> them
with <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then">.then(...)</a>
methods, which looks fairly alien to me.

A key difference between `XMLHttpRequest` and `fetch` is their respective ways of handling timeouts, which I'm ignoring for now.


