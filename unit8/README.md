<h1>Tying everything together into a blog</h1>

I'm finishing this series of tutorials by redoing the blog project in the Udacity <a href="https://classroom.udacity.com/courses/cs253">
Web Development</a> course, but in an Erlangish way instead of the original Google App Engine with Python.

Now that I've grasped the basics of websocket, I'm viewing the browser much as an Erlang node which sends and receives Json messages.

There is little new Erlang code in this unit, but I've had to dive deep into Javascript, abandoning Unit 2's 
<a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit2">basic form technique</a> and Unit 6's
<a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit6">Ajax</a>.

<h2>Javascript websocket client</h2>

A style rule I'm addopting to make Javascript more Erlangish is to write all my <em>nodes</em> as  
<a href="https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener">
event listeners</a> using this basic pattern:

```javascript
<target>.addEventListener("EventType", (event) => {
  <response to event goes here...> 
});
```

<h3>Opening the websocket connection</h3>

The palaver with the opening handshake, upgrade etc is easily done in Javascript by creating a global
<a href="https://developer.mozilla.org/en-US/docs/Web/API/WebSocket">WebSocket</a> object, which I'll call `websocket`.

```javascript
const websocket = new WebSocket("ws://localhost:3030/blog");
```

According to the documentation, your browser is likely to insist on a secure `wss://...` address unless it's a localhost connection, but I 
haven't advanced as far as a remote server with a proper domain name yet.

<h3>Closing the websocket connection</h3>

To get the browser to signal to the server the websocket connection can be closed because the user has moved to another website
or closed the tab, we can use <a href="https://developer.mozilla.org/en-US/docs/Web/API/Window">window</a> as
our `<target>.addEventListener("EventType", ...)`.

For `"EventType"` we have a choice of 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Window/beforeunload_event">beforeunload</a> or
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Window/unload_event">unload</a>. I'm going with
unload to keep the connection open until the last moment.

```javascript
window.addEventListener("unload", (event) => {
  websocket.close();
});
```

Websocket objects only have two <a href="https://developer.mozilla.org/en-US/docs/Web/API/WebSocket#Methods">methods</a>, <em>close()</em>
used above, which could alternatively be written more verbosely
`websocket.close(1000, "Normal Closure")` &mdash; MDN's 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent">CloseEvent</a> section has a definitive list of codes and reasons &mdash;
and <a href="https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send">websocket.send(Data)</a>. 

<h3>Receiving and responding to messages</h3>

```javascript
websocket.addEventListener("message", (event) => {
  msg = JSON.parse(event.data);
  <lots of code to redraw web page goes here>
});
```

MDN's <a href="https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent">MessageEvent</a> lists the properties,
the key one being `event.data`, which since I'm using Json could be converted into Javascript data with
`msg = JSON.parse(event.data);`.

If I want the client to initiate the conversation with the server, I would use

```javascript
websocket.addEventListener("open", (event) => {
   websocket.send(Data);
});
```

Something Erlang's and SWI Prolog's websocket implementations used in
<a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit7">Unit 7</a>
make very easy is switching between <em>text</em> and <em>binary</em> &mdash; you simply send
`{binary, Data}` for Erlang and `binary(Data)` for Prolog, and the receiver figures out from the opcode
what to do with it.

In Javascript, <em>binary</em> has to be specified by `websocket.binaryType = `
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Blob">"blob"</a> or
<a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer">"arraybuffer"</a>
and then lots of extra lines of code are needed for translation, so I've just kept the default <em>text</em>.

<h3>What if the websocket server is down?</h3>

```javascript
websocket.addEventListener("error", (event) => {
  window.alert('WebSocket error: ', event);
});
```

<h3>Client-side templating</h3>

Something I only became aware of doing this exercise was html's relatively new
<a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template">&lt;template&gt;</a> element, which
is well supported by modern browsers. MDN's 
<a href="https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_templates_and_slots">Using templates and slots</a>
section describes it as part of the 
<a href="https://developer.mozilla.org/en-US/docs/Web/Web_Components">Web Components</a> family to be used with the
<a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/slot">&lt;slot&gt;</a> element, which in turn relies on the
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Element/attachShadow">Element.attachShadow()</a> method.

One snag with &lt;slot&gt; is only very recent versions of Firefox and Chrome support it, and Microsoft browsers don't. 
Another is I find the documentation on Web Components incomprehensible.

Fortunately, just using &lt;template&gt; while ignoring &lt;slot&gt; and its associated shadow DOM is pretty easy.

We simply put a 
```html
<template id="mytemplate">
...
</template>
``` 
section in our html file which text editors, <em>tidy</em>,
etc see as <em>normal</em> html. Browsers don't render it, so in a sense templates are treaded like comments except syntax
highlighters and linters do see it as valid html.

Javascript can then extract what's inside the template with:

```javascript
let template = document.getElementById("mytemplate").content;
```

then do substitutions of data received from the server with lines like:

```javascript
template.querySelector("div.post-title").textContent = post.title;
```

Selecting `textContent` as opposed to `innerHTML` sorts out <a href="https://developer.mozilla.org/en-US/docs/Glossary/Cross-site_scripting">
Cross-site scripting</a> problems without any need to substitute `<` with `&lt;`.

Finally, the template can be inserted into the relevant position in the html with:

```javascript
let html = template.cloneNode(true);
document.getElementById("content").appendChild(html);
```

<h2>Erlang websocket server</h2>

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
<a href="http://erlang.org/doc/man/erlang.html#now-0">now()</a> to get a precise instant in time, 
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



<h2>SQL table</h2>

```sql
CREATE TABLE IF NOT EXISTS posts 
  ( id            SERIAL PRIMARY KEY
  , author        TEXT REFERENCES users (name)
  , subject       TEXT NOT NULL
  , content       TEXT NOT NULL
  , created       TIMESTAMP DEFAULT current_timestamp
  , last_modified TIMESTAMP DEFAULT current_timestamp
  );
```

