<h1>Tying everything together into a blog</h1>

A wonderful online resource I only recently discovered is
<a href="http://aosabook.org/en/index.html">The Architecture of Open Source Applications</a> which provides a compendium of examples
by experienced designers on how they tackled various projects. 

One of the "small examples" in the series is similar to what I'm doing here:
<a href="http://aosabook.org/en/500L/an-event-driven-web-framework.html">An Event-Driven Web Framework</a>. The article's author
Leo Zovic first lists the various ways in which servers and clients can message each other
&mdash; besides websocket which I'm using here, there's Ajax which I used in 
<a href="https://github.com/roblaing/erlang-webapp-howto/tree/master/unit6">Unit 6</a>, 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events">Server-sent events</a>, and 
<a href="https://www.ibm.com/developerworks/library/wa-reverseajax1/index.html">Comet</a>, also knows as "reverse Ajax" or 
the "long poll" technique.

Zovic warned that all these "long-lived connections" approaches could cause problems if the site gets too popular because each connection
equates to a thread, and threads are only "cheap" in a small, select group of programming languages:

<q>There are programming environments such as Racket, Erlang, and Haskell that provide thread-like constructs that are "lightweight" enough to consider the first option.</q> 

For his chosen programming language, Common Lisp, Zovic opted for an <em>event-driven</em> pattern, which I wasn't aware differs from a
<em>thread-based</em> pattern:

<q>This nomenclature is a bit confusing, and has its origin in early operating-systems research. It refers to how communication is done between multiple concurrent processes. In a thread-based system, communication is done through a synchronized resource such as shared memory. In an event-based system, processes generally communicate through a queue where they post items that describe what they have done or what they want done, which is maintained by our single thread of execution. Since these items generally describe desired or past actions, they are referred to as 'events'.</q>

I'm finishing this series of tutorials by redoing the blog project in the Udacity <a href="https://classroom.udacity.com/courses/cs253">
Web Development</a> course, but in an Erlangish way instead of the original Google App Engine with Python.

I started this exercise before reading Zovic's warning about Websocket, but since I'm using one of the three languages he says can viably handle
this approach, decided to stick with it.

There is little new Erlang in this exercise, which mainly involves pushing the work such as html templating from the server to browser, thereby
reducing the lines of Erlang code while increasing the lines of Javascript code.

The switch to getting the client and server to communicate by short Json messages means I no longer use a cookie for user authentication.
Instead I'm storing whatever key-value pairs I need in 
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API">Web Storage</a> on the browser and ETS on the server,
joined by a unique ID replaced every login session to give it little value to hackers.

<h2>Javascript style</h2>

Something that frustrates me about Javascript &mdash; and I'm guessing many others &mdash; is it's a language designed by committee,
cluttered with synonymous ways of doing anything. Googling the "best" way leads to a swamp of conflicting advice, so I've settled
on the examples in <a href="https://developer.mozilla.org/en-US/">MDN</a> as my reference.

A style rule I'm adopting to make Javascript more Erlangish is to write all my <em>nodes</em> as  
<a href="https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener">
event listeners</a> using this basic pattern:

```javascript
<target>.addEventListener("EventType", (event) => {
  <response to event goes here...> 
});
```

To access the `<target>` when it's an `element`, as opposed to `window` or `document`, I'm going to standardise on
`document.querySelector("CSS Selector")` which involves refreshing my memory of when to use dots, hashes, square brackets,
and greater than signs in
<a href="https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors">CSS selectors</a>.

I find trying to keep up with JQuery, TypeScript, Angular, React... whatever is top of the pops this week exhausting, and
sticking to a minimal <em>patois</em> of plain vanilla Javascript keeps things manageable.

<h2>Javascript websocket client</h2>

My first attempt involved opening a websocket when the page was loaded and then assuming I could keep it open until the page was
unloaded. The result was clicking the submit button sometimes succeeded in getting the post added to the database, and othertimes 
ignored because the connection between the browser and the server had mysteriously died without warning.

This prompted me to rewrite my code to assume short-lived websockets, created when needed and then closed.

To initially populate the page with posts, I moved the websocket as a short-lived variable with its methods into:

```javascript
window.addEventListener("DOMContentLoaded", (event) => {
  let websocket = new WebSocket("ws://localhost:3030/blog");
  ...
});
```

<h3>Opening the websocket connection</h3>

The palaver with the opening handshake, upgrade etc is easily done in Javascript by creating a global
<a href="https://developer.mozilla.org/en-US/docs/Web/API/WebSocket">WebSocket</a> object, which I'll call `websocket`.

```javascript
let websocket = new WebSocket("ws://localhost:3030/blog");
```

According to the documentation, your browser is likely to insist on a secure `wss://...` address unless it's a localhost connection, but I 
haven't advanced as far as a remote server with a proper domain name yet.

<h3>Closing the websocket connection</h3>

I originally got the browser to signal to the server the websocket connection can be closed because the user has moved to another website
or closed the tab, we can use 

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

<h2>Erlang websocket server</h2>

Cowboy's User Guide recommends in websocket's
<a href="https://ninenines.eu/docs/en/cowboy/2.7/guide/ws_handlers/#_saving_memory">saving memory</a> section to add
the optional <em>hibernate</em> atom to the tuple returned by its various handlers to save memory, which I've done.

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



<h3>Client-side templating</h3>

A cool, albeit frustrating, thing about Javascript is its rapid evolution, and I discovered all kinds of new ways to do things during this exercise,
including the <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template">&lt;template&gt;</a>
element, allowing me to ditch my server-side text substitution hack. MDN's 
<a href="https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_templates_and_slots">Using templates and slots</a>
section describes it as part of the 
<a href="https://developer.mozilla.org/en-US/docs/Web/Web_Components">Web Components</a> family to be used with the
<a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/slot">&lt;slot&gt;</a> element, which in turn relies on the
<a href="https://developer.mozilla.org/en-US/docs/Web/API/Element/attachShadow">Element.attachShadow()</a> method.

One snag with &lt;slot&gt; is only very recent versions of Firefox and Chrome support it, and Microsoft browsers don't. 
Another is I find the documentation on Web Components incomprehensible.

Fortunately, just using &lt;template&gt; while ignoring &lt;slot&gt; and its associated shadow DOM is pretty easy.

Templating this way encourages using 
<a href="https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Using_HTML_sections_and_outlines">
section elements</a> &lt;nav&gt;, &lt;article&gt;, &lt;section&gt;, 
&lt;aside&gt;, &lt;header&gt;, and &lt;footer&gt;. 

We simply put
```html
<section>
<template>
  <header class="post-heading">
    <h2 class="post-title">Title</h2>
    <span class="post-date">Date</span>
  </header>
  <pre class="post-content">Art</pre>
</template>
</section>
``` 
code in our html file which text editors, <em>tidy</em>,
etc see as <em>normal</em> html. Browsers don't render it, so in a sense templates are treaded like comments except syntax
highlighters and linters do see it as valid html. I've kept &lt;section&gt; and &lt;template&gt; at the same indentation level
since the &lt;template&gt; element will effectively get substituted into the &lt;section&gt; element in the rendered page.

Javascript can then extract what's inside the given section's template child with

```javascript
let template = document.querySelector("section > template").content;
```

then do substitutions of data received from the server with lines like:

```javascript
template.querySelector("h2.post-title").textContent = post.title;
```

Using `textContent` instead of `innerHTML` sorts out <a href="https://developer.mozilla.org/en-US/docs/Glossary/Cross-site_scripting">
Cross-site scripting</a> problems without any need to substitute `<` with `&lt;`.

The filled in template can then be inserted into the
rendered html page using:

```javascript
let html = template.cloneNode(true);
document.querySelector("nav").appendChild(html);
```
As in this example, <a href="https://developer.mozilla.org/en-US/docs/Web/API/Node/appendChild">Node.appendChild()</a> 
can be used iteratively to render a template filled in with different values any number of times.

One gotcha was using a button in a template for a form I only wanted displayed if the user was logged in. I found
the solution in this article <a href="https://css-tricks.com/crafting-reusable-html-templates/">Crafting Reusable HTML Templates</a>.
It turned out the Javascript linking the button to an event listener had to be in a &lt;script&gt; section within the 
&lt;template&gt; block, not in the external Javascript file where it was seen as `null`.

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

