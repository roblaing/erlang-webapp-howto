<h1>Interlanguage communication via websocket</h2>

In the story so far, I've "glued" a Javascript browser and PostgreSQL database together with an Erlang web server.

The next challenge is to use Erlang to glue on further components written in whatever language &mdash; Prolog in my example.

Hypothetically, Prolog comes glued on. One of Erlang's original creators, Robert Virding, wrote a version of Prolog called 
<a href="https://github.com/rvirding/erlog">Erlog</a>, which runs on OTP.

But I didn't get far with Erlog. The executable `bin/erlog` gave a crash report instead of launching a Prolog REPL on my machine,
which I'm guessing is because I'm using a recent OTP version, 22, whereas Erlog's documentation says it was
developed with Erlang versions R14B02 - 17.

And in any event I couldn't figure out from Erlog's sparse documentation how to use it as a "library language" from the Erlang REPL.

So instead I'm going to use the very thoroughly documented and maintained <a href="https://www.swi-prolog.org/">SWI Prolog</a>. 

The easiest way to get Erlang to interact with other programs on the same Linux machine would be with 
<a href="https://erlang.org/doc/man/os.html#cmd-1">cmd(Command) -> string()</a>. But I'm made of sterner stuff, so am going to use 
<em>websocket</em> &mdash; <a href="https://tools.ietf.org/html/rfc6455">RFC 6455</a> to its friends &mdash; 
a technology I'm completely unfamiliar with, but which is supported by just about every programing language, including
SWI Prolog, Erlang, and Javascript.

<h2>With websocket, the client becomes the server</h2>

I initially grappled with how to write a websocket server with Cowboy before grasping I had the problem the wrong way round &mdash;
I didn't want my web server to be a websocket server, but a websocket client.

Just as the <em>99s</em> team have written Cowboy as a replacement for the builtin library inets's 
<a href="http://erlang.org/doc/apps/inets/http_server.html">httpd</a>, they have written
<a href="https://ninenines.eu/docs/en/gun/2.0/guide/">Gun</a> as a replacement for inets's
<a href="http://erlang.org/doc/apps/inets/http_client.html">httpc</a> which is a client for
traditional HTTP/1.1, whereas Gun is also a client for websocket and HTTP/2.

Anyways, we need to add another dependency to rebar.config:

```erlang
...
       , {gun, {git, "https://github.com/ninenines/gun", {branch, "master"}}}
...
```

and add it to the applications list in the applications resource file.

<h2>SWI-Prolog websocket server</h2>

Starting with the example in the official SWI Prolog 
<a href="http_open_websocket(+URL, -WebSocket, +Options)">websocket</a> documentation, I created
swipl_server.pl as follows:

```prolog
:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(ws),
                http_upgrade_to_websocket(echo, []),
                [spawn([])]).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

echo(WebSocket) :-
    ws_receive(WebSocket, Message),
    debug(websocket, 'Got ~p', [Message]),
    ws_send(WebSocket, Message),
    (   Message.opcode == close
    ->  true
    ;   echo(WebSocket)
    ).
```

Then from a swipl REPL, I entered the following:
```prolog
?- consult("swipl_server.pl").
true.

?- debug(websocket).
true.

?- server(3031).
% Started server at http://localhost:3031/
true.
```

SWI Prolog's <a href="https://www.swi-prolog.org/pldoc/doc_for?object=debug/3">debug/3</a> predicate is similar to 
Erlang's io:format/2, but only prints messages to the terminal if it is "switched on" by having whatever atom is used as
the first argument (called <em>Topic</em> in the documentation, and <em>websocket</em> in this example) called by debug/1.

<h2>Erlang websocket client</h2>

Opening a separate tab in my bash terminal emulator, and in the root of the Unit 7 project, I ran `rebar3 shell` (having
already run `rebar3 release` to get Gun installed), and then ran the following commands, with the debug output from the
swipl terminal besides it:

<table>

<tr>
<td><pre><code>
1> {ok, ConnPid} = gun:open("localhost", 3031).
{ok,<0.252.0>}
</code></pre></td>
</td>
<td><pre><code>
</code></pre></td>
</tr>

<tr>
<td><pre><code>
2> {ok, Protocol} = gun:await_up(ConnPid).
{ok,http}
</code></pre></td>
</td>
<td><pre><code>
</code></pre></td>
</tr>

<tr>
<td><pre><code>
3> StreamRef = gun:ws_upgrade(ConnPid, "/ws").
#Ref<0.596962980.3933732865.48581>
</code></pre></td>
</td>
<td><pre><code>
?- % [Thread 9] ws_receive(<stream>(0x7f93d00266c0,0x7f93d0028980)): OpCode=9, RSV=0
</code></pre>
... repeated every few seconds
</td>
</tr>

<tr>
<td><pre><code>
4> {upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef).
{upgrade,[<<"websocket">>],
         [{<<"upgrade">>,<<"websocket">>},
          {<<"connection">>,<<"Upgrade">>},
          {<<"sec-websocket-accept">>,
           <<"XXw/nNAZ45OQePJ6JBRRoiRH3qk=">>}]}
</code></pre></td>
</td>
<td><pre><code>
</code></pre></td>
</tr>

<tr>
<td><pre><code>
5> Frame = {text, <<"Hello!">>}.
{text,<<"Hello!">>}
6> gun:ws_send(ConnPid, Frame).
ok
</code></pre></td>
</td>
<td>
<pre><code>
% [Thread 9] ws_receive(<stream>(0x7f93d00266c0,0x7f93d0028980)) --> 
websocket{data:"Hello!",format:string,opcode:text}
</code></pre>
... repeated every few seconds
<pre><code>
% [Thread 9] Got websocket{data:"Hello!",format:string,opcode:text}
% [Thread 9] ws_receive(<stream>(0x7f16380266c0,0x7f1638028980)): OpCode=9, RSV=0
</code></pre>
... repeated every few seconds
</td>
</tr>

<tr>
<td><pre><code>
7> {ws, Frame} = gun:await(ConnPid, StreamRef).
{ws,{text,<<"Hello!">>}}
</code></pre></td>
</td>
<td><pre><code>
% [Thread 9] ws_receive(<stream>(0x7f16380266c0,0x7f1638028980)): OpCode=9, RSV=0
</code></pre>
... repeated every few seconds
</td>
</tr>

<tr>
<td><pre><code>
8> gun:close(ConnPid).
ok
</code></pre></td>
</td>
<td><pre><code>
% [Thread 9] Got websocket{data:end_of_file,opcode:close}
ERROR: [Thread 9] Socket error: Broken pipe
</code></pre></td>
</tr>

</table>

