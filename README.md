# Howto write simple web applications with Erlang

I'm adding Erlang to the list of languages &mdash; Python, Ruby, Node.js, [Prolog](https://github.com/roblaing/swipl-webapp-howto)... &mdash; I've taught myself to write simple web applications with.

A reason I'm migrating my site <https://www.newsgames.biz> from SWI Prolog to Erlang is to make use of parallel computing, 
something Erlang was designed from scratch to do. A snag with the commonly used scripting languages mentioned above is they
either have a <a href="https://en.wikipedia.org/wiki/Global_interpreter_lock">Global interpreter lock (GIL)</a> which means
they can't do multithreading, or &mdash; in the case of SWI Prolog &mdash; include wrappers to C's pthreads and mqueue libraries
which are not for the faint hearted.

Erlang occupies a small niche which its founder Joe Armstrong termed <em>Concurrency Oriented Programming</em>, hiding
the complexities of distributed computing from application developers. 

## Installation

Somewhat confusingly, Erlang is packaged as `otp_src_<version number>.tar.gz`. OTP is to Erlang what JVM is to Java, and much
as other languages such as Clojure use JVM as a kind of "operating system on top of an operating sytem", a Ruby-inspired language,
Elixer, runs on OTP.

Erlang is a "son of Prolog", which I like. But if you prefer Ruby, Elixer may be a better option. Erlang comes included in OTP,
whereas Elixer &mdash; and a range of other languages such as <a href="https://github.com/rvirding/erlog">Erlog</a> which I'm keen to tinker with
&mdash; are add-ons.

To get started with Erlang, head to <https://www.erlang.org/downloads>, download the latest `otp_src_<version number>.tar.gz`
and follow the build instructions.

Erlang has an extremely rich &mdash; but bewildering to a novice &mdash; set of tools. I initially tried to be a purist
and only use what was included in the OTP package, but soon got tripped up with its <a href="http://erlang.org/doc/apps/inets/index.html">inets
</a> library. I couldn't figure out how to do redirections from forms as explained in Unit 2, and a question I posted at
<https://stackoverflow.com/questions/59868567/can-erlangs-mod-esi-redirect> has so far gone unanswered.

Abandoning inets, I opted for a third-party Erlang web server library, <a href="https://ninenines.eu/">Cowboy</a> &mdash; other choices include
<a href="https://hexdocs.pm/elli/">Elli</a>, <a href="https://github.com/mochi/mochiweb">MochiWeb</a>... which I have no experience with.

Adding third-party libraries to your Erlang project is very easy once you've decided between <a href="https://www.rebar3.org/">rebar3</a>
or <a href="https://erlang.mk/">erlang.mk</a>. I've gone for rebar3. Though the Cowboy beginner tutorial uses erlang.mk (they share developers),
I've used rebar3 with Cowboy and found it very easy.

So to get started, you need to install OTP and rebar3 by following the instructions on their respective websites.

For an overview of using rebar3, I suggest an online book <a href="https://adoptingerlang.org/">Adopting Erlang</a> which includes links to
the many other educational resources available to Erlang learners.

Step 1 is (from whatever project root directory you are using) to invoke

```bash
rebar3 new release unit1
```

which will create a tree of subdirectories and skeleton files which looks like:

```
unit1/
├── .gitignore    
├── rebar.config
├── LICENSE
├── README.md
├── apps/
│   └──unit1/
│      └── src/
│          ├── unit1_app.erl
│          ├── unit1_sup.erl
│          └── unit1.app.src
└── config/
    ├── sys.config
    └── vm.args
```

[Unit 1](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit1): Basic, static page server


