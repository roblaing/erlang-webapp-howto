# Howto write simple web applications with Erlang

*Robert Laing*

I'm adding Erlang to the list of languages &mdash; Python, Ruby, Node.js, [Prolog](https://github.com/roblaing/swipl-webapp-howto)... &mdash; I've taught myself to write simple web applications with.

I've developed <a href="https://www.joeblog.co.za/">joeblog.co.za</a> in tandem with writing these tutorials, so it's pretty much an example
of a <em>proper, production</em> site using what I've learned and am trying to teach here.

A reason I'm migrating my site <https://www.newsgames.biz> from SWI Prolog to Erlang is to make use of parallel computing, 
something Erlang was designed from scratch to do. A snag with the commonly used scripting languages mentioned above is they
have a <a href="https://en.wikipedia.org/wiki/Global_interpreter_lock">Global interpreter lock (GIL)</a> which means
they can't do multithreading. 

While SWI Prolog and other <em>functional</em> languages (as opposed to the horde of contemporary 
"pop" languages which have painted themselves into a corner) can access the underlying tools to do it, a language designed
from the ground up for parallel computing is a more elegant option.

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
rebar3 new release <myproject>
```

I've used unit1, unit2,... as `<myproject>` name to keep each section small and separate, but the idea it to gradually
develop one project called whatever you want step-by-step, leading to a fully-fledged blog with
user identification inspired by a course Reddid founder Steve Huffman gave on Udacity a few years ago.

```bash
rebar3 new release unit1
```

will create a tree of subdirectories and skeleton files which looks like:

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

Unit 1: [Basic, static page server](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit1)

Unit 2: [Forms with validation and redirection](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit2)

Unit 3: [Linking to a database (PostgreSQL)](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit3)

Unit 4: [User authentication](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit4) 

Unit 5: [Web services](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit5)

Unit 6: [Ajax](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit6)
 
Unit 7: [Interlanguage communication via websocket](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit7)

Unit 8: [Tying everything together into a blog](https://github.com/roblaing/erlang-webapp-howto/tree/master/unit8)

