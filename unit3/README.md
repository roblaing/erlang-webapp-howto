<h1>Unit 3: Linking to a database</h1>

I like PostgreSQL, but even after whittling the choice down to one specific database, the choice of
third-party libraries for Erlang remains bewildering. Fairly randomly, I've selected one called 
<em>PG...Oh god not nother Postgres client in Erlang...</em>, <a href="https://github.com/erleans/pgo">pgo</a>.

Other choices include using the database bundled with OTP, Mnesia, and the standard libraries also include
ODBC support which should make nearly every database accessible. As I mentioned in 
<a href="https://github.com/roblaing/swipl-webapp-howto/tree/master/unit3">Unit 3</a> of my SWI Prolog web application 
tutorial (which I'm redoing here in Erlang), I'm no fan of Microsoft's poorly documentated ODBC "standard".

I'm assuming you have PostgreSQL (or MySQL, or whatever you want to use) installed and running on your computer. If
not, head to <a href="https://www.postgresql.org/">https://www.postgresql.org/</a> and follow the instructions.

In this tutorial, I'm recreating a project from Udacity's Steve Huffman course which he called ASCIIChan. This lets
users (without verification at this stage, we add that in Unit 4) submit ASCII art to the site, which can be 
obtained from <a href="https://www.asciiart.eu/">https://www.asciiart.eu/</a>. The submitted art is stored in a database and
imediately rendered on the page. 

Within your database, you need a table which in Postgres's SQL dialect looks like so:

```sql
CREATE TABLE IF NOT EXISTS arts (
    id      SERIAL PRIMARY KEY,
    title   TEXT NOT NULL,
    art     TEXT NOT NULL,
    created TIMESTAMP DEFAULT current_timestamp
);
```
<h2>1. Adding the database driver as a dependency</h2>

If you're not using Mnesia or ODBC, you need to add whatever third-party database client you are using to the dependency list 
in <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit3/rebar.config">rebar.config</a> 
along with Cowboy. For pgo, my list looks like this:

```erlang
{deps, [ {cowboy, {git, "https://github.com/ninenines/cowboy.git", {branch, "master"}}},
         {pgo, {git, "https://github.com/tsloughter/pgo.git", {branch, "master"}}}
       ]
}.
```
To get this new application started, add it to the list in
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit3/apps/unit3/src/unit3.app.src">
apps/unit3/src/unit3.app.src
</a>

```erlang
  {applications,
   [kernel,
    stdlib,
    cowboy,
    pgo
   ]},
```

<h2>2. Put database login etc info in config/sys.config</h2>

Your application gets the database name, username, password if needed, and whatever else from a tuple
you need to add to the <a href="">config/sys.config</a> file created by rebar3:

```erlang
[
  {unit3, []},
  {pgo, [{pools, [{default, #{ pool_size => 10
                             , host => "127.0.0.1"
                             , database => "my_database"
                             , user => "user_name"
                             , password => "my_password"
                             }
                   }
                  ]
         }
        ]
  }
].
```

<h2>3. Adding routes</h2>

The <a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit3/apps/unit3/src/unit3_app.erl">
apps/unit3/src/unit3_app.erl</a> file only differs from the ones in unit1 and unit2 (besides the different application name)
in having the following route and handler name in the list:

```erlang
    , {"/arts", arts_handler, []}
```

<h2>4. Writing the handler</h2>

I discovered the hard way in my earlier adventures in SWI Prolog that ASCII art is good test material because if there are problems 
with user input text messing up your HTML or exposing you to SQL-injection attacks by not escaping single quotes, a random
selection of ASCII art is likely to bring up the symptoms.

A particularly good test I accidently discovered was the <b>Little Linux penguin</b> by Joan G. Stark:

```
       .---.
      /     \
      \.@-@./
      /`\_/`\
     //  _  \\
    | \     )|_
   /`\_`>  <_/ \
jgs\__/'---'\__/

```

The reason is it contains single quotes, which are dangerous within SQL strings which are bounded by single quotes. 
To escape single quotes not intended to end the string, they must be preceeded by a single quote. I achieved this
in 
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit3/apps/unit3/src/arts_handler.erl">arts_handler</a>
like so:


```erlang
      ...
      EscapedTitle = string:replace(Title, "'", "''", all),
      EscapedArt = string:replace(Art, "'", "''", all),
      Query = io_lib:format("INSERT INTO arts (title, art) VALUES ('~s', '~s')", [EscapedTitle, EscapedArt]),
      pgo:query(Query),
      ...
```

Most of arts_handler is nearly indentical to Unit 2's
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit2/apps/unit2/src/form_handler.erl">form_handler</a>,
but what is new is the need to render a list obtained from the database into HTML &mdash; a very common task in web applications.

Once you have some data in the arts table, calling

```erlang
pgo:query("SELECT title, art FROM arts ORDER BY created DESC")
```

returns a map with three keys:

```erlang
{command => select, num_rows => N, rows => [{Title1, Art1}, {Title2, Art2}, ...]}
```

So what we want to do is translate each of the {Title1, Art1} tuple in the rows list into an HTML string to 
insert into our template.

<h3>A quick digression into list processing in Erlang</h3>

Anyone coming from a traditional C-family language might assume you do this with a <em>for loop</em>,
and an initial shock when learning languages under the jargon umbrella of <em>functional programming</em> 
is no support for <em>for loops</em>.

A reason is Erlang, like its ancestor Prolog, does not permit variables to change their value once they have been set, breaking
for loops which rely on constantly overwriting the value of a counter by incrementing or decrementing it from a start to and end value.  

Even in traditional programming languages, I've noticed an increasing <em>for loops considered harmful</em> attitude, with the alternatives
from functional languages getting adopted by Python, Ruby, Javascript etc.

While limiting oneself to <em>immutable</em> variables at first seems weird, languages which have adopted this have been able to embrace
parallel computing far more easily by avoiding the horror of <em>mutex locks</em>. 

Some of the ways to iterate in Erlang include:

<dl>
  <dt><a href="https://learnyousomeerlang.com/recursion">Recursion</a></dt>
  <dd>Notes I wrote on the many ways of iterating in Prolog are on <a href="https://swish.swi-prolog.org/p/Iteration2.swinb">
     Swish</a>. This method is most fresh to me at the moment, so I'll start with it and then rewrite my code using map and 
     as a list comprehension to refresh my memory.</a>.
  <dt><a href="https://erlang.org/doc/man/lists.html#map-2">lists:map(Fun, List1) -> List2</a></dt>
  <dd>I first encountered <em>map</em> in a course on Meta Language (ML), and find the <em>map-reduce</em>
      way of thinking it encourages very helpful. The word map is a bit confusing in Erlang since it also
      uses it for what Python calls <em>dictionaries</em> and Json <em>objects</em>.</dd>
  <dt><a href="https://erlang.org/doc/programming_examples/list_comprehensions.html">List Comprehensions</a></dt>
  <dd>Popularised by Python, but invented by Erlang.</dd>
</dl>

<h4>Recursive solution</h4>

Note this is called with <code>title_art(Rows, "")</code> with the initial value of the Html string
produced set to empty.

```erlang
-spec title_art([{Title::string(), Art::string()}], Html0::string()) -> Html1::string().
%% @doc Convert the title and art entries read from the arts table into HTML.
title_art([], Html) -> Html;
title_art([{Title, Art}|Tail], Html0) ->
    Html1 = io_lib:format("
~s
    <h2>~s</h2>
    <pre>
~s
    </pre>
", [Html0, Title, Art]),
    title_art(Tail, Html1).
```

The indentation is a bit ugly because I'm more worried about the indentation in the HTML generated than this code.

An important thing I haven't been doing so far is using Erlang's `-spec ...` command which is part of its documentation
system Edoc and its Dialyzer specification checker.

I generally try to follow the recipe taught by MIT's free
<a href="https://htdp.org/2019-02-24/part_preface.html#%28part._sec~3asystematic-design%29">How to design programs</a> textbook
which teaches you to write down a <em>signature</em> (equating to the -spec line) and a <em>purpose statement</em>
(equating to the %% @doc ... line) before starting to code. This helps making it clear in your mind what your
function is going to produce and consume, and reminds you not to break its <em>contract</em> with existing code that uses
it when you rewrite it.

<h4>Map</h4>

Mapping problems typically involve an output list which is the same as the input list with each item translated somehow.
This is not quite what we want here since we want one item at the end, making this a <em>foldl</em> style problem.

Rewriting this using <a href="https://erlang.org/doc/man/lists.html#foldl-3">foldl(Fun, Acc0, List) -> Acc1</a>
gets rid of needing to keep an accumulator argument in the function, so perhaps this should be rewritten as
a one arity function, getting rid of the now <code>_</code> <em>don't care</em> second argument.

Again, the indentation is weird because the focus is on the indentation of the generated Html.

```erlang
title_art(Rows, _) ->
  lists:foldl(fun({Title, Art}, Html) ->
    Html ++ io_lib:format("
    <h2>~s</h2>
    <pre>
~s
    </pre>
", [Title, Art]) end, "", Rows).
```


