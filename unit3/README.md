<h1>Unit 3: Linking to a database</h1>

I like PostgreSQL, but even after whittling the choice down to one specific database, the choice of
third-party libraries for Erlang remains bewildering. Fairly randomly, I've selected one called 
<em>PG...Oh god not nother Postgres client in Erlang...</em>, <a href="https://github.com/erleans/pgo">pgo</a>.

Other choices include using the database bundled with OTP, Mnesia, and the standard libraries also include
ODBC support which should make most databases accessible. As I mentioned in 
<a href="https://github.com/roblaing/swipl-webapp-howto/tree/master/unit3">Unit 3</a> of my SWI Prolog web application 
tutorial (which I'm redoing here in Erlang), I'm no fan of Microsoft's ODBC "standard" whose poor documentation cost me
several hours.

I'm assuming you have PosgreSQL (or MySQL, or whatever you want to use) installed and running on your computer. If
not, head to <a href="https://www.postgresql.org/">https://www.postgresql.org/</a> and follow the instruction.

Within your database, I assume you have a table which in Postgres's SQL dialect looks like so:

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
<a href="https://github.com/roblaing/erlang-webapp-howto/blob/master/unit3/apps/unit3/src/unit3.app.src">:

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



