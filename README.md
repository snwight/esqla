erlsqlacore
===========

Using erlport [thank you Dmitry! hdima/erlport] to expose a grossly simplified subset of the SQLAlchemy Core API as a small set of Erlang shell commands.

I haven't packaged this up yet, too busy iterating...

The working functionality is current only the get:

erlsqlacore:get("table_name", [ {"column_name", val}, ...], 
			      [ {"limit": val}, {"offset": val} ])

I should describe this thing better, though.

erlsqlacore.erl delivers a simple command line get/upsert/remove interface to the Python SQLAlchemy, specifically the Core API of that product, which abstracts the connection-level details of interacting with effectively any extant SQL/RDBMS database - I've tested with MySql 5.x, Postgres 9.2.x, and Sqlite. 

In addition to erlsqlacore I provide two Python management modules:
ErlSqlaCore.py, which handles the Erlang port interactions, and
SqlaCore.py, which massages data in and out of the database.

I will leave installation of SQLAlchemy to the reader, for now. I'm running 0.7.9, the latest stable version. 

I include a sample SQL database and load file (combined into "schema1.sql") which is currently hard-coded to load when the ersqlacore starts up.

The command line interface once you're in the Erlang shell and have loaded/compiled erlsqlacore.erl is:

> Port = erlsqlacore:init(). 
> erlsqlacore:start(Port, "sqlite:////tmp/test.db").

... now you've loaded the test DB into sqlite [our default] and can start making queries, for example:

> erlsqlacore:get(P, ["artist", [{"artistid", 1}], [{}]]).

...which spits back a permuted row from the test data:
[["track","trackid",1],
 ["track","trackname","love song one"],
 ["track","tracklength",360],
 ["track","trackdate","2008-08-08"]
