erlsqlacore
===========

Using erlport [ thank you Dmitry! https://github.com/hdima/erlport ] to expose a grossly simplified subset of the SQLAlchemy Core API as a small set of Erlang shell commands.

I haven't packaged this up yet, too busy iterating...

The command line interface once you're in the Erlang shell is:

>> compile
c(esqla).

>> establish port connection
Port = esqla:init().

>> start up SQLAlchemy engine, in this example with Sqlite and on-disk storage
>> just change the 'configuration string' to connect to a running Postgres cluster, e.g.:
>>     "postgresql://user:password@/dbname"
>> or Mysql, e.g.:
>>     "mysql://user:password@localhost/dbname"
>> ...check the excellent SQLAlchemy documentation online for details at sqlalchemy.org
>>
>> start() also loads the test database into the chosen backend
esqla:start(Port, "sqlite:////tmp/test.db").

>> get() returns entire matching row currently - absent parameters match ANY
esqla:get([ "table_name", [ {"column_name", val}, ...], 
			        [ {"limit": val}, {"offset": val} ] ])

>> upsert() is update (on primary key match) OR insert (if primary key non-existent) 
esqla:upsert([ "table_name", {"primary_key_name", val},
			        [ {"column_name", val}, ...] ])
				
>> remove() enthusiastically deletes matching rows - absent parameters match ANY!!!
esqla:remove([ "table_name", [ {"column_name", val}, ...] 


I should describe this thing better, though.

esqla.erl delivers a simple command line get/upsert/remove interface to Python SQLAlchemy, specifically the Core API of that product, which abstracts the connection-level details of interacting with basically any extant SQL/RDBMS database - I've tested with MySql 5.x, Postgres 9.2.x, and Sqlite. 

In addition to esqla I provide two Python management modules:

ErlSqlaCore.py, which handles the Erlang port interactions, and

SqlaCore.py, which massages data in and out of the database.

I will leave installation of SQLAlchemy to the reader, for now. I'm running 0.7.9, the latest stable version. 

I include a sample SQL database and load file (combined into "schema1.sql") which is currently hard-coded to load when the ersqlacore starts up.


>> example get() from test DB:
esqla:get(P, ["artist", [{}],  [{}] ]).                                
[["artist","artistid",1],
 ["artist","artistname","bobby"],
 ["artist","artistgender","M"],
 ["artist","artistbday","1961-05-15"],
 ["artist","artistid",2],
 ["artist","artistname","diane"],
 ["artist","artistgender","F"],
 ["artist","artistbday","1960-07-08"],
 ["artist","artistid",3],
 ["artist","artistname","lashana"],
 ["artist","artistgender","TX"],
 ["artist","artistbday","1991-04-09"],
 ["artist","artistid",4],
 ["artist","artistname","lucy"],
 ["artist","artistgender","F"],
 ["artist","artistbday","1950-12-25"],
 ["artist","artistid",5],
 ["artist","artistname","sid"],
 ["artist","artistgender","M"],
 ["artist","artistbday","1960-12-24"],
 ["artist","artistid",6],
 ["artist","artistname","brian"],
 ["artist","artistgender","M"],
 ["artist","artistbday","1950-02-25"],
 ["artist","artistid",7],
 ["artist","artistname",[...]],
 ["artist",[...]|...],
 [[...]|...]]


>> add a match filter:
esqla:get(P, ["artist", [{"artistid", 1}], [{}]]).
[["track","trackid",1],
 ["track","trackname","love song one"],
 ["track","tracklength",360],
 ["track","trackdate","2008-08-08"]


>> delete that row:
esqla:remove(P, ["artist", [{"artistid", 1}]).
1

>> insert it again:
esqla:upsert(P, ["artist", {"artistid", 1}, [{"artistname","bobby"}, {artistgender","M"}, {"artistbday","1961-05-15"}]]).
1
