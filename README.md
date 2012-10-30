esqla
======

Using erlport [ thank you Dmitry! https://github.com/hdima/erlport ] to expose a grossly simplified subset of the SQLAlchemy Core API as a small set of Erlang shell commands.

I've packaged this up as an OTP application, comprised of three Erlang modules (esqla_app, esqla_sup, esqla_server), the obligatory config source file (esqla.app.src) and two Python scripts (esqla.py and sqla.py). In addition you will need erlport and of course SQLAlchemy 0.7.4 or later.  

You'll want to clone this repos:
> git clone git@github.com:snwight/erlsqlacore.git

> cd esqla/priv

Grab erlport and clone it as-is into esqla/priv:
> git clone git://github.com/hdima/erlport.git

> cd esqla

Assuming you have rebar in your exec path:

> rebar compile

> cd ebin

Start a shell and the application:

> erl
>> Erlang R15B02 (erts-5.9.2) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]
>> Eshell V5.9.2  (abort with ^G)

1> l(esqla_app).
{module,esqla_app}

2>  application:start(esqla).
ok

This initializes and starts up the Python SQLAlchemy engine, in this example with Sqlite and on-disk storage - just change the 'configuration string' to connect to a running Postgres cluster, e.g.:
    "postgresql://user:password@/dbname"
or Mysql, e.g.:
    "mysql://user:password@localhost/dbname"
...check the excellent SQLAlchemy documentation online for details at sqlalchemy.org


And here is the new three-command API:

get() - returns entire matching row currently - absent parameters match ANY
> get("table", [{"column", val},...], [{"limit", val}, {"offset", val}])

upsert() - update OR insert new (if primary key non-existent) 
> upsert("table", {"primary_key, val}, [{"column", val},...])

remove() - deletes matching rows
> remove("table", [{"column", val},...]) 

schemata() - dumps out the current active schema as 'reflected' by SQLAlchemy
> schemata()


I should describe this thing better, though.

esqla.erl delivers a simple command line get/upsert/remove and schema inspect interface to Python SQLAlchemy, specifically the Core API of that product, which abstracts the connection-level details of interacting with basically any extant SQL/RDBMS database - I've tested with MySql 5.x, Postgres 9.2.x, and Sqlite. 

esqla.py is the interface between erlport an the Erlang application logic. sqla.py is the interface between esqla.py an the SQLAlchemy engine. No Erlang port details leak into it, or SQLAlchemy details out of it. 


I will leave installation of SQLAlchemy to the reader, for now. I'm running 0.7.9, the latest stable version. 

I include a sample SQL database and load file (combined into "schema1.sql") which is currently hard-coded to load when the ersqlacore starts up.


example session using default test database:

Erlang R15B02 (erts-5.9.2) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.2  (abort with ^G)
1> l(esqla_app).
{module,esqla_app}

2>  application:start(esqla).
ok

3> esqla_server:schemata().
SQL schema:: 
TABLE: album
        albumid
        albumname
        albumdate
        PRIMARY KEY: albumid
TABLE: album_label
        albumid
        labelid
        PRIMARY KEY: albumid
        FOREIGN KEY: labelid  ON label: labelid 
        FOREIGN KEY: albumid  ON album: albumid 
TABLE: album_tracks
        albumid
        trackid
        PRIMARY KEY: albumid
        PRIMARY KEY: trackid
        FOREIGN KEY: trackid  ON track: trackid 
        FOREIGN KEY: albumid  ON album: albumid 
TABLE: artist
        artistid
        artistname
        artistgender
        artistbday
        PRIMARY KEY: artistid
TABLE: grammy
        grammyid
        grammywinner
        grammyclass
        grammydate
        PRIMARY KEY: grammyid
        FOREIGN KEY: grammyid  ON album: albumid 
TABLE: label
        labelid
        labelname
        labelcity
        PRIMARY KEY: labelid
TABLE: track
        trackid
        trackname
        tracklength
        trackdate
        PRIMARY KEY: trackid
TABLE: track_artist
        trackid
        artistid
        PRIMARY KEY: trackid
        PRIMARY KEY: artistid
        FOREIGN KEY: artistid  ON artist: artistid 
        FOREIGN KEY: trackid  ON track: trackid 
VIEW: artist_discography
        artistname
        artistid
        trackname
        track_id
        albumname
        album_id
VIEW DEFINITION: 
CREATE VIEW artist_discography AS
select artistname, artist.artistid as artistid,
trackname, track.trackid as track_id,
albumname, album.albumid as album_id
from artist, track, album, track_artist, album_tracks
where artist.artistid = track_artist.artistid 
and track.trackid = track_artist.trackid
and album.albumid = album_tracks.albumid
and album_tracks.trackid = track_artist.trackid
ok

4> esqla_server:get("artist", [{}], [{}]).
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

5>  esqla_server:get("track", [{"trackid", 10}], [{}]). 
[["track","trackid",10],
 ["track","trackname","something happened part 1"],
 ["track","tracklength",135],
 ["track","trackdate","1997-08-09"]]

6> esqla_server:get("track", [{"trackname", "hate song one"}], [{}]).
[["track","trackid",6],
 ["track","trackname","hate song one"],
 ["track","tracklength",180],
 ["track","trackdate","2001-08-09"]]

7> esqla_server:upsert("track", {"trackid", 10}, [{"trackname", "love song seven"}]).
1

8> esqla_server:get("track", [{"trackid", 10}], [{}]). 
[["track","trackid",10],
 ["track","trackname","love song seven"],
 ["track","tracklength",135],
 ["track","trackdate","1997-08-09"]]

9> esqla_server:remove("track", [{"trackid", 10}]).      
1

10> esqla_server:get("track", [{"trackid", 10}], [{}]).   
[]

11> esqla_server:remove("track", [{"trackname","love long two"}]). 
0

12> esqla_server:remove("track", [{"trackname","love song two"}]).
1

13> esqla_server:upsert("track", {"trackid", 21}, [{"trackname","love song 666"}]).
1

