<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
  <meta http-equiv="Content-Style-Type" content="text/css">
  <title></title>
  <meta name="Generator" content="Cocoa HTML Writer">
  <meta name="CocoaVersion" content="949.54">
  <style type="text/css">
    p.p1 {margin: 0.0px 0.0px 0.0px 0.0px; font: 12.0px Helvetica}
    p.p2 {margin: 0.0px 0.0px 0.0px 0.0px; font: 12.0px Helvetica; min-height: 14.0px}
  </style>
</head>
<body>
<p class="p1">esqla</p>
<p class="p1">======</p>
<p class="p2"><br></p>
<p class="p1">Using erlport [ thank you Dmitry! https://github.com/hdima/erlport ] to expose a grossly simplified subset of the SQLAlchemy Core API as a small set of Erlang shell commands.</p>
<p class="p2"><br></p>
<p class="p1">I've packaged this up as an OTP application, comprised of three Erlang modules (esqla_app, esqla_sup, esqla_server), the obligatory config source file (esqla.app.src) and two Python scripts (esqla.py and sqla.py). In addition you will need erlport and of course SQLAlchemy 0.7.4 or later. <span class="Apple-converted-space"> </span></p>
<p class="p2"><br></p>
<p class="p1">You'll want to clone this repos:</p>
<p class="p1">&gt; git clone git@github.com:snwight/erlsqlacore.git</p>
<p class="p2"><br></p>
<p class="p1">&gt; cd esqla/priv</p>
<p class="p2"><br></p>
<p class="p1">Grab erlport and clone it as-is into esqla/priv:</p>
<p class="p1">&gt; git clone git://github.com/hdima/erlport.git</p>
<p class="p2"><br></p>
<p class="p1">&gt; cd esqla</p>
<p class="p2"><br></p>
<p class="p1">Assuming you have rebar in your exec path:</p>
<p class="p2"><br></p>
<p class="p1">&gt; rebar compile</p>
<p class="p2"><br></p>
<p class="p1">&gt; cd ebin</p>
<p class="p2"><br></p>
<p class="p1">Start a shell and the application:</p>
<p class="p2"><br></p>
<p class="p1">&gt; erl</p>
<p class="p1">&gt;&gt; Erlang R15B02 (erts-5.9.2) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]</p>
<p class="p1">&gt;&gt; Eshell V5.9.2<span class="Apple-converted-space">  </span>(abort with ^G)</p>
<p class="p2"><br></p>
<p class="p1">1&gt; l(esqla_app).</p>
<p class="p1">{module,esqla_app}</p>
<p class="p2"><br></p>
<p class="p1">2&gt;<span class="Apple-converted-space">  </span>application:start(esqla).</p>
<p class="p1">ok</p>
<p class="p2"><br></p>
<p class="p1">This initializes and starts up the Python SQLAlchemy engine, in this example with Sqlite and on-disk storage - just change the 'configuration string' to connect to a running Postgres cluster, e.g.:</p>
<p class="p1"><span class="Apple-converted-space">    </span>"postgresql://user:password@/dbname"</p>
<p class="p1">or Mysql, e.g.:</p>
<p class="p1"><span class="Apple-converted-space">    </span>"mysql://user:password@localhost/dbname"</p>
<p class="p1">...check the excellent SQLAlchemy documentation online for details at sqlalchemy.org</p>
<p class="p2"><br></p>
<p class="p2"><br></p>
<p class="p1">And here is the new three-command API:</p>
<p class="p2"><br></p>
<p class="p1">get() - returns entire matching row currently - absent parameters match ANY</p>
<p class="p1">&gt; get("table", [{"column", val},...], [{"limit", val}, {"offset", val}])</p>
<p class="p2"><br></p>
<p class="p1">upsert() - update OR insert new (if primary key non-existent)<span class="Apple-converted-space"> </span></p>
<p class="p1">&gt; upsert("table", {"primary_key, val}, [{"column", val},...])</p>
<p class="p2"><br></p>
<p class="p1">remove() - deletes matching rows</p>
<p class="p1">&gt; remove("table", [{"column", val},...])<span class="Apple-converted-space"> </span></p>
<p class="p2"><br></p>
<p class="p1">schemata() - dumps out the active schema as reflected through SQLAlchemy</p>
<p class="p1">&gt; schemata()</p>
<p class="p2"><br></p>
<p class="p2"><br></p>
<p class="p1">I should describe this thing better, though.</p>
<p class="p2"><br></p>
<p class="p1">esqla.erl delivers a simple command line get/upsert/remove and schema inspect interface to Python SQLAlchemy, specifically the Core API of that product, which abstracts the connection-level details of interacting with basically any extant SQL/RDBMS database - I've tested with MySql 5.x, Postgres 9.2.x, and Sqlite.<span class="Apple-converted-space"> </span></p>
<p class="p2"><br></p>
<p class="p1">esqla.py is the interface between erlport and the Erlang application logic.<span class="Apple-converted-space"> </span></p>
<p class="p1">sqla.py is the interface between esqla.py and the SQLAlchemy engine. No Erlang port details leak into it, nor SQLAlchemy details out of it.<span class="Apple-converted-space"> </span></p>
<p class="p2"><br></p>
<p class="p1">I will leave installation of SQLAlchemy to the reader, for now. I'm running 0.7.9, the latest stable version.<span class="Apple-converted-space"> </span></p>
<p class="p2"><br></p>
<p class="p1">I include a sample SQL database and load file (combined into "schema1.sql") which is currently hard-coded to load when the ersqlacore starts up.</p>
<p class="p2"><br></p>
<p class="p2"><br></p>
<p class="p1">Here's an example session using the default test database:</p>
<p class="p2"><br></p>
<p class="p1">Erlang R15B02 (erts-5.9.2) [source] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]</p>
<p class="p2"><br></p>
<p class="p1">Eshell V5.9.2<span class="Apple-converted-space">  </span>(abort with ^G)</p>
<p class="p1">1&gt; l(esqla_app).</p>
<p class="p1">{module,esqla_app}</p>
<p class="p2"><br></p>
<p class="p1">2&gt;<span class="Apple-converted-space">  </span>application:start(esqla).</p>
<p class="p1">ok</p>
<p class="p2"><br></p>
<p class="p1">3&gt; esqla_server:schemata().</p>
<p class="p1">SQL schema::<span class="Apple-converted-space"> </span></p>
<p class="p1">TABLE: album</p>
<p class="p1"><span class="Apple-converted-space">        </span>albumid</p>
<p class="p1"><span class="Apple-converted-space">        </span>albumname</p>
<p class="p1"><span class="Apple-converted-space">        </span>albumdate</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: albumid</p>
<p class="p1">TABLE: album_label</p>
<p class="p1"><span class="Apple-converted-space">        </span>albumid</p>
<p class="p1"><span class="Apple-converted-space">        </span>labelid</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: albumid</p>
<p class="p1"><span class="Apple-converted-space">        </span>FOREIGN KEY: labelid<span class="Apple-converted-space">  </span>ON label: labelid<span class="Apple-converted-space"> </span></p>
<p class="p1"><span class="Apple-converted-space">        </span>FOREIGN KEY: albumid<span class="Apple-converted-space">  </span>ON album: albumid<span class="Apple-converted-space"> </span></p>
<p class="p1">TABLE: album_tracks</p>
<p class="p1"><span class="Apple-converted-space">        </span>albumid</p>
<p class="p1"><span class="Apple-converted-space">        </span>trackid</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: albumid</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: trackid</p>
<p class="p1"><span class="Apple-converted-space">        </span>FOREIGN KEY: trackid<span class="Apple-converted-space">  </span>ON track: trackid<span class="Apple-converted-space"> </span></p>
<p class="p1"><span class="Apple-converted-space">        </span>FOREIGN KEY: albumid<span class="Apple-converted-space">  </span>ON album: albumid<span class="Apple-converted-space"> </span></p>
<p class="p1">TABLE: artist</p>
<p class="p1"><span class="Apple-converted-space">        </span>artistid</p>
<p class="p1"><span class="Apple-converted-space">        </span>artistname</p>
<p class="p1"><span class="Apple-converted-space">        </span>artistgender</p>
<p class="p1"><span class="Apple-converted-space">        </span>artistbday</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: artistid</p>
<p class="p1">TABLE: grammy</p>
<p class="p1"><span class="Apple-converted-space">        </span>grammyid</p>
<p class="p1"><span class="Apple-converted-space">        </span>grammywinner</p>
<p class="p1"><span class="Apple-converted-space">        </span>grammyclass</p>
<p class="p1"><span class="Apple-converted-space">        </span>grammydate</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: grammyid</p>
<p class="p1"><span class="Apple-converted-space">        </span>FOREIGN KEY: grammyid<span class="Apple-converted-space">  </span>ON album: albumid<span class="Apple-converted-space"> </span></p>
<p class="p1">TABLE: label</p>
<p class="p1"><span class="Apple-converted-space">        </span>labelid</p>
<p class="p1"><span class="Apple-converted-space">        </span>labelname</p>
<p class="p1"><span class="Apple-converted-space">        </span>labelcity</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: labelid</p>
<p class="p1">TABLE: track</p>
<p class="p1"><span class="Apple-converted-space">        </span>trackid</p>
<p class="p1"><span class="Apple-converted-space">        </span>trackname</p>
<p class="p1"><span class="Apple-converted-space">        </span>tracklength</p>
<p class="p1"><span class="Apple-converted-space">        </span>trackdate</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: trackid</p>
<p class="p1">TABLE: track_artist</p>
<p class="p1"><span class="Apple-converted-space">        </span>trackid</p>
<p class="p1"><span class="Apple-converted-space">        </span>artistid</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: trackid</p>
<p class="p1"><span class="Apple-converted-space">        </span>PRIMARY KEY: artistid</p>
<p class="p1"><span class="Apple-converted-space">        </span>FOREIGN KEY: artistid<span class="Apple-converted-space">  </span>ON artist: artistid<span class="Apple-converted-space"> </span></p>
<p class="p1"><span class="Apple-converted-space">        </span>FOREIGN KEY: trackid<span class="Apple-converted-space">  </span>ON track: trackid<span class="Apple-converted-space"> </span></p>
<p class="p1">VIEW: artist_discography</p>
<p class="p1"><span class="Apple-converted-space">        </span>artistname</p>
<p class="p1"><span class="Apple-converted-space">        </span>artistid</p>
<p class="p1"><span class="Apple-converted-space">        </span>trackname</p>
<p class="p1"><span class="Apple-converted-space">        </span>track_id</p>
<p class="p1"><span class="Apple-converted-space">        </span>albumname</p>
<p class="p1"><span class="Apple-converted-space">        </span>album_id</p>
<p class="p1">VIEW DEFINITION:<span class="Apple-converted-space"> </span></p>
<p class="p1">CREATE VIEW artist_discography AS</p>
<p class="p1">select artistname, artist.artistid as artistid,</p>
<p class="p1">trackname, track.trackid as track_id,</p>
<p class="p1">albumname, album.albumid as album_id</p>
<p class="p1">from artist, track, album, track_artist, album_tracks</p>
<p class="p1">where artist.artistid = track_artist.artistid<span class="Apple-converted-space"> </span></p>
<p class="p1">and track.trackid = track_artist.trackid</p>
<p class="p1">and album.albumid = album_tracks.albumid</p>
<p class="p1">and album_tracks.trackid = track_artist.trackid</p>
<p class="p1">ok</p>
<p class="p2"><br></p>
<p class="p1">4&gt; esqla_server:get("artist", [{}], [{}]).</p>
<p class="p1">[["artist","artistid",1],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistname","bobby"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistgender","M"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistbday","1961-05-15"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistid",2],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistname","diane"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistgender","F"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistbday","1960-07-08"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistid",3],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistname","lashana"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistgender","TX"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistbday","1991-04-09"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistid",4],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistname","lucy"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistgender","F"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistbday","1950-12-25"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistid",5],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistname","sid"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistgender","M"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistbday","1960-12-24"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistid",6],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistname","brian"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistgender","M"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistbday","1950-02-25"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistid",7],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist","artistname",[...]],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["artist",[...]|...],</p>
<p class="p1"><span class="Apple-converted-space"> </span>[[...]|...]]</p>
<p class="p2"><br></p>
<p class="p1">5&gt;<span class="Apple-converted-space">  </span>esqla_server:get("track", [{"trackid", 10}], [{}]).<span class="Apple-converted-space"> </span></p>
<p class="p1">[["track","trackid",10],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["track","trackname","something happened part 1"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["track","tracklength",135],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["track","trackdate","1997-08-09"]]</p>
<p class="p2"><br></p>
<p class="p1">6&gt; esqla_server:get("track", [{"trackname", "hate song one"}], [{}]).</p>
<p class="p1">[["track","trackid",6],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["track","trackname","hate song one"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["track","tracklength",180],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["track","trackdate","2001-08-09"]]</p>
<p class="p2"><br></p>
<p class="p1">7&gt; esqla_server:upsert("track", {"trackid", 10}, [{"trackname", "love song seven"}]).</p>
<p class="p1">1</p>
<p class="p2"><br></p>
<p class="p1">8&gt; esqla_server:get("track", [{"trackid", 10}], [{}]).<span class="Apple-converted-space"> </span></p>
<p class="p1">[["track","trackid",10],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["track","trackname","love song seven"],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["track","tracklength",135],</p>
<p class="p1"><span class="Apple-converted-space"> </span>["track","trackdate","1997-08-09"]]</p>
<p class="p2"><br></p>
<p class="p1">9&gt; esqla_server:remove("track", [{"trackid", 10}]). <span class="Apple-converted-space">     </span></p>
<p class="p1">1</p>
<p class="p2"><br></p>
<p class="p1">10&gt; esqla_server:get("track", [{"trackid", 10}], [{}]).<span class="Apple-converted-space">   </span></p>
<p class="p1">[]</p>
<p class="p2"><br></p>
<p class="p1">11&gt; esqla_server:remove("track", [{"trackname","love long two"}]).<span class="Apple-converted-space"> </span></p>
<p class="p1">0</p>
<p class="p2"><br></p>
<p class="p1">12&gt; esqla_server:remove("track", [{"trackname","love song two"}]).</p>
<p class="p1">1</p>
<p class="p2"><br></p>
<p class="p1">13&gt; esqla_server:upsert("track", {"trackid", 21}, [{"trackname","love song 666"}]).</p>
<p class="p1">1</p>
<p class="p2"><br></p>
</body>
</html>
