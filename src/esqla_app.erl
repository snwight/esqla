%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module: esqla_app
%% description: application module for esqla, Erlang API to SQLAlchemy Core
%% author: github.com/snwight, northwight@gmail.com
%% license: dbad
%% date: oct 2012
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(esqla_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    esqla_sup:start_link().

stop(PythonPort) ->
    %% this is just sqlite cleanup - real DBs aren't this simple
    {ok, Db} = application:get_env(db_file),
    os:cmd(lists:concat(["rm ", Db])),
    port_close(PythonPort),
    ok.
