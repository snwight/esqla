%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module: esqla
%% description: erlang port to connect Erlang processes to SQLAlchemy 
%% author: github.com/snwight
%% license: dbad
%% date: oct 2012
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(esqla).
-export([init/0, start/2, get/2, upsert/2, remove/2]).

init() ->
    %% start up python sibling module - it will await our instructions
    Port = open_port({spawn, "python -u ../priv/ErlSqlaCore.py"},
		     [{packet, 4}, binary, nouse_stdio, 
		      {env, [{"PYTHONPATH", "../priv/erlport"}]}]),
    port_command(Port, term_to_binary({port, term_to_binary(Port)})),
    handle(Port).

handle(Port) ->
    receive
        {Port, {data, Data}} ->
	    case binary_to_term(Data) of 
		<<"ported">> -> Port;
		<<"started">> -> "started up";
		Term -> Term
	    end;
	_Other ->
	    port_close(Port),
	    {whoops, _Other}
    after
        5000 ->
            {error, timeout}
    end.

start(Port, ConfigString) ->
    %% pass SQLAlchemy configuration URI into python land
    Payload = list_to_binary(ConfigString),
    port_command(Port, term_to_binary({start, Payload})),
    handle(Port).

get(Port, [Table, KeyVals, Hints]) ->
    %% retrieve matching rows
    Payload = [Table, KeyVals, Hints],
    port_command(Port, term_to_binary({get, Payload})),
    handle(Port).

upsert(Port, [Table, PKeyVal, KeyVals]) ->
    %% update matching or insert new rows
    Payload = [Table, PKeyVal, KeyVals],
    port_command(Port, term_to_binary({upsert, Payload})),
    handle(Port).

remove(Port, [Table, KeyVals]) ->
    %% delete matching rows
    Payload = [Table, KeyVals],
    port_command(Port, term_to_binary({remove, Payload})),
    handle(Port).

    
