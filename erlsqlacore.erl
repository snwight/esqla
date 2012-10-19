%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module: erlsqlacore
%% description: use erlport to connect Erlang processes to SQLAlchemy 
%% author: github.com/snwight
%% license: dbad
%% date: oct 2012
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(erlsqlacore).
-export([init/0, start/2, get/2, upsert/2, remove/2]).

init() ->
    %% start up python sibling module - it will await our instructions
    Port = open_port({spawn, "python -u ErlSqlaCore.py"},
		     [{packet, 1}, binary, 
		      {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({port, term_to_binary(Port)})),
    handle(Port).

handle(Port) ->
    receive
        {Port, {data, Data}} ->
	    case binary_to_term(Data) of 
		<<"ported">> -> Port;
		<<"started">> -> "started up";
		X -> X
	    end;
	_Other ->
	    port_close(Port),
	    {whoops, _Other}
    after
        5000 ->
            {error, timeout}
    end.

start(Port, ConfigString) ->
    port_command(Port, term_to_binary({start, term_to_binary(ConfigString)})),
    handle(Port).

get(Port, ArgList) ->
    port_command(Port, term_to_binary({get, term_to_binary(ArgList)})),
    handle(Port).

upsert(Port, ArgList) ->
    port_command(Port, term_to_binary({upsert, term_to_binary(ArgList)})),
    handle(Port).

remove(Port, ArgList) ->
    port_command(Port, term_to_binary({remove, term_to_binary(ArgList)})),
    handle(Port).

    
