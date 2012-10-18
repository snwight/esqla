%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module: erlsqlacore
%% description: use erlport to connect Erlang processes to SQLAlchemy 
%% author: github.com/snwight
%% license: dbad
%% date: oct 2012
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(erlsqlacore).
-export([init/0]).


% start up python sibling module - it will await our instructions
init() ->
    Port = open_port({spawn, "python -u ErlSqlaCore.py"},
		     [{packet, 1}, binary, 
		      {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({start, "sqlite:////tmp/test.db"})),
    handle(Port).

handle(Port) ->
    receive
        {Port, {data, Data}} ->
	    Term = binary_to_term(Data),
	    erlang:display(Term),
	    {ok, true};
	_Other ->
	    {whoops, false}
    after
        5000 ->
            {error, timeout}
    end.

