%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module: esqla_server
%% description: server module for esqla, Erlang API to SQLAlchemy Core
%% author: github.com/snwight, northwight@gmail.com
%% license: dbad
%% date: oct 2012
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(esqla_server).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API functions
-export([get/2, upsert/3, remove/2]).

%%
%% operation and maintenance API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([SqlaConfigString, TestDB]) ->
    %% start up python sibling module - it will await our instructions
    PythonPort = open_port({spawn, "python -u ../priv/ErlSqlaCore.py"},
			   [{packet, 4}, binary, nouse_stdio, 
			    {env, [{"PYTHONPATH", "../priv/erlport"}]}]),
    %% pass SQLAlchemy configuration URI into python land
    Payload = [list_to_binary(SqlaConfigString), list_to_binary(TestDB)],
    port_command(PythonPort, term_to_binary({start, Payload})),
    Reply = handle_python(PythonPort),
    {Reply, PythonPort}.

%%
%% primary client API!
%%
get(TableName, KVList) ->
    %% retrieve matching rows
    ArgList = [TableName, KVList],
    gen_server:call(?MODULE, {get, ArgList}).

upsert(TableName, PrimaryKV, KVList) ->
    %% update row if primary keys match or insert new row
    ArgList = [TableName, PrimaryKV, KVList],
    gen_server:call(?MODULE, {upsert, ArgList}).

remove(TableName, KVList) ->
    %% delete matching row/s
    ArgList = [TableName, KVList],
    gen_server:call(?MODULE, {remove, ArgList}).

%%
%% callback functions 
%%
handle_call(_Request, _From, PythonPort) ->
    port_command(PythonPort, term_to_binary(_Request)),
    Reply = handle_python(PythonPort),
    {reply, Reply, PythonPort}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%
handle_python(PythonPort) ->
    receive
        {PythonPort, {data, Data}} ->
	    %% Convert binary data to term
            {ok, binary_to_term(Data)};
	_Other ->
	    port_close(PythonPort),
	    {whoops, _Other}
    after
        5000 ->
            {error, timeout}
    end.

