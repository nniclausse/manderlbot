%%%----------------------------------------------------------------------
%%% File    : mdb_bot.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Bot behaviours and connection manager
%%% Created : 11 Aug 2002 by Dimitri Fontaine <dim@tuxfamily.org>
%%%----------------------------------------------------------------------

-module(mdb_bot).
-author('dim@tuxfamily.org').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/0, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([say/1, say/2, action/1, rejoin/0, reconf/2]).

-define(timeout, 5000).

%% Configure debugging mode:
-include("mdb_macros.hrl").

%% Include record description
-include("mdb.hrl").
-include("config.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:cast(?MODULE, {stop}).

%% Controling the bot environment
%% Send a message a the channel the bot is connected to
say(Message) ->
    gen_server:call(?MODULE, {say, Message}, ?timeout).

say(Message, To) ->
    gen_server:call(?MODULE, {say, Message, To}, ?timeout).

action(Message) ->
    gen_server:call(?MODULE, {action, Message}, ?timeout).

%% Rejoin the channel (Use it when you have been kicked)
rejoin() ->
    gen_server:call(?MODULE, rejoin, ?timeout).

reconf(NickName, ConfigFile) ->
    gen_server:call(?MODULE, {reconf, NickName, ConfigFile}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([RealName, Controler, Host, Port, Channel]) ->
    {ok, Sock} = mdb_connection:connect(Host, Port),

    ok = mdb_connection:log(Sock, Channel, RealName),
    io:format("~p joined ~p (~p)~n", [Channel#channel.botname,
				      Channel#channel.name, Sock]),

    {ok, BList} = config_srv:getBList(Channel#channel.name),

    State = #state{bot_pid=self(),
		   channel    = Channel#channel.name,
		   controler  = Controler,
		   socket     = Sock,
		   nickname   = Channel#channel.botname,
		   date       = calendar:local_time(),
		   behaviours = BList,
		   host       = Host,
		   port       = Port
		  },
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({say, Message}, From, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    irc_lib:say(Sock, Channel, Message),

    {noreply, State};

handle_call({say, Message, To}, From, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    io:format("DANS TON CUL: ~p~n", [To]),
    
    irc_lib:say(Sock, To, Message),
    {noreply, State};

handle_call({action, Message}, From, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    irc_lib:action(Sock, Channel, Message),
    {noreply, State};

handle_call(rejoin, From, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    irc_lib:join(Sock, Channel),
    {noreply, State};

handle_call({reconf, NickName, ConfigFile}, From, State) ->
    %% First read the conf file given
    %% Then get our behaviours list, and replace it in the State
    case State#state.controler of
	NickName ->
	    case config_srv:reconf(State#state.channel, ConfigFile) of
		{ok, BList} -> {noreply, State#state{behaviours=BList}};
		_Error      -> {noreply, State}
	    end;

	Other ->
	    irc_lib:say(State#state.socket, State#state.channel,
			NickName ++ ": " ++
			"Who do you think you are to 'reconf' me ?"),
	    {noreply, State}
    end;

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({tcp, Socket, Data}, State) ->
    Buffer = State#state.buffer,
    List = lists:append(binary_to_list(Buffer), binary_to_list(Data)),
	    
	case mdb_dispatch:process_data(Socket, List, State) of
	    ok   ->
		%% This was just a 'ping' request
		{noreply, State};

	    Rest ->
		NewState = State#state{buffer=list_to_binary(Rest)},
		{noreply, NewState}
	end;

handle_info({tcp_einval, Socket}, State) ->
    {noreply, State};

handle_info({tcp_error, Socket}, State) ->
    {ok, NewState} = mdb_connection:manage_reconnect(State),
    {noreply, NewState};

handle_info({tcp_closed, Socket}, State) ->
    {ok, NewState} = mdb_connection:manage_reconnect(State),
    {noreply, NewState}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
