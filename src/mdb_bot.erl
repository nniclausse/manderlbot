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

-export([say/2, say/3, action/2, mute/2, rejoin/1, reconf/3]).

-define(timeout, 25000).

%% Configure debugging mode:
-include("mdb_macros.hrl").

%% Include record description
-include("mdb.hrl").
-include("config.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?MODULE, {stop}).

%% Controling the bot environment
%% Send a message a the channel the bot is connected to
say(BotPid, Message) ->
    gen_server:call(BotPid, {say, Message}, ?timeout).

say(BotPid, Message, To) ->
    gen_server:call(BotPid, {say, Message, To}, ?timeout).

action(BotPid, Message) ->
    gen_server:call(BotPid, {action, Message}, ?timeout).

mute(BotPid, NickName) ->
    gen_server:call(BotPid, {mute, NickName}, ?timeout).

%% Rejoin the channel (Use it when you have been kicked)
rejoin(BotPid) ->
    gen_server:call(BotPid, rejoin, ?timeout).

reconf(BotPid, NickName, ConfigFile) ->
    gen_server:call(BotPid, {reconf, NickName, ConfigFile}, ?timeout).

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
init([RealName, Controler, Host, Port, Channel, BList]) ->
    io:format("launching a new bot: ~p~n", [Channel]),

    {ok, Sock} = mdb_connection:connect(Host, Port),

    ok = mdb_connection:log(Sock, Channel, RealName),

    %% To avoid some re-entrance issue when starting bot from a reconf,
    %% we may start the bot giving it its behaviours list...
    {ok, RealBList} = case BList of
			  [] ->
			      config_srv:getBList(Channel#channel.name);
			  List ->
			      {ok, BList}
		      end,

    State = #state{bot_pid=self(),
		   channel    = Channel#channel.name,
		   controler  = Controler,
		   socket     = Sock,
		   nickname   = Channel#channel.botname,
		   date       = calendar:local_time(),
		   behaviours = RealBList,
		   host       = Host,
		   port       = Port,
		   mode       = unmuted
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

    {reply, ok, State};

handle_call({say, Message, To}, From, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    irc_lib:say(Sock, To, Message),
    {reply, ok, State};

handle_call({action, Message}, From, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    irc_lib:action(Sock, Channel, Message),
    {reply, ok, State};

handle_call(rejoin, From, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    irc_lib:join(Sock, Channel),
    {reply, ok, State};

handle_call({reconf, NickName, ConfigFile}, From, State) ->
    %% First read the conf file given
    %% Then get our behaviours list, and replace it in the State
    case State#state.controler of
	NickName ->
	    case config_srv:reconf(State#state.channel, ConfigFile) of
		{ok, BList} -> {reply, ok, State#state{behaviours=BList}};
		_Error      -> {reply, {error, reconf}, State}
	    end;

	Other ->
	    irc_lib:say(State#state.socket, State#state.channel,
			NickName ++ ": " ++
			"Who do you think you are to 'reconf' me ?"),
	    {reply, {error, controller}, State}
    end;

handle_call({mute, NickName}, From, State) ->
    case State#state.controler of
	NickName ->
	    case State#state.mode of
		muted   ->
		    irc_lib:action(State#state.socket, State#state.channel,
				   "is back"),
		    {reply, ok, State#state{mode = unmuted}};

		unmuted ->
		    irc_lib:action(State#state.socket, State#state.channel,
				   "is away"),
		    {reply, ok, State#state{mode = muted}}
	    end;

	Other ->
	    irc_lib:say(State#state.socket, State#state.channel,
			NickName ++ ": " ++
			"Who do you think you are to mute me ?"),
	    {reply, {error, controller}, State}
    end.
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
