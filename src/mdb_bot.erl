%%%----------------------------------------------------------------------
%%% File    : mdb_bot.erl
%%% Author  : Mickaël Rémond <mickael.remond@erlang-fr.org>
%%% Purpose : This is the behaviour for a generic bot.
%%%           It manage IRC server connexion, automatically answer to
%%%           server pings (necessary to stay online) and behaviour
%%%           management.
%%% Created : 8 Sep 2001 by Mickaël Rémond <mickael.remond@erlang-fr.org>
%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or modify  
%%% it under the terms of the GNU General Public License as published by 
%%% the Free Software Foundation; either version 2 of the License, or   
%%% (at your option) any later version.                                
%%%
%%%----------------------------------------------------------------------
%%%
%%% See COPYING for detailled license
%%%
%%%----------------------------------------------------------------------

-module(mdb_bot).

-author('mickael.remond@erlang-fr.org').
-created('Date: 20010908').
-revision(' $Id$ ').
-vsn(' $Revision$ ').

%% External exports (API)
-export([start/1, start_link/1, say/2, say/3, action/2, rejoin/1, reconf/3]).

%% special process callbacks
-export([init/3,
	 system_code_change/4,
	 system_continue/3,
	 system_terminate/4]).

%% Internal exports
-export([loop/3]).

%% Configure debugging mode:
-include("mdb_macros.hrl").

%% Include record description
-include("mdb.hrl").
-include("config.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start(Params) ->
    start([], Params).

start_link(Params) ->
    start_link([],Params).

%% Controling the bot environment
%% Send a message a the channel the bot is connected to
say(BotPid, Message) ->
    BotPid ! {self(), {say, Message}},
    get_answer(self(), 10000).

say(To, BotPid, Message) ->
    BotPid ! {self(), {say, Message, To}},
    get_answer(self(), 10000).

action(BotPid, Message) ->
    BotPid ! {self(), {action, Message}},
    get_answer(self(), 10000).

%% Rejoin the channel (Use it when you have been kicked)
rejoin(BotPid) ->
    BotPid ! {self(), {rejoin}},
    get_answer(self(), 10000).

reconf(BotPid, NickName, ConfigFile) ->
    BotPid ! {self(), {reconf, NickName, ConfigFile}},
    get_answer(self(), 10000).


%%%----------------------------------------------------------------------
%%% Specific process callbacks
%%%----------------------------------------------------------------------

start(Options, Params) ->
    proc_lib:start(?MODULE, init, [self(), Options, Params]).

start_link(Options, Params) ->
    proc_lib:start_link(?MODULE, init, [self(), Options, Params]).

init(Parent, Options, [RealName, Controler, Host, Port, Channel]) ->
    process_flag(trap_exit, true),
    Deb = sys:debug_options(Options),
    proc_lib:init_ack(Parent, {ok, self()}),

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

    loop(State, Parent, Deb).

%% The loop is too big:
%% I should strip it.
loop(State, Parent, Deb) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);

        {'EXIT', Parent, Reason} ->
            cleanup(State),
            exit(Reason);

	%% Socket management
	%% Receiving data...
	{tcp, Socket, Data} ->
	    Buffer = State#state.buffer,
	    List = lists:append(binary_to_list(Buffer), binary_to_list(Data)),
	    
	    Rest = mdb_dispatch:process_data(Socket, List, State),

	    NewState = State#state{buffer=list_to_binary(Rest)},
	    ?MODULE:loop(NewState, Parent, Deb);

	%% Close/error => reconnect
	{tcp_closed, Socket} ->
	    ?dbg("Socket closed", []),
	    {ok, NewState} = mdb_connection:manage_reconnect(State),
	    loop(NewState, Parent, Deb);

	{tcp_error, Socket, Reason} ->
	    ?dbg("Socket error: ~p", [Reason]),
	    {ok, NewState} = mdb_connection:manage_reconnect(State),
	    loop(NewState, Parent, Deb);

	%% Callback management -> handle_call
	{From, OurMsgs} ->
	    %% io:format("API: ~p - ~p~n", [From, OurMsgs]),
            NewDeb = sys:handle_debug(Deb, {?MODULE, write_debug},
                                      ?MODULE, {in, OurMsgs, From}),
	    
	    {Answer, NewState} = handle_msg(OurMsgs, State),
	    From ! {From, Answer},

            NewerDeb = sys:handle_debug(NewDeb,
					{?MODULE, write_debug},	?MODULE,
					{out, {self(), Answer}, From}),
	    ?MODULE:loop(NewState, Parent, NewerDeb);

	%% Unknown message
	%% In this case we also need to launch a reconnect
	%% "ERROR :Closing Link: Manderlbot[bas1-71.idf2-1.club-internet.fr] by forward.openprojects.net (Ping timeout for Manderlbot[bas1-71.idf2-1.club-internet.fr]" = When the irc client did not answer to a ping
        What ->
	    io:format("~p~n", [What]),
            NewDeb = sys:handle_debug(Deb, {?MODULE, write_debug},
                                      ?MODULE, {in, What}),
	    %% mdb_connection:manage_reconnect(State),
            ?MODULE:loop(State, Parent, NewDeb)

    %% Message timeout: give the opportunity to handle system messages
    after 100 ->
	    ?MODULE:loop(State, Parent, Deb)
    end.

cleanup(State) ->
    ok.

%%----------------------------------------------------------------------
%% Function: handle_msg/2
%% Purpose:  Internal callback funtion
%%----------------------------------------------------------------------
handle_msg({say, Message}, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    irc_lib:say(Sock, Channel, Message),
    {ok, State};

handle_msg({say, Message, To}, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    io:format("DANS TON CUL: ~p~n", [To]),
    
    irc_lib:say(Sock, To, Message),
    {ok, State};

handle_msg({action, Message}, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    irc_lib:action(Sock, Channel, Message),

    {ok, State};

handle_msg({rejoin}, State) ->
    Channel = State#state.channel,
    Sock = State#state.socket,

    irc_lib:join(Sock, Channel),

    {ok, State};

handle_msg({reconf, NickName, ConfigFile}, State) ->
    %% First read the conf file given
    %% Then get our behaviours list, and replace it in the State
    case State#state.controler of
	NickName ->
	    case config_srv:reconf(State#state.channel, ConfigFile) of
		{ok, BList} -> {ok, State#state{behaviours=BList}};
		_Error      -> {ok, State}
	    end;

	Other ->
	    irc_lib:say(State#state.socket, State#state.channel,
			NickName ++ ": " ++
			"Who do you think you are to 'reconf' me ?"),
	    {ok, State}
    end;

handle_msg(Other, State) ->
    {ok, State}.


%% Here are the sys call back functions
system_continue(Parent, Deb, State) ->
    loop(State, Parent, Deb).

system_terminate(Reason, Parent, Deb, State) ->
    cleanup(State),
    exit(Reason).

system_code_change(Data, OldVsn, Module, Extra) -> {ok, Data}.



%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% get_answer/2
%% Used in synchronous calls
%% Get back the result of a command sent to the server and returns
%% the result
%%----------------------------------------------------------------------
get_answer(Pid, Timeout) ->
    receive
	{Pid, Result} ->
	    Result
    after 
	Timeout ->
	    timeout
    end.
