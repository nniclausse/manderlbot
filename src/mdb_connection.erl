%%%----------------------------------------------------------------------
%%% File    : mdb_connection.erl
%%% Author  : Mickaël Rémond <mickael.remond@erlang-fr.org>
%%% Purpose : Connection management library.
%%%           Used by mdb_bot.erl
%%% Created : 16 Sep 2001, Mickaël Rémond <mickael.remond@erlang-fr.org>
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

-module(mdb_connection).

-author('mickael.remond@erlang-fr.org').
-created('Date: 20010916').
-revision(' $Id$ ').
-vsn(' $Revision$ ').

%% External exports (API)
-export([connect/2, log/3, manage_reconnect/1]).

%% Configure debugging mode:
-include("mdb_macros.hrl").
-include("config.hrl").
-include("mdb.hrl").


%%----------------------------------------------------------------------
%% connect/2
%% Physically connects to the IRC server
%%----------------------------------------------------------------------
connect(Server, Ip_port) ->
    %% TCP connection to the IRC server
    Connect = fun() -> gen_tcp:connect(Server, Ip_port,
				       [binary,
					{packet, 0},
					{nodelay, true},
					{keepalive, true}, 
					{active, true},
					{reuseaddr, true}])
	      end,

    case Connect() of
	{ok, Sock} ->
	    ?dbg("Connected to ~p", [Server]),
	    {ok, Sock};

	{error, Reason} ->
	    %% If there is an error, wait 30 secondes and try to reconnect
	    ?dbg("Server connection error: ~p", [Reason]),
	    timer:sleep(30000),
	    connect(Server, Ip_port)
    end.

%%----------------------------------------------------------------------
%% log/3
%% connect to a given channel
%%
%% FIXME:
%%  Try to re use the Sock when connecting several bots on the same
%%  host:port. This would allow for RealName / Nickname usage again. 
%%
%%----------------------------------------------------------------------
log(Sock, Channel = #channel{}, RealName) ->
    %% Logging in
    log_in(Sock, Channel#channel.botname, RealName),

    %% Join the given channel
    irc_lib:join(Sock, Channel#channel.name).

%%----------------------------------------------------------------------
%% log_in/3
%% Logging in: Give nick and realname to the server
%%----------------------------------------------------------------------
%%log_in(Sock, Nickname, RealName, Password) ->
log_in(Sock, Nickname, RealName) ->
    irc_lib:login(Sock, Nickname, RealName).
    %%irc_lib:passwd(Sock, "Password")

%%----------------------------------------------------------------------
%% manage_reconnect/1
%% When something fails, automatically reconnects the bot
%%----------------------------------------------------------------------
manage_reconnect(State) ->
    Host = State#state.host,
    Port = State#state.port,
    Chan = State#state.channel,

    %% FIXME : add the RealName in the State, and get it here
    Nick = State#state.nickname,

    {ok, Sock} = connect(Host, Port),
    log(Sock, Chan, Nick),

    {ok, State#state{socket = Sock,
		     date   = calendar:local_time()
		    }}.
