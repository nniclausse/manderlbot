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

%%----------------------------------------------------------------------
%% log/2
%% Start an IRC bot and connect it to a given channel
%%----------------------------------------------------------------------
log(Sock, Channel = #channel{}, RealName) ->
    %% Logging in
    _Motd = log_in(Sock, Channel#channel.botname, RealName),

    %% Join the given channel
    irc_lib:join(Sock, Channel#channel.name),
    ok.

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
	%% If everything went ok
	{ok, Sock} -> {ok, Sock};
	%% If there is an error, wait 30 secondes and try to reconnect
	{error, Reason} ->
	    ?dbg("Server connection error: ~p", [Reason]),
	    timer:sleep(30000),
	    connect(Server, Ip_port)
    end.

%%----------------------------------------------------------------------
%% log_in/3
%% Logging in: Give nick and realname to the server
%%----------------------------------------------------------------------
%%log_in(Sock, Nickname, RealName, Password) ->
log_in(Sock, Nickname, RealName) ->
    log_in_nick(Sock, Nickname),
    log_in_pong(Sock),
    log_in_pass(Sock, "Password"),
    log_in_user(Sock, Nickname, RealName),
    Motd = wait_for_motd(5000).

    %% TODO: Add an event notification: logged in as Realname aka Nick

%%----------------------------------------------------------------------
%% log_in_nick/2
%% Send nickname during logging
%%----------------------------------------------------------------------
log_in_nick(Sock, Nickname) ->
    NickCommand = ["NICK ", Nickname, "\r\n"],
    gen_tcp:send(Sock, NickCommand).

%%----------------------------------------------------------------------
%% log_in_pong/1
%% Some server send an initial ping after the nickname to check the
%% connection
%% Handle this ping/pong session correctly
%%----------------------------------------------------------------------
log_in_pong(Sock) ->
    Result = receive
		 {tcp, Sock, Data} ->
		     binary_to_list(Data)
	     after 120000 ->
		     binary_to_list(<<>>)
	     end,
    %% Test is the first part is a connection string
    TokenizedResult= string:tokens(Result, "\r\n"),
    IsPingString = misc_tools:nth(1, TokenizedResult),
    testPingPong(Sock, IsPingString).

%%----------------------------------------------------------------------
%% log_in_pass/2
%% If the IRC server is password protected, this function is supposed
%% send the needed password
%%----------------------------------------------------------------------
log_in_pass(Sock, Password) ->
    PassCommand = ["PASS ", Password, "\r\n"],
    gen_tcp:send(Sock, PassCommand).    

%%----------------------------------------------------------------------
%% log_in_nick/3
%% Send the user information to terminate the log in phase
%%----------------------------------------------------------------------
log_in_user(Sock, Nickname, Realname) ->
    UserCommand = lists:concat(["USER ", Nickname,
				" dummy dummy :", Realname, "\r\n"]),
    gen_tcp:send(Sock, UserCommand).

%%----------------------------------------------------------------------
%% wait_for_motd/1
%% This function gets the Message Of The Day that IRC servers usually
%% send after the connection (usage rules of the server)
%%----------------------------------------------------------------------
wait_for_motd(Timeout) ->
    wait_for_motd(Timeout, []).
wait_for_motd(Timeout, Acc) ->
    receive
	{tcp, _Sock, Data} ->
	    wait_for_motd(Timeout, Acc ++ binary_to_list(Data))
    after Timeout ->
	    Acc
    end.


%%----------------------------------------------------------------------
%% testPingPong/2
%% Check if the incoming data is a server ping
%% If so, answer it and thus maintains the connection
%%----------------------------------------------------------------------
testPingPong(Sock, Data) ->
    case string:substr(Data, 1, 4) of
	"PING" ->
	    Id = string:substr(Data, 6),
	    irc_lib:pong(Sock, Id);
	Other ->
	    ok
    end.

%%----------------------------------------------------------------------
%% manage_reconnect/1
%% When something fails, automatically reconnects the bot
%%----------------------------------------------------------------------
manage_reconnect(State) ->
    %% TODO: Implement this function.
    %% When I am not connected, the connection/reconnection process is
    %% already handled by the irc server (irc_srv)
    ok.


