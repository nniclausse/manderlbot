%%%----------------------------------------------------------------------
%%% File    : irc_lib.erl
%%% Author  : Mickaël Rémond <mickael.remond@erlang-fr.org>
%%% Purpose : This library gathers all functions to format and send
%%%           IRC commands.
%%%           It manage IRC server connexion, automatically answer to
%%%           server pings (necessary to stay online) and behaviour
%%%           management.
%%% Created : 11 Sep 2001, Mickaël Rémond <mickael.remond@erlang-fr.org>
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

-module(irc_lib).

-author('mickael.remond@erlang-fr.org').
-created('Date: 20010911').
-revision(' $Id$ ').
-vsn(' $Revision$ ').

%% IRC operations
-export([pong/2, join/2, say/3, action/3]).

%% IRC helper functions
-export([is_chanop/1,
	 nickname/1,
	 split_nick_user/1]).

%%----------------------------------------------------------------------
%% Function: pong/2
%% Purpose:  Send a pong answer with the right id
%% Args:     Sock = socket
%%           Id   = ping id to send back to the server
%% Returns:  ok
%%     or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
pong(Sock, Id)->
    Pong = lists:append("PONG ", Id),
    gen_tcp:send(Sock, Pong).

%%----------------------------------------------------------------------
%% join/2
%% Format an IRC join command: Used to join a discussion channel
%%----------------------------------------------------------------------
join(Sock, Channel) ->
    Command = lists:append("JOIN ", Channel),
    command(Sock, Command).

%%----------------------------------------------------------------------
%% say/3
%% Say something in the given channel
%%----------------------------------------------------------------------
say(Sock, Channel, Message) ->
    Command = lists:concat(["PRIVMSG ", Channel, " :", Message]),
    command(Sock, Command).

%%----------------------------------------------------------------------
%% say/3
%% Say something in the given channel
%%----------------------------------------------------------------------
action(Sock, Channel, Message) ->
    Command = "PRIVMSG " ++ Channel ++ " :"
	++ [1] ++ "ACTION " ++ Message ++ [1],
    command(Sock, Command).

%%----------------------------------------------------------------------
%% command/2
%% Send a command to the IRC server
%%----------------------------------------------------------------------
command(Sock, Command) ->
    CompleteCmd = [Command, "\r\n"],
    gen_tcp:send(Sock, "\r\n"), % FIXME: Workaround: The first message of a
                                % sequence does not seem to be received by the
                                % server ...
				% Sending a blank line to awake the line...
    gen_tcp:send(Sock, CompleteCmd).

%%----------------------------------------------------------------------
%% is_chanop/1
%% Returns true if the given nick is a chanop or false otherwise
%% A chanop as an '@' before its nickname.
%%----------------------------------------------------------------------
is_chanop([$@ | Nickname]) ->
    true;
is_chanop(Nickname) ->
    false.

%%----------------------------------------------------------------------
%% nickname/1
%% Return the nickname (removing '@' to chanop)
%% A chanop as an '@' before its nickname.
%%----------------------------------------------------------------------
nickname([$@ | Nickname]) ->
    Nickname;
nickname(Nickname) ->
    Nickname.

%%----------------------------------------------------------------------
%% split_nick_user/1
%% Return the nickname in lower case and
%% the user
%%----------------------------------------------------------------------
split_nick_user(HeaderFrom) ->
    %% Split the string between Nick and User (separated by !)
    [Nick, User] = string:tokens(HeaderFrom, "!"),
    
    %% Remove chanop indicator from nick
    Nick2 = nickname(Nick),

    %% convert Nickname to lowercase
    Nick3 = misc_tools:lower_string(Nick2),

    %% Return a list: Nick, User
    [Nick3, User].
