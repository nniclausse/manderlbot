%%% File    : mdb_bhv_think.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : 
%%% Created : 12 Aug 2003 by Nicolas Niclausse <nico@niclux.org>

-module(mdb_bhv_think).
-vc('$Id$ ').
-author('nico@niclux.org').

-export([behaviour/5]). % MDB behaviour API

-include("mdb.hrl").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  Answer the given data, but waiting for random time
%%%           between the 2 lines to say.
%%%----------------------------------------------------------------------
behaviour(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    case length(Data) of
	1 ->
	    [String] = Data,
	    mdb_bot:say(BotPid, String, NickFrom);

	2 ->
	    [H|T] = Data,
	    mdb_bot:say(BotPid, H, NickFrom),
	    %% we sleep for a random time
	    {A, B, C} = now(),
	    random:seed(A, B, C),
	    timer:sleep(random:uniform(?RNDTIME) + ?TIME),
	    mdb_bot:say(BotPid, T, NickFrom);

	More ->
	    behaviour(Input, BotName, Data, BotPid, Channel)
    end;

behaviour(Input, BotName, Data, BotPid, Channel) ->
    case length(Data) of
	1 ->
	    [String] = Data,
	    mdb_bot:say(BotPid, String);

	2 ->
	    [H|T] = Data,
	    mdb_bot:say(BotPid, H),
	    %% we sleep for a random time
	    {A, B, C} = now(),
	    random:seed(A, B, C),
	    timer:sleep(random:uniform(?RNDTIME) + ?TIME),
	    mdb_bot:say(BotPid, T);

	More ->
	    behaviour(Input, BotName, Data, BotPid, Channel)
    end.

