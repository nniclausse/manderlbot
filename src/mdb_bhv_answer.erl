%%% File    : mdb_bhv_answer.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : 
%%% Created : 12 Aug 2003 by Nicolas Niclausse <nico@niclux.org>

-module(mdb_bhv_answer).
-vc('$Id$ ').
-author('nico@niclux.org').

-export([behaviour/5]). % MDB behaviour API

-include("mdb.hrl").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  Answer the given (config) data to the one who talk
%%%----------------------------------------------------------------------
behaviour(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    lists:map(fun(String) ->
		      mdb_bot:say(BotPid, String, NickFrom)
	      end,
	      Data);

behaviour(Input, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    lists:map(fun(String) ->
		      mdb_bot:say(BotPid, NickFrom ++ ": " ++ String)
	      end,
	      Data).
