%%% File    : mdb_bhv_say.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : 
%%% Created : 12 Aug 2003 by Nicolas Niclausse <nico@niclux.org>

-module(mdb_bhv_say).
-vc('$Id$ ').
-author('nico@niclux.org').

-export([behaviour/5]). % MDB behaviour API

-include("mdb.hrl").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  Say the data in the channel or to the speaker
%%%----------------------------------------------------------------------
behaviour(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    lists:map(fun(String) ->
		      mdb_bot:say(BotPid, String, NickFrom)
	      end,
	      Data);

behaviour(Input, BotName, Data, BotPid, Channel) ->
    lists:map(fun(String) ->
		      mdb_bot:say(BotPid, String)
	      end,
	      Data).

