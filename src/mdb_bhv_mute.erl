%%% File    : mdb_bhv_mute.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : 
%%% Created : 12 Aug 2003 by Nicolas Niclausse <nico@niclux.org>

-module(mdb_bhv_mute).
-vc('$Id$ ').
-author('nico@niclux.org').

-export([behaviour/5]). % MDB behaviour API

-include("mdb.hrl").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  allow the bot not to react for a while
%%%----------------------------------------------------------------------
behaviour(Input, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    io:format("mute ~n", []),
    mdb_bot:mute(BotPid, NickFrom).		     
