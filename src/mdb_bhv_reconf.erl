%%% File    : mdb_bhv_reconf.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : 
%%% Created : 12 Aug 2003 by Nicolas Niclausse <nico@niclux.org>

-module(mdb_bhv_reconf).
-vc('$Id$ ').
-author('nico@niclux.org').

-export([behaviour/5]). % MDB behaviour API

-include("mdb.hrl").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  Re-read now the config file
%%%----------------------------------------------------------------------
behaviour(Input, BotName, ConfigFile, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    mdb_bot:reconf(BotPid, NickFrom, ConfigFile).		     
