%%% File    : mdb_bhv_debian_pkg.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : 
%%% Created : 12 Aug 2003 by Nicolas Niclausse <nico@niclux.org>

-module(mdb_bhv_debian_file).
-vc('$Id$ ').
-author('nico@niclux.org').

-export([behaviour/5]). % MDB behaviour API

-include("mdb.hrl").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  search for files in debian package
%%%----------------------------------------------------------------------
behaviour(Input, BotName, Data, BotPid, Channel) ->
    io:format("DEBIAN file input: ~p~n", [Input#data.body]),
    [Key, String | Args] = string:tokens(Input#data.body," "),
    case Args of 
        [] ->
            io:format("DEBIAN criteria: ~p~n", [String]),
            debian:search([file, String], Input, BotPid, BotName, Channel);

        [Version | _] -> % which debian distrib
            io:format("DEBIAN criteria: ~p,~p~n", [String, Version]),
            debian:search([file, String, Version], Input,
			  BotPid, BotName, Channel)
    end.

