%%% File    : mdb_bhv_google.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : ask google for the first match of a given keyword
%%% Created : 16 Jul 2002 by Nicolas Niclausse <nico@niclux.org>
%%%----------------------------------------------------------------------
%%%
%%% This file is part of Manderlbot.
%%%
%%% Manderlbot is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% Manderlbot is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% See LICENSE for detailled license
%%%
%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two. If you modify this file, you may extend this exception
%%% to your version of the file, but you are not obligated to do
%%% so.  If you do not wish to do so, delete this exception
%%% statement from your version.
%%%
%%%----------------------------------------------------------------------

-module(mdb_bhv_google).
-author('nico@niclux.org').
-revision(' $Id$ ').
-vsn(' $Revision$ ').

-export([behaviour/5]). % MDB behaviour API
-export([search/5, parse/1, set_request/1]).

-include("mdb.hrl").

-define(google_name, "www.google.com").
-define(google_port, 80).
-define(notfound, "Aucun document ne correspond").
-define(CR, "\n").

%%%----------------------------------------------------------------------
%%% Function: behaviour/5
%%% Purpose:  ask google and give the first response
%%%----------------------------------------------------------------------
behaviour(Input, BotName, Data, BotPid, Channel) ->
    io:format("GOOGLE input: ~p~n", [Input#data.body]),

    [Key | Args] = string:tokens(Input#data.body," "),
    Criteria= misc_tools:join("+", Args),
	
    io:format("GOOGLE criteria: ~p~n", [Criteria]),

    search(Criteria, Input, BotPid, BotName, Channel).


search(Keywords, Input, BotPid, BotName, Channel) ->
    mdb_search:search({Keywords,
                       Input, BotPid, BotName, Channel,
                       #search_param{type   = ?MODULE, 
                                     server = ?google_name,
                                     port   = ?google_port }
                      }).

%%----------------------------------------------------------------------
%% Func: parse/1
%% Purpose: Parse data
%% Returns: {stop, Result} | {stop} | {continue} | {continue, Result}
%%      continue -> continue parsing of incoming data
%%      stop     -> stop parsing of incoming data
%%      Result   -> String to be printed by mdb
%%----------------------------------------------------------------------
parse(Data) ->
	case regexp:first_match(Data, "Location: http://[^\"]+") of %"
		{match,Start,Length} -> % ok, found
		    {stop, string:substr(Data, 11, length(Data)-12) };
		_ -> 
			case regexp:first_match(Data, "<br><br>" ++ ?notfound) of %"
				{match, _S, _L} ->
					{stop, "not found"};
				_NoMatch -> 
					{continue}
			end
	end.

%%----------------------------------------------------------------------
%% Func: set_request/1
%% Purpose: Set the request given Keywords 
%% Returns: String
%%----------------------------------------------------------------------
set_request(Keywords) ->
    "GET /search?q=" ++ Keywords ++"&hl=fr&btnI=J%27ai+de+la+chance HTTP/1.0"
	++ ?CR ++ ?CR.
