%%% File    : mdb_bhv_dict.erl
%%% Author  : Nicolas Niclausse <nico@niclux.org>
%%% Purpose : search for word definition using the DICT protocol (RFC 2229)
%%% Created : 16 Jul 2002 by Nicolas Niclausse <nico@niclux.org>

-module(mdb_bhv_dict).
-author('nniclausse@idealx.com').
-revision(' $Id$ ').
-vsn(' $Revision$ ').

-export([behaviour/5]). % MDB behaviour API
-export([search/5, search/6, parse/1, set_request/1]).

-include("mdb.hrl").

%%%----------------------------------------------------------------------
%%% Function: dict/5
%%% Purpose:  ask dict for a word definition
%%%----------------------------------------------------------------------
behaviour(Input, BotName, Data, BotPid, Channel) ->
    [DictName] = Data,
    io:format("DICT input: ~p~n", [Input#data.body]),
    io:format("DICT name:  ~p~n", [DictName]),

    [Key | Args] = string:tokens(Input#data.body," "),
    Criteria = string:strip(Args),
   
    io:format("DICT criteria: ~p~n", [Criteria]),

    case DictName of
	[] ->
	    search(Criteria, Input, BotPid, BotName, Channel);
	_ ->
	    search(Criteria, Input,
			    BotPid, BotName, Channel, DictName)
    end.

%% search with default dictionnary 
search(Keywords, Input, BotPid, BotName, Channel) ->
    io:format("params: ~p~n", [getConf()]),
    mdb_search:search({Keywords, Input, BotPid, BotName, Channel, getConf()}).

search(Keywords, Input, BotPid, BotName, Channel, Dict) ->
    io:format("params: ~p~n", [getConf()]),
    mdb_search:search({[Keywords, Dict],
		       Input, BotPid, BotName, Channel, getConf()}).

getConf() ->
    {ok, {Host, Port, Default}} = config_srv:getDictConf(),
    #search_param{type = ?MODULE, server = Host, port = Port}.

%%----------------------------------------------------------------------
%% Func: parse/1
%% Purpose: Parse data
%% Returns: {stop, Result} | {stop} | {continue} | {continue, Result}
%%      continue -> continue parsing of incoming data
%%      stop     -> stop parsing of incoming data
%%      Result   -> String to be printed by mdb
%%----------------------------------------------------------------------
parse("250" ++ Data) -> %% completed
    {stop};
parse("552" ++ Data) -> %% no match
    {stop, "not found"};
parse("150" ++ Data) -> %% response headers (n def. found)
    {continue};
parse("151" ++ Data) -> %% response headers (database name)
    {continue};
parse("") ->
    {continue};
parse(".\r\n") ->
    {continue};
parse(Data) ->
    case regexp:first_match(Data, "^[0-9][0-9][0-9] ") of
	{match,Start,Length} -> % skip protocol data
	    {continue};
	_ -> 
	    {continue, lists:subtract(Data,"\r\n")}
    end.

%%%search using Dict dictionnary
set_request([Keyword, Dict]) ->
    set_request(Keyword, Dict);

%%% get default dict configuration
set_request(Keyword) ->
    {ok, {_, _, Default}} = config_srv:getDictConf(),
    set_request(Keyword, Default).

set_request(Keyword, Dict) ->
    "DEFINE " ++ Dict ++ " " ++ Keyword	++ io_lib:nl().