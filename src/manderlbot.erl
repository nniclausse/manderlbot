%%%----------------------------------------------------------------------
%%% File    : manderlbot.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : This app is an IRC bot
%%% Created : 19 Feb 2002 by Dimitri Fontaine <dim@tuxfamily.org>
%%%----------------------------------------------------------------------

-module(manderlbot).
-author('dim@tuxfamily.org').

-include("config.hrl").
-define(CONFIG_FILE, "../config.xml").

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%----------------------------------------------------------------------
start(Type, StartArgs) ->
    %%case mdb_sup:start_link(StartArgs) of
    case manderlbot_sup:start_link() of
	{ok, Pid} -> 
	    %% Do our stuff
	    init(),
	    {ok, Pid};
	Error ->
	    Error
    end.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%----------------------------------------------------------------------
stop(State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
init() ->
    %% Start a bot per chan
    {ok, Conf} = config_srv:getConf(),

    Name      = Conf#config.name,
    Controler = Conf#config.controler,

    lists:foreach(fun(Server = #server{host     = Host,
				       port     = Port,
				       channels = ChanList}) ->

			  StartBot = fun(Chan = #channel{}) ->
					     mdb_botlist:add(Name,
							     Controler,
							     Host,
							     Port,
							     Chan)
				     end,
			  lists:foreach(StartBot, ChanList)
		  end,
		  Conf#config.servers),
    ok.
