%%%----------------------------------------------------------------------
%%% File    : mdb_behaviours_srv.erl
%%% Author  : Dimitri Fontaine <tapoueh@free.fr>
%%% Purpose : Manage the manderlbot config
%%% Created :  2 Mar 2002 by Dimitri Fontaine <tapoueh@free.fr>
%%%----------------------------------------------------------------------

-module(config_srv).
-author('fontaine@whitestar').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/1, getConf/0, getDictConf/0, reconf/3,
	 getBList/2, getBehaviours/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("config.hrl").
-include("mdb.hrl").

%% We need a big timeout in order to be able to launch new bots.
-define(timeout, 10000).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(ConfigFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfigFile], []).

getConf() ->
    gen_server:call(?MODULE, {getConf}, ?timeout).

getDictConf() ->
    gen_server:call(?MODULE, {getDictConf}, ?timeout).

reconf(Chan, BotName, ConfigFile) ->
    gen_server:call(?MODULE, {reconf, Chan, BotName, ConfigFile}, ?timeout).

getBList(Channel, BotName) ->
    gen_server:call(?MODULE, {getlist, Channel, BotName}, ?timeout).

getBehaviours(BNames) ->
    gen_server:call(?MODULE, {getBehaviours, BNames}, ?timeout).
    

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([ConfigFile]) ->
    case config:read(ConfigFile) of
	{ok, Config}    -> {ok, Config};
	{error, Reason} -> {stop, Reason}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({getConf}, From, Config) ->
    {reply, {ok, Config}, Config};

handle_call({getDictConf}, From, Config = #config{dict=Dict}) ->
    {reply, {ok, Dict}, Config};

handle_call({reconf, Channel, BotName, ConfigFile}, From, Config) ->
    case config:read(ConfigFile) of
	{ok, NewConfig = #config{}} ->
	    %% Don't forget to check for new chans to join
	    checkForNewChans(NewConfig),

	    {reply,
	     {ok, getBehaviours(NewConfig, Channel, BotName)}, NewConfig};

	Error ->
	    {reply, Error, Config}
    end;

handle_call({getlist, Channel, BotName}, From, Conf) ->
    io:format("~p ~p ~p~n", [Channel, BotName,
			      getBehaviours(Conf, Channel, BotName)]),
    {reply, {ok, getBehaviours(Conf, Channel, BotName)}, Conf};


handle_call({getBehaviours, notfound}, From, Conf) ->
    {reply, {ok, []}, Conf};

handle_call({getBehaviours, BNames}, From, Conf=#config{behaviours=BList}) ->
    %% io:format("BNames: ~p~n", [BNames]),
    {reply,
     {ok, lists:filter(fun(Behaviour=#behaviour{id=Id}) ->
			       lists:member(Id, BNames)
		       end,
		       build_behaviours_list(BList, []))},
     Conf};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% build_behaviours_list/2
%%   Build a behaviour list from the behaviours found in the
%%   config file
%%----------------------------------------------------------------------
build_behaviours_list([], Acc) ->
    Acc;

build_behaviours_list([BC=#cfg_behaviour{action=Action}|BClist], Acc) ->
    %% Here we map the actions defined in the config file
    %% With the code to use in order to make the action
    Behaviour =
	#behaviour{id       = BC#cfg_behaviour.name,

		   pattern  = #data{
		     header_from    = {regexp, BC#cfg_behaviour.from},
		     header_to      = {regexp, BC#cfg_behaviour.to},
		     header_op      = {regexp, BC#cfg_behaviour.op},
		     header_options = {regexp, BC#cfg_behaviour.option},
		     body           = {regexp, BC#cfg_behaviour.pattern}},

		   exclude_pattern = #data{
		     header_from    = {regexp, BC#cfg_behaviour.exl_from},
		     header_to      = {regexp, BC#cfg_behaviour.exl_to},
		     header_op      = {regexp, BC#cfg_behaviour.exl_op},
		     header_options = {regexp, BC#cfg_behaviour.exl_option},
		     body           = {regexp, BC#cfg_behaviour.exl_pattern}},

		   function = mdb_behaviours:getFun(Action),
		   data     = BC#cfg_behaviour.data},

    build_behaviours_list(BClist, [Behaviour|Acc]).



%%----------------------------------------------------------------------
%% getBehaviours/3
%%   Read the config and find on it our behaviours
%%----------------------------------------------------------------------
getBehaviours(#config{servers=SList}, Chan, BotName) ->
    getBehaviours(SList, Chan, BotName);

getBehaviours([#server{channels=CList}|STail], Chan, BotName) ->
    case getBehaviours(CList, Chan, BotName) of
	notfound -> getBehaviours(STail, Chan, BotName);
	BList    -> BList
    end;

getBehaviours([#channel{name=Chan, botname=BotName, behaviours=BList}|CTail],
	      Chan, BotName) ->
    BList;

getBehaviours([#channel{}|CTail], Chan, BotName) ->
    getBehaviours(CTail, Chan, BotName);

getBehaviours([], Chan, BotName) ->
    notfound.


%%----------------------------------------------------------------------
%% checkForNewChans/3
%%   For each server/chan in the config, add it to the mdb_botlist.
%%   If no bot instance is connected, a new one will be started, 
%%   calling mdb_botlist:add(Name, Controler, Host, Port, Chan)
%%----------------------------------------------------------------------
checkForNewChans(Config) ->
    checkForNewChans(Config, [], Config).

checkForNewChans(#config{name=Name, controler=Ctlr, servers=SList},
		 Params,
		 Config
		) ->
    checkForNewChans(SList, [Name, Ctlr], Config);

checkForNewChans([#server{host=Host, port=Port,
			  passwd=Pass, channels=CList}|Stail],
		 [Name, Ctlr],
		 Config
		) ->
    checkForNewChans(CList, [Name, Ctlr, Host, Port, Pass], Config),
    checkForNewChans(Stail, [Name, Ctlr], Config);

checkForNewChans([Channel=#channel{name=Chan, botname=BotName}|CTail],
		 [Name, Ctlr, Host, Port, Pass],
		 Config) ->
    
    %% In order to avoid a re-entrance which faults in timeout,
    %% we pass directly from here the new bot BList !
    mdb_botlist:add(Name, Ctlr, Host, Port, Pass, Channel,
		    getBehaviours(Config, Chan, BotName)),

    checkForNewChans(CTail, [Name, Ctlr, Host, Port, Pass], Config);

checkForNewChans([], Params, Config) ->
    done.
