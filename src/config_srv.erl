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
-export([start_link/1, getConf/0, getDictConf/0, reconf/2,
	 getBList/1, getBehaviours/1]).

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

reconf(Chan, ConfigFile) ->
    gen_server:call(?MODULE, {reconf, Chan, ConfigFile}, ?timeout).

getBList(Channel) ->
    gen_server:call(?MODULE, {getlist, Channel}, ?timeout).

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

handle_call({reconf, Channel, ConfigFile}, From, Config) ->
    case config:read(ConfigFile) of
	{ok, NewConfig = #config{}} ->
	    %% Don't forget to check for new chans to join
	    %% In order to avoid a re-entrance which faults in timeout,
	    %% we pass directly from here the new bot BList !
	    checkForNewChans(NewConfig),
	    {reply, {ok, getBehaviours(NewConfig, Channel)}, NewConfig};

	Error ->
	    {reply, Error, Config}
    end;

handle_call({getlist, Channel}, From, Conf) ->
    {reply, {ok, getBehaviours(Conf, Channel)}, Conf};


handle_call({getBehaviours, BNames}, From, Conf = #config{behaviours=BList}) ->
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
    %% Auto add the rejoin-on-kick ability
    Rejoin = #behaviour{id       = "rejoin",
			pattern  = #data{header_op="KICK"},
			function = {mdb_behaviours, rejoin},
			data     = []
			},

    Reconf = #behaviour{id       = "reconf",
			pattern  = #data{body={regexp, "^%BOTNAME.*reconf"}},
			function = {mdb_behaviours, reconf},
			data     = "../config.xml"
			},

    [Rejoin, Reconf | Acc];

build_behaviours_list([BC=#cfg_behaviour{name=Id,
					 data=Data,
					 action="login"}|BClist], Acc) ->
    Login = #behaviour{id       = Id,
		       pattern  = #data{header_op   = "JOIN",
					header_from = "%BOTNAME.*"},
		       function = {mdb_behaviours, say},
		       data     = Data
		      },
    build_behaviours_list(BClist, [Login|Acc]);

build_behaviours_list([BC=#cfg_behaviour{}|BClist], Acc) ->
    %% Here we map the actions defined in the config file
    %% With the code to use in order to make the action
    Fun = case BC#cfg_behaviour.action of
	      "say"    -> {mdb_behaviours, say};
	      "answer" -> {mdb_behaviours, answer};
	      "random" -> {mdb_behaviours, random};
	      "timer"  -> {mdb_behaviours, timer};
	      "think"  -> {mdb_behaviours, action};
	      "bloto"  -> {mdb_behaviours, bloto};
	      "google" -> {mdb_behaviours, google};
	      "dict"   -> {mdb_behaviours, dict};
	      "mute"   -> {mdb_behaviours, mute};
	      "debian_pkg"  -> {mdb_behaviours, debian_pkg};
	      "debian_file" -> {mdb_behaviours, debian_file};
	      Other    -> {mdb_behaviours, say}
	  end,

    Behaviour = #behaviour{id       = BC#cfg_behaviour.name,
			   pattern  = #data{body={regexp,
						  BC#cfg_behaviour.pattern}},
			   function = Fun,
			   data     = BC#cfg_behaviour.data},

    build_behaviours_list(BClist, [Behaviour|Acc]).



%%----------------------------------------------------------------------
%% getBehaviours/2
%%   Read the config and find on it our behaviours
%%----------------------------------------------------------------------
getBehaviours(#config{servers=SList}, Chan) ->
    getBehaviours(SList, Chan);

getBehaviours([#server{channels=CList}|STail], Chan) ->
    case getBehaviours(CList, Chan) of
	notfound -> getBehaviours(STail, Chan);
	BList    -> BList
    end;

getBehaviours([#channel{name=Chan, behaviours=BList}|CTail], Chan) ->
    ["reconf", "rejoin" | BList];

getBehaviours([#channel{name=Name}|CTail], Chan) ->
    getBehaviours(CTail, Chan);

getBehaviours([], Chan) ->
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

checkForNewChans([#server{host=Host, port=Port, channels=CList}|Stail],
		 [Name, Ctlr],
		 Config
		) ->
    checkForNewChans(CList, [Name, Ctlr, Host, Port], Config),
    checkForNewChans(Stail, [Name, Ctlr], Config);

checkForNewChans([Channel=#channel{name=Chan}|CTail],
		 [Name, Ctlr, Host, Port],
		 Config) ->
    mdb_botlist:add(Name, Ctlr, Host, Port, Channel, getBehaviours(Config, Chan)),
    checkForNewChans(CTail, [Name, Ctlr, Host, Port], Config);

checkForNewChans([], Params, Config) ->
    done.
