%%%----------------------------------------------------------------------
%%% File    : mdb_sup.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Supervise all the bot instances (dynamic)
%%% Created : 19 Feb 2002 by Dimitri Fontaine <dim@tuxfamily.org>
%%%----------------------------------------------------------------------

-module(manderlbot_sup).
-author('dim@tuxfamily.org').

-include("mdb.hrl").
-include("config.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init([]) ->
    BotSup = {mdb_bot_sup, {mdb_bot_sup, start_link, []},
	      permanent, 2000, supervisor, [mdb_bot_sup]},

    BotLst = {mdb_botlist, {mdb_botlist, start_link, []},
	      permanent, 2000, worker, [mdb_botlist]},

    BServ  = {config_srv, {config_srv, start_link, ["../config.xml"]},
	      permanent, 2000, worker, [config_srv]},

    BLoto  = {bloto, {bloto, start_link, []},
	      permanent, 2000, worker, [bloto]},

    BSearch = {mdb_search, {mdb_search, start_link, []},
	      permanent, 2000, worker, [mdb_search]},

    {ok, {{one_for_one, 3, 60}, [BotSup, BotLst, BServ, BLoto, BSearch]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
