%%%----------------------------------------------------------------------
%%% File    : mdb_sup.erl
%%% Author  : Dimitri Fontaine <tapoueh@free.fr>
%%% Purpose : Supervise all the bot instances (dynamic)
%%% Created : 19 Feb 2002 by Dimitri Fontaine <tapoueh@free.fr>
%%%----------------------------------------------------------------------

-module(manderlbot_sup).
-author('tapoueh@free.fr').

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

    BServ  = {config_srv, {config_srv, start_link, ["../config.xml"]},
	      permanent, 2000, worker, [config_srv]},

    BLoto  = {bloto, {bloto, start_link, []},
	      permanent, 2000, worker, [bloto]},

    {ok, {{one_for_all, 3, 60}, [BotSup, BServ, BLoto]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
