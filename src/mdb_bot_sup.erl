%%%----------------------------------------------------------------------
%%% File    : mdb_sup.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Supervise all the bot instances (dynamic)
%%% Created : 19 Feb 2002 by Dimitri Fontaine <dim@tuxfamily.org>
%%%----------------------------------------------------------------------

-module(mdb_bot_sup).
-author('dim@tuxfamily.org').

-include("mdb.hrl").
-include("config.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_child/6, start_child/7]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name, Controler, Host, Port, Pass, Chan) ->
    supervisor:
	start_child(?MODULE, [[Name, Controler, Host, Port, Pass, Chan, []]]).

start_child(Name, Controler, Host, Port, Pass, Chan, BList) ->
    supervisor:
	start_child(?MODULE,
		    [[Name, Controler, Host, Port, Pass, Chan, BList]]).

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
    Bot = {manderlbot, {mdb_bot, start_link, []},
	   transient, 2000, worker, [mdb_bot]},

    {ok, {{simple_one_for_one, 3, 60}, [Bot]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
