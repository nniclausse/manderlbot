%%%----------------------------------------------------------------------
%%% File    : mdb_behaviours_new.erl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Implements the bot generic behaviours, to be used in the
%%%           config file.
%%% Created : 27 Feb 2002 by Dimitri Fontaine <dim@tuxfamily.org>
%%%----------------------------------------------------------------------

-module(mdb_behaviours).
-author('tapoueh@free.fr').

%% Exported behaviours
-export([getFun/1]).

-export([say/5, action/5, answer/5, random/5, timer/5, bloto/5,
         google/5, dict/5, rejoin/5, reconf/5, mute/5,
         debian_pkg/5, debian_file/5, pyramid/5
        ]).

-include("mdb.hrl").
-define(TIME, 2000).
-define(RNDTIME, 3000).


%%%----------------------------------------------------------------------
%%% Function: getFun/1
%%% Purpose:  Given the action name, returns the Module and Function to
%%%           call.
%%%----------------------------------------------------------------------
getFun(Action) ->
    case Action of
	"rejoin"      -> {mdb_behaviours, rejoin};
	"reconf"      -> {mdb_behaviours, reconf};
	"say"         -> {mdb_behaviours, say};
	"answer"      -> {mdb_behaviours, answer};
	"random"      -> {mdb_behaviours, random};
	"timer"       -> {mdb_behaviours, timer};
	"think"       -> {mdb_behaviours, action};
	"bloto"       -> {mdb_behaviours, bloto};
	"google"      -> {mdb_behaviours, google};
	"dict"        -> {mdb_behaviours, dict};
	"mute"        -> {mdb_behaviours, mute};
	"debian_pkg"  -> {mdb_behaviours, debian_pkg};
	"debian_file" -> {mdb_behaviours, debian_file};
	"pyramid"     -> {mdb_behaviours, pyramid};
	Other         -> {mdb_behaviours, say}
    end.


%% A plugin function for behaviour is of the form:
%% Fun(Input, BotName, Data, BotPid, Channel)

%%%----------------------------------------------------------------------
%%% Function: say/5
%%% Purpose:  Say the data in the channel or to the speaker
%%%----------------------------------------------------------------------
say(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    lists:map(fun(String) ->
		      mdb_bot:say(BotPid, String, NickFrom)
	      end,
	      Data);

say(Input, BotName, Data, BotPid, Channel) ->
    lists:map(fun(String) ->
		      mdb_bot:say(BotPid, String)
	      end,
	      Data).


%%%----------------------------------------------------------------------
%%% Function: action/5
%%% Purpose:  Answer with Action IRC usage - /me
%%%----------------------------------------------------------------------
action(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    lists:map(fun(String) ->
		      mdb_bot:action(BotPid, String, NickFrom)
	      end,
	      Data);

action(Input, BotName, Data, BotPid, Channel) ->
    lists:map(fun(String) ->
		      mdb_bot:action(BotPid, String)
	      end,
	      Data).


%%%----------------------------------------------------------------------
%%% Function: answer/5
%%% Purpose:  Answer the given (config) data to the one who talk
%%%----------------------------------------------------------------------
answer(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    lists:map(fun(String) ->
		      mdb_bot:say(BotPid, String, NickFrom)
	      end,
	      Data);

answer(Input, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    lists:map(fun(String) ->
		      mdb_bot:say(BotPid, NickFrom ++ ": " ++ String)
	      end,
	      Data).


%%%----------------------------------------------------------------------
%%% Function: random/5
%%% Purpose:  Choose at random a line in Data and answer it.
%%%----------------------------------------------------------------------
random(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    {A, B, C} = now(),
    random:seed(A, B, C),
    mdb_bot:say(BotPid,
		lists:nth(random:uniform(length(Data)), Data),
		NickFrom);

random(Input, BotName, Data, BotPid, Channel) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    mdb_bot:say(BotPid, lists:nth(random:uniform(length(Data)), Data)).


%%%----------------------------------------------------------------------
%%% Function: timer/5
%%% Purpose:  Answer the given data, but waiting for random time
%%%           between the 2 lines to say.
%%%----------------------------------------------------------------------
timer(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    case length(Data) of
	1 ->
	    [String] = Data,
	    mdb_bot:say(BotPid, String, NickFrom);

	2 ->
	    [H|T] = Data,
	    mdb_bot:say(BotPid, H, NickFrom),
	    %% we sleep for a random time
	    {A, B, C} = now(),
	    random:seed(A, B, C),
	    timer:sleep(random:uniform(?RNDTIME) + ?TIME),
	    mdb_bot:say(BotPid, T, NickFrom);

	More ->
	    say(Input, BotName, Data, BotPid, Channel)
    end;

timer(Input, BotName, Data, BotPid, Channel) ->
    case length(Data) of
	1 ->
	    [String] = Data,
	    mdb_bot:say(BotPid, String);

	2 ->
	    [H|T] = Data,
	    mdb_bot:say(BotPid, H),
	    %% we sleep for a random time
	    {A, B, C} = now(),
	    random:seed(A, B, C),
	    timer:sleep(random:uniform(?RNDTIME) + ?TIME),
	    mdb_bot:say(BotPid, T);

	More ->
	    say(Input, BotName, Data, BotPid, Channel)
    end.


%%%----------------------------------------------------------------------
%%% Function: bloto/5
%%% Purpose:  Play to business loto...
%%%----------------------------------------------------------------------
bloto(Input, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    case bloto:add(NickFrom, Channel) of
	{winner, Nick} ->
	    mdb_bot:say(BotPid, Nick ++ " " ++ Data);

	Other ->
	    ok
    end.


%%%----------------------------------------------------------------------
%%% Function: rejoin/5
%%% Purpose:  When kicked, rejoin the chan
%%%----------------------------------------------------------------------
rejoin(Input, BotName, Data, BotPid, Channel) ->
    mdb_bot:rejoin(BotPid).


%%%----------------------------------------------------------------------
%%% Function: reconf/5
%%% Purpose:  Re-read now the config file
%%%----------------------------------------------------------------------
reconf(Input, BotName, ConfigFile, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    mdb_bot:reconf(BotPid, NickFrom, ConfigFile).		     


%%%----------------------------------------------------------------------
%%% Function: mute/5
%%% Purpose:  allow the bot not to react for a while
%%%----------------------------------------------------------------------
mute(Input, BotName, Data, BotPid, Channel) ->
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    io:format("mute ~n", []),
    mdb_bot:mute(BotPid, NickFrom).		     


%%%----------------------------------------------------------------------
%%% Function: google/5
%%% Purpose:  ask google and give the first response
%%%----------------------------------------------------------------------
google(Input, BotName, Data, BotPid, Channel) ->
    io:format("GOOGLE input: ~p~n", [Input#data.body]),

    [Key | Args] = string:tokens(Input#data.body," "),
    Criteria= misc_tools:join("+", Args),
	
    io:format("GOOGLE criteria: ~p~n", [Criteria]),

    google:search(Criteria, Input, BotPid, BotName, Channel).

%%%----------------------------------------------------------------------
%%% Function: debian_pkg/5
%%% Purpose:  search for debian packages
%%%----------------------------------------------------------------------
debian_pkg(Input, BotName, Data, BotPid, Channel) ->
    io:format("DEBIAN package input: ~p~n", [Input#data.body]),
	[Key, String | Args] = string:tokens(Input#data.body," "),
	case Args of 
	    [] ->
		io:format("DEBIAN criteria: ~p~n", [String]),
		debian:search([package, String],
			      Input, BotPid, BotName, Channel);

	    [Version | _] -> % which debian distrib
		io:format("DEBIAN criteria: ~p,~p~n", [String, Version]),
		debian:search([package, String, Version],
			      Input, BotPid, BotName, Channel)
    end.

%%%----------------------------------------------------------------------
%%% Function: debian_file/5
%%% Purpose:  search for files in debian package
%%%----------------------------------------------------------------------
debian_file(Input, BotName, Data, BotPid, Channel) ->
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


%%%----------------------------------------------------------------------
%%% Function: dict/5
%%% Purpose:  ask dict for a word definition
%%%----------------------------------------------------------------------
dict(Input, BotName, Data, BotPid, Channel) ->
    [DictName] = Data,
    io:format("DICT input: ~p~n", [Input#data.body]),
    io:format("DICT name:  ~p~n", [DictName]),

    [Key | Args] = string:tokens(Input#data.body," "),
    Criteria = string:strip(Args),
   
    io:format("DICT criteria: ~p~n", [Criteria]),

    case DictName of
	[] ->
	    mdb_dict:search(Criteria, Input, BotPid, BotName, Channel);
	_ ->
	    mdb_dict:search(Criteria, Input,
			    BotPid, BotName, Channel, DictName)
    end.

%%%----------------------------------------------------------------------
%%% Function: pyramid/5
%%% Purpose:  implements a pyramid game, see comments
%%%----------------------------------------------------------------------
pyramid(Input = #data{header_to=BotName}, BotName, Data, BotPid, Channel) ->
    %%  - first player giving the bot the answer, before beginning the game,
    %%    in private dialog
    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),
    [Header, Word] =  string:tokens(Input#data.body, ": "),

    case pyramid:setWord(NickFrom, Channel,
			 misc_tools:downcase(string:strip(Word))) of
	{ok, Message} ->
	    mdb_bot:say(BotPid, Message, NickFrom),
	    mdb_bot:say(BotPid, NickFrom ++ " has set a word to guess !");

	{error, Reason} ->
	    mdb_bot:say(BotPid, Reason, NickFrom)
    end;

pyramid(Input, BotName, Data, BotPid, Channel) ->
    %% For this game, we have to detect some different cases on the
    %% same behaviour, that is :
    %%
    %%  - beginning of game, first player inviting second and giving the
    %%    number of tries
    %%
    %%  - second player guess

    [NickFrom|IpFrom] = string:tokens(Input#data.header_from, "!"),

    io:format("body: ~p~n", [Input#data.body]),

    case regexp:match(Input#data.body, "[A-za-z]+/[0-9]") of
	{match, S, L} ->
	    [Player2, Nguess] =
		string:tokens(string:substr(Input#data.body, S, L), "/"),

	    {ok, [{integer, 1, Iguess}],1} = erl_scan:string(Nguess),
	    
	    io:format("pyramid: ~p invite ~p in ~p~n",
		      [NickFrom, Player2, Iguess]),

	    {_ok, SMsg} = pyramid:start(NickFrom, Channel, Player2, Iguess),
	    mdb_bot:say(BotPid, SMsg);

	_Whatever  ->
	    %% That is a guess
	    [Header, Word] =  string:tokens(Input#data.body, ": "),
	    {_State, GMsg}  =
		pyramid:guess(NickFrom, Channel,
			      misc_tools:downcase(string:strip(Word))),

	    mdb_bot:say(BotPid, GMsg)
    end.
