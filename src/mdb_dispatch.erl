%%%----------------------------------------------------------------------
%%% File    : mdb_dispatch.erl
%%% Author  : Mickaël Rémond <mickael.remond@erlang-fr.org>
%%% Purpose : Library gather the process of IRC event and the execution
%%%           of "behaviours" (event handling code).
%%% Created : 16 Sep 2001, Mickaël Rémond <mickael.remond@erlang-fr.org>
%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or modify  
%%% it under the terms of the GNU General Public License as published by 
%%% the Free Software Foundation; either version 2 of the License, or   
%%% (at your option) any later version.                                
%%%
%%%----------------------------------------------------------------------
%%%
%%% See COPYING for detailled license
%%%
%%%----------------------------------------------------------------------

-module(mdb_dispatch).

-author('mickael.remond@erlang-fr.org').
-created('Date: 20010916').
-revision(' $Id$ ').
-vsn(' $Revision$ ').

%% External exports (API)
-export([process_data/3, treat_recv/3, append_botname/2]).

%% -- Includes --
%% Configure debugging mode:
-include("mdb_macros.hrl").

%% Include record description
-include("mdb.hrl").
-include("config.hrl").

-define(BOTNAME, "%BOTNAME").

%%----------------------------------------------------------------------
%% process_data/3
%% Parse the incoming data into lines,
%% and spawn a treat_recv process on each one
%%----------------------------------------------------------------------
process_data(Sock, Data, State=#state{}) -> 
    Pos = string:str(Data, "\r\n"),
    case Pos of 
	0 -> Data ;
	_ ->
	    Line = string:substr( Data, 1, Pos-1 ),
	    Rest = string:substr( Data, Pos+2, string:len(Data) - (Pos-1) ), 

	    %% treat_recv(Sock, list_to_binary(Line), State),
	    proc_lib:spawn(?MODULE, treat_recv,
			   [Sock, list_to_binary(Line), State]),
	    process_data(Sock, Rest, State)
    end;

process_data(Sock, Data, State) ->
    io:format("process_data: ~p ~p ~p ~n", [Sock, Data, State]).

%%----------------------------------------------------------------------
%% treat_recv/3
%% If this is a PING from the server:
%%----------------------------------------------------------------------
treat_recv(Sock, <<$P, $I, $N, $G, $ , Rest/binary>>, State) ->
    %%io:format("PING ~p~n", [binary_to_list(Rest)]),
    irc_lib:pong(Sock, binary_to_list(Rest));

%%----------------------------------------------------------------------
%% treat_recv/3
%% Otherwise:
%%----------------------------------------------------------------------
treat_recv(Sock, Data, State=#state{}) ->
    Result = binary_to_list(Data),

    %% The Parsed_Result is a list of data records.
    Parsed_result = input_to_record(Result),

    %% Get the list of behaviours that match to the current IRC line
    %% And for which the corresponding fun will be executed
    {ok, BList} = config_srv:getBehaviours(State#state.behaviours),
    lists:map(fun(X) -> 
		      io:format("~p~n", [X]),
		      MatchingList =
			  match_event(X, BList, State#state.nickname),
		      io:format("   ~p~n", [MatchingList]),
		      dispatch_message(MatchingList, X, State)
		      end,
	      Parsed_result),

    %%Print what is received
    lists:map(fun(Res) -> ?dbg("HEADER: [~s/~s/~s/~s] - BODY: [~s]", 
    				    [Res#data.header_from,
    				     Res#data.header_op,
				     Res#data.header_to,
				     Res#data.header_options,
				     Res#data.body]) end,
    	      Parsed_result).

%%----------------------------------------------------------------------
%% dispatch_message/3
%% We are executing the behaviour whose pattern is matching with
%% the input from the IRC server
%%----------------------------------------------------------------------
dispatch_message(Behaviours, Input, State = #state{mode=muted}) ->
    lists:map(fun(Behaviour = #behaviour{id = "mute"}) ->
		      {M, F} = Behaviour#behaviour.function,
		      apply(M, F, [Input,
				   State#state.nickname,
				   Behaviour#behaviour.data,
				   State#state.bot_pid,
				   State#state.channel]);
		 (_) ->
		      ?dbg("MUTED", [])
	      end,
	      Behaviours);

dispatch_message(Behaviours, Input, State = #state{}) ->
    lists:map(fun(Behaviour) ->
		      ?dbg("Match= ~p", [Behaviour#behaviour.function]),
		      {M, F} = Behaviour#behaviour.function,
		      apply(M, F, [Input,
				   State#state.nickname,
				   Behaviour#behaviour.data,
				   State#state.bot_pid,
				   State#state.channel])
	      end,
	      Behaviours).

%%----------------------------------------------------------------------
%% input_to_record/1
%% Convert a given input to a list of preparsed data records
%%----------------------------------------------------------------------
input_to_record(ServerData) ->
    Lines = string:tokens(ServerData, "\r\n"),
    parse_lines(Lines, []).

%%----------------------------------------------------------------------
%% parse_lines/2
%% Each input line will be a data record
%%----------------------------------------------------------------------
parse_lines([], Result) ->
    lists:reverse(Result);
parse_lines([Line|Lines], Result) ->
    parse_lines(Lines, [parse_line(Line) | Result]).

%%----------------------------------------------------------------------
%% parse_line/1
%% Each line is split between the data record fields
%%----------------------------------------------------------------------
parse_line([$: | ServerData]) ->
    BodyPos = string:chr(ServerData, $:),

    case BodyPos > 0 of
	true ->
	    Header  = string:substr(ServerData, 1, BodyPos - 1),
	    Body    = string:substr(ServerData, BodyPos + 1),
	    Result  = string:tokens(Header, " "),

	    case length(Result) of
		1 ->
		    Header_from = lists:nth(1, Result),
		    #data{header_from = Header_from,
			  body = Body};

		2 ->
		    Header_from = lists:nth(1, Result),
		    Header_op = lists:nth(2,Result),
		    #data{header_from = Header_from,
			  header_op = Header_op,
			  body = Body};

		Other ->
		    Header_from = lists:nth(1, Result),
		    Header_op = lists:nth(2,Result),
		    Header_to = lists:nth(3, Result),
		    Header_options = lists:flatten(lists:nthtail(3, Result)),

		    #data{header_from = Header_from,
			  header_op = Header_op,
			  header_to = Header_to,
			  header_options = Header_options,
			  body = Body}
	    end;

	false ->
	    [Header_from, Header_op, Header_to | _Rest] =
		string:tokens(ServerData, " "),

	    #data{header_from = Header_from,
		  header_op = Header_op,
		  header_to = Header_to,
		  body = ""}
    end;

%% I think that missing a ping generate a message that fall in this case and
%% crash the process
parse_line(ServerData) ->
    ?dbg("In ParseLine: unidentified: ~p", [ServerData]),
    %% Ignore.
    #data{}.


%%----------------------------------------------------------------------
%% match_event/3
%% Returns the list of behaviour that should be executed on an irc input
%%----------------------------------------------------------------------
match_event(Data, Behaviours, Nickname) ->
    match_event(data_as_list(Data), Behaviours, Nickname, []).

match_event(Data, [], Nickname, Acc) ->
    lists:reverse(Acc);

match_event(Data, [Behaviour|Behaviours], Nickname, Acc) ->
    MatchCritList =
	append_botname(data_as_list(Behaviour#behaviour.pattern), Nickname),

    case is_matching(Data, MatchCritList) of
	true ->
	    match_event(Data, Behaviours, Nickname, [Behaviour|Acc]);
	false ->
	    match_event(Data, Behaviours, Nickname, Acc)
    end.


%%----------------------------------------------------------------------
%% data_as_list/1
%% Convert the data record to a list of values
%%----------------------------------------------------------------------
data_as_list(Data) ->
    DataList = tuple_to_list(Data),
    [RecordName | Rest] = DataList,
    Rest.

%%----------------------------------------------------------------------
%% append_botname/2
%% Convert '%BOTNAME' wherever in the list by its real name
%%----------------------------------------------------------------------
append_botname(List, Botname) ->
    lists:map(fun(Exp = {regexp, '_'}) -> Exp;

		 ({regexp, String}) ->
		      {ok, NewString, _C} =
			  regexp:sub(String, ?BOTNAME, Botname),
		      {regexp, NewString};

		 (Other) ->
		      Other
	      end,
	      List).

%%----------------------------------------------------------------------
%% is_matching/2
%% Check if the first list (data record field values) match the 
%% Criterium
%%----------------------------------------------------------------------
is_matching(Data, Criterium) ->
    is_matching(Data, Criterium, true).

is_matching(_Data, _Criterium, false) ->
    false;

is_matching([],[], Result) ->
    Result;

is_matching([Element|Elements], [Criterium|Criteria], Result) ->
    %% io:format("is_matching: ~p ~p~n", [Criterium, Element]),
    case Criterium of
	'_' ->
	    is_matching(Elements, Criteria, true);

	{regexp, '_'} ->
	    is_matching(Elements, Criteria, true);

	{regexp, Expr} ->
	    is_matching(Elements, Criteria,
			is_matching_regexp(Element, Expr));

	%% Should tag the Criterium value as {exact, Criterium}	    
	Element ->
	    is_matching(Elements, Criteria, true);

	_Other ->
	    is_matching(Elements, Criteria, false)
    end.

%%----------------------------------------------------------------------
%% is_matching_regexp/2
%% Check the match based on a regexp expression
%%----------------------------------------------------------------------
is_matching_regexp(String, Regexp) ->
    %% io:format("is_matching_regexp: ~p ~p~n", [String, Regexp]),
    case regexp:match(misc_tools:downcase(String), Regexp) of
	{match, _Start, _Length} ->  true;
	nomatch                  ->  false;
	{error, Error}           ->  false
    end.
