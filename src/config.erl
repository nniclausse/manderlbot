%%%----------------------------------------------------------------------
%%% File    : config.erl
%%% Author  : Dimitri Fontaine <tapoueh@free.fr>
%%% Purpose : Read the manderlbot XML xonfig file
%%% Created : 19 Feb 2002 by Dimitri Fontaine <tapoueh@free.fr>
%%%----------------------------------------------------------------------

-module(config).
-author('tapoueh@free.fr').

-include("config.hrl").
-include("xmerl.hrl").

-export([read/1]).

%%%----------------------------------------------------------------------
%%% Function: read/1
%%% Purpose:  read the xml config file
%%%----------------------------------------------------------------------
read(Filename) ->
    case xmerl_scan:file(Filename) of
	{ok, Root = #xmlElement{}} ->
	    %% io:format("root: ~p~n~p~n", [Root#xmlElement.name,
		%%			 Root#xmlElement.content]),
	    {ok, parse(Root, #config{})};

	_Error ->
	    {error, config}
    end.


%%%----------------------------------------------------------------------
%%% Function: parse/2
%%% Purpose:  parse the xmerl structure
%%%----------------------------------------------------------------------
parse(Element = #xmlElement{parents = []}, #config{}) ->
    Name = getAttr(Element#xmlElement.attributes, name),
    Controler = getAttr(Element#xmlElement.attributes, controler),
	
    lists:foldl(fun parse/2,
		#config{name = Name, controler = Controler},
		Element#xmlElement.content);


%% parsing the Server elements
parse(Element = #xmlElement{name=server}, Conf = #config{servers=SList}) ->
    Server = getAttr(Element#xmlElement.attributes, host),
    Port   = getAttr(Element#xmlElement.attributes, port),

    {ok, [{integer,1,IPort}],1} = erl_scan:string(Port),

    lists:foldl(fun parse/2,
		Conf#config{servers = [#server{host=Server,
					       port=IPort
					      }|SList]},
		Element#xmlElement.content);


%% Parsing the Channel element
parse(Element = #xmlElement{name=channel},
      Conf = #config{servers=[CurServ|SList]}) ->

    ChanList = CurServ#server.channels,
    Chan     = getAttr(Element#xmlElement.attributes, name),
    Bot      = getAttr(Element#xmlElement.attributes, botname),
    B        = string:tokens(
		 getAttr(Element#xmlElement.attributes, behaviours), ", "),

    lists:foldl(fun parse/2,
		Conf#config{servers = [CurServ#server{channels =
						      [#channel{name=Chan,
								botname=Bot,
								behaviours=B}
						       |ChanList]}
				       |SList]},
		Element#xmlElement.content);


%% Parsing the behaviour element
parse(Element = #xmlElement{name=behaviour},
      Conf = #config{servers=[CurServ|SList], behaviours=BList}) ->

    [CurChan|ChanList] = CurServ#server.channels,

    Name    = getAttr(Element#xmlElement.attributes, name),
    Pattern = getAttr(Element#xmlElement.attributes, pattern),
    Action  = getAttr(Element#xmlElement.attributes, action),
    Data    = getText(Element#xmlElement.content),

    lists:foldl(fun parse/2,
		Conf#config{behaviours =
			    [#cfg_behaviour{name=Name, pattern = Pattern,
					    action=Action, data=Data}
			     | BList]},
		Element#xmlElement.content);


%% Parsing other elements
parse(Element = #xmlElement{}, Conf = #config{}) ->
    lists:foldl(fun parse/2, Conf, Element#xmlElement.content);

%% Parsing non #xmlElement elements
parse(Element, Conf = #config{}) ->
    Conf.


%%%----------------------------------------------------------------------
%%% Function: getAttr/2
%%% Purpose:  search the attibute list for the given one
%%%----------------------------------------------------------------------
getAttr([Attr = #xmlAttribute{name=Name}|Tail], Name) ->
    Attr#xmlAttribute.value;

getAttr([H|T], Name) ->
    getAttr(T, Name);

getAttr([], Name) ->
    "".


%%%----------------------------------------------------------------------
%%% Function: getText/1
%%% Purpose:  get the text of the XML node
%%%----------------------------------------------------------------------
getText([Text = #xmlText{value=Value}|Tail]) -> build_list(
						  string:strip(Value, both));
getText(_Other)                              -> "".



build_list(String) ->
    %% Separator is '%'
    string:tokens(String, "%").
