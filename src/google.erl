%%% File    : google.erl
%%% Author  : Nicolas Niclausse <nniclausse@idealx.com>
%%% Purpose : ask google for the first match of a given keyword
%%% Created : 16 Jul 2002 by Nicolas Niclausse <nniclausse@idealx.com>

-module(google).
-author('nniclausse@idealx.com').

-export([search/1]).

search(Keyword) ->
    init({"www.google.com", 80, Keyword}).

do_recv(Socket) ->
    do_recv(Socket, [], []).

do_recv(Socket, Bs, URL) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, B} ->
	    %% search for redirected url
	    case regexp:first_match(B, "<A HREF=\"http://[^\"]+\">here</A>") of
		{match,Start,Length} -> % ok, found
		    do_recv(Socket, [Bs, B],
			    string:substr(B, 10, length(B)-22));
		_ -> 
		    do_recv(Socket, [Bs, B], URL)
	    end;
        {error, einval} -> % non fatal error, continue
            do_recv(Socket, Bs, URL);
        {error, closed} -> 
            {ok, URL}
    end.

init({Server, Port, Keyword}) ->
    case gen_tcp:connect(Server, Port,
			 [list,
			  {packet, line},
			  {active, false}]) of
	{ok, Socket} -> 
	    Request = google_request(Keyword),
	    gen_tcp:send(Socket, Request),
	    {ok, URL} = do_recv(Socket),
	    URL;
	{error, Reason} ->
	    {stop, connfailed}
    end.

google_request(Keyword) ->
    "GET /search?q=" ++ Keyword ++"&hl=fr&btnI=J%27ai+de+la+chance HTTP/1.0"
	++ io_lib:nl() ++ io_lib:nl().
