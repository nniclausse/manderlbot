%%%----------------------------------------------------------------------
%%% File    : config.hrl
%%% Author  : Dimitri Fontaine <dim@tuxfamily.org>
%%% Purpose : Define some config element as erlang structures
%%% Created : 19 Feb 2002 by Dimitri Fontaine <dim@tuxfamily.org>
%%%----------------------------------------------------------------------

-author('dim@tuxfamily.org').


-record(config,	{name,                          % the name of the bot
		 controler,                     % the nick of the one wich
						% controls the bot from irc

		 dict={"localhost", "2628", "wn"},
		 servers=[],
		 behaviours=[]
		}).

-record(server, {host,
		 port,
		 channels = []
		}).

-record(channel, {name,
		  botname,
		  behaviours = []
		 }).
		  

-record(cfg_behaviour, {name,
			action,
			pattern,
			from,
			to,
		       data = []
		      }).


