%%%----------------------------------------------------------------------
%%% File    : config.hrl
%%% Author  : Dimitri Fontaine <tapoueh@free.fr>
%%% Purpose : 
%%% Created : 19 Feb 2002 by Dimitri Fontaine <tapoueh@free.fr>
%%%----------------------------------------------------------------------

-author('tapoueh@free.fr').


-record(config,	{name,                          % the name of the bot
		 controler,                     % the nick of the one wich
						% controls the bot from irc
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
		       data = []
		      }).


