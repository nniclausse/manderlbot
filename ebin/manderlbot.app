%% Manderlbot Application Configuration File
%% -----
%%
{application, manderlbot,
 [
  {description, "Manderlbot"},
  {vsn, "0.7"},
  {id, "Manderlbot"},
  {modules, [bloto, config, config_srv, debian, google, irc_lib, manderlbot,
	     manderlbot_sup, mdb_behaviours, mdb_bot, mdb_bot_sup, mdb_botlist,
	     mdb_connection, mdb_dict, mdb_dispatch, mdb_search, misc_tools,
	     pyramid]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, [{root_path, "/usr/lib/erlang/lib/manderlbot-0.7"},
	 {config_file, "config.xml"}]},
  {mod, {manderlbot, []}}
 ]
}.
