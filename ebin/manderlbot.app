%% Manderlbot Application Configuration File
%% -----
%%
{application, manderlbot,
 [
  {description, "Manderlbot"},
  {vsn, "0.8.2"},
  {id, "Manderlbot"},
  {modules, [bloto, config, config_srv, debian, google, irc_lib, manderlbot,
	     manderlbot_sup, mdb_behaviours, mdb_bot, mdb_bot_sup,
	     mdb_botlist, mdb_connection, mdb_dict, mdb_dispatch, mdb_search,
	     misc_tools, pyramid]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, [{root_path, "/etc"}, {config_file, "manderlbot.xml"}]},
  {mod, {manderlbot, []}}
 ]
}.
