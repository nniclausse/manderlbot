%% Manderlbot Application Configuration File
%% -----
%%
{application, manderlbot,
 [
  {description, "Manderlbot"},
  {vsn, "0.9.0"},
  {id, "Manderlbot"},
  {modules, [config, config_srv, debian, irc_lib, make_boot, manderlbot,
	     manderlbot_sup, mdb_bhv_action, mdb_bhv_answer, mdb_bhv_bloto,
	     mdb_bhv_debian_file, mdb_bhv_debian_pkg, mdb_bhv_dict,
	     mdb_bhv_google, mdb_bhv_mute, mdb_bhv_pyramid, mdb_bhv_random,
	     mdb_bhv_reconf, mdb_bhv_rejoin, mdb_bhv_say, mdb_bhv_think,
	     mdb_bhv_timer, mdb_bot, mdb_bot_sup, mdb_botlist,
	     mdb_connection, mdb_dispatch, mdb_search, misc_tools]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, [{root_path, "/etc"}, {config_file, "manderlbot.xml"}]},
  {mod, {manderlbot, []}}
 ]
}.
