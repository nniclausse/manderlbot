%%
-define(EMERG, 0). % The system is unusable. 
-define(ALERT, 1). % Action should be taken immediately to address the problem.
-define(CRIT,  2). % A critical condition has occurred. 
-define(ERR,   3). % An error has occurred. 
-define(WARN,  4). % A significant event that may require attention has occurred. 
-define(NOTICE,5). % An event that does not affect system operation has occurred. 
-define(INFO,  6). % An normal operation has occurred. 
-define(DEBUG, 7). % Debugging info