% up until final pass (Native code), 
% compiler didn't mind discrepancies 
% between module name and file name
% -module(hehe). 
-module(world). 
-export([hello/0]).

-include("world.hrl").

hello() -> ?GREETING.
