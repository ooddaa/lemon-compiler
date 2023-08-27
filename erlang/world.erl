-module(hehe).
-export([hello/0]).

-include("world.hrl").

hello() -> ?GREETING.
