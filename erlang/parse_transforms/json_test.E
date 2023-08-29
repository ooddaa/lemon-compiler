-file("json_test.erl", 1).

-module(json_test).

-export([test/1]).

test(V) ->
    [{<<"name">>, <<"Opiate">>},
     {<<"format">>,
      [{<<"type">>, <<"EP">>},
       {<<"author">>, <<"Tool">>},
       {<<"length">>, [0, 26, 52]},
       {<<"songs">>, -6},
       {<<"year">>, 1992},
       {<<"rocks">>, true},
       {<<"var">>, V}]}].



