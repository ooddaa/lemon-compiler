% https://blog.stenmans.org/theBeamBook/#SEC-parse_transform
-module(json_test).
-compile({parse_transform, json_parser}).
-export([test/1]).

test(V) -> 
  <<{{
      "name"    : "Opiate",
      "format"  : {
        "type"  : "EP",
        "author": "Tool",
        "length": [0, 26, 52],
        "songs" : (-6),
        "year"  : 1992,
        "rocks" : true,
        "var"   : V
        }  
    }}>>.