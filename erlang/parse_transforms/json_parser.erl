-module(json_parser).
-export([parse_transform/2]).

parse_transform(AST, _Options) -> 
  % io:format("~p~n", [AST]),
  % AST.
  json(AST, []).

-define(FUNCTION(Clauses), {function, Name, Arity, Label, Clauses}).

%% this should access code inside of functions
json([?FUNCTION(Clauses)| Elements], Res) -> 
  json(Elements, [?FUNCTION(json_clauses(Clauses)) | Res]);
json([Other|Elements], Res) -> json(Elements, [Other | Res]);
json([], Res) -> lists:reverse(Res).  

%% this should get the body of function == Json markup <<{ JSON }>>
%% {clause,
%%    {6,1},
%%    [{var,{6,6},'V'}],
%%    [],
%%    [JSON] % code here
%% }
json_clauses([{clause, Cline, A1, A2, Code} | Clauses]) -> 
  [{clause, Cline, A1, A2, json_code(Code)} | json_clauses(Clauses)];
json_clauses([]) -> [].

%% {bin,
%%    {7,3},
%%    [{bin_element,
%%        {7,5},
%%        {tuple,
%%         {7,5},
%%         [{tuple,
%%           {7,6},
%%           [{remote,{8,17},{string,{8,7},"name"},{string,{8,19},"Opiate"}},
-define(JSON(Json), {bin, _, [{bin_element
                                          , _
                                          , {tuple, _ , [Json]}
                                          , _
                                          , _ }]}).

%% Look for <<"Json">> 
json_code([])                       -> [];
json_code([?JSON(Json)| MoreCode])  -> [parse_json(Json) | json_code(MoreCode)];
json_code(Code)                     -> Code.

%% Json object [{}] | {Label, Term}
parse_json({tuple, Line, []})       ->  {cons, Line, {tuple, Line, []}};
parse_json({tuple, Line, Fields})   ->  parse_json_fields(Fields, Line);

%% Json Array -> List
%% {cons,
%%   {12,19},
%%   {integer,{12,20},0},
%%   {cons,
%%     {12,23},
%%     {integer,{12,23},26},
%%     {cons,{12,27},{integer,{12,27},52},{nil,{12,29}}}}}
%% Array -> List
parse_json({cons, Line, Head, Tail}) -> {cons, Line, parse_json(Head), parse_json(Tail)};
parse_json({nil, Line}) -> {nil, Line};

%% Json String ->  <<String>>
parse_json({string, Line, String}) -> str_to_bin(String, Line);

%% Json Integer -> Integer
parse_json({integer, Line, Integer}) -> {integer, Line, Integer};

%% Json Float -> Float
parse_json({float, Line, Float}) -> {float, Line, Float};

% true|false|null
parse_json({atom, Line, true}) -> {atom, Line, true};
parse_json({atom, Line, false}) -> {atom, Line, false};
parse_json({atom, Line, null}) -> {atom, Line, null};

%% Variables should contain Erlang encoded Json
%% {var,{16,19},'V'}
parse_json({var, Line, Var}) -> {var, Line, Var};

%% Json negative integers/floats
%% {op,{13,20},'-',{integer,{13,21},6}}
parse_json({op, Line, '-', {Type, _, N}}) when Type =:= integer 
                                             ; Type =:= float -> 
                                          {Type, Line, -N}.

%% parse_json(Code) -> io:format("Code: ~p~n", [Code]), Code.

%% parse Json keys aka fields 
%% {remote,
%%  {13,17},
%%  {string,{13,9},"songs"},
%%  {op,{13,20},'-',{integer,{13,21},6}}},
-define(FIELD(Label, Code), {remote, Line, {string, _, Label}, Code}).
parse_json_fields([], Line) -> {nil, Line};
parse_json_fields([?FIELD(Label, Code) | Rest], _) -> 
  cons(tuple(str_to_bin(Label, Line), parse_json(Code), Line), parse_json_fields(Rest, Line), Line).

tuple(Label, Code, Line) -> {tuple, Line, [Label, Code]}.
cons(Head, Tail, Line) -> {cons, Line, Head, Tail}.

str_to_bin(String, Line) -> 
  {bin
    , Line 
    , [{bin_element
        , Line
        , {string, Line, String}
        , default
        , default
       }
      ]
  }.