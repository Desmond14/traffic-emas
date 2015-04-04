-module(evaluation_test).

-include_lib("eunit/include/eunit.hrl").

-type data() :: input_data:data().

%% @doc Unit tests for `evaluation:evaluate_solution/2` function
evaluation1_test() ->
    Fitness = evaluate_example(1, 1),
    ?assertEqual(Fitness, 5).

evaluation2_test() ->
    Fitness = evaluate_example(1, 3),
    ?assertEqual(Fitness, 21).

evaluation3_test() ->
    Fitness = evaluate_example(1, 4),
    ?assertEqual(Fitness, 17).

evaluation4_test() ->
    Fitness = evaluate_example(2, 2),
    ?assertEqual(Fitness, 1).

evaluation5_test() ->
    Fitness = evaluate_example(2, 3),
    ?assertEqual(Fitness, 11).

evaluation6_test() ->
    Fitness = evaluate_example(3, 4),
    ?assertEqual(Fitness, 16).

evaluation7_test() ->
    Fitness = evaluate_example(4, 3),
    ?assertEqual(Fitness, 13).

evaluation8_test() ->
    Fitness = evaluate_example(5, 1),
    ?assertEqual(Fitness, 5).

evaluation9_test() ->
    Fitness = evaluate_example(5, 4),
    ?assertEqual(Fitness, 24).

evaluation10_test() ->
    Fitness = evaluate_example(3, 4),
    ?assertEqual(Fitness, 16).

evaluation11_test() ->
    Fitness = evaluate_example(6, 1),
    ?assertEqual(Fitness, 3).

evaluation12_test() ->
    Fitness = evaluate_example(6, 2),
    ?assertEqual(Fitness, 4).

evaluation13_test() ->
    Fitness = evaluate_example(6, 3),
    ?assertEqual(Fitness, 13).


%% @doc Calculate fitness for a specific combination of dataset and solution
-spec evaluate_example(pos_integer(), pos_integer()) -> float().
evaluate_example(Sol, D) ->
    Solution = solution(Sol),
    Data = predefined_set(D),
    evaluation:evaluate_solution(Solution, Data).


%% @doc Example solutions
-spec solution(pos_integer()) -> proplists:proplist().
solution(1) ->
    [{0,1,0,1},{0,1,0,1},
     {1,0,1,0},{1,0,1,0},
     {0,1,0,1},{0,1,0,1},
     {1,0,1,0},{1,0,1,0}];

solution(2) ->
    [{0,1,0,1},{0,1,0,1},
     {0,1,0,1},{0,1,0,1},
     {0,1,0,1},{0,1,0,1},
     {0,1,0,1},{0,1,0,1}];

solution(3) ->
    [{1,0,1,0},{1,0,1,0},
     {1,0,1,0},{1,0,1,0},
     {1,0,1,0},{1,0,1,0},
     {1,0,1,0},{1,0,1,0}];

solution(4) ->
    [{0,1,0,1},{0,1,0,1},
     {0,1,0,1},{0,1,0,1},
     {0,1,0,1},{0,1,0,1},
     {1,0,1,0},{1,0,1,0}];

solution(5) ->
    [{0,1,0,1},{0,1,0,1},
     {0,1,0,1},{0,1,0,1},
     {1,0,1,0},{1,0,1,0},
     {1,0,1,0},{1,0,1,0}];

solution(6) ->
    [{1,1,0,0},{1,1,0,0},
     {1,1,0,0},{1,1,0,0},
     {1,1,0,0},{1,1,0,0},
     {1,1,0,0},{1,1,0,0}].


%% @doc Example initial datasets
-spec predefined_set(pos_integer()) -> data().
predefined_set(1) ->
    North = [],
    East = [{-2, south}, {-1, north}],
    South = [],
    West = [],
    pack_set(North, East, South, West);

predefined_set(2) ->
    North = [{-2, south}],
    East = [],
    South = [],
    West = [],
    pack_set(North, East, South, West);

predefined_set(3) ->
    North = [{0, east}, {-2, south}, {-1, east}],
    East = [{-4, north}, {-1, south}],
    South = [{0, east}, {-3, east}, {-2, north}, {-1, south}],
    West = [{-2, east}, {-5, east}],
    pack_set(North, East, South, West);

predefined_set(4) ->
    North = [{-3, east}, {-2, south}, {-1, east}],
    East = [{-2, north}, {-1, south}],
    South = [{-3, east}, {-2, north}, {-1, south}],
    West = [{-2, south}],
    pack_set(North, East, South, West).


%% @doc Packs lists with position into a proper data structure
-spec pack_set([tuple()], [tuple()], [tuple()], [tuple()]) -> data().
pack_set(North, East, South, West) ->
    NTagged = [{X, north, Dest} || {X, Dest} <- North],
    ETagged = [{X, east, Dest} || {X, Dest} <- East],
    STagged = [{X, south, Dest} || {X, Dest} <- South],
    WTagged = [{X, west, Dest} || {X, Dest} <- West],
    All = NTagged ++ ETagged ++ STagged ++ WTagged,
    MoveField = [{{X, Lane}, {Dest, false}} || {X, Lane, Dest} <- All],
    Sorted = lists:sort(MoveField),
    gb_trees:from_orddict(Sorted).