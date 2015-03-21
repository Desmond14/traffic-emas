-module(input_data).

%% API
-export([load_data/0]).

-type data() :: dict:dict().

-define(BOARD_SIZE, 20).
-define(TOTAL_CARS, 20).

-spec load_data() -> data().
load_data() ->
    set3().

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec set1() -> data().
set1() ->
    North = [],
    East = [{-2, south}, {-1, north}],
    South = [],
    West = [],
    pack_set(North, East, South, West).


set2() ->
    North = [{-2, south}],
    East = [],
    South = [],
    West = [],
    pack_set(North, East, South, West).

set3() ->
    North = [{0, east}, {-2, south}, {-1, east}],
    East = [{-4, north}, {-1, south}],
    South = [{0, east}, {-3, east}, {-2, north}, {-1, south}],
    West = [{-2, east}, {-5, east}],
    pack_set(North, East, South, West).

set4() ->
    North = [{-3, east}, {-2, south}, {-1, east}],
    East = [{-2, north}, {-1, south}],
    South = [{-3, east}, {-2, north}, {-1, south}],
    West = [{-2, south}],
    pack_set(North, East, South, West).

-spec pack_set([tuple()], [tuple()], [tuple()], [tuple()]) -> data().
pack_set(North, East, South, West) ->
    NTagged = [{X, north, Dest} || {X, Dest} <- North],
    ETagged = [{X, east, Dest} || {X, Dest} <- East],
    STagged = [{X, south, Dest} || {X, Dest} <- South],
    WTagged = [{X, west, Dest} || {X, Dest} <- West],
    All = NTagged ++ ETagged ++ STagged ++ WTagged,
    MoveField = [{{X, Lane, Dest}, false} || {X, Lane, Dest} <- All],
    dict:from_list(MoveField).
