-module(input_data).

%% API
-export([load_data/0]).

-type data() :: dict:dict().
-type density() :: dense | normal | sparse.

-define(TOTAL_CARS, 200).

-spec load_data() -> data().
load_data() ->
    random_set(normal).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Predefined hardcoded example datasets
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
    pack_set(North, East, South, West);

predefined_set(_) ->
    random_set(normal).


%% @doc Returns a random car configuration with a given density
%% Uses macro ?TOTAL_CARS to determine the total number of cars to generate
-spec random_set(density()) ->dict:dict().
random_set(TrafficDensity) ->
    PerLane = ?TOTAL_CARS div 4,
    MaxDist = case TrafficDensity of
                  dense ->
                      PerLane * 2;
                  normal ->
                      PerLane * 3;
                  sparse ->
                      PerLane * 5
              end,
    Positions = lists:foldl(fun(_, Cars) ->
                                    random_position(MaxDist, Cars)
                            end, sets:new(), lists:seq(1, ?TOTAL_CARS)),
    MapDict = dict:from_list([{1, north}, {2, east}, {3, south}, {4, west}]),
    MapLane = fun(Key) ->
                      dict:fetch(Key, MapDict)
              end,
    CarList = [{{X, MapLane(Lane), MapLane(random:uniform(4))}, false}
               || {X, Lane} <- sets:to_list(Positions)],
    dict:from_list(CarList).


%% @doc Adds a new unique random car position to a set
-spec random_position(pos_integer(), sets:set()) -> sets:set().
random_position(MaxDist, CarSet) ->
    RandomPosition = {(-1) * random:uniform(MaxDist), random:uniform(4)},
    case sets:is_element(RandomPosition, CarSet) of
        true ->
            random_position(MaxDist, CarSet);
        false ->
            sets:add_element(RandomPosition, CarSet)
    end.


%% @doc Packs lists with position into a proper data structure
-spec pack_set([tuple()], [tuple()], [tuple()], [tuple()]) -> data().
pack_set(North, East, South, West) ->
    NTagged = [{X, north, Dest} || {X, Dest} <- North],
    ETagged = [{X, east, Dest} || {X, Dest} <- East],
    STagged = [{X, south, Dest} || {X, Dest} <- South],
    WTagged = [{X, west, Dest} || {X, Dest} <- West],
    All = NTagged ++ ETagged ++ STagged ++ WTagged,
    MoveField = [{{X, Lane, Dest}, false} || {X, Lane, Dest} <- All],
    dict:from_list(MoveField).
