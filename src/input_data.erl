-module(input_data).

%% API
-export([load_data/0]).

-type data() :: gb_trees:tree({integer(), atom(), atom()}, boolean()).
-type density() :: dense | normal | sparse.

-define(TOTAL_CARS, 16).

-spec load_data() -> data().
load_data() ->
    random_set(normal).

%% =============================================================================
%% Internal functions
%% =============================================================================

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
    CarList = [{{X, MapLane(Lane)}, {MapLane(random:uniform(4)), false}}
               || {X, Lane} <- sets:to_list(Positions)],
    Sorted = lists:sort(CarList),
    gb_trees:from_orddict(Sorted).


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