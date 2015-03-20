-module(input_data).

%% API
-export([load_data/0]).

-type data() :: {[tuple()], [tuple()], [tuple()], [tuple()]}.


-spec load_data() -> data().
load_data() ->
    set1().

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec set1() -> data().
set1() ->
    North = [],
    East = [{0, south}, {2, north}],
    South = [{0, south}],
    West = [{1, west}],
    Lanes = [add_moved_field(Lane) || Lane <- [North, East, South, West]],
    list_to_tuple(Lanes).


%% @doc Add a moved field with a false value to each vehicle
-spec add_moved_field([tuple()]) -> [tuple()].
add_moved_field(Lane) ->
    [{X, Dest, false} || {X, Dest} <- Lane].