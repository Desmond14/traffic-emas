-module(evaluation).

-export([evaluate_solution/2]).

-type bit() :: 0 | 1.
-type gene() :: {bit(), bit(), bit(), bit()}.
-type solution() :: [gene()].
-type data() :: input_data:data().
-type dest() :: north | east | south | west.
-type position() :: {integer(), dest(), dest()}.

-define(LIGHTS, 0).


-spec evaluate_solution(solution(), data()) -> float().
evaluate_solution(Solution, Data) ->
    time_loop(Solution, Data, 0).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Main loop which iterates through the solution genes
-spec time_loop(solution(), data(), float()) -> float().
time_loop([], _, Result) ->
    Result;

time_loop([Lights | Solution], Data, Result) ->
    {NewData, Moves} = move_cars(Lights, Data),
    EraseCars = [{{X, Lane, Dest}, Moved}
                 || {{X, Lane, Dest}, Moved} <- gb_trees:to_list(NewData), X < 1],
    ResetMoves = [{Car, false} || {Car, _Moved} <- EraseCars],
    time_loop(Solution, gb_trees:from_orddict(ResetMoves), Result + Moves).


%% @doc Launches the loop that tries to move all the cars that can be moved
-spec move_cars(gene(), data()) -> {data(), float()}.
move_cars(Lights, InitialData) ->
    move_one_car(gb_trees:to_list(InitialData), InitialData, Lights, false, 0).


%% @doc Iterate through all the cars and try to move them if there is space
%% When no car can be moved, the iteration is finished
-spec move_one_car(list(), data(), gene(), boolean(), integer()) ->
                          {data(), integer()}.
move_one_car([], DataTree, _Lights, false, Res) ->
    {DataTree, Res};

move_one_car([], DataTree, Lights, true, Res) ->
    move_one_car(gb_trees:to_list(DataTree), DataTree, Lights, false, Res);

move_one_car([{_CarPosition, true} | Rest], DataTree, Lights, Flag, Res) ->
    move_one_car(Rest, DataTree, Lights, Flag, Res);

move_one_car([{Position, false} | Rest], DataTree, Lights, Flag, Res) ->
    NewPosition = next_square(Position),
    case is_taken(NewPosition, DataTree) or not car_can_go(Position, Lights) of
        true ->
            move_one_car(Rest, DataTree, Lights, Flag, Res);
        false ->
            RemoveOld = gb_trees:delete(Position, DataTree),
            AddNew = gb_trees:insert(NewPosition, true, RemoveOld),
            move_one_car(Rest, AddNew, Lights, true, Res + 1)
    end.


%% @doc Computes the next position after moving a car
-spec next_square(position()) -> position().
next_square({X, Lane, Dest}) ->
    case X of
        -1 ->
            case right_of(Lane) == Dest of
                true ->
                    {1, Dest, Dest};
                false ->
                    {0, Lane, Dest}
            end;
        0 ->
            case Lane == Dest of
                true ->
                    {1, Dest, Dest};
                false ->
                    {0, left_of(Lane), Dest}
            end;
        X ->
            {X + 1, Lane, Dest}
    end.


%% @doc Checks if there is a car in a given position
-spec is_taken(position(), data()) -> boolean().
is_taken({X, Lane, _Dest}, DataTree) ->
    gb_trees:is_defined({X, Lane, north}, DataTree) or
        gb_trees:is_defined({X, Lane, east}, DataTree) or
        gb_trees:is_defined({X, Lane, south}, DataTree) or
        gb_trees:is_defined({X, Lane, west}, DataTree).


%% @doc Decides if a car can move given the light configuration
-spec car_can_go(position(), gene()) -> boolean().
car_can_go({-1, Lane, _Dest}, Lights) ->
    LightDict = get_light_dictionary(Lights),
    dict:fetch(Lane, LightDict);

car_can_go({0, Lane, _Dest}, Lights) ->
    LightDict = get_light_dictionary(Lights),
    LeftLane = left_of(Lane),
    not dict:fetch(LeftLane, LightDict);

car_can_go(_Position, _Lights) ->
    true.


%% @doc Creates a dictionary for a light configuration
-spec get_light_dictionary(gene()) -> dict:dict().
get_light_dictionary({N, E, S, W}) ->
    List = [{north, N == 1}, {east, E == 1}, {south, S == 1}, {west, W == 1}],
    dict:from_list(List).


%% @doc Returns the lane to the right of the parameter
-spec right_of(dest()) -> dest().
right_of(north) ->
    east;

right_of(east) ->
    south;

right_of(south) ->
    west;

right_of(west) ->
    north.


%% @doc Returns the lane to the left of the parameter
-spec left_of(dest()) -> dest().
left_of(north) ->
    west;

left_of(west) ->
    south;

left_of(south) ->
    east;

left_of(east) ->
    north.