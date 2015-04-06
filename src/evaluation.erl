-module(evaluation).

-export([evaluate_solution/2]).

-type bit() :: 0 | 1.
-type gene() :: {bit(), bit(), bit(), bit()}.
-type solution() :: [gene()].
-type data() :: input_data:data().
-type dest() :: north | east | south | west.
-type position() :: {integer(), dest()}.

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
    UpdatedData = [{{X, Lane}, {Dest, false}}
                   || {{X, Lane}, {Dest, _Mv}} <- gb_trees:to_list(NewData),
                      X < 1],
    time_loop(Solution, gb_trees:from_orddict(UpdatedData), Result + Moves).


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

move_one_car([{_Pos, {_Dest, true}} | Rest], DataTree, Lights, Flag, Res) ->
    move_one_car(Rest, DataTree, Lights, Flag, Res);

move_one_car([{Position, {Dest, false}} | Rest], DataTree, Lights, Flag, Res) ->
    NewPosition = next_square(Position, Dest),
    case is_taken(NewPosition, DataTree) or not car_can_go(Position, Lights) of
        true ->
            move_one_car(Rest, DataTree, Lights, Flag, Res);
        false ->
            RemoveOld = gb_trees:delete(Position, DataTree),
            AddNew = gb_trees:insert(NewPosition, {Dest, true}, RemoveOld),
            move_one_car(Rest, AddNew, Lights, true, Res + 1)
    end.


%% @doc Computes the next position after moving a car
-spec next_square(position(), dest()) -> position().
next_square({X, Lane}, Dest) ->
    case X of
        -1 ->
            case right_of(Lane) == Dest of
                true ->
                    {1, Dest};
                false ->
                    {0, Lane}
            end;
        0 ->
            case Lane == Dest of
                true ->
                    {1, Dest};
                false ->
                    {0, left_of(Lane)}
            end;
        X ->
            {X + 1, Lane}
    end.


%% @doc Checks if there is a car in a given position
-spec is_taken(position(), data()) -> boolean().
is_taken({X, Lane}, DataTree) ->
    gb_trees:is_defined({X, Lane}, DataTree).


%% @doc Decides if a car can move given the light configuration
-spec car_can_go(position(), gene()) -> boolean().
car_can_go({-1, Lane}, Lights) ->
    get_light_for(Lights, Lane);

car_can_go({0, Lane}, Lights) ->
    LeftLane = left_of(Lane),
    not get_light_for(Lights, LeftLane);

car_can_go(_Position, _Lights) ->
    true.


%% @doc Checks if a there is a green light on a given crossing
-spec get_light_for(gene(), dest()) -> boolean().
get_light_for({N, _E, _S, _W}, north) ->
    N == 1;

get_light_for({_N, E, _S, _W}, east) ->
    E == 1;

get_light_for({_N, _E, S, _W}, south) ->
    S == 1;

get_light_for({_N, _E, _S, W}, west) ->
    W == 1.


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