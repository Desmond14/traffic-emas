-module(evaluation).

-export([evaluate_solution/2]).

-type bit() :: 0 | 1.
-type gene() :: {bit(), bit(), bit(), bit()}.
-type solution() :: [gene()].
-type data() :: input_data:data().
-type dest() :: north | east | south | west.

-define(LIGHTS, 0).


-spec evaluate_solution(solution(), data()) -> float().
evaluate_solution(Solution, InitETS) ->
    Name = list_to_atom(pid_to_list(self())),
    MyETS = ets:new(Name, [set, public]),
    Data = ets:tab2list(InitETS),
    ets:insert(MyETS, Data),
    Fitness = time_loop(Solution, MyETS, 0),
    ets:delete(MyETS),
    Fitness.

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Main loop which iterates through the solution genes
-spec time_loop(solution(), data(), float()) -> float().
time_loop([], _, Result) ->
    Result;

time_loop([Lights | Solution], ETS, Result) ->
    Moves = move_cars(Lights, ETS),
    ets:match_delete(ETS, [{ {{'$1', '_'}, '_', '_'}, [{'<', '$1', 1}], [] }]),
    MovedCars = ets:match(ETS, {'$1', '_', true}),
    [ets:update_element(ETS, Key, {3, false})
        || Key <- lists:flatten(MovedCars)],
    time_loop(Solution, ETS, Result + Moves).


%% @doc Launches the loop that tries to move all the cars that can be moved
-spec move_cars(gene(), data()) -> {data(), float()}.
move_cars(Lights, ETS) ->
    move_one_car(ets:tab2list(ETS), ETS, Lights, false, 0).


%% @doc Iterate through all the cars and try to move them if there is space
%% When no car can be moved, the iteration is finished
-spec move_one_car(list(), data(), gene(), boolean(), integer()) ->
                          {data(), integer()}.
move_one_car([], _ETS, _Lights, false, Res) ->
    Res;

move_one_car([], ETS, Lights, true, Res) ->
    move_one_car(ets:tab2list(ETS), ETS, Lights, false, Res);

move_one_car([{_CarPosition, _Dest, true} | Rest], ETS, Lights, Flag, Res) ->
    move_one_car(Rest, ETS, Lights, Flag, Res);

move_one_car([{{X, Lane}, Dest, false} | Rest], ETS, Lights, Flag, Res) ->
    NewPosition = next_square(X, Lane, Dest),

    case is_taken(NewPosition, ETS) or not car_can_go(X, Lane, Lights) of
        true ->
            move_one_car(Rest, ETS, Lights, Flag, Res);
        false ->
            ets:delete(ETS, {X, Lane}),
            ets:insert(ETS, {NewPosition, Dest, true}),
            move_one_car(Rest, ETS, Lights, true, Res + 1)
    end.


%% @doc Computes the next position after moving a car
-spec next_square(integer(), dest(), dest()) -> {integer(), dest()}.
next_square(X, Lane, Dest) ->
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
-spec is_taken({integer(), dest()}, data()) -> boolean().
is_taken({X, Lane}, ETS) ->
    ets:member(ETS, {X, Lane}).


%% @doc Decides if a car can move given the light configuration
-spec car_can_go(integer(), dest(), gene()) -> boolean().
car_can_go(-1, Lane, Lights) ->
    LightDict = get_light_dictionary(Lights),
    dict:fetch(Lane, LightDict);

car_can_go(0, Lane, Lights) ->
    LightDict = get_light_dictionary(Lights),
    LeftLane = left_of(Lane),
    not dict:fetch(LeftLane, LightDict);

car_can_go(_X, _Lane, _Lights) ->
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