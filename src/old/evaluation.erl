-module(evaluation).

-export([evaluate_solution/2, next_square/2, car_can_go/2]).

-type bit() :: 0 | 1.
-type gene() :: {bit(), bit(), bit(), bit(), bit(), bit(), bit(), bit(), bit(), bit(), bit(), bit(), bit(), bit(), bit(), bit()}.
-type quarter_gene() :: {bit(), bit(), bit(), bit()}.
-type solution() :: [gene()].
-type data() :: input_data:data().
-type dest() :: north | south | east | west.
-type lane() :: left | right.
-type position() :: {integer(), dest(), lane()}.

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

    UpdatedData = [{{X, InitialDest, Lane}, Dest, false}
                 || {{X, InitialDest,  Lane}, Dest, _Mv} <- NewData, X < 4],

    time_loop(Solution, UpdatedData, Result + Moves).


%% @doc Launches the loop that tries to move all the cars that can be moved
-spec move_cars(gene(), data()) -> {data(), float()}.
move_cars(Lights, InitialData) ->
    move_one_car(InitialData, InitialData, Lights, false, 0).


%% @doc Iterate through all the cars and try to move them if there is space
%% When no car can be moved, the iteration is finished
-spec move_one_car(list(), data(), gene(), boolean(), integer()) ->
                          {data(), integer()}.
move_one_car([], DataList, _Lights, false, Res) ->
    {DataList, Res};

move_one_car([], DataList, Lights, true, Res) ->
    move_one_car(DataList, DataList, Lights, false, Res);

move_one_car([{_Pos, _Dest, true} | Rest], DataList, Lights, Flag, Res) ->
    move_one_car(Rest, DataList, Lights, Flag, Res);

move_one_car([{Position, Dest, false} | Rest], DataList, Lights, Flag, Res) ->
    NewPosition = next_square(Position, Dest),
    case is_taken(NewPosition, DataList) or not car_can_go(NewPosition, Lights) of
        true ->
            move_one_car(Rest, DataList, Lights, Flag, Res);
        false ->
            NewData = lists:keyreplace(Position,
                                       1,
                                       DataList,
                                       {NewPosition, Dest, true}),

            move_one_car(Rest, NewData, Lights, true, Res + 1)
    end.


%% @doc Computes the next position after moving a car
-spec next_square(position(), dest()) -> position().
next_square({X, Direction, Lane}, Dest) ->
    case X of
        -1 ->
            case right_of(Direction) == Dest of
                true ->
                    {3, Dest, right};
                false ->
                    {0, Direction, Lane}
            end;
        1 ->
            case Direction == Dest of
                true ->
                    {2, Dest, Lane};
                false ->
                    {1, left_of(Direction), left}
            end;
        X ->
            {X + 1, Direction, Lane}
    end.


%% @doc Checks if there is a car in a given position
-spec is_taken(position(), data()) -> boolean().
is_taken(Key, DataList) ->
    lists:keymember(Key, 1, DataList) orelse lists:keymember(translate(Key), 1, DataList).


%%TODO: check name to sth like collision_position or similar
-spec translate(position()) -> position().
translate({-1, Dest, right}) ->
    {2, previous_of(Dest), right};

translate({-1, Dest, left}) ->
    {1, previous_of(Dest), right};

translate({0, Dest, right}) ->
    {2, previous_of(Dest), left};

translate({0, Dest, left}) ->
    {1, previous_of(Dest), left};

translate({1, Dest, right}) ->
    {-1, next_of(Dest), left};

translate({1, Dest, left}) ->
    {0, next_of(Dest), left};

translate({2, Dest, right}) ->
    {-1, next_of(Dest), right};

translate({2, Dest, left}) ->
    {0, next_of(Dest), right};

translate({Position, Dest, Lane}) ->
    {Position, Dest, Lane}.


%% @doc Decides if a car can move given the light configuration
-spec car_can_go(position(), gene()) -> boolean().
car_can_go({-1, Dest, Lane}, Lights) ->
    get_light_for(Lights, {-1, Dest, Lane});

car_can_go({-0, Dest, Lane}, Lights) ->
    get_light_for(Lights, {0, Dest, Lane});

car_can_go({1, Dest, Lane}, Lights) ->
    not get_light_for(Lights, translate({1, Dest, Lane}));

car_can_go({2, Dest, Lane}, Lights) ->
    not get_light_for(Lights, translate({2, Dest, Lane}));

car_can_go(_Position, _Lights) ->
    true.


%% @doc Checks if a there is a green light on a given crossing
-spec get_light_for(gene(), position()) -> boolean().
get_light_for(Lights, {Position, Dest, Lane}) ->
    QuarterLights = get_quarter_lights(Dest, Lights),
    get_light_for(QuarterLights, Position, Lane) == 1.


-spec get_quarter_lights(dest(), gene()) -> quarter_gene().
get_quarter_lights(south, Lights) ->
    {erlang:element(1, Lights), erlang:element(2, Lights), erlang:element(3, Lights), erlang:element(4, Lights)};

get_quarter_lights(west, Lights) ->
    {erlang:element(5, Lights), erlang:element(6, Lights), erlang:element(7, Lights), erlang:element(8, Lights)};

get_quarter_lights(north, Lights) ->
    {erlang:element(9, Lights), erlang:element(10, Lights), erlang:element(11, Lights), erlang:element(12, Lights)};

get_quarter_lights(east, Lights) ->
    {erlang:element(13, Lights), erlang:element(14, Lights), erlang:element(15, Lights), erlang:element(16, Lights)}.


-spec get_light_for(quarter_gene(), position(), lane()) -> bit().
get_light_for({_S, _W, _N, E}, -1, left) ->
    E;

get_light_for({_S, _W, N, _E}, -1, right) ->
    N;

get_light_for({S, _W, _N, _E}, 0, left) ->
    S;

get_light_for({_S, W, _N, _E}, 0, right) ->
    W.


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


-spec next_of(dest()) -> dest().
next_of(north) ->
    west;

next_of(west) ->
    south;

next_of(south) ->
    east;

next_of(east) ->
    north.


-spec previous_of(dest()) -> dest().
previous_of(north) ->
    east;

previous_of(east) ->
    south;

previous_of(south) ->
    west;

previous_of(west) ->
    north.