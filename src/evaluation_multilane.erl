-module(evaluation_multilane).

%% API
-export([evaluate_solution/2, calculate_dist_to_semaphore/4, calculate_dist_to_car_ahead/2]).

-type trit() :: 0 | 1 | 2.
-type gene() :: map().
-type solution() :: [gene()].
-type intersection() :: input:intersection().
-type data() :: input:data().

-type car() :: input:car().

-define(MAX_INT, 134217727).

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
  {UpdatedData, Moves} = move_cars(Lights, Data),
  time_loop(Solution, UpdatedData, Result + Moves).

%% @doc Launches the loop that tries to move all the cars that can be moved
-spec move_cars(gene(), data()) -> {data(), float()}.
move_cars(Lights, InitialData) ->
  {Intersection, Cars} = InitialData,
  move_one_car(Intersection, Intersection, Cars, Cars, Lights, 0).


%% @doc Iterate through all the cars and try to move them if there is space
%% When no car can be moved, the iteration is finished
-spec move_one_car(intersection(), intersection(), [car()], [car()], gene(), integer()) -> {data(), integer()}.
move_one_car(_InitialIntersection, UpdatedIntersection, [], UpdatedCars, _Lights, Distance) ->
  {{UpdatedIntersection, UpdatedCars}, Distance};

move_one_car(InitialIntersection, UpdatedIntersection, CarsToProcess, UpdatedCars, Lights, Distance) ->
  [Car | _] = CarsToProcess,
  DistToSemaphore = calculate_dist_to_semaphore(InitialIntersection, maps:get(position,Car), maps:get(path_to_dest,Car), 0),
  %%DistToCarAhead = calculateDistToCarAhead(InitialIntersection, maps:get(position,Car), maps:get(path_to_dest,Car)),
  {dupa, dupa}.

%% We moved outside intersection and didn't encounter semaphore
calculate_dist_to_semaphore(Intersection, CurrentPosition, [], Result) ->
  CurrentNode = maps:get(maps:get(node_id, CurrentPosition), Intersection),
  io:format("Current Node: ~p~n", [maps:get(id, CurrentNode)]),
  case maps:get(type, CurrentNode) of
    semaphore -> Result;
    lane -> ?MAX_INT %max integer
  end;

calculate_dist_to_semaphore(Intersection, CurrentPosition, PathToDest, Result) ->
  CurrentNode = maps:get(maps:get(node_id, CurrentPosition), Intersection),
  io:format("Current Node: ~p~n", [maps:get(id, CurrentNode)]),
  case maps:get(type, CurrentNode) of
    semaphore -> Result;
    lane ->
      [NextLaneId | _] = PathToDest,
      PathToNextLane = maps:get(NextLaneId, maps:get(next_lane_paths, CurrentNode)),
      [NextNodeId| _] = PathToNextLane,
      PositionOnCurrentNode = maps:get(position_on_node, CurrentPosition),
      CurrentNodeLength = maps:get(length, CurrentNode),
      calculate_dist_to_semaphore(Intersection, #{node_id=>NextNodeId, position_on_node=>1}, tl(PathToDest), Result + CurrentNodeLength - PositionOnCurrentNode+1)
  end.


%% TODO: move to separate module
-spec calculate_dist_to_car_ahead(intersection(), car()) -> non_neg_integer().
calculate_dist_to_car_ahead(InitialIntersection, Car) ->
  CarsOnInitialPosition = intersection:get_cars_on(maps:get(position, Car), InitialIntersection),
  if
    (length(CarsOnInitialPosition)>1) ->
      0;
    true ->
      #{path_to_dest:=PathToDest, position:=InitialPosition, id:=CarId} = Car,
      FullPathToDest = intersection:get_path_with_semaphores(maps:get(node_id, InitialPosition), PathToDest, InitialIntersection),
      {NextPosition, RemainingPath} = intersection:next_position(InitialPosition, FullPathToDest, 1, InitialIntersection),
      case {NextPosition, RemainingPath} of
        {#{}, []} -> ?MAX_INT;
        _ -> calculate_dist_to_car_ahead(InitialIntersection, Car, NextPosition, RemainingPath, 1)
      end
  end.

%%calculate_dist_to_car_ahead(_Intersection, _Car, #{}, [], _Result) ->
%%  TODO: problem when car
%%  ?MAX_INT;

calculate_dist_to_car_ahead(Intersection, Car, CurrentPosition, PathToDest, Result) ->
  CurrentNode = maps:get(maps:get(node_id, CurrentPosition), Intersection),
  PositionOnNode = maps:get(position_on_node, CurrentPosition),
  CarsOnLane = maps:get(cars_on, CurrentNode),
  CarsOnPosition = maps:get(PositionOnNode, CarsOnLane, []),
  IsExaminedCarOnPosition = lists:member(maps:get(id, Car), CarsOnPosition),
  if
    length(CarsOnPosition) == 0 ->
      case intersection:next_position(CurrentPosition, PathToDest, 1, Intersection) of
        {#{}, []} -> ?MAX_INT;
        {NextPosition, RemainingPath} ->
          calculate_dist_to_car_ahead(Intersection, Car, NextPosition, RemainingPath, Result+1)
      end;
    true ->
      Result
  end.
