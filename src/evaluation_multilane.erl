-module(evaluation_multilane).

%% API
-export([evaluate_solution/2, calculate_dist_to_first_blocking_semaphore/3]).

-define(GREEN_FOR_LOWER_ID, 0).
-define(BOTH_RED, 1).
-define(GREEN_FOR_HIGHER_ID, 2).

-type trit() :: ?GREEN_FOR_LOWER_ID | ?BOTH_RED | ?GREEN_FOR_HIGHER_ID.
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
  NormalizedCars = convert_paths_to_full_paths(Cars, Intersection),
  move_one_car(Intersection, Intersection, NormalizedCars, NormalizedCars, Lights, 0).

-spec convert_paths_to_full_paths([car()], intersection()) -> [car()].
convert_paths_to_full_paths(Cars, Intersection) ->
  convert_one_car(Cars, [], Intersection).

convert_one_car([], ConvertedCars, _Intersection) ->
  ConvertedCars;

convert_one_car(InitialCars, ConvertedCars, Intersection) ->
  CarToConvert = hd(InitialCars),
  ConvertedCar = maps:update(path_to_dest, car:get_full_path(CarToConvert, Intersection), CarToConvert),
  convert_one_car(tl(InitialCars), lists:append(ConvertedCars, [ConvertedCar]), Intersection).

%% @doc Iterate through all the cars and try to move them if there is space
%% When no car can be moved, the iteration is finished
-spec move_one_car(intersection(), intersection(), [car()], [car()], gene(), integer()) -> {data(), integer()}.
move_one_car(_InitialIntersection, UpdatedIntersection, [], UpdatedCars, _Lights, Distance) ->
  {{UpdatedIntersection, UpdatedCars}, Distance};

move_one_car(InitialIntersection, UpdatedIntersection, CarsToProcess, UpdatedCars, Lights, Distance) ->
  [Car | _] = CarsToProcess,
%%  case is_in_safe_distance_to_blocking_semaphore(InitialIntersection, Car, Lights) of
%%    true -> follow_nagel_schreckenberg()
%%  end,
  move_one_car(InitialIntersection, UpdatedIntersection, tl(CarsToProcess), UpdatedCars, Lights, Distance).

is_in_safe_distance_to_blocking_semaphore(Intersection, Car, Lights) ->
  InitialVelocity = car:get_velocity(Car),
  DistToBlockingSemaphore = calculate_dist_to_first_blocking_semaphore(Intersection, Car, Lights),
  is_in_safe_distance_to_blocking_semaphore(InitialVelocity, DistToBlockingSemaphore).

is_in_safe_distance_to_blocking_semaphore(0, Distance) ->
  Distance > 0;

is_in_safe_distance_to_blocking_semaphore(Velocity, Distance) ->
  NewVelocity = Velocity-1,
  DistanceLeft = Distance - NewVelocity,
  is_in_safe_distance_to_blocking_semaphore(NewVelocity, DistanceLeft).

calculate_dist_to_first_blocking_semaphore(Intersection, Car, Lights) ->
  calculate_dist_to_first_blocking_semaphore(Intersection, maps:get(position,Car), maps:get(path_to_dest,Car), Lights, 0).

calculate_dist_to_first_blocking_semaphore(Intersection, CurrentPosition, [], Lights, Result) ->
  ?MAX_INT;

calculate_dist_to_first_blocking_semaphore(Intersection, CurrentPosition, FullPathToDest, Lights, Result) ->
  {NextPosition, DistToNextNode} = dist_to_next_node(CurrentPosition, FullPathToDest, Intersection),
  calculate_dist_to_first_blocking_semaphore(Intersection, NextPosition, CurrentPosition, tl(FullPathToDest), Lights, Result + DistToNextNode).

calculate_dist_to_first_blocking_semaphore(Intersection, CurrentPosition, PreviousPosition, [], Lights, Result) ->
  ?MAX_INT;

calculate_dist_to_first_blocking_semaphore(Intersection, CurrentPosition, PreviousPosition, FullPathToDest, Lights, Result) ->
  CurrentNode = maps:get(maps:get(node_id, CurrentPosition), Intersection),
  case maps:get(type, CurrentNode) of
    semaphore ->
      LightEnablesToEnter = light_enables_to_enter(maps:get(node_id, PreviousPosition), maps:get(node_id, CurrentPosition), Intersection, Lights),
      if
        not LightEnablesToEnter ->
          Result;
        LightEnablesToEnter ->
          {NextPosition, DistToNextNode} = dist_to_next_node(CurrentPosition, FullPathToDest, Intersection),
          calculate_dist_to_first_blocking_semaphore(Intersection, NextPosition, CurrentPosition, tl(FullPathToDest), Lights, Result + DistToNextNode)
      end;
    lane ->
      {NextPosition, DistToNextNode} = dist_to_next_node(CurrentPosition, FullPathToDest, Intersection),
      calculate_dist_to_first_blocking_semaphore(Intersection, NextPosition, tl(FullPathToDest), Lights, Result + DistToNextNode)
  end.

dist_to_next_node(CurrentPosition, FullPathToDest, Intersection) ->
  [NextNodeId | _] = FullPathToDest,
  NextPosition = #{node_id=>NextNodeId, position_on_node=>1},
  CurrentNodeId = maps:get(node_id, CurrentPosition),
  CurrentNodePosition = maps:get(position_on_node, CurrentPosition),
  DistToNextNode = intersection:get_node_length(CurrentNodeId, Intersection) - CurrentNodePosition + 1,
  {NextPosition, DistToNextNode}.

light_enables_to_enter(FromId, ToId, Intersection, Lights) ->
  ToNode = maps:get(ToId, Intersection),
  IncomingNodes = maps:get(incoming_nodes, ToNode),
  LightMarker = maps:get(ToId, Lights),
  [FirstIncoming | _] = IncomingNodes,
  if
    FirstIncoming == FromId ->
      LightMarker == ?GREEN_FOR_LOWER_ID;
    true ->
      LightMarker == ?GREEN_FOR_HIGHER_ID
  end.
