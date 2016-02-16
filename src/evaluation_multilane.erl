-module(evaluation_multilane).

%% API
-export([evaluate_solution/2]).

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
  case is_in_safe_distance_to_blocking_semaphore(InitialIntersection, Car, Lights) of
    true ->
      nasch:follow_nagel(Car, InitialIntersection, UpdatedIntersection, Lights);
    false ->
      CanStopBefore = can_stop_before_blocker(InitialIntersection, Car, Lights),
      if
        CanStopBefore ->
          decelerate_minimaly(InitialIntersection, UpdatedIntersection, Car, Lights);
        not CanStopBefore ->
          nasch:follow_nagel(Car, InitialIntersection, UpdatedIntersection, Lights)
      end
  end,
  move_one_car(InitialIntersection, UpdatedIntersection, tl(CarsToProcess), UpdatedCars, Lights, Distance).

decelerate_minimaly(Intersection, UpdatedIntersection, Car, Lights) ->
  DistToBlocker = car:calculate_dist_to_blocker(Intersection, Car, Lights),
  DecelerateBy = minimal_deceleration_to_stop_before(DistToBlocker, Car),
  car:move_car(car:set_velocity(car:get_velocity(Car)-DecelerateBy), UpdatedIntersection).

minimal_deceleration_to_stop_before(DistToBlocker, Car) ->
  minimal_deceleration_to_stop_before(DistToBlocker, Car, car:get_max_deceleration(Car)).

minimal_deceleration_to_stop_before(DistToBlocker, Car, -1) ->
  0;

minimal_deceleration_to_stop_before(DistToBlocker, Car, DecelerateBy) ->
  NewVelocity = max(0, car:get_velocity(Car)-DecelerateBy),
  NewDistanceToBlocker = max(0, DistToBlocker-NewVelocity),
  case can_stop_before_blocker(NewDistanceToBlocker, NewVelocity, car:get_max_velocity(Car), car:get_max_deceleration(Car)) of
    true ->
      minimal_deceleration_to_stop_before(DistToBlocker, Car, DecelerateBy-1);
    false ->
      DecelerateBy+1
  end.

is_in_safe_distance_to_blocking_semaphore(Intersection, Car, Lights) ->
  InitialVelocity = car:get_velocity(Car),
  DistToBlockingSemaphore = car:calculate_dist_to_first_blocking_semaphore(Intersection, Car, Lights),
  is_in_safe_distance_to_blocking_semaphore(InitialVelocity, DistToBlockingSemaphore).

is_in_safe_distance_to_blocking_semaphore(0, Distance) ->
  Distance > 0;

is_in_safe_distance_to_blocking_semaphore(Velocity, Distance) ->
  NewVelocity = Velocity-1,
  DistanceLeft = Distance - NewVelocity,
  is_in_safe_distance_to_blocking_semaphore(NewVelocity, DistanceLeft).

can_stop_before_blocker(Intersection, Car, Lights) ->
  DistToFirstBlocking = car:calculate_dist_to_first_blocking_semaphore(Intersection, Car, Lights),
  can_stop_before_blocker(DistToFirstBlocking, car:get_velocity(Car), car:get_max_velocity(Car), car:get_max_deceleration(Car)).

can_stop_before_blocker(DistanceTo, 0, MaxVelocity, MaxDeceleration) ->
  DistanceTo > 0;

can_stop_before_blocker(DistanceTo, Velocity, MaxVelocity, MaxDeceleration) ->
  NewVelocity = max(0, Velocity-MaxDeceleration),
  can_stop_before_blocker(DistanceTo-NewVelocity, NewVelocity, MaxVelocity, MaxDeceleration).

