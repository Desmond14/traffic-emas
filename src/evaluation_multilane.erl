-module(evaluation_multilane).
-export([evaluate_solution/2, move_cars/2]).

-include("model.hrl").

-type gene() :: #{node_id()=>trit()}.
-type solution() :: [gene()].

-spec evaluate_solution(solution(), input()) -> float().
evaluate_solution(Solution, Data) ->
  {Intersection, Cars} = Data,
  NormalizedCars = convert_paths_to_full_paths(Cars, Intersection),
  time_loop(Solution, {Intersection, NormalizedCars}, 0).

-spec convert_paths_to_full_paths([car()], intersection()) -> [car()].
convert_paths_to_full_paths(Cars, Intersection) ->
  convert_one_car(Cars, [], Intersection).

convert_one_car([], ConvertedCars, _Intersection) ->
  ConvertedCars;

convert_one_car(InitialCars, ConvertedCars, Intersection) ->
  CarToConvert = hd(InitialCars),
%%  io:format("Converting Car ~p", [CarToConvert]),
  ConvertedCar = maps:update(path_to_dest, car:get_full_path(CarToConvert, Intersection), CarToConvert),
  convert_one_car(tl(InitialCars), lists:append(ConvertedCars, [ConvertedCar]), Intersection).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Main loop which iterates through the solution genes
-spec time_loop(solution(), input(), float()) -> float().
time_loop([], _, Result) ->
  Result;

time_loop([Lights | Solution], Data, Result) ->
  {UpdatedData, Moves} = move_cars(Lights, Data),
%%  io:format("Updated data ~p~n", [UpdatedData]),
  time_loop(Solution, UpdatedData, Result + Moves).

%% @doc Launches the loop that tries to move all the cars that can be moved
-spec move_cars(gene(), input()) -> {input(), float()}.
move_cars(Lights, InitialData) ->
  {Intersection, Cars} = InitialData,
  move_one_car(Intersection, Intersection, Cars, [], Lights, 0).

%% @doc Iterate through all the cars and try to move them if there is space
%% When no car can be moved, the iteration is finished
-spec move_one_car(intersection(), intersection(), [car()], [car()], gene(), integer()) -> {input(), integer()}.
move_one_car(_InitialIntersection, UpdatedIntersection, [], UpdatedCars, _Lights, Distance) ->
%%  io:format("All cars moved. Updated cars: ~p~n", [UpdatedCars]),
  {{UpdatedIntersection, UpdatedCars}, Distance};

move_one_car(InitialIntersection, IntersectionToUpdate, CarsToProcess, UpdatedCars, Lights, Distance) ->
  [Car | _] = CarsToProcess,
%%  io:format("Moving car ~p~n", [Car]),
  case is_in_safe_distance_to_blocking_semaphore(InitialIntersection, Car, Lights) of
    true ->
      {UpdatedCar, UpdatedIntersection} = nasch:follow_nagel(Car, InitialIntersection, IntersectionToUpdate, Lights);
    false ->
      CanStopBefore = can_stop_before_blocker(InitialIntersection, Car, Lights),
      if
        CanStopBefore ->
          {UpdatedCar, UpdatedIntersection} = decelerate_minimaly(InitialIntersection, IntersectionToUpdate, Car, Lights);
        not CanStopBefore ->
          {UpdatedCar, UpdatedIntersection} = nasch:follow_nagel(Car, InitialIntersection, IntersectionToUpdate, Lights)
      end
  end,
  case UpdatedCar of
    outside_intersection ->
      move_one_car(InitialIntersection, UpdatedIntersection, tl(CarsToProcess), UpdatedCars, Lights, Distance + car:get_velocity(Car));
    _ ->
      move_one_car(InitialIntersection, UpdatedIntersection, tl(CarsToProcess), lists:append(UpdatedCars, [UpdatedCar]), Lights, Distance + car:get_velocity(UpdatedCar))
  end.


decelerate_minimaly(Intersection, UpdatedIntersection, Car, Lights) ->
  DistToBlocker = car:calculate_dist_to_blocker(Intersection, Car, Lights),
  DecelerateBy = minimal_deceleration_to_stop_before(DistToBlocker, Car),
  car:move_car(car:set_velocity(car:get_velocity(Car)-DecelerateBy, Car), UpdatedIntersection).

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
  can_stop_before_blocker(DistToFirstBlocking-car:get_velocity(Car), car:get_velocity(Car), car:get_max_velocity(Car), car:get_max_deceleration(Car)).

can_stop_before_blocker(DistanceTo, 0, MaxVelocity, MaxDeceleration) ->
  DistanceTo > 0;

can_stop_before_blocker(DistanceTo, Velocity, MaxVelocity, MaxDeceleration) ->
  NewVelocity = max(0, Velocity-MaxDeceleration),
  can_stop_before_blocker(DistanceTo-NewVelocity, NewVelocity, MaxVelocity, MaxDeceleration).

