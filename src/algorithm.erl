%% @doc A module contains implementation of car movement algorithm.

-module(algorithm).
-export([move/3]).

-include("model.hrl").

%% @doc Moves car basing on its current state, situation on intersection and lights configuration
%% Returns tuple with updated car and updated intersection.
-spec move(car(), intersection(), lights()) -> {optional_car(), intersection()}.
move(Car, Intersection, Lights) ->
  case is_in_safe_distance_to_blocking_semaphore(Intersection, Car, Lights) of
    true ->
      follow_nagel_schreckenberg(Car, Intersection, Lights);
    false ->
      case can_stop_before_blocker(Intersection, Car, Lights) of
        true ->
          decelerate_minimaly(Intersection, Car, Lights);
        false ->
          follow_nagel_schreckenberg(Car, Intersection, Lights)
      end
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec follow_nagel_schreckenberg(car(), intersection(), lights()) -> {optional_car(), intersection()}.
follow_nagel_schreckenberg(Car, Intersection, Lights) ->
  DistToBlocker = car:calculate_dist_to_blocker(Intersection, Lights, Car),
  CarWithUpdatedVelocity = car:set_velocity(new_velocity(Car, DistToBlocker), Car),
  car:move_car(CarWithUpdatedVelocity, Intersection).

-spec new_velocity(car(), non_neg_integer()) -> non_neg_integer().
new_velocity(Car, DistToCarAhead) ->
  Velocity = car:get_velocity(Car),
  MaxVelocity = car:get_max_velocity(Car),
  MaxAcceleration = car:get_max_acceleration(Car),
  MaxDeceleration = car:get_max_deceleration(Car),
  if
    DistToCarAhead > Velocity ->
      NewVelocity = min(MaxVelocity, min(DistToCarAhead - 1, Velocity + MaxAcceleration));
    DistToCarAhead =< Velocity ->
      NewVelocity = max(0, max(DistToCarAhead - 1, Velocity - MaxDeceleration))
  end,
  NewVelocity.

%% @doc Decelerates car's velocity just enough to be able to stop before next car or red light
%% in consecutive time steps. Returns updated car (or oustide_intersection atom) and intersection updated
%% with car movement accordingly to decelerated velocity.
-spec decelerate_minimaly(intersection(), car(), lights()) -> {optional_car(), intersection()}.
decelerate_minimaly(Intersection, Car, Lights) ->
  DistToBlocker = car:calculate_dist_to_blocker(Intersection, Lights, Car),
  DecelerateBy = minimal_deceleration_to_stop_before(DistToBlocker, Car),
  car:move_car(car:set_velocity(car:get_velocity(Car)-DecelerateBy, Car), Intersection).

-spec minimal_deceleration_to_stop_before(non_neg_integer(), car()) -> non_neg_integer().
minimal_deceleration_to_stop_before(DistToBlocker, Car) ->
  minimal_deceleration_to_stop_before(DistToBlocker, Car, car:get_max_deceleration(Car)).

minimal_deceleration_to_stop_before(_DistToBlocker, _Car, -1) ->
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

-spec is_in_safe_distance_to_blocking_semaphore(intersection(), car(), lights()) -> boolean().
is_in_safe_distance_to_blocking_semaphore(Intersection, Car, Lights) ->
  InitialVelocity = car:get_velocity(Car),
  DistToBlockingSemaphore = car:calculate_dist_to_first_blocking_semaphore(Intersection, Car, Lights),
  is_in_safe_distance_to_blocking_semaphore(InitialVelocity, DistToBlockingSemaphore).

-spec is_in_safe_distance_to_blocking_semaphore(non_neg_integer(), non_neg_integer()) -> boolean().
is_in_safe_distance_to_blocking_semaphore(0, Distance) ->
  Distance > 0;

is_in_safe_distance_to_blocking_semaphore(Velocity, Distance) ->
  NewVelocity = Velocity-1,
  DistanceLeft = Distance - NewVelocity,
  is_in_safe_distance_to_blocking_semaphore(NewVelocity, DistanceLeft).

-spec can_stop_before_blocker(intersection(), car(), lights()) -> boolean().
can_stop_before_blocker(Intersection, Car, Lights) ->
  DistToFirstBlocking = car:calculate_dist_to_first_blocking_semaphore(Intersection, Car, Lights),
  can_stop_before_blocker(DistToFirstBlocking-car:get_velocity(Car), car:get_velocity(Car), car:get_max_velocity(Car), car:get_max_deceleration(Car)).

-spec can_stop_before_blocker(non_neg_integer(), non_neg_integer(), pos_integer(), pos_integer()) -> boolean().
can_stop_before_blocker(DistanceTo, 0, _MaxVelocity, _MaxDeceleration) ->
  DistanceTo > 0;

can_stop_before_blocker(DistanceTo, Velocity, MaxVelocity, MaxDeceleration) ->
  NewVelocity = max(0, Velocity-MaxDeceleration),
  can_stop_before_blocker(DistanceTo-NewVelocity, NewVelocity, MaxVelocity, MaxDeceleration).
