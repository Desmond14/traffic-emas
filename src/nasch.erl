-module(nasch).

%% API
-export([follow_nagel/4]).

-type car_or_outside() :: input:car() | outside_intersection.
-type car() :: input:car().
-type position() :: input:position().
-type intersection() :: input:intersection().

-spec follow_nagel(car(), intersection(), intersection(), any()) -> {car_or_outside(), intersection()}.
follow_nagel(Car, Intersection, UpdatedIntersection, Lights) ->
  DistToBlocker = car:calculate_dist_to_blocker(Intersection, Car, Lights),
%%  io:format("CarId: ~p, DistToBlocker: ~p~n", [car:get_id(Car), DistToBlocker]),
  CarWithUpdatedVelocity = car:set_velocity(new_velocity(Car, DistToBlocker), Car),
  car:move_car(CarWithUpdatedVelocity, UpdatedIntersection).

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