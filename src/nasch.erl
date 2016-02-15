-module(nasch).

%% API
-export([follow_nasch/2]).

-type car_or_outside() :: input:car() | outside_intersection.
-type car() :: input:car().
-type position() :: input:position().
-type intersection() :: input:intersection().

-spec follow_nasch(car(), intersection()) -> {car_or_outside(), intersection()}.
follow_nasch(Car, Intersection) ->
  DistToCarAhead = car:calculate_dist_to_car_ahead(Intersection, Car),
  CarWithUpdatedVelocity = car:set_velocity(new_velocity(Car, DistToCarAhead), Car),
  car:move_car(CarWithUpdatedVelocity, Intersection).

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