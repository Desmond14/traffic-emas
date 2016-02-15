-module(car).

%% API
-export([get_velocity/1, get_position/1, get_path_to_dest/1, get_full_path/2, calculate_dist_to_car_ahead/2, get_max_velocity/1, get_max_acceleration/1, get_max_deceleration/1, move_car/2, set_velocity/2, set_config/2, get_id/1]).

-type car() :: input:car().
-type position() :: input:position().
-type intersection() :: input:intersection().

-define(MAX_INT, 134217727).

get_id(Car) ->
  maps:get(id, Car).

-spec get_velocity(car()) -> non_neg_integer().
get_velocity(Car) ->
  maps:get(velocity, Car).

-spec get_max_velocity(car()) -> non_neg_integer().
get_max_velocity(Car) ->
  maps:get(max_velocity, maps:get(config, Car)).

-spec get_max_acceleration(car()) -> pos_integer().
get_max_acceleration(Car) ->
  maps:get(max_acceleration, maps:get(config, Car)).

-spec get_max_deceleration(car()) -> pos_integer().
get_max_deceleration(Car) ->
  maps:get(max_deceleration, maps:get(config, Car)).

-spec get_position(car()) -> position().
get_position(Car) ->
  maps:get(position, Car).

-spec get_path_to_dest(car()) -> integer().
get_path_to_dest(Car) ->
  maps:get(path_to_dest, Car).

get_full_path(Car, Intersection) ->
  intersection:get_path_with_semaphores(maps:get(node_id, get_position(Car)), get_path_to_dest(Car), Intersection).

-spec set_velocity(non_neg_integer(), car()) -> car().
set_velocity(Velocity, Car) ->
  maps:update(velocity, Velocity, Car).

set_config(Config, Car) ->
  maps:update(config, Config, Car).

-spec calculate_dist_to_car_ahead(intersection(), car()) -> non_neg_integer().
calculate_dist_to_car_ahead(InitialIntersection, Car) ->
  CarsOnInitialPosition = intersection:get_cars_on(get_position(Car), InitialIntersection),
  if
    (length(CarsOnInitialPosition)>1) ->
      0;
    true ->
      #{path_to_dest:=FullPathToDest, position:=InitialPosition, id:=CarId} = Car,
      {NextPosition, RemainingPath} = intersection:next_position(InitialPosition, FullPathToDest, 1, InitialIntersection),
      calculate_dist_to_car_ahead(InitialIntersection, Car, NextPosition, RemainingPath, 1)
  end.

calculate_dist_to_car_ahead(Intersection, Car, CurrentPosition, FullPathToDest, Result) ->
  MapSize = maps:size(CurrentPosition),
  if
    MapSize == 0 ->
      ?MAX_INT;
    MapSize > 0 ->
      CurrentNode = maps:get(maps:get(node_id, CurrentPosition), Intersection),
      PositionOnNode = maps:get(position_on_node, CurrentPosition),
      CarsOnLane = maps:get(cars_on, CurrentNode),
      CarsOnPosition = maps:get(PositionOnNode, CarsOnLane, []),
      if
        length(CarsOnPosition) == 0 ->
          {NextPosition, RemainingPath} = intersection:next_position(CurrentPosition, FullPathToDest, 1, Intersection),
          io:format("NextPosition: ~p, RemainingPath: ~p~n", [NextPosition, RemainingPath]),
          calculate_dist_to_car_ahead(Intersection, Car, NextPosition, RemainingPath, Result+1);
        length(CarsOnPosition) > 0 ->
          Result
      end
  end.

-spec move_car(car(), intersection()) -> {car(), intersection()}.
move_car(Car, Intersection) ->
  Position = car:get_position(Car),
  PathToDest = car:get_path_to_dest(Car),
  Velocity = car:get_velocity(Car),
  {UpdatedPosition, RemainingPath} = intersection:next_position(Position, PathToDest, Velocity, Intersection),
  IsMovedOutsideIntersection = maps:size(UpdatedPosition) == 0,
  if
    IsMovedOutsideIntersection ->
      {outside_intersection, intersection:remove_car_from(get_id(Car), Position, Intersection)};
    not IsMovedOutsideIntersection ->
      UpdatedCar = maps:update(path_to_dest, RemainingPath, maps:update(position, UpdatedPosition, Car)),
      UpdatedIntersection = intersection:move_car(get_id(Car), Position, UpdatedPosition, Intersection),
      {UpdatedCar, UpdatedIntersection}
end.
