%% @doc A module contains utility functions to operate on car data structure. All API functions should take car() type as a last argument.

-module(car).
-export([get_velocity/1, get_position/1, get_path_to_dest/1, get_full_path/2, calculate_dist_to_car_ahead/2, get_max_velocity/1, get_max_acceleration/1, get_max_deceleration/1, move_car/2, set_velocity/2, set_config/2, get_id/1, calculate_dist_to_first_blocking_semaphore/3, calculate_dist_to_blocker/3, get_node_id/1, set_path_to_dest/2, new_car/5]).

-include("model.hrl").

-spec get_id(car()) -> car_id().
get_id(Car) ->
  maps:get(id, Car).

-spec get_node_id(car()) -> node_id().
get_node_id(Car) ->
  maps:get(node_id, get_position(Car)).

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

-spec get_full_path(intersection(), car()) -> [node_id()].
get_full_path(Intersection, Car) ->
  intersection:get_path_with_semaphores(get_node_id(Car), get_path_to_dest(Car), Intersection).

-spec set_velocity(non_neg_integer(), car()) -> car().
set_velocity(Velocity, Car) ->
  maps:update(velocity, Velocity, Car).

-spec set_config(car_config(), car()) -> car().
set_config(Config, Car) ->
  maps:update(config, Config, Car).

-spec set_path_to_dest([node_id()], car()) -> car().
set_path_to_dest(PathToDest, Car) ->
  maps:update(path_to_dest, PathToDest, Car).

%% @doc Moves car to the next position on intersection. Returns tuple with car and intersection, both updated.
-spec move_car(car(), intersection()) -> {optional_car(), intersection()}.
move_car(Car, Intersection) ->
  Position = car:get_position(Car),
  PathToDest = car:get_path_to_dest(Car),
  Velocity = car:get_velocity(Car),
  {UpdatedPosition, RemainingPath} = intersection:next_position(Position, PathToDest, Velocity, Intersection),
  case UpdatedPosition of
    outside_intersection ->
      {outside_intersection, intersection:remove_car_from(get_id(Car), Position, Intersection)};
    _ ->
      UpdatedCar = maps:update(path_to_dest, RemainingPath, maps:update(position, UpdatedPosition, Car)),
      UpdatedIntersection = intersection:move_car(get_id(Car), Position, UpdatedPosition, Intersection),
      {UpdatedCar, UpdatedIntersection}
  end.

%% @doc Calculates distance to first node with red light or car, whatever comes first.
%% Returns ?MAX_INT if no blocker in front of ther car.
-spec calculate_dist_to_blocker(intersection(), lights(), car()) -> non_neg_integer().
calculate_dist_to_blocker(Intersection, Lights, Car) ->
  min(calculate_dist_to_car_ahead(Intersection, Car), calculate_dist_to_first_blocking_semaphore(Intersection, Car, Lights)).

-spec calculate_dist_to_car_ahead(intersection(), car()) -> non_neg_integer().
calculate_dist_to_car_ahead(InitialIntersection, Car) ->
  CarsOnInitialPosition = intersection:get_cars_on(get_position(Car), InitialIntersection),
  if
    (length(CarsOnInitialPosition)>1) ->
      0;
    true ->
      #{path_to_dest:=FullPathToDest, position:=InitialPosition} = Car,
      {NextPosition, RemainingPath} = intersection:next_position(InitialPosition, FullPathToDest, 1, InitialIntersection),
      case NextPosition of
        outside_intersection ->
          ?MAX_INT;
        _ ->
          calculate_dist_to_car_ahead(InitialIntersection, Car, NextPosition, RemainingPath, 1)
      end
  end.

%% TODO: move car argument at the end
-spec calculate_dist_to_first_blocking_semaphore(intersection(), car(), any()) -> non_neg_integer().
calculate_dist_to_first_blocking_semaphore(Intersection, Car, Lights) ->
  calculate_dist_to_first_blocking_semaphore(Intersection, maps:get(position,Car), maps:get(path_to_dest,Car), Lights, 0).

-spec new_car(car_id(), position(), non_neg_integer(), map(), [node_id()]) -> car().
new_car(Id, Position, Velocity, Config, PathToDest) ->
  #{id=>Id, position=>Position, velocity=>Velocity, config=>Config, path_to_dest=>PathToDest}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec calculate_dist_to_car_ahead(intersection(), car(), position(), [node_id()], non_neg_integer()) -> non_neg_integer().
calculate_dist_to_car_ahead(Intersection, Car, CurrentPosition, FullPathToDest, Result) ->
    CurrentNode = maps:get(maps:get(node_id, CurrentPosition), Intersection),
    PositionOnNode = maps:get(position_on_node, CurrentPosition),
    CarsOnLane = maps:get(cars_on, CurrentNode),
    CarsOnPosition = maps:get(PositionOnNode, CarsOnLane, []),
    if
      length(CarsOnPosition) == 0 ->
        {NextPosition, RemainingPath} = intersection:next_position(CurrentPosition, FullPathToDest, 1, Intersection),
        case NextPosition of
          outside_intersection ->
            ?MAX_INT;
          _ ->
            calculate_dist_to_car_ahead(Intersection, Car, NextPosition, RemainingPath, Result+1)
        end;
      length(CarsOnPosition) > 0 ->
        Result
    end.

-spec calculate_dist_to_first_blocking_semaphore(intersection(), position(), [node_id()], lights(), non_neg_integer()) -> non_neg_integer().
calculate_dist_to_first_blocking_semaphore(_Intersection, _CurrentPosition, [], _Lights, _Result) ->
  ?MAX_INT;

calculate_dist_to_first_blocking_semaphore(Intersection, CurrentPosition, FullPathToDest, Lights, Result) ->
  {NextPosition, DistToNextNode} = dist_to_next_node(CurrentPosition, FullPathToDest, Intersection),
  calculate_dist_to_first_blocking_semaphore(Intersection, NextPosition, CurrentPosition, tl(FullPathToDest), Lights, Result + DistToNextNode).

-spec calculate_dist_to_first_blocking_semaphore(intersection(), position(), position(), [node_id()], lights(), non_neg_integer()) -> non_neg_integer().
calculate_dist_to_first_blocking_semaphore(_Intersection, _CurrentPosition, _PreviousPosition, [], _Lights, _Result) ->
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

-spec light_enables_to_enter(node_id(), node_id(), intersection(), lights()) -> boolean().
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

-spec dist_to_next_node(position(), [node_id()], intersection()) -> {position(), non_neg_integer()}.
dist_to_next_node(CurrentPosition, FullPathToDest, Intersection) ->
  [NextNodeId | _] = FullPathToDest,
  NextPosition = #{node_id=>NextNodeId, position_on_node=>1},
  CurrentNodeId = maps:get(node_id, CurrentPosition),
  CurrentNodePosition = maps:get(position_on_node, CurrentPosition),
  DistToNextNode = intersection:get_node_length(CurrentNodeId, Intersection) - CurrentNodePosition + 1,
  {NextPosition, DistToNextNode}.

