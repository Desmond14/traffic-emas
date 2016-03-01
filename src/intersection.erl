%% @doc A module contains utility functions to operate on car intersection structure. All API functions should take intersection() type as a last argument.

-module(intersection).
-export([get_next_lane_path/3, get_path_with_semaphores/3, get_node_type/2, next_position/4, add_car_on/3, get_cars_on/2, get_node_length/2, move_car/4, remove_car_from/3, get_node_id/1, add_incoming_nodes/3, get_incoming_nodes/2, calculate_lanes_capacity/1, get_lanes/1, new_position/2, get_next_lane_paths/2]).

-include("model.hrl").

-spec get_node_id(intersection_node()) -> node_id().
get_node_id(Node) ->
  maps:get(id, Node).

-spec get_outcoming_nodes(intersection_node()) -> [node_id()].
get_outcoming_nodes(Node) ->
  maps:get(outcoming_nodes, Node).

-spec get_node_type(node_id(), intersection()) -> node_type().
get_node_type(NodeId, Intersection) ->
maps:get(type, maps:get(NodeId, Intersection)).

-spec get_node_length(node_id(), intersection()) -> pos_integer().
get_node_length(NodeId, Intersection) ->
  maps:get(length, maps:get(NodeId, Intersection)).

-spec get_incoming_nodes(node_id(), intersection()) -> [node_id()].
get_incoming_nodes(NodeId, Intersection) ->
  maps:get(incoming_nodes, maps:get(NodeId, Intersection)).

-spec get_next_lane_paths(node_id(), intersection()) -> map().
get_next_lane_paths(NodeId, Intersection) ->
  maps:get(next_lane_paths, maps:get(NodeId, Intersection)).

-spec get_cars_on(position(), intersection()) -> [car_id()].
get_cars_on(Position, Intersection) ->
  #{node_id := NodeId, position_on_node := PositionOnNode} = Position,
  maps:get(PositionOnNode, maps:get(cars_on, maps:get(NodeId, Intersection)), []).

-spec add_incoming_nodes(node_id(), [node_id()], intersection()) -> intersection().
add_incoming_nodes(_SourceNodeId, [], Intersection) ->
  Intersection;

add_incoming_nodes(SourceNodeId, [DestNodeId | Rest], Intersection) ->
  Node = maps:get(DestNodeId, Intersection),
  IncomingNodes = maps:get(incoming_nodes, Node, []),
  UpdatedNode = maps:put(incoming_nodes, lists:append(IncomingNodes, [SourceNodeId]), Node),
  add_incoming_nodes(SourceNodeId, Rest, maps:put(DestNodeId, UpdatedNode, Intersection)).

%% @doc Returns list of all consecutive node ids including both semaphores and lanes
%% to go from lane InitialLane through lanes from list PathToDest
-spec get_path_with_semaphores(node_id(), [node_id()], intersection()) -> [node_id()].
get_path_with_semaphores(InitialLaneId, PathToDest, Intersection) ->
  get_path_with_semaphores(InitialLaneId, PathToDest, [], Intersection).

%% @doc Extracts list of consecutive nodes to go from lane From to lane To.
%% List includes To node but does not include From node
%% TODO: throw custom error instead of bad_key exception error
-spec get_next_lane_path(node_id(), node_id(), intersection()) -> [node_id()].
get_next_lane_path (From, To, Intersection) ->
  maps:get(To, maps:get(next_lane_paths, maps:get(From, Intersection))).

%% @doc Returns tuple with next position and remaining path to destination when moving
%% from CurrentPosition by DistanceToPass cells using PathToDest full path or tuple with
%% outside_intersection atom and empty path if next position is outside Intersection.
-spec next_position(position(), [node_id()], non_neg_integer(), intersection()) -> {optional_position(), [node_id()]}.
next_position(CurrentPosition, PathToDest, 0, _Intersection) ->
  {CurrentPosition, PathToDest};

next_position(CurrentPosition, PathToDest, DistanceToPass, Intersection) ->
  NodeId = maps:get(node_id, CurrentPosition),
  case get_node_type(NodeId, Intersection) of
    %% TODO: what if semaphore is the last element on path
    semaphore -> next_position(#{node_id=>hd(PathToDest), position_on_node=>1}, tl(PathToDest), DistanceToPass-1, Intersection);
    lane ->
      DoesFitInLane = fits_in_lane(CurrentPosition, Intersection, DistanceToPass),
      if
        DoesFitInLane ->
          NewPosition = maps:get(position_on_node, CurrentPosition) + DistanceToPass,
          {#{node_id=>NodeId, position_on_node=>NewPosition}, PathToDest};
        length(PathToDest) > 0 ->
          NextNodeBeginning = #{node_id=>hd(PathToDest), position_on_node=>1},
          PositionOnNode = maps:get(position_on_node, CurrentPosition),
          DistanceLeft = DistanceToPass-get_node_length(NodeId, Intersection)+PositionOnNode-1,
          next_position(NextNodeBeginning, tl(PathToDest), DistanceLeft, Intersection);
        true ->
          {outside_intersection, []}
      end
  end.

-spec remove_car_from(car_id(), position(), intersection()) -> intersection().
remove_car_from(CarId, Position, Intersection) ->
  #{node_id := NodeId, position_on_node := PositionOnNode} = Position,
  Node = maps:get(NodeId, Intersection),
  maps:put(NodeId, remove_car_from_node(CarId, PositionOnNode, Node), Intersection).

-spec move_car(car_id(), position(), position(), intersection()) -> intersection().
move_car(CarId, OldPosition, NewPosition, Intersection) ->
  add_car_on(CarId, NewPosition, remove_car_from(CarId, OldPosition, Intersection)).

-spec add_car_on(car_id(), position(), intersection()) -> intersection().
add_car_on(CarId, Position, Intersection) ->
  #{node_id := NodeId, position_on_node := PositionOnNode} = Position,
  Node = maps:get(NodeId, Intersection),
  maps:put(NodeId, add_car_on_node(CarId, PositionOnNode, Node), Intersection).

-spec calculate_lanes_capacity(intersection()) -> pos_integer().
calculate_lanes_capacity(Intersection) ->
  calculate_lanes_capacity(maps:values(Intersection), 0).


-spec get_lanes(intersection()) -> [intersection_node()].
get_lanes(Intersection) ->
  get_lanes(maps:values(Intersection), []).

-spec new_position(node_id(), pos_integer()) -> position().
new_position(NodeId, PositionOnNode) ->
  #{node_id=>NodeId, position_on_node=>PositionOnNode}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_path_with_semaphores(node_id(), [node_id()], [node_id()], intersection()) -> [node_id()].
get_path_with_semaphores(_InitialLane, [], Result, _Intersection) ->
  Result;

get_path_with_semaphores(InitialLaneId, PathToDest, Result, Intersection) ->
  PathToNextLane = get_next_lane_path(InitialLaneId, hd(PathToDest), Intersection),
  get_path_with_semaphores(hd(PathToDest), tl(PathToDest), lists:append(Result, PathToNextLane), Intersection).

-spec fits_in_lane(position(), intersection(), non_neg_integer()) -> boolean().
fits_in_lane(CurrentPosition, Intersection, DistanceToPass) ->
  NodeId = maps:get(node_id, CurrentPosition),
  PositionOnNode = maps:get(position_on_node, CurrentPosition),
  PositionOnNode + DistanceToPass =< get_node_length(NodeId, Intersection).

-spec remove_car_from_node(car_id(), non_neg_integer(), intersection_node()) -> intersection_node().
remove_car_from_node(CarId, PositionOnNode, Node) ->
  CarsOnNodeMap = maps:get(cars_on, Node),
  CarsOnPosition = maps:get(PositionOnNode, CarsOnNodeMap, []),
  UpdatedCarsOnNode = maps:put(PositionOnNode, lists:delete(CarId, CarsOnPosition), CarsOnNodeMap),
  Node#{cars_on => UpdatedCarsOnNode}.

-spec add_car_on_node(car_id(), non_neg_integer(), intersection_node()) -> intersection_node().
add_car_on_node(CarId, PositionOnNode, Node) ->
  CarsOnNodeMap = maps:get(cars_on, Node),
  CarsOnPosition = maps:get(PositionOnNode, CarsOnNodeMap, []),
  UpdatedCarsOnNode = maps:put(PositionOnNode, lists:append(CarsOnPosition, [CarId]), CarsOnNodeMap),
  Node#{cars_on => UpdatedCarsOnNode}.

-spec calculate_lanes_capacity([intersection_node()], non_neg_integer()) -> non_neg_integer().
calculate_lanes_capacity([], Result) ->
  Result;

calculate_lanes_capacity([Node | Rest], Result) ->
  case maps:get(type, Node) of
    lane ->
      calculate_lanes_capacity(Rest, Result + maps:get(length, Node));
    semaphore ->
      calculate_lanes_capacity(Rest, Result)
  end.

-spec get_lanes([intersection_node()], [intersection_node()]) -> [intersection_node()].
get_lanes([], Lanes) ->
  Lanes;

get_lanes([Node | Rest], Lanes) ->
  case maps:get(type, Node) of
    lane ->
      get_lanes(Rest, [Node | Lanes]);
    _ ->
      get_lanes(Rest, Lanes)
  end.