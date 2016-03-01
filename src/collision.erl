-module(collision).
-export([collision_occured/2]).

-include("model.hrl").

-type positions_counter() :: #{position()=>pos_integer()}.

-spec collision_occured(input(), [car()]) -> boolean().
collision_occured({InitialIntersection, InitialCars}, UpdatedCars) ->
  VisitedPositions = mark_visited_positions(InitialCars, UpdatedCars, InitialIntersection),
  collision_occured(VisitedPositions).

%% =============================================================================
%% Internal functions
%% =============================================================================

collision_occured(VisitedPositions) ->
  check_node(maps:values(VisitedPositions)).

-spec check_node([pos_integer()]) -> boolean().
check_node([]) ->
  false;

check_node([CarPassedThroughNode | _Rest]) when CarPassedThroughNode > 1 ->
  true;

check_node([_CarPassedThroughNode | Rest]) ->
  check_node(Rest).

-spec mark_visited_positions([car()], [car()], intersection()) -> positions_counter().
mark_visited_positions(InitialCars, UpdatedCars, Intersection) ->
  case InitialCars of
    [] ->
      maps:new();
    _ ->
      mark_visited_positions(InitialCars, UpdatedCars, Intersection, #{})
  end.

-spec mark_visited_positions([car()], [car()], intersection(), positions_counter()) -> positions_counter().
mark_visited_positions(_InitialCars, [], _Intersection, VisitedPositions) ->
  VisitedPositions;

mark_visited_positions([Car | InitialCars], [UpdatedCar | UpdatedCars], Intersection, VisitedPositions) ->
  UpdatedCarId = car:get_id(UpdatedCar),
  case car:get_id(Car) of
    UpdatedCarId ->
      mark_visited_positions(InitialCars, UpdatedCars, Intersection, mark_path(Car, car:get_position(UpdatedCar), Intersection, VisitedPositions));
    _ ->
      mark_visited_positions(InitialCars, [UpdatedCar | UpdatedCars], Intersection, mark_path_to_end(Car, Intersection, VisitedPositions))
  end.

-spec mark_path(car(), position(), intersection(), positions_counter()) -> positions_counter().
mark_path(Car, FinalPosition, Intersection, VisitedPositions) ->
  #{position:=InitialPosition, path_to_dest:=PathToDest} = Car,
  mark_path(InitialPosition, FinalPosition, PathToDest, Intersection, VisitedPositions).

-spec mark_path(position(), position(), [node_id()], intersection(), positions_counter()) -> positions_counter().
mark_path(InitialPosition, FinalPosition, PathToDest, Intersection, VisitedPositions) ->
  UpdatedVisited = increment_position(InitialPosition, VisitedPositions),
  case InitialPosition == FinalPosition of
    true ->
      UpdatedVisited;
    false ->
      {NextPosition, RemainingPath} = intersection:next_position(InitialPosition, PathToDest, 1, Intersection),
      mark_path(NextPosition, FinalPosition, RemainingPath, Intersection, UpdatedVisited)
  end.

-spec mark_path_to_end(car(), intersection(), positions_counter()) -> positions_counter().
mark_path_to_end(Car, Intersection, VisitedPositions) ->
  mark_path_to_end(car:get_position(Car), car:get_path_to_dest(Car), Intersection, VisitedPositions).

-spec mark_path_to_end(optional_position(), [node_id()], intersection(), positions_counter()) -> positions_counter().
mark_path_to_end(outside_intersection, _PathToDest, _Intersection, VisitedPositions) ->
  VisitedPositions;

mark_path_to_end(Position, PathToDest, Intersection, VisitedPositions) ->
  UpdatedVisited = increment_position(Position, VisitedPositions),
  {NextPosition, RemainingPath} = intersection:next_position(Position, PathToDest, 1, Intersection),
  mark_path_to_end(NextPosition, RemainingPath, Intersection, UpdatedVisited).

-spec increment_position(position(), positions_counter()) -> positions_counter().
increment_position(Position, VisitedPositions) ->
  VisitedCount = maps:get(Position, VisitedPositions, 0),
  maps:put(Position, VisitedCount+1, VisitedPositions).