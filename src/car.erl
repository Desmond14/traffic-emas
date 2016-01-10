-module(car).

%% API
-export([]).

%%distance_to_car_ahead(Intersection, Car) ->
%%  #{CurrentPosition, PathToDest}
%%
%%distance_to_car_ahead(PathToDest, Intersection, Car) ->
%%  CurrentNode = maps:get(maps:get(node_id, CurrentPosition), InitialIntersection),
%%  PositionOnNode = maps:get(position_on_node, CurrentPosition),
%%  CarsOnLane = maps:get(cars_on, CurrentNode),
%%  CarsOnPosition = maps:get(PositionOnNode, CarsOnLane, []),
%%  IsExaminedCarOnPosition = lists:member(CarId, CarsOnPosition),
%%  if
%%    length(CarsOnPosition) == 0 ->
%%      case intersection:next_position(CurrentPosition, PathToDest, 1, InitialIntersection) of
%%        {#{}, _} -> ?MAX_INT;
%%        {NextPosition, RemainingPath} ->
%%          calculate_dist_to_car_ahead(InitialIntersection, NextPosition, RemainingPath, CarId, Result+1)
%%      end;
%%    true ->
%%      Result
%%  end.