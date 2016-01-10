-module(intersection_test).

-import(intersection, [get_next_lane_path/3, get_path_with_semaphores/3, get_node_type/2]).

-include_lib("eunit/include/eunit.hrl").


get_path_to_next_lane_test() ->
  Intersection = input:load_intersection_definition(),
  Result = intersection:get_next_lane_path(1, 10, Intersection),
  ?assertEqual([4,8,9,10], Result).

gets_path_with_semaphores_test() ->
  get_path_with_semaphores_test(6, [3], [5,4,3]).

%% TODO: add case with longer PathToDest

gets_empty_path_with_semaphores_when_path_to_dest_is_empty_test() ->
  get_path_with_semaphores_test(6, [], []).

get_path_with_semaphores_test(NodeId, PathToDest, ExpectedPathWithSemaphores) ->
  Intersection = input:load_intersection_definition(),
  Result = get_path_with_semaphores(NodeId, PathToDest, Intersection),
  ?assertEqual(ExpectedPathWithSemaphores, Result).

gets_lane_node_type_test() ->
  gets_node_type_test(3, lane).

gets_semaphore_node_type_test() ->
  gets_node_type_test(5, semaphore).

gets_node_type_test(NodeId, ExpectedType) ->
  Intersection = input:load_intersection_definition(),
  Result = get_node_type(NodeId, Intersection),
  ?assertEqual(ExpectedType, Result).

returns_next_position_at_beginning_of_next_lane_test() ->
  InitialPosition = #{node_id=>7, position_on_node=>4},
  ExpectedPosition = #{node_id=>11, position_on_node=>1},
  ExpectedRemainingPath = [],
  next_position_test(InitialPosition, [8, 11], 3, {ExpectedPosition, ExpectedRemainingPath}).

returns_next_position_at_the_end_of_current_lane_test() ->
  InitialPosition = #{node_id=>7, position_on_node=>4},
  ExpectedPosition = #{node_id=>7, position_on_node=>5},
  ExpectedRemainingPath = [8, 11],
  next_position_test(InitialPosition, [8, 11], 1, {ExpectedPosition, ExpectedRemainingPath}).

returns_next_position_on_semaphore_test() ->
  InitialPosition = #{node_id=>7, position_on_node=>4},
  ExpectedPosition = #{node_id=>8, position_on_node=>1},
  ExpectedRemainingPath = [11],
  next_position_test(InitialPosition, [8, 11], 2, {ExpectedPosition, ExpectedRemainingPath}).

returns_empty_map_and_list_for_next_position_outside_intersection_test() ->
  InitialPosition = #{node_id=>7, position_on_node=>4},
  ExpectedPosition = maps:new(),
  ExpectedRemainingPath = [],
  next_position_test(InitialPosition, [8, 11], 8, {ExpectedPosition, ExpectedRemainingPath}).

returns_next_position_on_next_semaphore_test() ->
  InitialPosition = #{node_id=>8, position_on_node=>1},
  ExpectedPosition = #{node_id=>9, position_on_node=>1},
  ExpectedRemainingPath = [10],
  next_position_test(InitialPosition, [9, 10], 1, {ExpectedPosition, ExpectedRemainingPath}).

next_position_test(InitialPosition, PathToDest, DistanceToPass, ExpectedNextPosition) ->
  Intersection = input:load_intersection_definition(),
  Result = intersection:next_position(InitialPosition, PathToDest, DistanceToPass, Intersection),
  ?assertEqual(ExpectedNextPosition, Result).

adds_car_on_second_position_on_node_test() ->
  ExpectedCarsOnNode = #{1=>[1], 2=>[5]},
  add_car_on_test(5, #{node_id=>1, position_on_node=>2}, ExpectedCarsOnNode).

adds_car_on_empty_node_test() ->
  ExpectedCarsOnNode = #{1=>[5]},
  add_car_on_test(5, #{node_id=>3, position_on_node=>1}, ExpectedCarsOnNode).

adds_another_car_on_same_position_test() ->
  ExpectedCarsOnNode = #{1=>[1, 5]},
  add_car_on_test(5, #{node_id=>1, position_on_node=>1}, ExpectedCarsOnNode).

adds_car_on_semaphore_test() ->
  ExpectedCarsOnNode = #{1=>[5]},
  add_car_on_test(5, #{node_id=>4, position_on_node=>1}, ExpectedCarsOnNode).

add_car_on_test(CarId, PositionToAddOn, ExpectedCarsOnNode) ->
  Intersection = input:load_intersection_definition(),
  UpadtedIntersection = intersection:add_car_on(CarId, PositionToAddOn, Intersection),
  #{node_id := NodeId} = PositionToAddOn,
  ActualCarsOnNode = maps:get(cars_on, maps:get(NodeId, UpadtedIntersection)),
  ?assertEqual(ExpectedCarsOnNode, ActualCarsOnNode).

gets_empty_list_for_cars_on_test() ->
  get_cars_on_test(#{node_id=>3, position_on_node=>3}, []).

gets_not_empty_list_for_cars_on_test() ->
  get_cars_on_test(#{node_id=>1, position_on_node=>1}, [1]).

get_cars_on_test(Position, ExpectedResult) ->
  Intersection = input:load_intersection_definition(),
  Result = intersection:get_cars_on(Position, Intersection),
  ?assertEqual(ExpectedResult, Result).