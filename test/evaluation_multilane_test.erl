-module(evaluation_multilane_test).

-import(evaluation_multilane, [calculate_dist_to_semaphore/4, calculate_dist_to_car_ahead/2]).

-define(MAX_INT, 134217727).

-include_lib("eunit/include/eunit.hrl").

%%evaluation1_test() ->
%%  Solution = evaluate_solution().

calculate_dist_to_semaphore_test() ->
  Intersection = input:load_intersection_definition(),
  PathToDest = [11],
  CarPosition1 = #{node_id=>1, position_on_node=>1},
  Result = calculate_dist_to_semaphore(Intersection, CarPosition1, PathToDest, 0),
  ?assertEqual(5, Result),

  CarPosition2 = #{node_id=>1, position_on_node=>3},
  Result2 = calculate_dist_to_semaphore(Intersection, CarPosition2, PathToDest, 0),
  ?assertEqual(3, Result2),

  EmptyPathDest = [],
  CarPosition3 = #{node_id=>11, position_on_node=>1},
  Result3 = calculate_dist_to_semaphore(Intersection, CarPosition3, EmptyPathDest, 0),
  ?assertEqual(?MAX_INT, Result3),

  CarPosition4 = #{node_id=>8, position_on_node=>1},
  Result4 = calculate_dist_to_semaphore(Intersection, CarPosition4, EmptyPathDest, 0),
  ?assertEqual(0, Result4).

calculates_dist_to_car_on_the_same_lane_test() ->
  Intersection = input:load_intersection_definition(),
  UpdatedIntersection = intersection:add_car_on(5, #{node_id=>1, position_on_node=>4}, Intersection),
  [Car | _] = input:load_car_definitions(),
  calculate_dist_to_car_ahead_test(UpdatedIntersection, Car, 3).

calculates_dist_to_car_on_same_position_test() ->
  Intersection = intersection:add_car_on(5, #{node_id=>1, position_on_node=>1}, input:load_intersection_definition()),
  [Car | _] = input:load_car_definitions(),
  calculate_dist_to_car_ahead_test(Intersection, Car, 0).

calculates_dist_to_car_on_semaphore_test() ->
  Intersection = intersection:add_car_on(5, #{node_id=>8, position_on_node=>1}, input:load_intersection_definition()),
  [Car | _] = input:load_car_definitions(),
  calculate_dist_to_car_ahead_test(Intersection, Car, 6).

%% TODO: this case doesn't work
calculates_dist_to_car_on_next_lane_test() ->
  Intersection = intersection:add_car_on(5, #{node_id=>11, position_on_node=>1}, input:load_intersection_definition()),
  [Car | _] = input:load_car_definitions(),
  calculate_dist_to_car_ahead_test(Intersection, Car, 8).

%% TODO: add test case when no car ahead
calculate_dist_to_car_ahead_test(Intersection, Car, ExpectedResult) ->
  ?assertEqual(ExpectedResult, calculate_dist_to_car_ahead(Intersection, Car)).