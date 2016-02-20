-module(car_test).

-import(car, [calculate_dist_to_first_blocking_semaphore/3]).

%% API
-export([]).

-import(car, [calculate_dist_to_car_ahead/2]).

-define(MAX_INT, 134217727).
-include_lib("eunit/include/eunit.hrl").

get_full_path_test() ->
  Intersection = input:load_intersection_definition(),
  Cars = input:load_car_definitions(),
  ?assertEqual([4,8,11], car:get_full_path(Intersection, hd(Cars))),
  ?assertEqual([5,4,8,11], car:get_full_path(Intersection, lists:nth(2, Cars))).

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
  calculate_dist_to_car_ahead_test(Intersection, maps:update(path_to_dest, car:get_full_path(Intersection, Car), Car), 6).

calculates_dist_to_car_on_next_lane_test() ->
  Intersection = intersection:add_car_on(5, #{node_id=>11, position_on_node=>1}, input:load_intersection_definition()),
  [Car | _] = input:load_car_definitions(),
  calculate_dist_to_car_ahead_test(Intersection, maps:update(path_to_dest, car:get_full_path(Intersection, Car), Car), 7).

calculate_dist_as_max_int_when_no_car_ahead_test() ->
  Intersection = input:load_intersection_definition(),
  [Car | _] = input:load_car_definitions(),
  calculate_dist_to_car_ahead_test(Intersection, maps:update(path_to_dest, car:get_full_path(Intersection, Car), Car), ?MAX_INT).

calculate_dist_to_car_ahead_test(Intersection, Car, ExpectedResult) ->
  ?assertEqual(ExpectedResult, calculate_dist_to_car_ahead(Intersection, Car)).

calculate_dist_to_first_blocking_semaphore_test() ->
  Intersection = input:load_intersection_definition(),
  Car = #{position=>#{node_id=>1, position_on_node=>1}, path_to_dest=>[4, 8, 11]},
  Car2 = #{position=>#{node_id=>7, position_on_node=>2}, path_to_dest=>[8, 9, 10]},
  Lights = #{4=>0, 8=>0, 9=>0},
  ?assertEqual(?MAX_INT, calculate_dist_to_first_blocking_semaphore(Intersection, Car, Lights)),
  ?assertEqual(4, calculate_dist_to_first_blocking_semaphore(Intersection, Car2, Lights)),

  LightsWithFirstSemaphoreBlocking =  #{4=>1, 8=>0},
  ?assertEqual(5, calculate_dist_to_first_blocking_semaphore(Intersection, Car, LightsWithFirstSemaphoreBlocking)),

  LightsWithFirstSemaphoreBlocking2 =  #{4=>2, 8=>0},
  ?assertEqual(5, calculate_dist_to_first_blocking_semaphore(Intersection, Car, LightsWithFirstSemaphoreBlocking2)),

  LightsWithSecondSemaphoreBlocking =  #{4=>0, 8=>1},
  ?assertEqual(6, calculate_dist_to_first_blocking_semaphore(Intersection, Car, LightsWithSecondSemaphoreBlocking)),

  LightsWithSecondSemaphoreBlocking2 =  #{4=>0, 8=>2},
  ?assertEqual(6, calculate_dist_to_first_blocking_semaphore(Intersection, Car, LightsWithSecondSemaphoreBlocking2)).