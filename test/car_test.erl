-module(car_test).

%% API
-export([]).

-import(car, [calculate_dist_to_car_ahead/2]).

-include_lib("eunit/include/eunit.hrl").

get_full_path_test() ->
  Intersection = input:load_intersection_definition(),
  Cars = input:load_car_definitions(),
  ?assertEqual([4,8,11], car:get_full_path(hd(Cars), Intersection)),

  ?assertEqual([5,4,8,11], car:get_full_path(lists:nth(2, Cars), Intersection)).

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
  calculate_dist_to_car_ahead_test(Intersection, maps:update(path_to_dest, car:get_full_path(Car, Intersection), Car), 6).

%% TODO: this case doesn't work
calculates_dist_to_car_on_next_lane_test() ->
  Intersection = intersection:add_car_on(5, #{node_id=>11, position_on_node=>1}, input:load_intersection_definition()),
  [Car | _] = input:load_car_definitions(),
  calculate_dist_to_car_ahead_test(Intersection, maps:update(path_to_dest, car:get_full_path(Car, Intersection), Car), 7).

%% TODO: add test case when no car ahead
calculate_dist_to_car_ahead_test(Intersection, Car, ExpectedResult) ->
  ?assertEqual(ExpectedResult, calculate_dist_to_car_ahead(Intersection, Car)).