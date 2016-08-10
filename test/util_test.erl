-module(util_test).

-include_lib("eunit/include/eunit.hrl").

should_not_generate_any_car_test() ->
  {Intersection, _Cars} = input:load("test/util_test.intersection", "test/util_test.cars", 0.0),
  {_Intersection, Cars} = util:generate_cars_on(Intersection, {coverage, 0.0}),
  ?assertEqual(0, length(Cars)).

should_generate_4_cars_test() ->
  {Intersection, _Cars} = input:load("test/util_test.intersection", "test/util_test.cars", 0.0),
  {_Intersection, Cars} = util:generate_cars_on(Intersection, {coverage, 0.1}),
  ?assertEqual(2, length(Cars)).

should_generate_cars_on_all_lane_cells_test() ->
  {Intersection, _Cars} = input:load("test/util_test.intersection", "test/util_test.cars", 0.0),
  {UpdatedIntersection, Cars} = util:generate_cars_on(Intersection, {coverage, 1.0}),
  ?assertEqual(20, length(Cars)).

should_generate_given_number_of_cars_test() ->
  {Intersection, _Cars} = input:load("test/util_test.intersection", "test/util_test.cars", 0.0),
  {UpdatedIntersection, Cars} = util:generate_cars_on(Intersection, {cars_number, 15}),
  ?assertEqual(15, length(Cars)).

should_place_cars_on_intersection_test() ->
  {Intersection, _Cars} = input:load("test/util_test.intersection", "test/util_test.cars", 0.0),
  {UpdatedIntersection, Cars} = util:generate_cars_on(Intersection, {coverage, 0.1}),
  assert_cars_on_intersection(UpdatedIntersection, Cars).

assert_cars_on_intersection(Intersection, []) ->
  true;

assert_cars_on_intersection(Intersection, [Car | Rest]) ->
  CarPosition = car:get_position(Car),
  CarsOnPosition = intersection:get_cars_on(CarPosition, Intersection),
  ?assertEqual([car:get_id(Car)], CarsOnPosition),
  assert_cars_on_intersection(Intersection, Rest).

generate_for_real_test() ->
  Solution = util:evaluate_file("test/solution.test"),
  ct:pal("~p~n", [jsx:encode(Solution)]).

generate_cars_test() ->
  {Intersection, _Cars} = input:load("input.intersection", "input.cars", 0.0),
  {_UpdatedIntersection, Cars} = util:generate_cars_on(Intersection, {cars_number, 12}),
  ct:pal("~p~n", [Cars]).

generate_simple_lights_test() ->
  ct:pal("~p,~n", [util:generate_simple_lights(250, 20)]).
