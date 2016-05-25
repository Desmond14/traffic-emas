-module(util_test).

-include_lib("eunit/include/eunit.hrl").

should_not_generate_any_car_test() ->
  {Intersection, _Cars} = input:load("test/util_test.intersection", "test/util_test.cars"),
  {_Intersection, Cars} = util:generate_cars_on(Intersection, 0.0),
  ?assertEqual(0, length(Cars)).

should_generate_4_cars_test() ->
  {Intersection, _Cars} = input:load("test/util_test.intersection", "test/util_test.cars"),
  {_Intersection, Cars} = util:generate_cars_on(Intersection, 0.1),
  ?assertEqual(4, length(Cars)).

should_generate_cars_on_all_lane_cells_test() ->
  {Intersection, _Cars} = input:load("test/util_test.intersection", "test/util_test.cars"),
  {UpdatedIntersection, Cars} = util:generate_cars_on(Intersection, 1.0),
  ?assertEqual(40, length(Cars)),
  assert_cars_on_intersection(UpdatedIntersection, Cars).

should_place_cars_on_intersection_test() ->
  {Intersection, _Cars} = input:load("test/util_test.intersection", "test/util_test.cars"),
  {UpdatedIntersection, Cars} = util:generate_cars_on(Intersection, 0.1),
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
  io:format(jsx:encode(Solution)),
  ?assertEqual(true, false).

generate_cars_test() ->
  {Intersection, _Cars} = input:load("input.intersection", "input.cars"),
  {_UpdatedIntersection, Cars} = util:generate_cars_on(Intersection, 0.25),
  io:format("~p~n", [Cars]),
  ?assertEqual(true, false).

convert_to_json_test() ->
  ?assertEqual(ok, util:convert_to_jsons(20)).

%%load_full_cars_definition_test() ->
%%  {Intersection, Cars} = input:load("input.intersection", "small_input.cars"),
%%  io:format(jsx:encode(Cars)),
%%  ?assertEqual(true, false).