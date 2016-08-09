-module(collision_test).

-include_lib("eunit/include/eunit.hrl").

should_return_false_for_empty_car_lists_test() ->
  Intersection = input:load_intersection_definition("test/collision/test.intersection"),
  ?assertEqual(false, collision:collision_occured({Intersection, []}, [])).

should_detect_collision_on_intersection_when_cars_drive_to_the_same_cell_test() ->
  Input = input:load("test/collision/test3.intersection", "test/collision/test3.cars", 0.0),
  UpdatedCars = util:evaluate_file("test/collision/test3.cars_updated"),
  ?assertEqual(true, collision:collision_occured(Input, UpdatedCars)).

should_detect_collision_when_cars_cross_the_same_cell_test() ->
  Input = input:load("test/collision/test2.intersection", "test/collision/test2.cars", 0.0),
  UpdatedCars = util:evaluate_file("test/collision/test2.cars_updated"),
  ?assertEqual(true, collision:collision_occured(Input, UpdatedCars)).

should_detect_collision_when_faster_car_drives_on_slower_test() ->
  Input = input:load("test/collision/test4.intersection", "test/collision/test4.cars", 0.0),
  UpdatedCars = util:evaluate_file("test/collision/test4.cars_updated"),
  ?assertEqual(true, collision:collision_occured(Input, UpdatedCars)).

should_not_detect_collision_test() ->
  Input = input:load("test/collision/test5.intersection", "test/collision/test5.cars", 0.0),
  UpdatedCars = util:evaluate_file("test/collision/test5.cars_updated"),
  ?assertEqual(false, collision:collision_occured(Input, UpdatedCars)).