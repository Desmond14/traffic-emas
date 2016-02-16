-module(nasch_test).

-include_lib("eunit/include/eunit.hrl").

should_accelerate_by_1_test() ->
  Intersection = input:load_intersection_definition(),
  Car = hd(input:load_car_definitions()),
  ?assertEqual(1, car:get_velocity(Car)),
  {UpdatedCar, _} = nasch:follow_nagel(Car, Intersection, Intersection, #{4=>1}),
  ?assertEqual(2, car:get_velocity(UpdatedCar)).

should_move_by_two_cells_test() ->
  Intersection = input:load_intersection_definition(),
  Car = hd(input:load_car_definitions()),
  ?assertEqual(1, car:get_velocity(Car)),
  {UpdatedCar, _} = nasch:follow_nagel(Car, Intersection, Intersection, #{4=>1}),
  #{position_on_node := NewPositionOnNode} = car:get_position(UpdatedCar),
  ?assertEqual(3, NewPositionOnNode).

should_add_on_new_position_test() ->
  Intersection = input:load_intersection_definition(),
  Car = hd(input:load_car_definitions()),
  {_, UpdatedIntersection} = nasch:follow_nagel(Car, Intersection, Intersection, #{4=>1}),
  ?assertEqual(true, lists:member(car:get_id(Car), intersection:get_cars_on(#{node_id=>1, position_on_node=>3}, UpdatedIntersection))).

should_remove_from_old_position_test() ->
  Intersection = input:load_intersection_definition(),
  Car = hd(input:load_car_definitions()),
  {_, UpdatedIntersection} = nasch:follow_nagel(Car, Intersection, Intersection, #{4=>1}),
  ?assertEqual(false, lists:member(car:get_id(Car), intersection:get_cars_on(#{node_id=>1, position_on_node=>1}, UpdatedIntersection))).

should_decelerate_by_1_test() ->
  Intersection = input:load_intersection_definition(),
  Car = hd(input:load_car_definitions()),
  UpdatedIntersection = intersection:add_car_on(5, #{node_id=>1, position_on_node=>2}, Intersection),
  {UpdatedCar, _} = nasch:follow_nagel(Car, UpdatedIntersection, UpdatedIntersection, #{4=>1}),
  ?assertEqual(0, car:get_velocity(UpdatedCar)).

should_not_change_position_test() ->
  Intersection = input:load_intersection_definition(),
  Car = hd(input:load_car_definitions()),
  UpdatedIntersection = intersection:add_car_on(5, #{node_id=>1, position_on_node=>2}, Intersection),
  {UpdatedCar, _} = nasch:follow_nagel(Car, UpdatedIntersection, UpdatedIntersection, #{4=>1}),
  #{position_on_node := NewPositionOnNode} = car:get_position(UpdatedCar),
  ?assertEqual(1, NewPositionOnNode).

should_keep_velocity_test() ->
  Intersection = input:load_intersection_definition(),
  Car = hd(input:load_car_definitions()),
  UpdatedIntersection = intersection:add_car_on(5, #{node_id=>1, position_on_node=>3}, Intersection),
  {UpdatedCar, _} = nasch:follow_nagel(Car, UpdatedIntersection, UpdatedIntersection, #{4=>1}),
  ?assertEqual(1, car:get_velocity(UpdatedCar)).

should_move_by_one_cell_test() ->
  Intersection = input:load_intersection_definition(),
  Car = hd(input:load_car_definitions()),
  UpdatedIntersection = intersection:add_car_on(5, #{node_id=>1, position_on_node=>3}, Intersection),
  {UpdatedCar, _} = nasch:follow_nagel(Car, UpdatedIntersection, UpdatedIntersection, #{4=>1}),
  #{position_on_node := NewPositionOnNode} = car:get_position(UpdatedCar),
  ?assertEqual(2, NewPositionOnNode).

should_not_accelerate_more_than_max_velocity_test() ->
  Intersection = input:load_intersection_definition(),
  CarConfig = #{max_velocity=>3, max_acceleration=>4, max_deceleration=>2},
  Car = car:set_config(CarConfig, hd(input:load_car_definitions())),
  {UpdatedCar, _} = nasch:follow_nagel(Car, Intersection, Intersection, #{4=>1}),
  ?assertEqual(3, car:get_velocity(UpdatedCar)).

should_move_by_3_cells_test() ->
  Intersection = input:load_intersection_definition(),
  CarConfig = #{max_velocity=>3, max_acceleration=>4, max_deceleration=>2},
  Car = car:set_config(CarConfig, hd(input:load_car_definitions())),
  {UpdatedCar, _} = nasch:follow_nagel(Car, Intersection, Intersection, #{4=>1}),
  #{position_on_node := NewPositionOnNode} = car:get_position(UpdatedCar),
  ?assertEqual(4, NewPositionOnNode).

should_return_outside_intersection_test() ->
  CarConfig = #{max_velocity=>4, max_acceleration=>1, max_deceleration=>2},
  CarAtVeryEnd = #{id=>4, position=>#{node_id=>11, position_on_node=>5}, velocity=>2, config=>CarConfig, path_to_dest=>[]},
  UpdatedIntersection = intersection:add_car_on(5, #{node_id=>11, position_on_node=>5}, input:load_intersection_definition()),
  {UpdatedCar, _} = nasch:follow_nagel(CarAtVeryEnd, UpdatedIntersection, UpdatedIntersection, #{4=>1}),
  ?assertEqual(outside_intersection, UpdatedCar).