-module(bias_test).

-include_lib("eunit/include/eunit.hrl").

does_not_moves_car_on_position_one_test() ->
  Intersection = input:load_intersection_definition("basic.intersection"),
  Car = hd(input:load_car_definitions("basic.cars")),
  {UpdatedIntersection, [UpdatedCar|Cars]} = bias:bias_input({Intersection, [Car]}, 1.0, 5),
  ?assertEqual(1, car:get_position_on_node(UpdatedCar)).

moves_back_by_four_test() ->
  Intersection = input:load_intersection_definition("test/bias/test2.intersection"),
  Car5 = #{id=>5, position=>#{node_id=>2, position_on_node=>5}, velocity=>1, path_to_dest=>[11]},
  {UpdatedIntersection, [UpdatedCar|Cars]} = bias:bias_input({Intersection, [Car5]}, 1.0, 4),
  ?assertEqual(1, car:get_position_on_node(UpdatedCar)),
  CarsOn = intersection:get_cars_on(#{position_on_node=>1, node_id=>2}, UpdatedIntersection),
  ?assertEqual(5, hd(CarsOn)).

does_not_moves_car_back_on_other_car_test() ->
  Intersection = input:load_intersection_definition("test/bias/test1.intersection"),
  Car5 = #{id=>5, position=>#{node_id=>1, position_on_node=>4}, velocity=>1, path_to_dest=>[11]},
  {UpdatedIntersection, [UpdatedCar|Cars]} = bias:bias_input({Intersection, [Car5]}, 1.0, 10),
  ?assertEqual(2, car:get_position_on_node(UpdatedCar)),
  CarsOn = intersection:get_cars_on(#{position_on_node=>2, node_id=>1}, UpdatedIntersection),
  ?assertEqual(5, hd(CarsOn)).

should_return_all_cars_test() ->
  Intersection = input:load_intersection_definition("basic.intersection"),
  Cars = input:load_car_definitions("basic.cars"),
  {UpdatedIntersection, UpdatedCars} = bias:bias_input({Intersection, Cars}, 1.0, 5),
  ?assertEqual(4, length(UpdatedCars)).