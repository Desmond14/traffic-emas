-module(collision_test).

-include_lib("eunit/include/eunit.hrl").

should_return_false_for_empty_car_lists_test() ->
  Intersection = input:load_intersection_definition("basic.intersection"),
  ?assertEqual(false, collision:collision_occured({Intersection, []}, [])).

%%should_return_false_test() ->
%%  Intersection = input:load_intersection_definition("basic.intersection"),
%%  Cars = input:load_car_definitions("basic.cars"),
%%  UpdatedCars = [#{id=>1, position=>#{node_id=>1, position_on_node=>2}, path_to_dest=>[4,8,11]},
%%  Car2 = #{id=>2, position=>#{node_id=>6, position_on_node=>1}, velocity=>1, config=>CarConfig, path_to_dest=>[11]},
%%  Car3 = #{id=>3, position=>#{node_id=>7, position_on_node=>1}, velocity=>1, config=>CarConfig, path_to_dest=>[2]},
%%  Car4 = #{id=>4, position=>#{node_id=>12, position_on_node=>1}, velocity=>1, config=>CarConfig, path_to_dest=>[3]},
%%  [Car1, Car2, Car3, Car4].
%%  car:move_car()