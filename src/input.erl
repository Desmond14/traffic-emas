-module(input).
-export([load_intersection_definition/0, load_car_definitions/0, load_data/0]).

-include("model.hrl").

-spec load_data() -> input().
load_data() ->
  {load_intersection_definition(), load_car_definitions()}.

%TODO: add function to load data
-spec load_intersection_definition() -> intersection().
load_intersection_definition() ->
  P1 = #{id=>1, type=>lane, incoming_nodes=>[], outcoming_nodes=>[4], cars_on=>#{1=>[1]}, next_lane_paths=>#{3=>[4,3], 11=>[4,8,11], 10=>[4,8,9,10]}, length=>5},
  P2 = #{id=>2, type=>lane, incoming_nodes=>[5], outcoming_nodes=>[], cars_on=>#{}, next_lane_paths=>maps:new(), length=>5},
  P8 = #{id=>3, type=>lane, incoming_nodes=>[4], outcoming_nodes=>[], cars_on=>#{}, next_lane_paths=>#{}, length=>5},
  S1 = #{id=>4, type=>semaphore, incoming_nodes=>[1,5], outcoming_nodes=>[3,8], cars_on=>#{}, length=>1},
  S2 = #{id=>5, type=>semaphore, incoming_nodes=>[6,9], outcoming_nodes=>[2,4], cars_on=>#{}, length=>1},
  P3 = #{id=>6, type=>lane, incoming_nodes=>[], outcoming_nodes=>[5], cars_on=>#{1=>[2]}, next_lane_paths=>#{2=>[5,2], 3=>[5,4,3], 11=>[5,4,8,11]}, length=>5},
  P7 = #{id=>7, type=>lane, incoming_nodes=>[], outcoming_nodes=>[8], cars_on=>#{1=>[3]}, next_lane_paths=>#{11=>[8,11], 10=>[8,9,10], 2=>[8,9,5,2]}, length=>5},
  S3 = #{id=>8, type=>semaphore, incoming_nodes=>[4,7], outcoming_nodes=>[9,11], cars_on=>#{}, length=>1},
  S4 = #{id=>9, type=>semaphore, incoming_nodes=>[8,12], outcoming_nodes=>[5,10], cars_on=>#{}, length=>1},
  P4 = #{id=>10, type=>lane, incoming_nodes=>[9], outcoming_nodes=>[], cars_on=>#{}, next_lane_paths=>#{}, length=>5},
  P6 = #{id=>11, type=>lane, incoming_nodes=>[8], outcoming_nodes=>[], cars_on=>#{}, next_lane_paths=>#{}, length=>5},
  P5 = #{id=>12, type=>lane, incoming_nodes=>[], outcoming_nodes=>[9], cars_on=>#{1=>[4]}, next_lane_paths=>#{10=>[9,10], 2=>[9,5,2], 3=>[9,5,4,3]}, length=>5},

  #{
    1=>P1, 2=>P2, 3=>P8, 4=>S1, 5=>S2, 6=>P3, 7=>P7, 8=>S3, 9=>S4, 10=>P4, 11=>P6, 12=>P5
  }.


-spec load_car_definitions() -> [car()].
load_car_definitions() ->
  CarConfig = #{max_velocity=>4, max_acceleration=>1, max_deceleration=>2},
  Car1 = #{id=>1, position=>#{node_id=>1, position_on_node=>1}, velocity=>1, config=>CarConfig, path_to_dest=>[11]},
  Car2 = #{id=>2, position=>#{node_id=>6, position_on_node=>1}, velocity=>1, config=>CarConfig, path_to_dest=>[11]},
  Car3 = #{id=>3, position=>#{node_id=>7, position_on_node=>1}, velocity=>1, config=>CarConfig, path_to_dest=>[2]},
  Car4 = #{id=>4, position=>#{node_id=>12, position_on_node=>1}, velocity=>1, config=>CarConfig, path_to_dest=>[3]},
  [Car1, Car2, Car3, Car4].