-module(evaluation_multilane_test).

-import(evaluation_multilane, [calculate_dist_to_semaphore/4, calculate_dist_to_car_ahead/2, calculate_dist_to_first_blocking_semaphore/3]).

-define(MAX_INT, 134217727).

-include_lib("eunit/include/eunit.hrl").

%%evaluation1_test() ->
%%  Solution = evaluate_solution().

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