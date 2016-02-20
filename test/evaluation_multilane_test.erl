-module(evaluation_multilane_test).

-define(MAX_INT, 134217727).

-include_lib("eunit/include/eunit.hrl").

move_cars_test() ->
  AllReds = #{4=>1, 5=>1, 8=>1, 9=>1},
  Lights = [AllReds, AllReds, AllReds, AllReds, AllReds],
  Data = input:load_data(),
  ?assertEqual(4*4, evaluation_multilane:evaluate_solution(Lights, Data)).

move_cars2_test() ->
  GreenOnlyForCarId1 = #{4=>0, 5=>1, 8=>0, 9=>1},
  Lights = [GreenOnlyForCarId1, GreenOnlyForCarId1, GreenOnlyForCarId1, GreenOnlyForCarId1, GreenOnlyForCarId1],
  Data = input:load_data(),
  ?assertEqual(25, evaluation_multilane:evaluate_solution(Lights, Data)).

move_cars3_test() ->
  GreenPartiallyForCarId3 = #{4=>2, 5=>0, 8=>1, 9=>1},
  Lights = [GreenPartiallyForCarId3, GreenPartiallyForCarId3, GreenPartiallyForCarId3, GreenPartiallyForCarId3, GreenPartiallyForCarId3],
  Data = input:load_data(),
  ?assertEqual(18, evaluation_multilane:evaluate_solution(Lights, Data)).


