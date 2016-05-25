-module(evaluation_multilane_test).


-include_lib("eunit/include/eunit.hrl").
-include("model.hrl").

move_cars_test() ->
  AllReds = #{4=>1, 5=>1, 8=>1, 9=>1},
  Lights = [AllReds, AllReds, AllReds, AllReds, AllReds],
  Data = input:load("test/evaluation_test.intersection", "test/evaluation_test.cars"),
  ?assertEqual(?INITIAL_FITNESS-5*4, evaluation_multilane:evaluate_solution(Lights, Data, false)).

move_cars2_test() ->
  GreenOnlyForCarId1 = #{4=>0, 5=>1, 8=>0, 9=>1},
  Lights = [GreenOnlyForCarId1, GreenOnlyForCarId1, GreenOnlyForCarId1, GreenOnlyForCarId1, GreenOnlyForCarId1],
  Data = input:load("test/evaluation_test.intersection", "test/evaluation_test.cars"),
%%  after third move car 1 is on destination lane
  ?assertEqual(?INITIAL_FITNESS-2*4-3*3, evaluation_multilane:evaluate_solution(Lights, Data, false)).


