-module(input_test).

-include_lib("eunit/include/eunit.hrl").

load1_test() ->
  Result = input:load_intersection_definition("basic.intersection"),
  ?assertEqual(maps:size(Result), 12).

load2_test() ->
  Result = input:load_car_definitions("basic.cars"),
  ?assertEqual(length(Result), 4).
