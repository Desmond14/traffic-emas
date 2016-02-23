-module(traffic_ops_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emas/include/emas.hrl").

should_mutate_test() ->
  SimParams = #sim_params{problem_size = 5, mutation_rate = 0.4},
  InitialSolution = traffic_ops:solution(SimParams),
  io:format("~p~n", [InitialSolution]),
  io:format("~p~n", [traffic_ops:mutation(InitialSolution, SimParams)]).
%%  ?assertEqual(false, true).

should_recombine_test() ->
  SimParams = #sim_params{problem_size = 5, mutation_rate = 0.4},
  InitialSolution = traffic_ops:solution(SimParams),
  InitialSolution2 = traffic_ops:solution(SimParams),
  io:format("~p~n~p~n", [InitialSolution, InitialSolution2]),
  io:format("~p~n", [traffic_ops:recombination(InitialSolution, InitialSolution2, SimParams)]),
%%  ?assertEqual(false, true).