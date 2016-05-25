-module(traffic_ops_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emas/include/emas.hrl").

should_mutate_test() ->
  SimParams = #sim_params{problem_size = 5, mutation_rate = 0.4},
  InitialSolution = traffic_ops:solution(SimParams),
  io:format("~p~n", [InitialSolution]),
  io:format("~p~n", [traffic_ops:mutation(InitialSolution, SimParams)]).


should_recombine_test() ->
  SimParams = #sim_params{problem_size = 10, mutation_rate = 0.4},
  InitialSolution = traffic_ops:solution(SimParams),
  InitialSolution2 = traffic_ops:solution(SimParams),
%%  {ok, SolutionFile} = file:open("/home/slakomy/ErlangProjects/solution.json", [write, binary]),
%%  EncodedSolution = jsx:encode(InitialSolution),
%%  file:write(SolutionFile, EncodedSolution),
%%  file:close(SolutionFile),
  ct:pal("~p~n~p~n", [InitialSolution, InitialSolution2]),
  ct:pal("~p~n", [traffic_ops:recombination(InitialSolution, InitialSolution2, SimParams)]).
%%  ?assertEqual(1, 2).

should_generate_solution_test() ->
  SimParams = #sim_params{problem_size = 20},
  Solution = traffic_ops:solution(SimParams),
  ct:pal("~p~n", [Solution]),
  ?assertEqual(20, length(Solution)).