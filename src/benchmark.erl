-module(benchmark).
-export([run_tests/1]).

-include("model.hrl").

run_tests(Options) ->
  Time = get_option(Options, time),
  ProblemSize = get_option(Options, problem_size),
  CarsNumber = get_option(Options, cars_number),
  file:write_file("result.txt", io_lib:fwrite("~p\t",[CarsNumber]), [append]),
  RandomizationChance = get_option(Options, randomization_chance),
  Repetitions = get_option(Options, repetitions),
  SimpleLightsSolutions = util:evaluate_file("simple_solutions.txt"),
  run_tests(Time, ProblemSize, CarsNumber, RandomizationChance, Options, SimpleLightsSolutions, Repetitions, []),
  file:write_file("result.txt", io_lib:fwrite("\n",[]), [append]).

run_tests(_Time, _ProblemSize, _CarsNumber, _RandomizationChance, _Options, _SimpleLightsSolutions, 0, Result) ->
  Result;

run_tests(Time, ProblemSize, CarsNumber, RandomizationChance, Options, SimpleLightsSolutions, Repetitions, Result) ->
  FitnessRatio = test(Time, ProblemSize, CarsNumber, RandomizationChance, Options, SimpleLightsSolutions),
  file:write_file("result.txt", io_lib:fwrite("~.3f\t",[FitnessRatio]), [append]),
  run_tests(Time, ProblemSize, CarsNumber, RandomizationChance, Options, SimpleLightsSolutions, Repetitions-1, Result ++ []).

get_option(Options, OptionName) ->
  case proplists:get_value(OptionName, Options) of
    undefined ->
      erlang:exit("Missing argument");
    Option ->
      Option
  end.

test(Time, ProblemSize, CarsNumber, RandomizationChance, Options, SimpleLightsSolutions) ->
  random:seed(erlang:now()),
  Intersection = normalization:normalize_intersection(input:convert_to_map(input:load_intersection_definition("input.intersection"))),
  InitialInput = util:generate_cars_on(Intersection, {cars_number, CarsNumber}),
  io:format("Initial input: ~p~n", [InitialInput]),
  Solutions = prepare_and_evaluate_variants(InitialInput, RandomizationChance, CarsNumber, ProblemSize, Time, Options),
  BiasedInput = bias:bias_input(InitialInput, RandomizationChance, ProblemSize),
  {BiasedIntersection, BiasedCars} = BiasedInput,
  {BestSolution, BestFitness} = find_best_matching_solution(Solutions, BiasedInput, {nil, -10000.0}),
  OptimizedFitness = evaluation_multilane:evaluate_solution(BestSolution, {BiasedIntersection, input:add_randomization_chance(BiasedCars, RandomizationChance)}, false),
  io:format("Trying simple lights~n", []),
  {_, SimpleLightsFitness} = find_best_matching_solution(SimpleLightsSolutions, {BiasedIntersection, BiasedCars}, {nil, -10000.0}),
  io:format("Optimized fitness: ~p, Simple lights fitness: ~p, Best fitness without bias: ~p", [OptimizedFitness, SimpleLightsFitness, BestFitness]),
  SimpleLightsFitness / BestFitness.

find_best_matching_solution([], _Input, Best) ->
  Best;

find_best_matching_solution([Solution | Solutions], Input, {BestSolution, BestFitness}) ->
  Fitness = evaluation_multilane:evaluate_solution(Solution, Input, false),
  io:format("Fitness: ~p, BestFitness: ~p~n", [Fitness, BestFitness]),
  case Fitness > BestFitness of
    true ->
      find_best_matching_solution(Solutions, Input, {Solution, Fitness});
    false ->
      find_best_matching_solution(Solutions, Input, {BestSolution, BestFitness})
  end.

prepare_and_evaluate_variants(InitialInput, RandomizationChance, CarsNumber, ProblemSize, Time, Options) ->
  {_, Cars} = InitialInput,
  file:write_file("priv/input.cars", io_lib:fwrite("~p.\n", [denormalize(Cars)])),
  {Solution, Fitness, _Energy} = emas:start(Time, Options),
  io:format("Wyliczony fitness: ~p\n", [Fitness]),
  prepare_and_evaluate_variants(InitialInput, RandomizationChance, CarsNumber, ProblemSize, Time, Options, [Solution]).

prepare_and_evaluate_variants(_InitialInput, _RandomizationChance, 0, _ProblemSize, _Time, _Options, Result) ->
  Result;

prepare_and_evaluate_variants(InitialInput, RandomizationChance, VariantsLeft, ProblemSize, Time, Options, Result) ->
  BiasedInput = bias:bias_input(InitialInput, RandomizationChance, ProblemSize),
  {_Intersection, Cars} = BiasedInput,
  file:write_file("priv/input.cars", io_lib:fwrite("~p.\n", [denormalize(Cars)])),
  {Solution, Fitness, _Energy} = emas:start(Time, Options),
  io:format("Wyliczony fitness: ~p\n", [Fitness]),
  prepare_and_evaluate_variants(InitialInput, RandomizationChance, VariantsLeft-1, ProblemSize, Time, Options, Result ++ [Solution]).

denormalize(Cars) ->
  denormalize(Cars, []).

denormalize([], Result) ->
  Result;

denormalize([Car | Cars], Result) ->
  PathToDest = [lists:last(car:get_path_to_dest(Car))],
  UpdatedCar = maps:update(path_to_dest, PathToDest, maps:remove(randomization_chance, Car)),
  denormalize(Cars, Result ++ [UpdatedCar]).