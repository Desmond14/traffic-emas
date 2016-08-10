-module(benchmark).
-export([test/5]).

-include("model.hrl").

test(Time, ProblemSize, CarsNumber, RandomizationChance, Options) ->
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
  {_, SimpleLightsFitness} = find_best_matching_solution(util:evaluate_file("simple_solutions.txt"), {BiasedIntersection, BiasedCars}, {nil, -10000.0}),
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
  {Intersection, Cars} = BiasedInput,
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