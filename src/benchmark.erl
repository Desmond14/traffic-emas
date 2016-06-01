-module(benchmark).
-export([evaluate/3]).

-include("model.hrl").

-spec evaluate([lights()], integer(), float()) -> {float(), float(), float()}.
evaluate(Solution, Times, RandomizationChance) ->
  Input = traffic_ops:config(RandomizationChance),
  evaluate_times(Solution, Input, Times).

-spec evaluate_times([lights()], input(), integer()) -> tuple().
evaluate_times(Solution, Input, Times) ->
  {Min, Total, Max} = evaluate_times(Solution, Input, Times, {5000, 0.0, -1000.0}),
  {Min, Total/Times, Max}.

evaluate_times(_Solution, _Input, 0, Result) ->
  Result;

evaluate_times(Solution, {Data, SaveSimulationCourse}, Times, {Min, Total, Max}) ->
  Fitness = evaluation_multilane:evaluate_solution(Solution, Data, SaveSimulationCourse),
  evaluate_times(Solution, {Data, SaveSimulationCourse}, Times-1, {min(Min, Fitness), Total+Fitness, max(Fitness, Max)}).