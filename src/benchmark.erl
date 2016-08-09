-module(benchmark).
%%-export([evaluate/3]).

-include("model.hrl").

-spec evaluate([lights()], integer(), float()) -> {float(), float(), float()}.
-export([bias_input/3]).

evaluate(Solution, Times, RandomizationChance) ->
  {Input, _SaveSimulationCourse} = traffic_ops:config(RandomizationChance),
  random:seed(erlang:now()),
  BiasedInput = bias_input(Input, RandomizationChance, length(Solution)),
  evaluate_times(Solution, BiasedInput, Times).

-spec evaluate_times([lights()], input(), integer()) -> tuple().
evaluate_times(Solution, Input, Times) ->
  {CollisionsNo, Min, Total, Max} = evaluate_times(Solution, Input, Times, {0, 5000, 0.0, -1000.0}),
  {CollisionsNo, Min, Total/Times, Max}.

evaluate_times(_Solution, _Input, 0, Result) ->
  Result;

evaluate_times(Solution, {Data, SaveSimulationCourse}, Times, {CollisionsNo, Min, Total, Max}) ->
  Fitness = evaluation_multilane:evaluate_solution(Solution, Data, SaveSimulationCourse),
  case Fitness < 0 of
    true ->
      evaluate_times(Solution, {Data, SaveSimulationCourse}, Times-1, {CollisionsNo+1, min(Min, Fitness), Total+Fitness, max(Fitness, Max)});
    false ->
      evaluate_times(Solution, {Data, SaveSimulationCourse}, Times-1, {CollisionsNo, min(Min, Fitness), Total+Fitness, max(Fitness, Max)})
  end.

-spec bias_input(input(), float(), pos_integer()) -> input().
bias_input({Intersection, Cars}, RandomizationChance, NumberOfSteps) ->
  bias_input(Cars, {Intersection, []}, RandomizationChance, NumberOfSteps).

-spec bias_input([car()], input(), float(), pos_integer()) -> input().
bias_input([], BiasedInput, _RandomizationChance, _NumberOfSteps) ->
  BiasedInput;

bias_input([Car | Cars], {Intersection, BiasedCars}, RandomizationChance, NumberOfSteps) ->
  Underperformance = calculate_underperformance(RandomizationChance, NumberOfSteps),
  {BiasedCar, BiasedIntersection} = try_move_back(Car, Underperformance, Intersection),
  bias_input(Cars, {BiasedIntersection, BiasedCars ++ [BiasedCar]}, RandomizationChance, NumberOfSteps).

-spec try_move_back(car(), non_neg_integer(), intersection()) -> tuple().
try_move_back(Car, Underperformance, BiasedIntersection) ->
  NewPosition = calculate_new_position(Car, Underperformance, BiasedIntersection),
  UpdatedIntersection = intersection:move_car(car:get_id(Car), car:get_position(Car), NewPosition, BiasedIntersection),
  {car:set_position(NewPosition, Car), UpdatedIntersection}.

-spec calculate_new_position(car(), non_neg_integer(), intersection()) -> position().
calculate_new_position(Car, Underperformance, Intersection) ->
  Position = car:get_position(Car),
  calculate_new_position(Position, maps:update(position_on_node, maps:get(position_on_node, Position)-1, Position), Underperformance-1, Intersection).

-spec calculate_new_position(position(), position(), integer(), intersection()) -> position().
calculate_new_position(Position, _EvaluatedPosition, -1, _Intersection) ->
  Position;

calculate_new_position(Position, EvaluatedPosition, Underperformance, Intersection) ->
  case is_position_possible(EvaluatedPosition, Intersection) of
    true ->
      calculate_new_position(EvaluatedPosition, maps:update(position_on_node, maps:get(position_on_node, EvaluatedPosition)-1, EvaluatedPosition), Underperformance-1, Intersection);
    false ->
      Position
  end.

-spec is_position_possible(position(), intersection()) -> boolean().
is_position_possible(Position, Intersection) ->
  case length(intersection:get_cars_on(Position, Intersection))>0 of
    true ->
      false;
    false ->
      maps:get(position_on_node, Position) > 0
  end.

-spec calculate_underperformance(float(), pos_integer()) -> non_neg_integer().
calculate_underperformance(RandomizationChance, NumberOfSteps) ->
  calculate_underperformance(RandomizationChance, NumberOfSteps, 0).

-spec calculate_underperformance(float(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
calculate_underperformance(_RandomizationChance, 0, Result) ->
  Result;

calculate_underperformance(RandomizationChance, Steps, Result) ->
  case random:uniform() < RandomizationChance of
    true ->
      calculate_underperformance(RandomizationChance, Steps-1, Result+1);
    false ->
      calculate_underperformance(RandomizationChance, Steps-1, Result)
  end.