-module(traffic_ops).

-behaviour(emas_genetic_ops).

-export ([evaluation/2, mutation/2, recombination/3, solution/1, config/0]).

-include("model.hrl").
-include_lib("emas/include/emas.hrl").

-type sim_params() :: emas:sim_params().
-type solution() :: emas:solution([map()]).

-define(SAME_LIGHT_AT_LEAST_FOR, 2).
-define(SAME_LIGHT_AT_MOST_FOR, 20).
-define(MAX_MUTATION_RANGE, 5).

-spec solution(sim_params()) -> solution().
solution(#sim_params{problem_size = ProblemSize}) ->
  {{Intersection, _}, _} = config(),
  random:seed(erlang:now()),
  generate_solution(Intersection, ProblemSize).

%% @doc Evaluates a given solution and returns a fitness value
-spec evaluation(solution(), sim_params()) -> float().
evaluation(Solution, #sim_params{extra = Input}) ->
    {Data, SaveSimulationCourse} = Input,
    Fitness = evaluation_multilane:evaluate_solution(Solution, Data, SaveSimulationCourse),
    float(Fitness).

%% @doc Crossover recombination in a random point
-spec recombination(solution(), solution(), sim_params()) -> {solution(), solution()}.
recombination(S1, S2, #sim_params{problem_size = ProblemSize}) ->
    CutPoint = random:uniform(ProblemSize),
    {S1a, S1b} = lists:split(CutPoint, S1),
    {S2a, S2b} = lists:split(CutPoint, S2),
    {S1a ++ S2b, S2a ++ S1b}.

%% @doc Mutates genes at random indexes
-spec mutation(solution(), sim_params()) -> solution().
mutation(Solution, #sim_params{mutation_rate = MutRate, mutation_range = MutRan, problem_size = ProblemSize}) ->
  SemaphoreIds = maps:keys(lists:nth(1, Solution)),
  Mutations = [random_mutation(Id, ProblemSize) || Id <-SemaphoreIds, random:uniform() < MutRate],
  mutate(Solution, Mutations).

random_mutation(IdToMutate, ProblemSize) ->
  StartTimeStep = random:uniform(ProblemSize),
  EndTimeStep = min(ProblemSize, StartTimeStep + random:uniform(?MAX_MUTATION_RANGE) - 1),
  {IdToMutate, StartTimeStep, EndTimeStep, random_trit()}.

mutate(Solution, Mutations) ->
  [mutate_with(lists:nth(Idx, Solution), Idx, Mutations) || Idx <- lists:seq(1, length(Solution))].

mutate_with(Gene, _Idx, []) ->
  Gene;

mutate_with(Gene, Idx, [{IdToMutate, StartTimeStep, EndTimeStep, Trit} | Tail]) ->
  case Idx >= StartTimeStep andalso Idx =< EndTimeStep of
    true ->
      mutate_with(maps:put(IdToMutate, Trit, Gene), Idx, Tail);
    false ->
      mutate_with(Gene, Idx, Tail)
  end.

%% @doc Loads the data for which lights will be optimized
-spec config() -> input().
config() ->
  {input:load("input.intersection", "input.cars"), false}.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec random_trit() -> trit().
random_trit() ->
    random:uniform(3) - 1.

-spec mutate_gene(lights(), float()) -> lights().
mutate_gene(Lights, MutationRange) ->
  NodesToMutate = [NodeId || NodeId <- maps:keys(Lights), random:uniform() < MutationRange],
  change_light(NodesToMutate, Lights).


-spec change_light([node_id()], lights()) -> lights().
change_light([], Lights) ->
  Lights;

change_light([NodeId | Rest], Lights) ->
  CurrentLight = maps:get(NodeId, Lights),
  case CurrentLight of
    1 ->
      case random:uniform() < 0.5 of
        true ->
          change_light(Rest, maps:update(NodeId, 0, Lights));
        false ->
          change_light(Rest, maps:update(NodeId, 2, Lights))
      end;
    _ ->
      change_light(Rest, maps:update(NodeId, 1, Lights))
  end.

-spec generate_solution(intersection(), pos_integer()) -> solution().
generate_solution(Intersection, ProblemSize) ->
  InitialLightValue = random_trit(),
  InitialLightDuration = draw_light_duration(ProblemSize),
  SemaphoreIds = intersection:extract_ids(intersection:get_semaphores(Intersection)),
  PartialSolution = [create_lights(InitialLightValue, SemaphoreIds) || _ <- lists:seq(1, InitialLightDuration)],
  generate_solution(PartialSolution, SemaphoreIds, ProblemSize-InitialLightDuration, InitialLightValue).

-spec generate_solution(solution(), [node_id()], non_neg_integer(), trit()) -> solution().
generate_solution(Solution, _SemaphoreIds, 0, _PreviousLightValue) ->
  Solution;

generate_solution(PartialSolution, SemaphoreIds, LeftProblemSize, PreviousLight) ->
  LightValue = draw_next_light(PreviousLight),
  LightDuration = draw_light_duration(LeftProblemSize),
  NextSolutionPart = [create_lights(LightValue, SemaphoreIds) || _ <- lists:seq(1, LightDuration)],
  generate_solution(PartialSolution ++ NextSolutionPart, SemaphoreIds, LeftProblemSize-LightDuration, LightValue).

-spec create_lights(trit(), [node_id()]) -> lights().
create_lights(LightValue, SemaphoreIds) ->
  create_lights(SemaphoreIds, maps:new(), LightValue).

create_lights([], Lights, _LightValue) ->
  Lights;

create_lights([SemaphoreId | Rest], Lights, LightValue) ->
  create_lights(Rest, maps:put(SemaphoreId, LightValue, Lights), LightValue).

draw_light_duration(ProblemSize) ->
  min(ProblemSize, random:uniform(?SAME_LIGHT_AT_MOST_FOR-?SAME_LIGHT_AT_LEAST_FOR+1)+?SAME_LIGHT_AT_LEAST_FOR-1).

draw_next_light(PreviousLight) ->
  case PreviousLight of
    ?BOTH_RED ->
      case random:uniform() < 0.5 of
        true ->
          ?GREEN_FOR_HIGHER_ID;
        false ->
          ?GREEN_FOR_LOWER_ID
      end;
    _ ->
      ?BOTH_RED
  end.