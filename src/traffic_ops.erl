-module(traffic_ops).

-behaviour(emas_genetic_ops).

-export ([evaluation/2, mutation/2, recombination/3, solution/1, config/0]).

-include("model.hrl").
-include_lib("emas/include/emas.hrl").

-type sim_params() :: emas:sim_params().
-type solution() :: emas:solution([map()]).

-define(SAME_LIGHT_AT_LEAST_FOR, 2).
-define(SAME_LIGHT_AT_MOST_FOR, 20).

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
mutation(Solution, #sim_params{mutation_rate = MutRate, mutation_range = MutRan}) ->
  [case random:uniform() < MutRate of
     true -> mutate_gene(Gene, MutRan);
     false -> Gene
   end || Gene <- Solution].


%% @doc Loads the data for which lights will be optimized
-spec config() -> input().
config() ->
  {input:load("input.intersection", "input.cars"), false}.

%% =============================================================================
%% Internal functions
%% =============================================================================

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
  InitialLightValue = draw_next_light(?BOTH_RED),
  SemaphoreIds = intersection:extract_ids(intersection:get_semaphores(Intersection)),
  generate_solution(InitialLightValue, SemaphoreIds, ProblemSize, []).

-spec generate_solution(trit(), [node_id()], non_neg_integer(), lights()) -> lights().
generate_solution(_InitialLightValue, _SemaphoreIds, 0, Lights) ->
  Lights;

generate_solution(InitialLightValue, SemaphoreIds, ProblemSize, Lights) ->
  LightCycle = create_lights_cycle(InitialLightValue, SemaphoreIds, ProblemSize),
  generate_solution(InitialLightValue, SemaphoreIds, ProblemSize-length(LightCycle), Lights ++ LightCycle).

-spec create_lights_cycle(trit(), [node_id()], non_neg_integer()) -> lights().
create_lights_cycle(InitialLightValue, SemaphoreIds, TotalLightsDuration) ->
  lists:sublist(
    create_lights(InitialLightValue, draw_light_duration(TotalLightsDuration), SemaphoreIds)
    ++ create_lights(?BOTH_RED, ?SAME_LIGHT_AT_LEAST_FOR, SemaphoreIds)
      ++ create_lights(opposite_light(InitialLightValue), draw_light_duration(TotalLightsDuration), SemaphoreIds)
      ++ create_lights(?BOTH_RED, ?SAME_LIGHT_AT_LEAST_FOR, SemaphoreIds),
    TotalLightsDuration).

%%Possible optimization by creating lights configuration only one?
-spec create_lights(trit(), pos_integer(), [node_id()]) -> lights().
create_lights(LightValue, LightDuration, SemaphoreIds) ->
  [create_lights_configuration(LightValue, SemaphoreIds) || _ <- lists:seq(1, LightDuration)].

create_lights_configuration(LightValue, SemaphoreIds) ->
  create_lights_configuration(LightValue, SemaphoreIds, maps:new()).

create_lights_configuration(_LightValue, [], Result) ->
  Result;

create_lights_configuration(LightValue, [NodeId | Rest], Result) ->
  create_lights_configuration(LightValue, Rest, maps:put(NodeId, LightValue, Result)).

draw_light_duration(ProblemSize) ->
  min(ProblemSize, random:uniform(?SAME_LIGHT_AT_MOST_FOR-?SAME_LIGHT_AT_LEAST_FOR+1)+?SAME_LIGHT_AT_LEAST_FOR-1).

opposite_light(Light) ->
  case Light of
    ?GREEN_FOR_HIGHER_ID -> ?GREEN_FOR_LOWER_ID;
    ?GREEN_FOR_LOWER_ID -> ?GREEN_FOR_HIGHER_ID
  end.

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