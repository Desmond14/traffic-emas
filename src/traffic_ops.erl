-module(traffic_ops).

-behaviour(emas_genetic_ops).

-export ([evaluation/2, mutation/2, recombination/3, solution/1, config/0]).

-include("model.hrl").
-include_lib("emas/include/emas.hrl").

-type sim_params() :: emas:sim_params().
-type solution() :: emas:solution([map()]).


%% @doc Generates a random solution, a list of 4-bit tuples
-spec solution(sim_params()) -> solution().
solution(#sim_params{problem_size = ProblemSize}) ->
  random:seed(erlang:now()),
    [generate_lights(random_triple())
     || _ <- lists:seq(1, ProblemSize)].

%% @doc Evaluates a given solution and returns a fitness value
-spec evaluation(solution(), sim_params()) -> float().
evaluation(Solution, #sim_params{extra = Data}) ->
    Fitness = evaluation_multilane:evaluate_solution(Solution, Data),
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
mutation(Solution, #sim_params{mutation_rate = MutRate}) ->
    [case random:uniform() < MutRate of
         true -> mutate_gene(Gene, MutRate);
         false -> Gene
     end || Gene <- Solution].


%% @doc Loads the data for which lights will be optimized
-spec config() -> term().
config() ->
    input:load("input.intersection", "input.cars").

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec random_triple() -> trit().
random_triple() ->
    random:uniform(3) - 1.

generate_lights(LightValue) ->
  Intersection = input:load_intersection_definition("basic.intersection"),
  generate_lights(maps:keys(Intersection), maps:new(), LightValue).

generate_lights([], Lights, _LightValue) ->
  Lights;

generate_lights([NodeId | Rest], Lights, LightValue) ->
  generate_lights(Rest, maps:put(NodeId, LightValue, Lights), LightValue).

-spec mutate_gene(lights(), float()) -> lights().
mutate_gene(Lights, MutRate) ->
  NodesToMutate = [NodeId || NodeId <- maps:keys(Lights), random:uniform() < MutRate],
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
          UpdatedLights = maps:update(NodeId, 0, Lights);
        false ->
          UpdatedLights = maps:update(NodeId, 2, Lights)
      end;
    _ ->
      UpdatedLights = maps:update(NodeId, 1, Lights)
  end,
  change_light(Rest, UpdatedLights).