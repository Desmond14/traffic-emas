-module(traffic_bin_ops).

-behaviour(emas_genetic_ops).

-export ([evaluation/2, mutation/2, recombination/3, solution/1, config/0]).

-include_lib("emas/include/emas.hrl").

-type sim_params() :: emas:sim_params().
-type solution() :: emas:solution(binary()).


%% @doc Generates a random solution, a list of 4-bit tuples
-spec solution(sim_params()) -> solution().
solution(#sim_params{problem_size = ProblemSize}) ->
    crypto:rand_bytes(ProblemSize div 2).


%% @doc Evaluates a given solution and returns a fitness value
-spec evaluation(solution(), sim_params()) -> float().
evaluation(Binary, #sim_params{extra = Data}) ->
    Solution = [{B1,B2,B3,B4} || <<B1:1,B2:1,B3:1,B4:1>> <= Binary],
    Fitness = evaluation:evaluate_solution(Solution, Data),
    float(Fitness).


%% @doc Crossover recombination in a random point
-spec recombination(solution(), solution(), sim_params()) -> {solution(), solution()}.
recombination(Sol1, Sol2, #sim_params{problem_size = ProblemSize}) ->
    CutPoint = random:uniform(ProblemSize div 2),
    <<S1a:CutPoint/binary, S1b/binary>> = Sol1,
    <<S2a:CutPoint/binary, S2b/binary>> = Sol2,
    {<<S1a/binary, S2b/binary>>, <<S2a/binary, S1b/binary>>}.


%% @doc Mutates (flips) genes at random indexes
-spec mutation(solution(), sim_params()) -> solution().
mutation(Solution, #sim_params{mutation_rate = MutationRate}) ->
    << << case random:uniform() < MutationRate of
              true -> flip_gene(Gene);
              false -> Gene
          end >> || <<Gene>> <= Solution >>.


%% @doc Loads the data that will be optimized
-spec config() -> term().
config() ->
    input_data:load_data().

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Flips the whole gene
-spec flip_gene(byte()) -> byte().
flip_gene(Byte) ->
    Byte bxor 255.
