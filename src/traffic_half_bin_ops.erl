-module(traffic_half_bin_ops).

-behaviour(emas_genetic_ops).

-export ([evaluation/2, mutation/2, recombination/3, solution/1, config/0]).

-include_lib("emas/include/emas.hrl").

-type sim_params() :: emas:sim_params().
-type solution() :: emas:solution(binary()).


%% @doc Generates a random solution, a list of 4-bit tuples
-spec solution(sim_params()) -> solution().
solution(#sim_params{problem_size = ProblemSize}) ->
    Solution = [{random_bit(), random_bit(), random_bit(), random_bit()}
                || _ <- lists:seq(1, ProblemSize)],
    erlang:term_to_binary(Solution).


%% @doc Evaluates a given solution and returns a fitness value
-spec evaluation(solution(), sim_params()) -> float().
evaluation(Binary, #sim_params{extra = Data}) ->
    Solution = erlang:binary_to_term(Binary),
    Fitness = evaluation:evaluate_solution(Solution, Data),
    float(Fitness).


%% @doc Crossover recombination in a random point
-spec recombination(solution(), solution(), sim_params()) -> {solution(), solution()}.
recombination(B1, B2, #sim_params{problem_size = ProblemSize}) ->
    {S1, S2} = {erlang:binary_to_term(B1), erlang:binary_to_term(B2)},
    CutPoint = random:uniform(ProblemSize),
    {S1a, S1b} = lists:split(CutPoint, S1),
    {S2a, S2b} = lists:split(CutPoint, S2),
    {erlang:term_to_binary(S1a ++ S2b), erlang:term_to_binary(S2a ++ S1b)}.


%% @doc Mutates (flips) genes at random indexes
-spec mutation(solution(), sim_params()) -> solution().
mutation(Binary, #sim_params{mutation_rate = MutRate}) ->
    Solution = erlang:binary_to_term(Binary),
    Mutated = [case random:uniform() < MutRate of
                   true -> flip_gene(Gene);
                   false -> Gene
               end || Gene <- Solution],
    erlang:term_to_binary(Mutated).


%% @doc Loads the data that will be optimized
-spec config() -> term().
config() ->
    input_data:load_data().

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec random_bit() -> 0 | 1.
random_bit() ->
    random:uniform(2) - 1.


%% @doc Flips the whole gene
-spec flip_gene(tuple()) -> tuple().
flip_gene({B1, B2, B3, B4}) ->
    {1 - B1, 1 - B2, 1 - B3, 1 - B4}.
