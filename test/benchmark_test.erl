-module(benchmark_test).

-include_lib("eunit/include/eunit.hrl").

debug_test() ->
  Options = [{time,18000},
    {genetic_ops,traffic_ops},
    {model,mas_sequential},
    {problem_size,5},
    {mutation_rate,0.2},
    {mutation_range,0.2},
    {initial_energy,10},
    {reproduction_threshold,11},
    {reproduction_transfer,5},
    {fight_transfer,10},
    {mutation_chance,0.75},
    {recombination_chance,0.3},
    {fight_number,2},
    {topology,mesh},
    {migration_probability,0.0001},
    {log_dir,standard_io},
    {islands,4},
    {population_size,100},
    {write_interval,1000},
    {arena_timeout,3000},
    {skel_workers,4},
    {skel_split_size,20},
    {skel_pull,enable},
    {cars_number,5},
    {randomization_chance,0.001}],
  benchmark:test(18000, 5, 5, 0.001, Options).