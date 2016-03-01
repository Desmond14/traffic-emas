-module(normalization_test).

-import(normalization, [normalize_intersection/1]).

-include_lib("eunit/include/eunit.hrl").

should_add_missing_cars_on_key_value_pairs_test() ->
  NormalizedIntersection = normalize_intersection(util:evaluate_file("test/normalization_test.intersection")),
  ?assertEqual([], intersection:get_cars_on(#{node_id=>1, position_on_node=>1}, NormalizedIntersection)).

should_not_change_existing_cars_on_key_value_pairs_test() ->
  NormalizedIntersection = normalize_intersection(util:evaluate_file("test/normalization_test.intersection")),
  ?assertEqual([1,3], intersection:get_cars_on(#{node_id=>8, position_on_node=>1}, NormalizedIntersection)).

should_add_missing_incoming_nodes_key_value_pairs_test() ->
  NormalizedIntersection = normalize_intersection(util:evaluate_file("test/normalization_test.intersection")),
  ?assertEqual([], intersection:get_incoming_nodes(1, NormalizedIntersection)).

should_not_change_existing_incoming_nodes_key_value_pairs_test() ->
  NormalizedIntersection = normalize_intersection(util:evaluate_file("test/normalization_test.intersection")),
  ?assertEqual([1,5], intersection:get_incoming_nodes(4, NormalizedIntersection)).
