-module(normalization).
-export([normalize_intersection/1]).

-include("model.hrl").

%% @doc Adds missing cars_on and and incoming_nodes keys with empty values where missing.
%% Does not overwrite existing key-value pairs.
-spec normalize_intersection(intersection()) -> intersection().
normalize_intersection(Intersection) ->
  WithCarsOn = normalize_cars_on(normalize_incoming_nodes(Intersection)),
  normalize_incoming_nodes(WithCarsOn).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec normalize_cars_on(intersection()) -> intersection().
normalize_cars_on(Intersection) ->
  normalize_cars_on(maps:keys(Intersection), Intersection).

-spec normalize_cars_on([node_id()], intersection()) -> intersection().
normalize_cars_on([], Intersection) ->
  Intersection;

normalize_cars_on([NodeId | Rest], Intersection) ->
  Node = maps:get(NodeId, Intersection),
  case maps:is_key(cars_on, Node) of
    true ->
      normalize_cars_on(Rest, Intersection);
    false ->
      UpdatedNode = maps:put(cars_on, #{}, Node),
      normalize_cars_on(Rest, maps:put(NodeId, UpdatedNode, Intersection))
  end.

-spec normalize_incoming_nodes(intersection()) -> intersection().
normalize_incoming_nodes(Intersection) ->
  normalize_incoming_nodes(maps:keys(Intersection), Intersection).

-spec normalize_incoming_nodes([node_id()], intersection()) -> intersection().
normalize_incoming_nodes([], Intersection) ->
  Intersection;

normalize_incoming_nodes([NodeId | Rest], Intersection) ->
  Node = maps:get(NodeId, Intersection),
  case maps:is_key(incoming_nodes, Node) of
    true ->
      normalize_incoming_nodes(Rest, Intersection);
    false ->
      UpdatedNode = maps:put(incoming_nodes, [], Node),
      normalize_incoming_nodes(Rest, maps:put(NodeId, UpdatedNode, Intersection))
  end.