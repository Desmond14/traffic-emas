-module(input).
-export([load_intersection_definition/1, load_car_definitions/1, load/2]).

-include("model.hrl").

-spec load(string(), string()) -> input().
load(IntersectionFilename, CarsFilename) ->
  Intersection = convert_to_map(load_intersection_definition(IntersectionFilename)),
  NormalizedIntersection = intersection:normalize(Intersection),
  Cars = convert_paths_to_full_paths(load_car_definitions(CarsFilename), Intersection),
  {add_cars_on(Cars, calculate_incoming_nodes(NormalizedIntersection)), Cars}.

load_intersection_definition(Filename) ->
  evaluate_file(Filename).

load_car_definitions(Filename) ->
  evaluate_file(Filename).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec convert_to_map([intersection_node()]) -> intersection().
convert_to_map(NodesDescription) ->
  convert_to_map(NodesDescription, maps:new()).

-spec convert_to_map([intersection_node()], intersection()) -> intersection().
convert_to_map([], Intersection) ->
  Intersection;

convert_to_map([NodeDescription | Rest], Intersection) ->
  convert_to_map(Rest, maps:put(intersection:get_node_id(NodeDescription), NodeDescription, Intersection)).

-spec calculate_incoming_nodes(intersection()) -> intersection().
calculate_incoming_nodes(Intersection) ->
  calculate_incoming_nodes(maps:values(Intersection), Intersection).

calculate_incoming_nodes([], Intersection) ->
  intersection:normalize_incoming_nodes(Intersection);

calculate_incoming_nodes([Node | Rest], Intersection) ->
  #{outcoming_nodes := OutcomingNodes, id := NodeId} = Node,
  calculate_incoming_nodes(Rest, intersection:add_incoming_nodes(NodeId, OutcomingNodes, Intersection)).

-spec add_cars_on([car()], intersection()) -> intersection().
add_cars_on([], Intersection) ->
  Intersection;

add_cars_on([Car | Cars], Intersection) ->
  CarId = car:get_id(Car),
  Position = car:get_position(Car),
  add_cars_on(Cars, intersection:add_car_on(CarId, Position, Intersection)).

%% @doc Replaces simple paths (with only lane ids) to full paths for each car.
%% Returns cars with paths updated.
-spec convert_paths_to_full_paths([car()], intersection()) -> [car()].
convert_paths_to_full_paths(Cars, Intersection) ->
  convert_one_car(Cars, [], Intersection).

-spec convert_one_car([car()], [car()], intersection()) -> [car()].
convert_one_car([], ConvertedCars, _Intersection) ->
  ConvertedCars;

convert_one_car(InitialCars, ConvertedCars, Intersection) ->
  CarToConvert = hd(InitialCars),
  ConvertedCar = car:set_path_to_dest(car:get_full_path(Intersection, CarToConvert), CarToConvert),
  convert_one_car(tl(InitialCars), lists:append(ConvertedCars, [ConvertedCar]), Intersection).

evaluate_file(Filename) ->
  ProjectPath = filename:dirname(filename:dirname(code:which(?MODULE))),
  FilePath = filename:join([ProjectPath, priv, Filename]),
  {ok, File} = file:read_file(FilePath),
  Content = unicode:characters_to_list(File),
  {ok,ErlTokens,_} = erl_scan:string(Content),
  {ok, ErlAbsForm} = erl_parse:parse_exprs(ErlTokens),
  {value,Value,_}=erl_eval:exprs(ErlAbsForm, erl_eval:new_bindings()),
  Value.