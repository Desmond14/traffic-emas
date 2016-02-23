-module(input).
-export([load_intersection_definition/1, load_car_definitions/1, load/2]).

-include("model.hrl").

-spec load(string(), string()) -> input().
load(IntersectionFilename, CarsFilename) ->
  Intersection = load_intersection_definition(IntersectionFilename),
  Cars = convert_paths_to_full_paths(load_car_definitions(CarsFilename), Intersection),
  {Intersection, Cars}.

load_intersection_definition(Filename) ->
  evaluate_file(Filename).

load_car_definitions(Filename) ->
  evaluate_file(Filename).

%% =============================================================================
%% Internal functions
%% =============================================================================

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