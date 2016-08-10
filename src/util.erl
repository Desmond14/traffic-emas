-module(util).
-export([evaluate_file/1, generate_cars_on/2, convert_to_jsons/1, generate_simple_lights/2]).

-include("model.hrl").

-define(FIRST_CAR_ID, 1).

-spec evaluate_file(string()) -> any().
evaluate_file(Filename) ->
  ProjectPath = filename:dirname(filename:dirname(code:which(?MODULE))),
  FilePath = filename:join([ProjectPath, priv, Filename]),
  {ok, File} = file:read_file(FilePath),
  Content = unicode:characters_to_list(File),
  {ok,ErlTokens,_} = erl_scan:string(Content),
  {ok, ErlAbsForm} = erl_parse:parse_exprs(ErlTokens),
  {value,Value,_}=erl_eval:exprs(ErlAbsForm, erl_eval:new_bindings()),
  Value.

-spec generate_cars_on(intersection(), tuple()) -> input().
generate_cars_on(Intersection, {coverage, LanesCoverage}) ->
  CarsToGenerate = round(LanesCoverage * intersection:calculate_lanes_capacity(intersection:get_incoming_lanes(Intersection))),
  generate_cars_on(Intersection, CarsToGenerate, ?FIRST_CAR_ID);

generate_cars_on(Intersection, {cars_number, CarsNumber}) ->
  generate_cars_on(Intersection, CarsNumber, ?FIRST_CAR_ID).

-spec convert_to_jsons(integer()) -> any().
convert_to_jsons(ProblemSize) ->
  convert_steps(0, ProblemSize),
  convert_solution().

%% ====================================================================
%% Internal functions
%% ====================================================================


get_all_positions(Intersection) ->
  Lanes = intersection:get_incoming_lanes(Intersection),
  get_all_positions(Lanes, []).

get_all_positions([], Result) ->
  Result;

get_all_positions([H|T], Result) ->
  NodeId = intersection:get_node_id(H),
  get_all_positions(T, Result ++ [intersection:new_position(NodeId, Position) || Position <- lists:seq(1, maps:get(length, H))]).

generate_cars_on(Intersection, CarsToGenerate, CarId) ->
  PossiblePositions = get_all_positions(Intersection),
  generate_cars_on(Intersection, PossiblePositions, [], CarsToGenerate, CarId).

generate_cars_on(Intersection, _PossiblePositions, Cars, 0, _CarId) ->
  {Intersection, Cars};

generate_cars_on(Intersection, PossiblePositions, GeneratedCars, CarsLeftToGenerate, CarId) ->
  Position = draw_unoccupied_position(PossiblePositions),
  NextLaneIds = maps:keys(intersection:get_next_lane_paths(maps:get(node_id, Position), Intersection)),
  CarConfig = #{max_velocity=>4, max_acceleration=>1, max_deceleration=>2},
  case NextLaneIds of
    [] ->
      Car = car:new_car(CarId, Position, 1, CarConfig, []);
    _ ->
      RandomDest = lists:nth(random:uniform(length(NextLaneIds)), NextLaneIds),
      Car = car:new_car(CarId, Position, 1, CarConfig, [RandomDest])
  end,
  UpdatedIntersection = intersection:add_car_on(CarId, Position, Intersection),
  generate_cars_on(UpdatedIntersection, lists:delete(Position, PossiblePositions), [Car | GeneratedCars], CarsLeftToGenerate-1, CarId + 1).

draw_unoccupied_position(PossiblePositions) ->
  lists:nth(random:uniform(length(PossiblePositions)), PossiblePositions).

convert_steps(StepNo, MaxStepNo) when StepNo =< MaxStepNo ->
  InputFilename = "result/step" ++ integer_to_list(StepNo),
%%  ct:pal("~p~n", [InputFilename]),
  OutputFilename = InputFilename ++ ".json",
%%  ct:pal("~p~n", [OutputFilename]),
  file:write_file(OutputFilename, jsx:encode([util:evaluate_file(InputFilename)])),
  convert_steps(StepNo+1, MaxStepNo);

convert_steps(_StepNo, _MaxStepNo) ->
  ok.

convert_solution() ->
  InputFilename = "result/solution",
  OutputFilename =  "result/solution.json",
  file:write_file(OutputFilename, jsx:encode([util:evaluate_file(InputFilename)])).

generate_simple_lights(Times, ProblemSize) ->
  {{Intersection, _}, _} = traffic_ops:config(),
  generate_simple_lights(Times, ProblemSize, Intersection, []).

generate_simple_lights(0, _ProblemSize, _Intersection, Result) ->
  Result;

generate_simple_lights(Times, ProblemSize, Intersection, Result) ->
  generate_simple_lights(Times-1, ProblemSize, Intersection, Result ++ [traffic_ops:generate_solution(Intersection, ProblemSize)]).