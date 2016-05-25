-module(util).
-export([evaluate_file/1, generate_cars_on/2, convert_to_jsons/1]).

-include("model.hrl").

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

-spec generate_cars_on(intersection(), float()) -> [car()].
generate_cars_on(Intersection, LanesCoverage) ->
  CarsToGenerate = round(LanesCoverage * intersection:calculate_lanes_capacity(Intersection)),
  generate_cars_on(Intersection, [], CarsToGenerate, 1).

-spec convert_to_jsons(integer()) -> any().
convert_to_jsons(ProblemSize) ->
  convert_steps(0, ProblemSize),
  convert_solution().

%% ====================================================================
%% Internal functions
%% ====================================================================

generate_cars_on(Intersection, Cars, 0, _CarId) ->
  {Intersection, Cars};

generate_cars_on(Intersection, GeneratedCars, CarsLeftToGenerate, CarId) ->
  Position = draw_unoccupied_position(Intersection),
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
  generate_cars_on(UpdatedIntersection, [Car | GeneratedCars], CarsLeftToGenerate-1, CarId + 1).

draw_unoccupied_position(Intersection) ->
  Lanes = intersection:get_lanes(Intersection),
  RandomLane = lists:nth(random:uniform(length(Lanes)), Lanes),
  LaneId = intersection:get_node_id(RandomLane),
  PositionOnLane = random:uniform(intersection:get_node_length(LaneId, Intersection)) + 1,
  RandomPosition = intersection:new_position(LaneId, PositionOnLane),
  case intersection:get_cars_on(RandomPosition, Intersection) of
    [] ->
      RandomPosition;
    _ ->
      draw_unoccupied_position(Intersection)
  end.

convert_steps(StepNo, MaxStepNo) when StepNo =< MaxStepNo ->
  InputFilename = "result/step" ++ integer_to_list(StepNo),
  ct:pal("~p~n", [InputFilename]),
  OutputFilename = InputFilename ++ ".json",
  ct:pal("~p~n", [OutputFilename]),
  file:write_file(OutputFilename, jsx:encode([util:evaluate_file(InputFilename)])),
  convert_steps(StepNo+1, MaxStepNo);

convert_steps(_StepNo, _MaxStepNo) ->
  ok.

convert_solution() ->
  InputFilename = "result/solution",
  OutputFilename =  "result/solution.json",
  file:write_file(OutputFilename, jsx:encode([util:evaluate_file(InputFilename)])).
