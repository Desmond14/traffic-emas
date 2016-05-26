-module(evaluation_multilane).
-export([evaluate_solution/3]).

-include("model.hrl").

-type gene() :: #{node_id()=>trit()}.
-type solution() :: [gene()].

-spec evaluate_solution(solution(), input(), boolean()) -> float().
evaluate_solution(Solution, Data, true) ->
  {_, Cars} = Data,
  save_solution(Solution),
  save_cars(Cars, integer_to_list(length(Solution))),
  time_loop(Solution, Data, ?INITIAL_FITNESS, true);

evaluate_solution(Solution, Data, false) ->
  time_loop(Solution, Data, ?INITIAL_FITNESS, false).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Main loop which iterates through the solution genes
-spec time_loop(solution(), input(), float(), boolean()) -> float().
time_loop([], _, Result, _SaveSimulationCourse) ->
  Result;

time_loop([Lights | Solution], Data, Result, SaveSimulationCourse) ->
  {UpdatedData, _Moves} = move_cars(Lights, Data),
  {_UpdatedIntersection, UpdatedCars} = UpdatedData,
  Penalty = count_cars_not_on_dest_lane(UpdatedCars),
  case SaveSimulationCourse of
    true ->
      save_cars(UpdatedCars, integer_to_list(length(Solution)));
    _ ->
      ok
  end,
  case collision:collision_occured(Data, UpdatedCars) of
    true ->
      -1000 * (length(Solution) + 1);
    false ->
      time_loop(Solution, UpdatedData, Result - Penalty, SaveSimulationCourse)
  end.

save_cars(Cars, Step) ->
  file:write_file(string:concat("result/step", Step), io_lib:fwrite("~p.\n", [Cars])).

save_solution(Solution) ->
  file:write_file("result/solution", io_lib:fwrite("~p.\n", [Solution])).

%% @doc Launches the loop that moves all cars in given time step
-spec move_cars(gene(), input()) -> {input(), float()}.
move_cars(Lights, {Intersection, Cars}) ->
  move_one_car(Intersection, Intersection, Cars, [], Lights, 0).

-spec move_one_car(intersection(), intersection(), [car()], [car()], gene(), integer()) -> {input(), integer()}.
move_one_car(_InitialIntersection, UpdatedIntersection, [], UpdatedCars, _Lights, Distance) ->
  {{UpdatedIntersection, UpdatedCars}, Distance};

move_one_car(InitialIntersection, IntersectionToUpdate, CarsToProcess, UpdatedCars, Lights, Distance) ->
  [Car | _] = CarsToProcess,
  {UpdatedCar, _UpdatedIntersection} = algorithm:move(Car, InitialIntersection, Lights),
  UpdatedIntersection = update_intersection(Car, UpdatedCar, IntersectionToUpdate),
  case UpdatedCar of
    outside_intersection ->
      move_one_car(InitialIntersection, UpdatedIntersection, tl(CarsToProcess), UpdatedCars, Lights, Distance + car:get_velocity(Car));
    _ ->
      move_one_car(InitialIntersection, UpdatedIntersection, tl(CarsToProcess), lists:append(UpdatedCars, [UpdatedCar]), Lights, Distance + car:get_velocity(UpdatedCar))
  end.

-spec update_intersection(optional_car(), car(), intersection()) -> intersection().
update_intersection(CarBeforeUpdate, UpdatedCar, IntersectionToUpdate) ->
  UpdatedIntersection = intersection:remove_car_from(car:get_id(CarBeforeUpdate), car:get_position(CarBeforeUpdate), IntersectionToUpdate),
  case UpdatedCar of
    outside_intersection ->
      UpdatedIntersection;
    _ ->
      intersection:add_car_on(car:get_id(UpdatedCar), car:get_position(UpdatedCar), UpdatedIntersection)
  end.

count_cars_not_on_dest_lane(Cars) ->
  count_cars_not_on_dest_lane(Cars, 0).

count_cars_not_on_dest_lane([], Result) ->
  Result;

count_cars_not_on_dest_lane([Car | Rest], Result) ->
  case length(car:get_path_to_dest(Car)) of
    0 ->
      count_cars_not_on_dest_lane(Rest, Result);
    _ ->
      count_cars_not_on_dest_lane(Rest, Result+1)
  end.