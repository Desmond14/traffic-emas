-module(evaluation_multilane).
-export([evaluate_solution/4]).

-include("model.hrl").

-type gene() :: #{node_id()=>trit()}.
-type solution() :: [gene()].

%%-spec evaluate_solution(solution(), input(), boolean()) -> float().
evaluate_solution(Solution, {Intersection, Cars}, true, ProblemSize) ->
  save_solution(Solution),
  save_cars(Cars, integer_to_list(length(Solution))),
  time_loop(Solution, {Intersection, Cars}, ?INITIAL_FITNESS, true, ProblemSize);

evaluate_solution(Solution, Data, false, ProblemSize) ->
  time_loop(Solution, Data, ?INITIAL_FITNESS, false, ProblemSize).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Main loop which iterates through the solution genes
%%-spec time_loop(solution(), input(), float(), boolean()) -> float().
time_loop([], _, Result, _SaveSimulationCourse, _ProblemSize) ->
  Result;

time_loop([Lights | Solution], Data, Result, SaveSimulationCourse, ProblemSize) ->
  {UpdatedIntersection, UpdatedCars} = move_cars(Lights, Data),
  Penalty = count_cars_not_on_dest_lane(UpdatedCars)*math:pow(1.73, ProblemSize-length(Solution)),
  case SaveSimulationCourse of
    true ->
      io:format("Penalty: ~p~n", [Penalty]),
      case round(Penalty) of
        0 ->
          file:write_file("timetoleave.txt", io_lib:fwrite("~p\t", [20-length(Solution)]), [append]),
          case collision:collision_occured(Data, UpdatedCars) of
            true ->
              -500000 * (length(Solution) + 1);
            false ->
              time_loop(Solution, {UpdatedIntersection, UpdatedCars}, Result - Penalty, false, ProblemSize)
          end;
        _ ->
          case collision:collision_occured(Data, UpdatedCars) of
            true ->
              -500000 * (length(Solution) + 1);
            false ->
              time_loop(Solution, {UpdatedIntersection, UpdatedCars}, Result - Penalty, SaveSimulationCourse, ProblemSize)
          end
      end;
    false ->
      case collision:collision_occured(Data, UpdatedCars) of
        true ->
          -500000 * (length(Solution) + 1);
        false ->
          time_loop(Solution, {UpdatedIntersection, UpdatedCars}, Result - Penalty, SaveSimulationCourse, ProblemSize)
      end
  end.

save_cars(Cars, Step) ->
  file:write_file(string:concat("result/step", Step), io_lib:fwrite("~p.\n", [Cars])).

save_solution(Solution) ->
  file:write_file("result/solution", io_lib:fwrite("~p.\n", [Solution])).

%% @doc Launches the loop that moves all cars in given time step
-spec move_cars(gene(), input()) -> input().
move_cars(Lights, {Intersection, Cars}) ->
  move_one_car(Intersection, Intersection, Cars, [], Lights).

-spec move_one_car(intersection(), intersection(), [car()], [car()], gene()) -> input().
move_one_car(_InitialIntersection, UpdatedIntersection, [], UpdatedCars, _Lights) ->
  {UpdatedIntersection, UpdatedCars};

move_one_car(InitialIntersection, IntersectionToUpdate, [Car | CarsLeftToProcess], UpdatedCars, Lights) ->
  {UpdatedCar, _UpdatedIntersection} = algorithm:move(Car, InitialIntersection, Lights),
  UpdatedIntersection = update_intersection(Car, UpdatedCar, IntersectionToUpdate),
  case UpdatedCar of
    outside_intersection ->
      move_one_car(InitialIntersection, UpdatedIntersection, CarsLeftToProcess, UpdatedCars, Lights);
    _ ->
      move_one_car(InitialIntersection, UpdatedIntersection, CarsLeftToProcess, lists:append(UpdatedCars, [UpdatedCar]), Lights)
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
  length([Car || Car <- Cars, length(car:get_path_to_dest(Car)) > 0]).
