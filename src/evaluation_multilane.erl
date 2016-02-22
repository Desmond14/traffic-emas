-module(evaluation_multilane).
-export([evaluate_solution/2]).

-include("model.hrl").

-type gene() :: #{node_id()=>trit()}.
-type solution() :: [gene()].

-spec evaluate_solution(solution(), input()) -> float().
evaluate_solution(Solution, {Intersection, Cars}) ->
  time_loop(Solution, {Intersection, Cars}, 0).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Main loop which iterates through the solution genes
-spec time_loop(solution(), input(), float()) -> float().
time_loop([], _, Result) ->
  Result;

time_loop([Lights | Solution], Data, Result) ->
  {UpdatedData, Moves} = move_cars(Lights, Data),
  time_loop(Solution, UpdatedData, Result + Moves).

%% @doc Launches the loop that moves all cars in given time step
-spec move_cars(gene(), input()) -> {input(), float()}.
move_cars(Lights, InitialData) ->
  {Intersection, Cars} = InitialData,
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

