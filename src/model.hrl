-author("slakomy").

-define(GREEN_FOR_LOWER_ID, 0).
-define(BOTH_RED, 1).
-define(GREEN_FOR_HIGHER_ID, 2).
-define(MAX_INT, 134217727).

-type trit() :: ?GREEN_FOR_LOWER_ID | ?BOTH_RED | ?GREEN_FOR_HIGHER_ID.
-type id() :: integer().
-type car_id() :: id().
-type node_id() :: id().
-type lights() :: #{node_id=>trit()}.
-type position() :: #{node_id=>node_id(), position_on_node=>non_neg_integer()}.
-type car_config() :: #{max_velocity=>pos_integer(), max_acceleration=>pos_integer(), max_deceleration=>pos_integer()}.
-type car() :: #{id=>car_id(), position=>position(), velocity=>non_neg_integer(), config=>car_config(), path_to_dest=>[node_id()]}.
-type intersection() :: map().
-type input() :: {intersection(), [car()]}.