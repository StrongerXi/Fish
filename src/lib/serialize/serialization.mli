(** A [t] represents a serialized object *)
type t


(* NOTE some functions are for integration testing purposes 
   to_* functions always return None if [t] is malformed *)

(* Game state, source and destination positions of a penguin move *)
val to_move_resp_query : t -> 
  ((Game_state.t * Position.t * Position.t), string) result
val from_action : Action.t -> t

val from_board_posn : (Board.t * Position.t) -> t
val to_board_posn   : t -> ((Board.t * Position.t), string) result

val from_game_state : Game_state.t -> t
val to_game_state   : t -> (Game_state.t, string) result

val from_string : string -> t option
val to_string : t -> string

val stream_from_channel : in_channel -> t Stream.t
