open Game

(** A [t] represents a serialized object *)
type t

(* NOTE some functions are for integration testing purposes 
   to_* functions always return None if [t] is malformed *)
val from_board_posn : (Board.t * Position.t) -> t
val to_board_posn   : t -> (Board.t * Position.t) option

val from_game_state : Game_state.t -> t
val to_game_state   : t -> Game_state.t option

val from_string : string -> t option
val to_string : t -> string

val stream_from_channel : in_channel -> t Stream.t
