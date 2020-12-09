(** A [t] represents a serialized object *)
type t


(* NOTE some functions are for integration testing purposes 
   to_* functions always return None if [t] is malformed *)

(* Game state, source and destination positions of a penguin move *)
val to_move_resp_query : t -> 
  ((Game_state.t * Position.t * Position.t), string) result

(* board dimension (# of row, # of col), players with names and search depth for
 * minimax strategy, and default # of fish on tile *)
val to_game_description : t -> 
  ((int * int * Player.t list * int), string) result

val from_pos : Position.t -> t
val to_pos   : t -> (Position.t, string) result

val from_color : Player_state.Player_color.t -> t
val to_color   : t -> (Player_state.Player_color.t, string) result

val from_action : Action.t -> t
val to_action   : t -> (Action.t, string) result

val from_board_posn : (Board.t * Position.t) -> t
val to_board_posn   : t -> ((Board.t * Position.t), string) result

val from_game_state : Game_state.t -> t
val to_game_state   : t -> (Game_state.t, string) result

val from_list : 'a list -> ('a -> t) -> t
val to_list   : t -> (t -> 'a) -> ('a list, string) result

val from_string : string -> t
val to_string   : t -> string option

val from_bool : bool -> t
val to_bool   : t -> bool option

val from_int : int -> t
val to_int   : t -> int option

val from_json_string : string -> t option
val to_json_string : t -> string

val stream_from_channel : in_channel -> t Stream.t
