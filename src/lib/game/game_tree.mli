(** A [t] represents the root node of a (sub) game tree.
    It includes a game state and knowledge of current player.
    The module provides functionality to generate subsequent game nodes.
    NOTE that it's immutable *)
type t

(** Errors if no player in given state has given color *)
val create : Game_state.t -> Player_color.t -> t

val get_state : t -> Game_state.t
val get_current_player : t -> Player_color.t

(** Construct a new node with player set to the next player *)
val skip_current_player : t -> t

(** Return a list that associates each of the legal action from current player
    to the resulting state from that action. *)
val get_next_nodes : t -> (Action.t * t) list
