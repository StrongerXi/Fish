(** A [t] represents the root node of a (sub) game tree.
    It includes:
      - a game state.
      - knowledge of current player.
    The module provides functionality to generate subsequent game nodes.
    NOTE that it's immutable *)
type t

(** Errors if no player in given state has given color
    REQUIRES: player turn ordering stays unchanged in this game tree *)
val create : Game_state.t -> Player_color.t -> t

val get_state : t -> Game_state.t
val get_current_player : t -> Player_color.t

(** Return a list that associates each of the legal action from current player
    to the resulting state from that action. An empty list means this is a leaf
    node. *)
val get_subtrees : t -> (Action.t * t) list
