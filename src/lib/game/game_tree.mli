(** A [t] represents the root node of a (sub) game tree.
    It includes:
      - a game state.
      - knowledge of current player.
    The module provides functionality to generate subsequent game nodes.
    NOTE that it's immutable *)
type t

(** Errors if no player in given state has given color *)
val create : Game_state.t -> Player_color.t -> t

val get_state : t -> Game_state.t
val get_current_player : t -> Player_color.t

(** Return a list that associates each of the legal action from current player
    to the resulting state from that action. *)
val get_next_nodes : t -> (Action.t * t) list

(** Skip players who can't make any legal move, until we reach a player that can
    move. 
    If no player can move, return empty list.
    Else the first tree in the returned list is the one whose current player can
    make a move (i.e., [get_next_nodes] will return non-empty result). The first
    trees in the list are the ones skipped, in reveresed order. *)
val skip_to_moveable_tree : t -> t list
