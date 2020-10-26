(** A [t] represents the root node of a (sub) game tree.
    It includes:
      - a game state.
      - subsequent game states and the actions to get there (lazily generated)
    NOTE that it's immutable *)
type t
module Player_color = Player_state.Player_color

(** Create a game tree starting from the given state *)
val create : Game_state.t -> t

val get_state : t -> Game_state.t

(** Return a list that associates each of the legal action from current player
    in the game state to the resulting state from that action. 
    An empty list means this is a leaf node. *)
val get_subtrees : t -> (Action.t * t) list
