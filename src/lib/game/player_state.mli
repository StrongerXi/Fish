(** A [t] represents the state of a player in a fish game, including
    - color
    - score (non-negative)
    - all of its penguins
    NOTE that it's immutable
*)
type t

val create : Player_color.t -> t
val get_player_color : t -> Player_color.t

(** Errors if score is negative *)
val set_score : t -> int -> t
val get_score : t -> int

(** Move the penguin at 1st position to the 2nd position.
    Return None if player has no penguin at 1st position *)
val move_penguin : t -> Position.t -> Position.t -> t option
val add_penguin : t -> Penguin.t -> t

(** Return all penguins owned by [t], reverse to the order they were added *)
val get_penguins : t -> Penguin.t list
