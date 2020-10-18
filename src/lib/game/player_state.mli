(** A [t] represents the state of a player in a fish game, including
    - color
    - score
    - positions of all of its penguins
    NOTE that it's immutable *)
type t

val create : Player_color.t -> t
val move_penguin : t -> Position.t -> Position.t 
val get_penguin_positions : t -> Position.t list
val get_player_color : t -> Player_color.t
val get_score : t -> int
val set_score : t -> int -> t
