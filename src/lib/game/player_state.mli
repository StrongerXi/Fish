(** A [t] represents the state of a player in a fish game, including
    - color
    - score
    - positions of all of its penguins
    NOTE that it's immutable 
*)
type t

val create : Player_color.t -> t
val get_player_color : t -> Player_color.t
val get_score : t -> int

(** Move the penguin at 1st position to the 2nd position, and update player
    score based on # of fish the penguin is holding. The penguin will then
    hold the given # of fish.
    Errors if player has no penguin at 1st position *)
val move_penguin : t -> Position.t -> Position.t -> t
val add_penguin : t -> Penguin.t -> t

(** Return positions of all penguins, in the order they were added *)
val get_penguin_positions : t -> Position.t list
