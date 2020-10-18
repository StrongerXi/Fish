(** A [t] represents state of all the players in a fish game, including their
    turn order
    It's agnostic to the board (boundary, etc.)
    NOTE that it's immutable *)
type t

(** Create a [t] with 1 player for each of the given colors.
    Errors if there exists any duplicates *)
val create : Player_color.t list -> t

(** Move the penguin at 1st position to the 2nd position, and update player
    score based on # of fish the penguin is holding. The penguin will then
    hold the given # of fish.
    Errors if no penguin is at source position *)
val move_penguin : t -> Position.t -> Position.t -> int -> t

(** Place a new penguin with given color at given position on the board.
    Errors if the no the participating player has given color *)
val place_penguin : t -> Player_color.t -> Position.t -> t

(** Get states of all the players, ordered by how they take turns *)
val get_ordered_players : t -> Player_state.t list
