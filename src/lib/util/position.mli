(** A [t] represents a position on the fish game board. Check @see 'game/board'
    for how a position is interpreted. *)
type t =
  { row : int
  ; col : int
  }

(** Creates a list of distinct positions (row, col) for
    0 <= row < [height] and 0 <= column < [width] *)
val create_positions_within : height:int -> width:int -> t list

(** Compare 2 positions lexicalgraphically.
    ex: (0, 0), (1, 2) -> -1 *)
val compare : t -> t -> int
