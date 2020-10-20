(** A [t] represents a tile on the fish game board , including:
    - whether it's empty
    - # of fish (positive, or 0 for empty tile)
    NOTE that it's immutable *)
type t

(** The empty tile. *)
val empty_tile : t

(** Create a tile with given # of fish on it.
    Errors if input is not positive *)
val create : int -> t

(** Return the # of fish on given tile, 0 if it's empty *)
val get_fish : t -> int

(** Return whether the tile is empty, i.e., removed *)
val is_empty : t -> bool
