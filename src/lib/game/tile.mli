(** A [t] represents a tile on the fish game board 
    NOTE that it's immutable *)
type t

(** The empty tile. NOTE empty tile always has 0 fish *)
val empty_tile : t

(** Create a tile with given # of fish on it.
    Errors if input is not positive *)
val create : int -> t

(** Return the # of fish on given tile, 0 if it's empty *)
val get_fish : t -> int

(** Return whether the tile is empty, i.e., removed *)
val is_empty : t -> bool
