(** A [t] represents configuration for a board. With the following paramters:
    - Width and height.
    - position of holes (removed tiles) on board. (ignored if out of bound)
    - minimum number of tiles with exactly 1 fish. (might not be enforced)
    - default number of fish on a tile.
*)
type t

(** Create a configuration with given [width] and [height] *)
val create : height:int -> width:int -> t
(* NOTE input config is passed at the end to allow chaining *)
val set_width  : int -> t -> t
val get_width  : t -> int
val set_height : int -> t -> t
val get_height : t -> int
val set_holes  : Position.t list -> t -> t
val get_holes  : t -> Position.t list
val set_min_num_of_one_fish_tile : int -> t -> t
val get_min_num_of_one_fish_tile : int -> t
val set_default_num_of_fish : int -> t -> t
val get_default_num_of_fish : t -> int
