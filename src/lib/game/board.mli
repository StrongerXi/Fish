(** A [t] represents the hexagon board in a fish game.
    The graphical interpretatio for positions (row, col) is as follows 
    ------        ------
   /(0, 0)\------/(0, 2)\
   \------/(0, 1)\------/
   /(2, 0)\------/(2, 2)\
   \------/(2, 1)\------/
          \------/
    NOTE mutability is implementation dependent, and should not be assumed.
*)
type t


(** Create a board with given configuration *)
val create : Board_config.t -> t

(** Retrive the tile at given position.
    Errors if the position is out of bound *)
val get_tile_at : t -> Position.t -> Tile.t

(** Remove the tile at given position.
    Errors if the position is out of bound *)
val remove_tile_at : t -> Position.t -> t

(** NOTE if [t] is immutable, this is just identity function *)
val get_copy : t -> t
