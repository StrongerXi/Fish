(** A [t] represents the hexagon board in a fish game.
    The graphical interpretatio for positions (row, col) is as follows 
    ------        ------
   /(0, 0)\------/(0, 2)\
   \------/(0, 1)\------/
   /(2, 0)\------/(2, 2)\
   \------/(2, 1)\------/
          \------/
    NOTE mutability is implementation dependent, and should not be assumed.
    Effectful function always return the updated object (mutable or not)
*)
type t

module Direction : sig
  type t =
    | North
    | South
    | Northeast
    | Southeast
    | Northwest
    | Southwest
end

(** Create a board with given configuration *)
val create : Board_config.t -> t

val get_width  : t -> int
val get_height : t -> int

(** Retrive the tile at given position.
    Errors if the position is out of bound *)
val get_tile_at : t -> Position.t -> Tile.t

(** Remove the tile at given position.
    Errors if the position is out of bound *)
val remove_tile_at : t -> Position.t -> t

(** Return all positions reachable from given position on the board via straight
    lines following each direction in [Direction.t]. Return a list that
    associates each direction with the reachable positions ordered by their
    distance from origin *)
val get_reachable_from : t -> Position.t -> (Direction.t * Position.t list) list

(** NOTE if [t] is immutable, this is just identity function *)
val get_copy : t -> t
