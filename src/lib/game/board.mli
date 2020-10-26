(** A [t] represents the hexagon board in a fish game.
    The graphical interpretatio for positions (row, col) is as follows 
    ------        ------
   /(0, 0)\------/(0, 1)\
   \------/(1, 0)\------/
   /(2, 0)\------/(2, 1)\
   \------/(3, 0)\------/
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

module Config : sig
  (** A [t] represents configuration for a board. With the following paramters:
      - Width and height.
      - position of holes (removed tiles) on board. (ignored if out of bound)
      - minimum number of tiles with exactly 1 fish. (might not be enforced)
      - default number of fish on a tile. *)
  type t

  (** Create a configuration with given [width] and [height] *)
  val create : height:int -> width:int -> t
  (* NOTE input config is passed at the end to enable chaining *)
  val set_width  : int -> t -> t
  val get_width  : t -> int
  val set_height : int -> t -> t
  val get_height : t -> int
  val set_holes  : Position.t list -> t -> t
  val get_holes  : t -> Position.t list
  val set_min_num_of_one_fish_tile : int -> t -> t
  val get_min_num_of_one_fish_tile : t -> int
  val set_default_num_of_fish : int -> t -> t
  val get_default_num_of_fish : t -> int
end 


(** Create a board with given configuration 
    Error if any dimenson is non-positive. *)
val create : Config.t -> t

val get_width  : t -> int
val get_height : t -> int

(** Whether the given position is within the board, i.e., a valid position  *)
val within_board : t -> Position.t -> bool

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

(** Discouraged unless you have good reason and know what you are doing
    width is determined by the longest row. Short rows will be filled with hole
    tiles. *)
val from_tiles : Tile.t list list -> t
