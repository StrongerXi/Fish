(** A [t] represents a penguin in a fish game:
    - where it is
    - how many fish it's holding
    NOTE that it's immutable *)
type t

(** Creating a penguin at given position, holding no fish *)
val create : Position.t -> t

(** Errors if input is negative *)
val set_fish_held : t -> int -> t
val get_fish_held : t -> int

val set_position : t -> Position.t -> t
val get_position : t -> Position.t
