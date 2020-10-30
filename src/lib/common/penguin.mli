(** A [t] represents a penguin in a fish game, including:
    - where it is
    NOTE that it's immutable *)
type t

(** Creating a penguin at given position *)
val create : Position.t -> t

val set_position : t -> Position.t -> t
val get_position : t -> Position.t
