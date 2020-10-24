(** A [t] represents the color of player and their penguins in a Fish game *)
type t =
  | Red
  | Brown
  | Black
  | White
[@@deriving compare]

let compare = [%compare: t]
