(** A [t] represents one of the all possible action a player can submit to a
    referee, who will then validate and apply the action. *)
type t =
  (* origin and destination for a penguin move *)
  | Move of Position.t * Position.t
  (* Skip the current turn *)
  | Skip
