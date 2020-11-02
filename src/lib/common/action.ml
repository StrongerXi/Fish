module Move = struct 
  (* origin and destination for a penguin move *)
  type t =
    { src : Position.t
    ; dst : Position.t
    }

  (** Compare 2 moves lexicalgraphically. *)
  let compare (m1 : t) (m2 : t) : int =
    let src_cmp = Position.compare m1.src m2.src in
    if src_cmp = 0 then Position.compare m1.dst m2.dst else src_cmp

  let to_string (m : t) : string =
    Printf.sprintf "[%s -> %s]" 
      (Position.to_string m.src) (Position.to_string m.dst)
end

(** A [t] represents one of the all possible action a player can submit to a
    referee, who will then validate and apply the action. *)
type t =
  | Move of Move.t
  | Skip (* Skip the current turn *)

let to_string (t : t) : string =
  match t with
  | Skip -> "[skip]"
  | Move(m) -> Move.to_string m
