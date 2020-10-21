type t =
  { pos : Position.t
  }

let create pos = { pos }

let set_position _ pos = { pos }

let get_position p = p.pos
