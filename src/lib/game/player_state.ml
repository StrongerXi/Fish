type t =
  { color : Player_color.t
  ; score : int
  ; penguins : Penguin.t list
  }

let create color = { color; score = 0; penguins = [] }

let get_player_color t = t.color

let set_score t score =
  if score > 0
  then { t with score }
  else failwith "score must be non-negative"

let get_score t = t.score

let move_penguin t src dst =
  let rec update_penguin pgs =
    match pgs with
    | [] -> failwith "no penguin is at move source"
    | p::pgs ->
      if src == Penguin.get_position p
      then (Penguin.set_position p dst)::pgs
      else p::(update_penguin pgs)
  in
  { t with penguins = update_penguin t.penguins }

let add_penguin t p = { t with penguins = p::t.penguins }

(** Return positions of all penguins, in the order they were added *)
let get_penguin_positions t = List.map Penguin.get_position t.penguins