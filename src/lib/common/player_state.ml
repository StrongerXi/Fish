
module Player_color = struct
  (** A [t] represents the color of player and their penguins in a Fish game *)
  type t =
    | Red
    | Brown
    | Black
    | White
  [@@deriving compare]
  let to_string = function
    | Red   -> "red"
    | Brown -> "brown"
    | Black -> "black"
    | White -> "white"
end

type t =
  { color : Player_color.t
  ; score : int
  ; penguins : Penguin.t list
  }

let create color = { color; score = 0; penguins = [] }
;;

let get_player_color t = t.color
;;

let set_score t score =
  if score > 0
  then { t with score }
  else failwith "score must be non-negative"
;;

let get_score t = t.score
;;

let move_penguin t src dst =
  let rec update_penguin pgs =
    match pgs with
    | [] -> None
    | p::pgs ->
      if src == Penguin.get_position p
      then Some ((Penguin.set_position p dst)::pgs)
      else Option.map (List.cons p) (update_penguin pgs)
  in
  Option.map (fun penguins -> { t with penguins }) @@ update_penguin t.penguins
;;

let add_penguin t p = { t with penguins = p::t.penguins }
;;

let get_penguins t = t.penguins
;;
