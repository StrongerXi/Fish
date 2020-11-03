open !Core

type t =
  { row : int
  ; col : int
  }
[@@deriving equal, show]

let create_positions_within ~height ~width =
  let open List.Let_syntax in
  let%bind row = List.init height ~f:Fun.id in
  let%bind col = List.init width ~f:Fun.id in
  return { row ; col }
;;

let compare (p1 : t) (p2 : t) = 
  let p1_cmp = Int.compare p1.row p2.row in
  if p1_cmp = 0
  then Int.compare p1.col p2.col
  else p1_cmp
;;
