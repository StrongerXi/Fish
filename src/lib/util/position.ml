type t =
  { row : int
  ; col : int
  }

let create_positions_within ~height ~width =
  List.init height (fun row ->
      List.init width (fun col -> { row ; col }))
  |> List.concat
;;

let compare (p1 : t) (p2 : t) = 
  let p1_cmp = Int.compare p1.row p2.row in
  if p1_cmp == 0
  then Int.compare p1.col p2.col
  else p1_cmp
