type t =
  { col : int
  ; row : int
  }

let create_positions_within ~height ~width =
  List.init height (fun row ->
      List.init width (fun col -> { row ; col }))
  |> List.concat
;;
