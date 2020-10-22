open !Core
module S = Fish.Serialize.Serialization
module B = Fish.Game.Board

(* deserialize a board-posn object from stdin, and output the # of reachable
 * positions starting from that position on the board *)
let () =
  let input = Core.In_channel.input_all Core.In_channel.stdin in
  match S.from_string input |> Option.bind ~f:S.to_board_posn with
  | Some(board, pos) -> 
    let reachable_count = 
      List.fold_left ~init:0 ~f:(fun n (_, dsts) -> n + (List.length dsts))
      @@ B.get_reachable_from board pos
    in Printf.printf "%d\n" reachable_count
  | None -> print_string "Invalid input\n"
