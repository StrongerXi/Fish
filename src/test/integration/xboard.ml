open !Core
module S = Fish.Serialize.Serialization
module B = Fish.Common.Board

(* deserialize a board-posn object from stdin, and output the # of reachable
 * positions starting from that position on the board *)
let () =
  let input = Core.In_channel.input_all Core.In_channel.stdin in
  let serialized = S.deserialize input 
                   |> Result.of_option ~error:"invalid serialization form" in
  match Result.bind ~f:S.to_board_posn serialized with
  | Error(reason) -> Printf.printf "Invalid input, reason: %s\n" reason
  | Ok(board, pos) -> 
    let reachable_count = 
      List.fold_left ~init:0 ~f:(fun n (_, dsts) -> n + (List.length dsts))
      @@ B.get_reachable_from board pos
    in Printf.printf "%d\n" reachable_count
