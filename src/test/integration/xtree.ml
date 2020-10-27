open !Core
module S = Fish.Serialize.Serialization
module B = Fish.Game.Board
module Action = Fish.Game.Action
module Move = Fish.Game.Action.Move
module Game_tree = Fish.Game.Game_tree
module GS = Fish.Game.Game_state
module PS = Fish.Game.Player_state
module PN = Fish.Game.Penguin
module Pos = Fish.Util.Position

let preference =
  let open B.Direction in
  [ North; Northeast; Southeast; South; Southwest; Northwest; ]

(** Find the moves in [moves] that land in one of [dsts], prioritizing
    earlier elements in [dsts]. Return multiple moves if they all end on the
    same dst. *)
let find_moves (moves : Move.t list) (dsts : Pos.t list) : Move.t list =
  let rec go (dsts : Pos.t list) : Move.t list =
    match dsts with
    | [] -> []
    | d::dsts ->
      match List.filter ~f:(fun m -> Core.phys_equal d m.dst) moves with
      | [] -> go dsts
      | moves -> moves
  in 
  go dsts

let rec get_moves_from_actions (actions : Action.t list) :  Move.t list =
  match actions with
  | [] -> []
  | Action.Skip::rest -> get_moves_from_actions rest
  | Action.Move(m)::rest -> m::(get_moves_from_actions rest)

(** deserialize a move_response_query object, apply the specified move, and find
    the move for next player that places a penguin onto a tile near previous
    move's destination, based on directional preference in [preference]. Break
    ties by selecting the top-leftmost move source. Output the selected move, or
    false if not possible *)
let () =
  (* Refactor with let syntax? TODO *)
  let input = Core.In_channel.input_all Core.In_channel.stdin in
  match S.from_string input |> Option.bind ~f:S.to_move_resp_query with
  | Some(state, src, dst) ->
    let subtrees = Game_tree.create state |> Game_tree.get_subtrees in
    let move = Action.Move({ src; dst }) in
    begin
      match List.Assoc.find ~equal:Core.phys_equal subtrees move with
      | None -> print_string "false\n"
      | Some(subtree) -> 
        let moves = Game_tree.get_subtrees subtree 
                      |> List.map ~f:(fun (move, _) -> move)
                      |> get_moves_from_actions in
        let dsts = List.map ~f:(B.Direction.step_from dst) preference in
        let moves = find_moves moves dsts (* break ties here (if any) *)
                    |> List.sort ~compare:Action.Move.compare in
        (match moves with
         | [] -> print_string "false\n"
         | m::_ -> S.from_action (Action.Move m) |> S.to_string |> Printf.printf "%s\n";)
    end
  | None -> print_string "Invalid input\n"
