open !Core
module S = Fish.Serialize.Serialization
module B = Fish.Game.Board
module Action = Fish.Game.Action
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
let find_moves (moves : (Pos.t * Pos.t) list) (dsts : Pos.t list) 
               : (Pos.t * Pos.t) list =
  let rec go (dsts : Pos.t list) : (Pos.t * Pos.t) list =
    match dsts with
    | [] -> []
    | d::dsts ->
      match List.filter ~f:(fun (_, dst) -> Core.phys_equal d dst) moves with
      | [] -> go dsts
      | moves -> moves
  in 
  go dsts

let get_moves_from_actions (actions : Action.t list) 
  : (Pos.t * Pos.t) list option =
  let move_from_action act =
    match act with
    | Action.Skip -> None
    | Action.Move(src, dst) -> Some(src, dst) in
  List.map ~f:(move_from_action) actions |> Option.all

(** deserialize a game state, and output the deserialized result of applying 
    it to [find_move_and_apply]. If the result is [None], then print "false". *)
let () =
  let input = Core.In_channel.input_all Core.In_channel.stdin in
  match S.from_string input |> Option.bind ~f:S.to_move_resp_query with
  | Some(state, src, dst) ->
    let subtrees = Game_tree.create state |> Game_tree.get_subtrees in
    let move = Action.Move(src, dst) in
    begin
      match List.Assoc.find ~equal:Core.phys_equal subtrees move with
      | None -> failwith "Given move is invalid on given state"
      | Some(subtree) -> 
        let actions = Game_tree.get_subtrees subtree 
                      |> List.map ~f:(fun (move, _) -> move) in
        (match get_moves_from_actions actions with
        | None -> print_string "false\n"
        | Some(moves) -> 
          let dsts = List.map ~f:(Board.Direction.step_from dst) preference in
          let moves = find_moves moves dsts in
          (match moves with
           | [] -> print_string "false\n"
           | (s, d)::_ -> 
             S.from_action (Action.Move(s, d)) |> S.to_string |> Printf.printf "%s\n";
          )
        )
    end
  | None -> print_string "Invalid input\n"
