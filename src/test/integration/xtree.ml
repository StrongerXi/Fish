open !Core
module S = Fish.Serialize.Serialization
module B = Fish.Common.Board
module Action = Fish.Common.Action
module Move = Fish.Common.Action.Move
module Game_tree = Fish.Common.Game_tree
module GS = Fish.Common.Game_state
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
      match List.filter ~f:(fun m -> Pos.equal d m.dst) moves with
      | [] -> go dsts
      | moves -> moves
  in go dsts

(** Return all the Moves within [actions], which might contain other types of
    actions. *)
let rec get_moves_from_actions (actions : Action.t list) :  Move.t list =
  match actions with
  | [] -> []
  | Action.Skip::rest -> get_moves_from_actions rest
  | Action.Move(m)::rest -> m::(get_moves_from_actions rest)


(** apply the specified move, and find the move for next player that places a
    penguin onto a tile near previous move's destination, based on directional
    preference in [preference]. Break ties by selecting the top-leftmost move
    source. Output the selected move, or false if not possible *)
let select_next_move_if_possible 
    (state : GS.t) (src : Pos.t) (dst : Pos.t) : Move.t option =
  let open Option.Let_syntax in
  let subtrees = Game_tree.create state |> Game_tree.get_subtrees
  and act = Action.Move({ src; dst }) in
  let%bind subtree = List.Assoc.find ~equal:Action.equal subtrees act in
  let moves = Game_tree.get_subtrees subtree 
              |> List.map ~f:(fun (move, _) -> move)
              |> get_moves_from_actions in
  let dsts = List.map ~f:(B.Direction.step_from dst) preference in
  let moves = find_moves moves dsts (* break ties here (if any) *)
              |> List.sort ~compare:Action.Move.compare in
  List.hd moves


(** deserialize a move_response_query object, and print out the serialized form
    of selected move from [select_next_move_if_possible], or print "false" if
    desired move isn't possible. *)
let () =
  let input = Core.In_channel.input_all Core.In_channel.stdin in
  let serialized = S.from_json_string input 
                   |> Result.of_option ~error:"invalid serialization form" in
  match Result.bind ~f:S.to_move_resp_query serialized with
  | Error(reason) -> Printf.printf "Invalid input, reason: %s\n" reason
  | Ok(state, src, dst) ->
    match select_next_move_if_possible state src dst with
    | None  -> print_string "false\n"
    | Some(m) -> 
      S.from_action (Action.Move m) |> S.to_json_string |> Printf.printf "%s\n";
