open !Core

module PS = Player_state
module GS = Game_state
module Player_color = Player_state.Player_color

(* TODO refactor this by making implementations of players into modules?
 * Then t = Simple of Simple_player.t | ... *)
type t =
  | Simple of int (* # of look ahead *)

let get_simple_player lookahead = Simple(lookahead)
;;

let take_turn (Simple(lookahead)) gt =
  let my_color = gt |> Game_tree.get_state |> GS.get_current_player |> PS.get_player_color in
  let current_player_is_me (gt : Game_tree.t) : bool =
    let current_player = Game_tree.get_state gt |> GS.get_current_player in
    Core.phys_same my_color @@ PS.get_player_color current_player
  in
  (* Evaluate [gt] from the perspective of player with [color], by looking ahead
   * as many steps as needed so that the player takes at least [turns_left]
   * more turns. Evaluate based on the minimax algorithm and player score *)
  let rec evaluate_state (turns_left : int) (gt : Game_tree.t) : int =
    let gs = Game_tree.get_state gt in
    if turns_left = 0
    then PS.get_score @@ GS.get_player_with_color gs my_color
    else 
      let score_selector, next_turns_left = 
        if current_player_is_me gt
        then (List.max_elt ~compare:Int.compare, turns_left - 1)
        else (List.min_elt ~compare:Int.compare, turns_left) in
      let score_opt =  
        Game_tree.get_subtrees gt 
        |> List.map ~f:(fun (_, gt) -> evaluate_state next_turns_left gt) 
        |> score_selector in 
      match score_opt with (* [None] means end of game *)
      | None -> PS.get_score @@ GS.get_player_with_color gs my_color
      | Some(score) -> score
  in
  (* This can break ties in moves with same score *)
  let act_compare (act1 : Action.t) (act2 : Action.t) : int =
    match act1, act2 with
    | (Action.Move(m1), Action.Move(m2)) -> Action.Move.compare m1 m2
    | _ -> failwith "Illegal state, we should only be comparing moves"
  in
  let scored_act_compare (act1, score1) (act2, score2) : int =
    if score1 = score2
    then act_compare act1 act2
    else Int.compare score1 score2
  in
  let best_scored_move =
    Game_tree.get_subtrees gt
    |> List.map ~f:(fun (act, subt) -> (act, evaluate_state (lookahead - 1) subt))
    |> List.max_elt ~compare:scored_act_compare
  in match best_scored_move with
  | None -> failwith "No legal action in given game state"
  | Some(act, _) -> act
;;

let place_penguin (Simple _) gs =
  let bd = GS.get_board_copy gs in
  let width, height = Board.get_width bd, Board.get_height bd in
  let best_pos = 
    Position.create_positions_within ~width ~height
    |> List.filter ~f:(fun p -> not @@ (Tile.is_hole @@ Board.get_tile_at bd p))
    |> List.sort ~compare:Position.compare
    |> List.hd in
  match best_pos with
  | None -> failwith "No position to place penguin on board"
  | Some(pos) -> pos
;;

let inform_disqualified (Simple _) = ()
;;
