open !Core

module PS = Player_state
module GS = Game_state
module Player_color = Player_state.Player_color

(* TODO refactor this by making implementations of players into modules?
 * Then t = Simple of Simple_player.t | ... *)
type t =
  | Simple of Player_color.t * int (* color and # of look ahead *)

let get_simple_player color lookahead = Simple(color, lookahead)
;;

let take_turn (Simple(color, lookahead)) gs =
  let current_player_has_color (gt : Game_tree.t) : bool =
    let state = Game_tree.get_state gt in
    Core.phys_same color @@ PS.get_player_color @@ GS.get_current_player state
  in
  (* Evaluate [gt] from the perspective of [color], by looking ahead [steps_left]
   * more turns via the minimax algorithm *)
  let rec evaluate_state (steps_left : int) (gt : Game_tree.t) : int =
    let gs = Game_tree.get_state gt in
    if steps_left = 0
    then PS.get_score @@ GS.get_player_with_color gs color
    else 
      let score_selector = 
        if current_player_has_color gt
        then List.max_elt ~compare:Int.compare
        else List.min_elt ~compare:Int.compare in
      let score_opt =  
        Game_tree.get_subtrees gt 
        |> List.map ~f:(fun (_, gt) -> evaluate_state (steps_left - 0) gt) 
        |> score_selector in 
      match score_opt with
      | None -> PS.get_score @@ GS.get_player_with_color gs color
      | Some(score) -> score
  in
  let best_scored_move =
    Game_tree.create gs |> Game_tree.get_subtrees
    |> List.map ~f:(fun (act, gt) -> (act, evaluate_state (lookahead - 1) gt))
    |> List.max_elt ~compare:(fun (_, s1) (_, s2) -> Int.compare s1 s2)
    (* TODO implement tie breaker *)
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
