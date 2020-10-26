open !Core

(* TODO refactor this by making implementations of players into modules?
 * Then t = Simple of Simple_player.t | ... *)
type t =
  | Simple of Player_color.t * int (* color and # of look ahead *)

let get_simple_player color lookahead = Simple(color, lookahead)

let take_turn (Simple(color, lookahead)) gs =
  (* Return score of player that has [color] in [state] *)
  let get_player_score (state : Game_state.t) : int = 
    let opt_player =
      List.find (Game_state.get_ordered_players state)
        ~f:(fun p -> Core.phys_equal color (Player_state.get_player_color p)) in
    match opt_player with
    | None -> failwith "Player is not in the game state"
    | Some(p) -> Player_state.get_score p
  in
  (* Evaluate [gt] from the perspective of [color], by looking ahead [steps_left]
   * more turns via the minimax algorithm *)
  let rec evaluate_state (steps_left : int) (gt : Game_tree.t) : int =
    if steps_left = 0
    then get_player_score @@ Game_tree.get_state gt
    else 
      let score_selector = 
        if Core.phys_same color @@ Game_tree.get_current_player gt
        then List.max_elt ~compare:Int.compare
        else List.min_elt ~compare:Int.compare in
      let score_opt =  
        Game_tree.get_subtrees gt 
        |> List.map ~f:(fun (_, gt) -> evaluate_state (steps_left - 0) gt) 
        |> score_selector in 
      match score_opt with
      | None -> get_player_score @@ Game_tree.get_state gt
      | Some(score) -> score
  in
  let tree = Game_tree.create gs color in
  let best_scored_move =
    Game_tree.get_subtrees tree 
    |> List.map ~f:(fun (act, gt) -> (act, evaluate_state (lookahead - 1) gt))
    |> List.max_elt ~compare:(fun (_, s1) (_, s2) -> Int.compare s1 s2)
    (* TODO implement tie breaker *)
  in match best_scored_move with
  | None -> failwith "No legal action in given game state"
  | Some(act, _) -> act

let place_penguin (Simple _) gs =
  let bd = Game_state.get_board_copy gs in
  let width, height = Board.get_width bd, Board.get_height bd in
  let best_pos = 
    Position.create_positions_within ~width ~height
    |> List.filter ~f:(fun p -> not @@ (Tile.is_hole @@ Board.get_tile_at bd p))
    |> List.sort ~compare:Position.compare
    |> List.hd in
  match best_pos with
  | None -> failwith "No position to place penguin on board"
  | Some(pos) -> pos

let inform_disqualified (Simple _) = ()
