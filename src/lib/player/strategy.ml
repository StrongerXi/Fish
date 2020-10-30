open !Core

module PS = Player_state
module GS = Game_state
module GT = Game_tree


module Penguin_placer = struct
  type t =
    | Scanning

  let use_scanning (gs : GS.t) : Position.t =
    let bd = GS.get_board_minus_penguins gs in
    let width, height = Board.get_width bd, Board.get_height bd in
    let best_pos = 
      Position.create_positions_within ~width ~height
      |> List.filter 
        ~f:(fun p -> not @@ (Tile.is_hole @@ Board.get_tile_at bd p))
      |> List.min_elt ~compare:Position.compare in (* favor top left pos *)
    match best_pos with
    | None -> failwith "No position to place penguin on board"
    | Some(pos) -> pos

  let create_scanning_strategy = Scanning

  let use t gs =
    match t with
    | Scanning -> use_scanning gs
end


module Turn_actor = struct
  type t = 
    (* positive # of turns a player should take while looking ahead *)
    | Minimax of int

  (* This can break ties in moves with same score *)
  let act_compare (act1 : Action.t) (act2 : Action.t) : int =
    match act1, act2 with
    | (Action.Move(m1), Action.Move(m2)) -> Action.Move.compare m1 m2
    | _ -> 0 (* treat all other types of action pairs as "equal" *)
  ;;

  let scored_act_compare (act1, score1) (act2, score2) : int =
    if score1 = score2
    then ~-(act_compare act1 act2) (* favor top-left move *)
    else Int.compare score1 score2
  ;;

  let use_minimax turns gt =
    let my_color =
      gt |> GT.get_state |> GS.get_current_player |> PS.get_player_color in
    let current_player_is_me (gt : GT.t) : bool =
      let current_player = GT.get_state gt |> GS.get_current_player in
      Core.phys_same my_color @@ PS.get_player_color current_player in
    let get_my_player_score (gt : GT.t) : int =
      PS.get_score @@ GS.get_player_with_color (GT.get_state gt) my_color in
    (* Evaluate [gt] from the perspective of player with [color], by looking
     * ahead as many steps as needed so that the player takes at least 
     * [turns_left] more turns. Evaluate based on the minimax algorithm and
     * player score *)
    let rec evaluate_tree (turns_left : int) (gt : GT.t) : int =
      if turns_left = 0
      then get_my_player_score gt
      else 
        let score_selector, next_turns_left = 
          if current_player_is_me gt
          then (List.max_elt ~compare:Int.compare, turns_left - 1)
          else (List.min_elt ~compare:Int.compare, turns_left) in
        let scores =  
          GT.get_subtrees gt 
          |> List.map ~f:(fun (_, gt) -> evaluate_tree next_turns_left gt) in
        match score_selector scores with (* [None] means end of game *)
        | None -> get_my_player_score gt
        | Some(score) -> score
    in
    let best_scored_move =
      GT.get_subtrees gt
      |> List.map ~f:(fun (act, subt) -> (act, evaluate_tree (turns - 1) subt))
      |> List.max_elt ~compare:scored_act_compare in
    match best_scored_move with
    | None -> failwith "No legal action in given game state"
    | Some(act, _) -> act
  ;;

  let create_minimax_strategy turns = Minimax(turns)

  let use t gt =
    match t with
    | Minimax(turns) -> use_minimax turns gt
end