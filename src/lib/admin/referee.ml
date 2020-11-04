open !Core

module Game_result = struct
  type t =
    { winners : Player.t list
    ; cheaters : Player.t list
    ; failed : Player.t list
    ; rest : Player.t list
    }
end

module Color = Common.Player_state.Player_color
module GT = Common.Game_tree
module GS = Common.Game_state
module PS = Common.Player_state

(** A [t] represents a referee which manages an entire fish game from start to
    end. A [t] manages exactly 1 game and becomes obselete after the game ends.
    It can:
    - Set up and run a game given an ordered list of [Player.t]s
    - Report final result of a game after it's finished *)
type t =
  (* current game state, updated during [run_game]. 
   * It's [None] before game starts or if all players have been removed. *)
  { mutable state : Game_state.t option
  ; mutable color_to_player : (Color.t * Player.t) list
  ; mutable cheaters : Color.t list
  ; mutable failed : Color.t list
  }
(* TODO how does client query current game or add observers state 
   during a [run_game]? Need some synchronization mechanism *)


let min_num_of_players = 2
let max_num_of_players = 4
let colors = [Color.Red; Color.Black; Color.White; Color.Brown;]

let create players =
  let player_count = List.length players in
  if player_count < min_num_of_players || player_count > max_num_of_players
  then failwith ("Invalid number of players: " ^ (string_of_int player_count));
  { state = None; cheaters = []; failed = []; 
    color_to_player = List.cartesian_product colors players }
;;

(** Errors if no player has [color] in [t].
    This abstracts out the mapping from color to player. *)
let get_player_with_color (t : t) (color : Color.t) : Player.t =
  match List.Assoc.find ~equal:Color.equal t.color_to_player color with
  | None -> failwith @@ "Color not found in referee: " ^ (Color.show color)
  | Some(player) -> player
;;

(** Return [None] if no player is left after the removal *)
let remove_current_player_opt (state : GS.t) : (GS.t * Color.t) option =
  match GS.get_ordered_players state with
  | [] -> None
  | p::_ ->
    let color_to_remove = PS.get_player_color p in
    let state_after_removal = GS.remove_current_player state in
    Some(state_after_removal, color_to_remove)
;;

let handle_current_player_cheated (t : t) (cont : GS.t -> unit)
  : unit =
  match remove_current_player_opt @@ Option.value_exn t.state with
  | None -> t.state <- None; (* game is over, last player is removed *)
  | Some(new_state, cheater_color) ->
    t.cheaters <- cheater_color::t.cheaters;
    cont new_state
;;

let handle_current_player_failed (t : t) (cont : GS.t -> 'a)
  : 'a =
  match remove_current_player_opt @@ Option.value_exn t.state with
  | None -> t.state <- None; (* game is over, last player is removed *)
  | Some(new_state, failed_color) ->
    t.failed <- failed_color::t.failed;
    cont new_state
;;

let number_of_holes_on_board (board : Board.t) : int =
  let width, height = Board.get_width board, Board.get_height board in 
  Position.create_positions_within ~width ~height
  |> List.filter 
    ~f:(fun pos -> not @@ Tile.is_hole @@ Board.get_tile_at board pos)
  |> List.length
;;

(** Fail if board doesn't have enough non-hole tiles to place penguins *)
let handle_penguin_placement (t : t) (state : GS.t) : unit =
  let board = GS.get_board_copy state in
  let num_of_players = List.length @@ GS.get_ordered_players state in
  let penguins_per_player = 6 - num_of_players in
  let all_players_have_enough_penguins state : bool =
    List.map ~f:PS.get_penguins @@ GS.get_ordered_players state |> 
    List.for_all ~f:(fun penguins -> penguins_per_player = (List.length penguins))
  in
  (* Update the state in [t] in each iteration. *)
  let rec loop state : unit =
    t.state <- Some(state);
    if not @@ all_players_have_enough_penguins state
    then
      let color = GS.get_current_player state |> PS.get_player_color in
      let player = get_player_with_color t color in
      match Player.place_penguin player state with
      | None -> handle_current_player_failed t loop
      | Some(pos) ->
        if Board.within_board board pos && 
           not @@ Tile.is_hole @@ Board.get_tile_at board pos
        then loop @@ GS.place_penguin state color pos
        else handle_current_player_cheated t loop
  in
  if penguins_per_player * num_of_players > (number_of_holes_on_board board)
  then failwith "Board doesn't have enough non-hole tiles for penguin placement"
  else loop state

(* Some mutually recursive helper functions to implement the game loop.
   Update the state in [t] in each iteration. *)
let rec game_loop (t : t) (tree : GT.t) : unit =
  t.state <- Some(GT.get_state tree);
  match GT.get_subtrees tree with
  | [] -> () (* Game over *)
  (* TODO inform skip event to observers *)
  | [(Action.Skip, next_sub_tree);] -> game_loop t next_sub_tree
  | subtrees -> 
    let state = GT.get_state tree in
    let color = PS.get_player_color @@ GS.get_current_player state in
    let player = get_player_with_color t color in
    match Player.take_turn player tree with
    | None -> handle_current_player_failed t
                (fun new_state -> game_loop t @@ GT.create new_state);
    | Some(action) ->
      match List.Assoc.find ~equal:Action.equal subtrees action with
      | None -> handle_current_player_cheated t
                  (fun new_state -> game_loop t @@ GT.create new_state);
      | Some(next_sub_tree) -> game_loop t next_sub_tree

let collect_result t : Game_result.t =
  let cheaters = List.map ~f:(get_player_with_color t) t.cheaters in
  let failed = List.map ~f:(get_player_with_color t) t.failed in
  match t.state with
  | None -> 
    { winners = []; rest = []; failed; cheaters }
  | Some(state) ->
    let players = GS.get_ordered_players state in
    let max_score = Option.value_map ~default:0 ~f:PS.get_score @@ List.hd players in
    let winners = 
      List.filter ~f:(fun p -> (PS.get_score p) = max_score) players
      |> List.map ~f:(fun p -> get_player_with_color t @@ PS.get_player_color p)
    in
    let rest = 
      List.filter ~f:(fun p -> (PS.get_score p) <> max_score) players
      |> List.sort 
        ~compare:(fun p1 p2 -> Int.compare (PS.get_score p1) (PS.get_score p2))
      |> List.map ~f:(fun p -> get_player_with_color t @@ PS.get_player_color p)
    in
    { winners; rest; failed; cheaters; }
;;

let run_game t config =
  let board = Board.create config in
  let colors = List.map ~f:Tuple.T2.get1 t.color_to_player in
  let state = GS.create board colors in
  handle_penguin_placement t state;
  (match t.state with
   | None -> (); (* all players failed/cheated during placement phase *)
   | Some(state) -> game_loop t (GT.create state));
  collect_result t
;;
