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
    It's made mutable to enable client to do things such as query or add observers
    during a [run_game] call.
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
let init_colors = [Color.Red; Color.Black; Color.White; Color.Brown;]

let create players =
  let player_count = List.length players in
  if player_count < min_num_of_players || player_count > max_num_of_players
  then failwith ("Invalid number of players: " ^ (string_of_int player_count));
  { state = None; cheaters = []; failed = []; 
    color_to_player = List.cartesian_product init_colors players }
;;

(** Errors if no player has [color] in [t].
    This abstracts out the mapping from color to player. *)
let get_player_with_color (t : t) (color : Color.t) : Player.t =
  match List.Assoc.find ~equal:Color.equal t.color_to_player color with
  | None -> failwith @@ "Color not found in referee: " ^ (Color.show color)
  | Some(player) -> player
;;

(** Assumes [t.state] is populated. Update [t.cheaters] accordingly
    Return the new game state, or [None] if all players are removed. *)
let handle_current_player_cheated (t : t) : GS.t option =
  let state = Option.value_exn t.state in
  let cheater_color = GS.get_current_player state |> PS.get_player_color in
  let player = get_player_with_color t cheater_color in
  Player.inform_disqualified player;
  t.cheaters <- cheater_color::t.cheaters;
  GS.remove_current_player @@ Option.value_exn t.state;
;;

(** Assumes [t.state] is populated. Update [t.failed] accordingly.
    Return the new game state, or [None] if all players are removed. *)
let handle_current_player_failed (t : t) : GS.t option =
  let state = Option.value_exn t.state in
  let failed_color = GS.get_current_player state |> PS.get_player_color in
  let player = get_player_with_color t failed_color in
  Player.inform_disqualified player;
  t.failed <- failed_color::t.cheaters;
  GS.remove_current_player @@ Option.value_exn t.state;
;;

let number_of_holes_on_board (board : Board.t) : int =
  let width, height = Board.get_width board, Board.get_height board in 
  Position.create_positions_within ~width ~height
  |> List.filter 
    ~f:(fun pos -> not @@ Tile.is_hole @@ Board.get_tile_at board pos)
  |> List.length
;;

(** Request penguin placement from current player in [gs] and remove the player
    if the it cheats/fails. Return the resulting game state or [None] if no
    player is left. *)
let handle_current_player_penguin_placement (t : t) (gs : GS.t) : GS.t option =
  let board = GS.get_board_copy gs in
  let player_state = GS.get_current_player gs in
  let color = PS.get_player_color player_state in
  let player = get_player_with_color t color in
  match Player.place_penguin player gs with
  | None -> handle_current_player_failed t
  | Some(pos) ->
    if Board.within_board board pos && 
       not @@ Tile.is_hole @@ Board.get_tile_at board pos
    then Option.some @@ GS.place_penguin gs color pos
    else handle_current_player_failed t
;;

(** Fail if board doesn't have enough non-hole tiles to place penguins *)
let handle_penguin_placement_phase (t : t) (state : GS.t) : unit =
  let num_of_players = List.length @@ GS.get_ordered_players state in
  let penguins_per_player = 6 - num_of_players in
  let all_players_have_enough_penguins state : bool =
    List.map ~f:PS.get_penguins @@ GS.get_ordered_players state |> 
    List.for_all ~f:(fun penguins -> penguins_per_player = (List.length penguins))
  in
  let rec loop state : unit =
    t.state <- Some(state);
    if all_players_have_enough_penguins state then ();
    let player_state = GS.get_current_player state in
    if penguins_per_player = List.length @@ PS.get_penguins player_state 
    then loop @@ GS.rotate_to_next_player state; (* skip saturated player *)
    Option.iter ~f:loop @@ handle_current_player_penguin_placement t state
  in
  let board = GS.get_board_copy state in
  if penguins_per_player * num_of_players > (number_of_holes_on_board board)
  then failwith "Board doesn't have enough non-hole tiles for penguin placement"
  else loop state
;;

(** Request turn action from current player in [gt] and remove the player
    if the it cheats/fails. Return the resulting game tree or [None] if no
    player is left. *)
let handle_current_player_turn_action 
    (t : t) (tree : GT.t) (subtrees : (Action.t * GT.t) list) : GT.t option =
    let state = GT.get_state tree in
    let color = PS.get_player_color @@ GS.get_current_player state in
    let player = get_player_with_color t color in
    match Player.take_turn player tree with
    | None -> Option.map ~f:GT.create @@ handle_current_player_failed t
    | Some(action) ->
      match List.Assoc.find ~equal:Action.equal subtrees action with
      | None -> Option.map ~f:GT.create @@ handle_current_player_cheated t
      | Some(next_sub_tree) -> Option.some next_sub_tree
;;

(* Some mutually recursive helper functions to implement the game loop.
   Update the state in [t] in each iteration. *)
let rec game_loop (t : t) (tree : GT.t) : unit =
  t.state <- Some(GT.get_state tree);
  match GT.get_subtrees tree with
  | [] -> () (* Game over *)
  (* TODO inform skip event to observers *)
  | [(Action.Skip, next_sub_tree);] -> game_loop t next_sub_tree
  | subtrees -> 
    Option.iter ~f:(game_loop t)
      @@ handle_current_player_turn_action t tree subtrees
;;

let collect_result t : Game_result.t =
  let cheaters = List.map ~f:(get_player_with_color t) t.cheaters in
  let failed = List.map ~f:(get_player_with_color t) t.failed in
  match t.state with
  | None -> { winners = []; rest = []; failed; cheaters }
  | Some(state) ->
    let players = GS.get_ordered_players state in
    let max_score = Option.value_map ~default:0 ~f:PS.get_score @@ List.hd players in
    let winners = 
      List.filter ~f:(fun p -> (PS.get_score p) = max_score) players
      |> List.map ~f:(fun p -> get_player_with_color t @@ PS.get_player_color p)
    in
    let rest = 
      List.filter ~f:(fun p -> (PS.get_score p) <> max_score) players
      |> List.map ~f:(fun p -> get_player_with_color t @@ PS.get_player_color p)
    in
    { winners; rest; failed; cheaters; }
;;

let run_game t config =
  let board = Board.create config in
  let colors = List.map ~f:Tuple.T2.get1 t.color_to_player in
  let state = GS.create board colors in
  handle_penguin_placement_phase t state;
  (match t.state with
   | None -> (); (* all players failed/cheated during placement phase *)
   | Some(state) -> game_loop t (GT.create state));
  collect_result t
;;
