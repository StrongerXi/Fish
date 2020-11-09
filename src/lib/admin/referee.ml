open !Core

module Game_result = struct
  type t =
    { winners : Player.t list
    ; cheaters : Player.t list
    ; failed : Player.t list
    ; rest : Player.t list
    }
end

module Game_observer = struct
  type event =
    | Register of Game_state.t
    | PenguinPlacement of Position.t
    | TurnAction of Action.t
    | Disqualify of Player_state.Player_color.t
    | EndOfGame of Game_result.t
  type t = event -> unit
end

module Color = Common.Player_state.Player_color
module GT = Common.Game_tree
module GS = Common.Game_state
module PS = Common.Player_state

type color_player_map = (Color.t * Player.t) list

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
    (* mapping from color to player. Fixed at beginning of game even if players
     * get removed during the game *)
  ; mutable color_to_player : color_player_map
  ; mutable cheaters : Color.t list
  ; mutable failed : Color.t list
  ; mutable observers : Game_observer.t list
  (* Note that synchronization is not added yet since there doesn't seem to be
   * any data race condition *)
  }

(* Some constants *)
module C = struct
  let min_num_of_players = 2
  let max_num_of_players = 4
  let init_colors = [Color.Red; Color.Black; Color.White; Color.Brown;]
  let placement_timeout_s = 10
  let turn_action_timeout_s = 10
  let assign_color_timeout_s = 10
  let inform_disqualified_timeout_s = 10
  let inform_observer_timeout_s = 10
end

(** Return [Some result] if [f ()] returns [result] within [sec] seconds. *)
let timeout (f : unit -> 'a) (sec : int) : 'a option =
  let comp = Lwt.map (fun f -> f ()) (Lwt.return f)
             |> Lwt.map Option.some in
  let timeout =
    Lwt_main.run @@ Lwt_unix.sleep (float_of_int sec);
    Lwt.return None in
  Lwt_main.run (Lwt.pick [comp; timeout])
;;

(** EFFECT: update [t.observers] *)
let add_game_observer t observer =
  t.observers <- observer::t.observers;
  Option.iter t.state ~f:(fun state -> observer (Register state))
;;

(** EFFECT: update [t.observers] to remove the observer(s) which time out *)
let inform_all_observers t event : unit=
  let remaining_observers = 
    List.filter_map ~f:Fun.id @@
    List.map t.observers
      ~f:(fun observer -> 
          Option.map ~f:(Fun.const observer) @@
          timeout (fun () -> observer event) C.inform_observer_timeout_s)
  in t.observers <- remaining_observers
;;

let num_of_penguin_per_player (state : GS.t) : int =
  6 - (List.length @@ GS.get_ordered_players state)
;;

(** ERRORS: if no player has [color] in [t].
    This abstracts out the mapping from color to player. *)
let get_player_with_color (t : t) (color : Color.t) : Player.t =
  match List.Assoc.find ~equal:Color.equal t.color_to_player color with
  | None -> failwith @@ "Color not found in referee: " ^ (Color.show color)
  | Some(player) -> player
;;

(** EFFECT: Update [t.cheaters] or [t.failed] if [t.state] is populated.
    RETURN: the new game state, or [None] if all players are removed. *)
let disqualify_current_player (t : t) (why : [`Cheat | `Fail]) : GS.t option =
  Option.bind t.state
    ~f:(fun state ->
        let color = GS.get_current_player state |> PS.get_player_color in
        let player = get_player_with_color t color in
        Core.ignore @@ timeout 
          (fun () -> player#inform_disqualified ()) 
          C.inform_disqualified_timeout_s;
        (match why with
         | `Cheat -> t.cheaters <- color::t.cheaters
         | `Fail  -> t.failed   <- color::t.failed);
        inform_all_observers t (Disqualify color);
        GS.remove_current_player state)
;;

let handle_current_player_cheated (t : t) : GS.t option =
  disqualify_current_player t `Cheat
;;

let handle_current_player_failed (t : t) : GS.t option =
  disqualify_current_player t `Fail
;;

(** EFFECT: update [t.cheaters] or [t.failed] if current player cheats/fails.
    RETURN: final game state or [None] if all players are removed. *)
let handle_current_player_penguin_placement (t : t) (gs : GS.t) : GS.t option =
  let board = GS.get_board_copy gs in
  let player_state = GS.get_current_player gs in
  let color = PS.get_player_color player_state in
  let player = get_player_with_color t color in
  let response = Option.join @@
    timeout (fun () -> player#place_penguin gs) C.placement_timeout_s in
  match response with (* same treatment to timeout and communication failure *) 
  | None -> handle_current_player_failed t
  | Some(pos) ->
    if Board.within_board board pos && 
       not @@ Tile.is_hole @@ Board.get_tile_at board pos
    then 
      (inform_all_observers t (PenguinPlacement pos);
       Option.some @@ GS.rotate_to_next_player @@ GS.place_penguin gs color pos)
    else handle_current_player_failed t
;;

(** EFFECT: upadte [t.state], [t.cheaters] and [t.failed]. *)
let handle_penguin_placement_phase (t : t) : unit =
  let penguins_per_player = 
    Option.value_map t.state ~default:0 ~f:num_of_penguin_per_player in
  let all_players_have_enough_penguins state : bool =
    List.map ~f:PS.get_penguins @@ GS.get_ordered_players state
    |> List.for_all ~f:(fun pgs -> penguins_per_player = (List.length pgs))
  in
  let get_next_state (state : GS.t) : GS.t option =
    if all_players_have_enough_penguins state then None
    else
      let player_state = GS.get_current_player state in
      if penguins_per_player = List.length @@ PS.get_penguins player_state
      then Option.some @@ GS.rotate_to_next_player state
      else handle_current_player_penguin_placement t state
  in
  (* A loop mainly responsible for updating [t.state] *)
  let rec loop (state : GS.t) : unit =
    match get_next_state state with
    | None -> ()
    | Some(next_state) -> 
      t.state <- Some(next_state);
      loop next_state
  in
  Option.iter ~f:loop t.state;
;;

(** EFFECT: update [t.cheaters] or [t.failed] if current player cheats/fails.
    RETURN: final game tree or [None] if all players are removed. *)
let handle_current_player_turn_action (t : t) (tree : GT.t) : GT.t option =
  let subtrees = GT.get_subtrees tree in
  let state = GT.get_state tree in
  let color = PS.get_player_color @@ GS.get_current_player state in
  let player = get_player_with_color t color in
  let response = Option.join @@
    timeout (fun () -> player#take_turn tree) C.turn_action_timeout_s in
  match response with (* same treatment for timeout and communication failure *) 
  | None -> Option.map ~f:GT.create @@ handle_current_player_failed t
  | Some(action) ->
    match List.Assoc.find ~equal:Action.equal subtrees action with
    | None -> Option.map ~f:GT.create @@ handle_current_player_cheated t
    | Some(next_sub_tree) -> 
      (inform_all_observers t (TurnAction action);
       Option.some next_sub_tree)
;;

(* EFFECT: upadte [t.state], [t.cheaters] and [t.failed]. *)
let handle_turn_action_phase (t : t) : unit =
  let get_next_subtree (tree : GT.t) : GT.t option =
    match GT.get_subtrees tree with
    | [] -> None (* Game over *)
    | [(Action.Skip, next_subtree);] -> 
      inform_all_observers t (TurnAction Action.Skip);
      Some(next_subtree)
    | _ -> handle_current_player_turn_action t tree
  in
  (* A loop mainly responsible for updating [t.state] *)
  let rec loop (tree : GT.t) : unit =
    match get_next_subtree tree with
    | None -> ()
    | Some(next_subtree) ->
      t.state <- Some(GT.get_state next_subtree);
      loop next_subtree
  in
  Option.iter ~f:loop @@ Option.map ~f:GT.create t.state;
;;

(** ASSUME: [t.color_to_player] has been properly instantiated.
    EFFECT: upadte [t.color_to_player], [t.state] and [t.failed]. *)
let handle_color_assignment_phase t : unit =
  (* assign color to current player and return resulting game state *)
  let handle_current_player state : GS.t option =
    let color = GS.get_current_player state |> PS.get_player_color in
    let player = get_player_with_color t color in
    let result = timeout 
        (fun () -> player#assign_color color) C.assign_color_timeout_s in
    match result with
    | None -> handle_current_player_failed t
    | _ -> Some(GS.rotate_to_next_player state)
  in
  let rec go more_times state : unit = (* short circuits if everyone left *)
    match more_times with
    | 0 -> ()
    | _ -> Option.iter (handle_current_player state) 
             ~f:(fun next_state ->
                 t.state <- Some(next_state);
                 go (more_times-1) next_state)
  in
  Option.iter t.state 
    ~f:(fun state -> go (List.length @@ GS.get_ordered_players state) state)
;;

(** Error if given invalid # of players *)
let create_color_to_player_mapping_exn players : color_player_map =
  let player_count = List.length players in
  if player_count < C.min_num_of_players || player_count > C.max_num_of_players
  then failwith ("Invalid number of players: " ^ (string_of_int player_count))
  else List.cartesian_product C.init_colors players

(** ASSUME: [t.color_to_player] has been instantiated properly.
    Fail if there aren't enough non-hole tiles to place penguins *)
let create_and_validate_game_state_exn t board_config : GS.t =
  let board = Board.create board_config in
  let colors = List.map ~f:Tuple.T2.get1 t.color_to_player in
  let state = GS.create board colors in
  let num_of_players = List.length @@ GS.get_ordered_players state in
  let penguins_per_player = num_of_penguin_per_player state in
  if penguins_per_player * num_of_players > (Board.num_of_non_hole_tiles board)
  then failwith "Board doesn't have enough non-hole tiles for penguin placement"
  else state
;;

(** Compile final game result based on [t.state], [t.cheaters] and [t.failed] *)
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

let collect_and_report_result t : Game_result.t =
  let result = collect_result t in
  inform_all_observers t (EndOfGame result);
  result
;;

(** Update fields in [t] for starting a new game *)
let init_referee_exn t players board_config =
  t.cheaters <- [];
  t.failed <- [];
  t.color_to_player <- create_color_to_player_mapping_exn players;
  t.state <- Some(create_and_validate_game_state_exn t board_config);
;;

let create () =
  { state = None; cheaters = []; failed = [];
    observers = []; color_to_player = [];}
;;

let run_game t players board_config =
  init_referee_exn t players board_config;
  Option.iter t.state ~f:(fun state -> inform_all_observers t (Register state));
  (* if all players are removed in any phase, [t.state] becomes [None] and
   * the control flow effectively short circuits all the way through *)
  handle_color_assignment_phase t;
  handle_penguin_placement_phase t;
  handle_turn_action_phase t;
  collect_and_report_result t;
;;
