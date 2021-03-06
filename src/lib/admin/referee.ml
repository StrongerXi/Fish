open !Core
module Mutex = Error_checking_mutex
module Timeout = Util.Timeout

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
    | PenguinPlacement of Game_state.t * Player_state.Player_color.t * Position.t
    | TurnAction of Game_state.t * Player_state.Player_color.t * Action.t
    | Disqualify of Game_state.t option * Player_state.Player_color.t
    | EndOfGame of Game_result.t
  type t = event -> unit
end

module Color = Common.Player_state.Player_color
module GT = Common.Game_tree
module GS = Common.Game_state
module PS = Common.Player_state

type timeout_config = 
  { assign_color_timeout_ms : int
  ; placement_timeout_ms : int
  ; turn_action_timeout_ms : int
  ; inform_disqualified_timeout_ms : int
  ; inform_observer_timeout_ms : int
  }

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
  ; conf : timeout_config
  ; state_lock : Mutex.t
  }

(* Some constants *)
module C = struct
  let min_num_of_players = 2
  let max_num_of_players = 4
  let init_colors = [Color.Red; Color.Black; Color.White; Color.Brown;]
end

(* synchronized read/write to the state *)
let read_state (t : t) : GS.t option =
  Mutex.lock t.state_lock;
  let state = t.state in
  Mutex.unlock t.state_lock;
  state
;;

let write_state (t : t) (state : GS.t option) : unit =
  Mutex.lock t.state_lock;
  t.state <- state;
  Mutex.unlock t.state_lock;
;;

(** EFFECT: update [t.observers] *)
let add_game_observer t observer =
  t.observers <- observer::t.observers;
  Option.iter (read_state t) ~f:(fun state -> observer (Register state))
;;

(** EFFECT: update [t.observers] to remove the observer(s) which time out *)
let inform_all_observers t event : unit=
  let remaining_observers = 
    List.filter t.observers
      ~f:(fun observer -> 
          Option.is_some @@ Timeout.call_with_timeout_ms
            (fun () -> observer event) t.conf.inform_observer_timeout_ms)
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
let disqualify_current_player (t : t) (color : Color.t) (why : [`Cheat | `Fail])
  : GS.t option =
  Option.bind (read_state t)
    ~f:(fun state ->
        let player = get_player_with_color t color in
        let new_state = GS.remove_current_player state in
        (match why with (* informing failed player could be dangerous *)
         | `Fail  -> t.failed <- color::t.failed
         | `Cheat -> t.cheaters <- color::t.cheaters;
           Core.ignore @@ Timeout.call_with_timeout_ms
             (fun () -> player#inform_disqualified ())
             t.conf.inform_disqualified_timeout_ms);
        player#dispose();
        inform_all_observers t (Disqualify(new_state, color));
        new_state)
;;

let handle_player_cheated (t : t) (color : Color.t) : GS.t option =
  disqualify_current_player t color `Cheat
;;

let handle_player_failure (t : t) (color : Color.t) : GS.t option =
  disqualify_current_player t color `Fail
;;

(** EFFECT: update [t.cheaters] or [t.failed] if current player cheats/fails.
    RETURN: final game state or [None] if all players are removed. *)
let handle_current_player_penguin_placement (t : t) (gs : GS.t) : GS.t option =
  let board = GS.get_board_copy gs in
  let color = GS.get_current_player gs |> PS.get_color in
  let player = get_player_with_color t color in
  let response =
    Option.join @@ (* timeout and communication failure are treated the same *)
    Timeout.call_with_timeout_ms
      (fun () -> player#place_penguin gs) t.conf.placement_timeout_ms in
  match response with (* same treatment to timeout and communication failure *) 
  | None -> handle_player_failure t color
  | Some(pos) ->
    if Board.within_board board pos && 
       not @@ Tile.is_hole @@ Board.get_tile_at board pos
    then 
      let new_state =
        GS.rotate_to_next_player @@ GS.place_penguin gs color pos in
      inform_all_observers t (PenguinPlacement(new_state, color, pos));
      Option.some new_state
    else handle_player_cheated t color
;;

(** EFFECT: upadte [t.state], [t.cheaters] and [t.failed]. *)
let handle_penguin_placement_phase (t : t) : unit =
  let state = read_state t in (* write is exclusively here during this phase *)
  let penguins_per_player = 
    Option.value_map state ~default:0 ~f:num_of_penguin_per_player in
  let all_players_have_enough_penguins state : bool =
    List.map ~f:PS.get_penguins @@ GS.get_ordered_players state
    |> List.for_all ~f:(fun pgs -> penguins_per_player = (List.length pgs))
  in
  let request_placement_or_skip_current_player (state : GS.t) : GS.t option =
    let player_state = GS.get_current_player state in
    if penguins_per_player = List.length @@ PS.get_penguins player_state
    then Option.some @@ GS.rotate_to_next_player state
    else handle_current_player_penguin_placement t state
  in
  (* EFFECT: update [t] after every placement *)
  let rec loop (state : GS.t) : unit =
    if not @@ all_players_have_enough_penguins state
    then
      (let next_state_opt = request_placement_or_skip_current_player state in
       write_state t next_state_opt;
       Option.iter next_state_opt ~f:loop)
  in
  Option.iter ~f:loop state;
;;

(** Skip on the player's behalf if it can't move.
    Return [None] if there was a communication failure or time out *)
let get_player_action (t : t) (player : Player.t) (tree : GT.t)
  : Action.t option =
  match GT.get_subtrees tree with
  | [(Action.Skip, _);] ->  Option.some Action.Skip
  | _ ->
    Option.join @@ (* timeout and communication failure are treated the same *)
    Timeout.call_with_timeout_ms
      (fun () -> player#take_turn tree) t.conf.turn_action_timeout_ms

(** EFFECT: update [t.cheaters] or [t.failed] if current player cheats/fails.
    RETURN: final game tree or [None] if all players are removed. *)
let handle_current_player_turn_action (t : t) (tree : GT.t) : GT.t option =
  let subtrees = GT.get_subtrees tree in
  let state = GT.get_state tree in
  let color = GS.get_current_player state |> PS.get_color in
  let player = get_player_with_color t color in
  match get_player_action t player tree with
  | None -> Option.map ~f:GT.create @@ handle_player_failure t color
  | Some(action) ->
    match List.Assoc.find ~equal:Action.equal subtrees action with
    | None -> Option.map ~f:GT.create @@ handle_player_cheated t color
    | Some(next_sub_tree) -> 
      let new_state = GT.get_state next_sub_tree in
      inform_all_observers t (TurnAction(new_state, color, action));
      Option.some next_sub_tree
;;

(* EFFECT: upadte [t.state], [t.cheaters] and [t.failed]. *)
let handle_turn_action_phase (t : t) : unit =
  (* EFFECT: update [t] after every action *)
  let rec loop (tree : GT.t) : unit =
    match GT.get_subtrees tree with
    | [] -> () (* Game over *)
    | _ ->
      let next_tree_opt = handle_current_player_turn_action t tree in
      write_state t @@ Option.map ~f:GT.get_state next_tree_opt;
      Option.iter next_tree_opt ~f:loop
  in
  Option.iter ~f:loop @@ Option.map ~f:GT.create (read_state t);
;;

(** ASSUME: [t.color_to_player] has been properly instantiated.
    EFFECT: upadte [t.color_to_player], [t.state] and [t.failed]. *)
let handle_color_assignment_phase t : unit =
  (* assign color to current player and return resulting game state *)
  let inform_player_with_color color state : GS.t option =
    let player = get_player_with_color t color in
    let result = Timeout.call_with_timeout_ms
        (fun () -> player#assign_color color) t.conf.assign_color_timeout_ms in
    match result with
    | Some(true) -> Some(GS.rotate_to_next_player state)
    | _ -> handle_player_failure t color
  in
  (* EFFECT: update [t] after each color assignment *)
  let rec go colors_to_inform state : unit =
    match colors_to_inform with
    | [] -> ()
    | color::rest ->
      let next_state_opt = inform_player_with_color color state in
      write_state t next_state_opt;
      Option.iter next_state_opt ~f:(go rest)
  in
  Option.iter (read_state t) ~f:(fun state ->
      go (List.map ~f:PS.get_color (GS.get_ordered_players state)) state)
;;

(** Error if given invalid # of players *)
let create_color_to_player_mapping_exn players : color_player_map =
  let rec zip_to_shortest xs ys =
    match xs, ys with
    | [], _ | _, [] -> []
    | x::xs, y::ys -> (x, y)::(zip_to_shortest xs ys)
  in
  let player_count = List.length players in
  if player_count < C.min_num_of_players || player_count > C.max_num_of_players
  then failwith ("Invalid number of players: " ^ (string_of_int player_count))
  else zip_to_shortest C.init_colors players

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
  match read_state t with
  | None -> { winners = []; rest = []; failed; cheaters }
  | Some(state) ->
    let players = GS.get_ordered_players state in
    let max_score = List.map ~f:PS.get_score players
                    |> List.max_elt ~compare:Int.compare
                    |> Option.value ~default:0 in
    let winners = 
      List.filter ~f:(fun p -> (PS.get_score p) = max_score) players
      |> List.map ~f:(Fn.compose (get_player_with_color t) PS.get_color)
    in
    let rest = 
      List.filter ~f:(fun p -> (PS.get_score p) <> max_score) players
      |> List.map ~f:(Fn.compose (get_player_with_color t) PS.get_color)
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
  write_state t (Some(create_and_validate_game_state_exn t board_config));
;;

let default_timeout_config =
  { placement_timeout_ms = 3000
  ; turn_action_timeout_ms = 3000
  ; assign_color_timeout_ms = 3000
  ; inform_disqualified_timeout_ms = 3000
  ; inform_observer_timeout_ms = 3000
  }
;;

let create ?(config = default_timeout_config) () =
  { state = None; cheaters = []; failed = []; observers = [];
    color_to_player = []; conf = config; state_lock = Mutex.create (); }
;;

let run_game t players board_config =
  init_referee_exn t players board_config;
  Option.iter (read_state t)
    ~f:(fun state -> inform_all_observers t (Register state));
  (* if all players are removed in any phase, [t.state] becomes [None] and
   * the control flow effectively short circuits all the way through *)
  handle_color_assignment_phase t;
  handle_penguin_placement_phase t;
  handle_turn_action_phase t;
  collect_and_report_result t;
;;
