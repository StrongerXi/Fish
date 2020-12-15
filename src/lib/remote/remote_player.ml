open !Core

module GT = Common.Game_tree
module GS = Common.Game_state
module PS = Common.Player_state
module PC = Common.Player_state.Player_color
module Pos = Util.Position
module Act = Common.Action
module S = Serialize.Serialization

module Call = struct
  (** A [t] represents a remote function call with its arguments *)
  type t =
    | Start
    | PlayAs   of Common.Player_state.Player_color.t
    | PlayWith of Common.Player_state.Player_color.t list
    | Setup    of Common.Game_state.t
    | TakeTurn of Common.Game_state.t * Common.Action.t list
    | End      of bool

  (* Some constants *)
  let start_name     = "start"
  let play_as_name   = "playing-as"
  let play_with_name = "playing-with"
  let setup_name     = "setup"
  let take_turn_name = "take-turn"
  let end_name       = "end"

  (* NOTE [deserialize (serialize t)] is guaranteed to be [Some t] *)

  (** Convert [t] into a serialized format. *)
  let serialize (t : t) : S.t =
    let helper (func_name : string) (args : S.t list) : S.t =
      let arr = [S.from_string func_name;
                 S.from_list args Fun.id] in
      S.from_list arr Fun.id
    in
    match t with
    | Start         -> helper start_name [S.from_bool true]
    | PlayAs(color) -> helper play_as_name [(S.from_color color)]
    | PlayWith(cs)  -> helper play_with_name [(S.from_list cs S.from_color)]
    | Setup(state)  -> helper setup_name[(S.from_game_state state)]
    | TakeTurn(gs, acts) ->
      helper take_turn_name
        [S.from_game_state gs; S.from_list acts S.from_action]
    | End(did_win) -> helper end_name [S.from_bool did_win]
  ;;

  (** Convert a serialized [t] back to the original [t].
      Return [None] if it's malformed *)
  let deserialize (s : S.t) : t option =
    let open Option.Let_syntax in
    let name_with_args : (string * S.t list) option =
      match%bind S.to_list s Fun.id |> Result.ok with
      | [func_name_t; args_t] ->
        let%bind func_name = S.to_string func_name_t in
        let%bind arg_ts = S.to_list args_t Fn.id |> Result.ok in
        return (func_name, arg_ts)
      | _ -> None
    in
    match%bind name_with_args with
    | s, [_] when String.(s = start_name) -> return Start
    | s, [color_s] when String.(s = play_as_name) ->
      let%bind color = S.to_color color_s |> Result.ok in
      return (PlayAs color)
    | s, [colors_s] when String.(s = play_with_name) ->
      let%bind colors = Result.bind ~f:Result.all (S.to_list colors_s S.to_color)
                        |> Result.ok in
      return (PlayWith colors)
    | s, [state_s] when String.(s = setup_name) ->
      let%bind state = S.to_game_state state_s |> Result.ok in
      return (Setup state)
    | s, [state_s; acts_s] when String.(s = take_turn_name)->
      let%bind state = S.to_game_state state_s |> Result.ok in
      let%bind acts = Result.bind ~f:Result.all (S.to_list acts_s S.to_action)
                      |> Result.ok in
      return @@ TakeTurn(state, acts)
    | s, [did_win_s] when String.(s = end_name) ->
      let%bind did_win = S.to_bool did_win_s in
      return (End did_win)
    | _ -> None
  ;;
end

(* Write [msg] to [oc] without delay from buffering *)
let write_to_outchan_now (oc : Out_channel.t) (msg : string) : unit =
  Out_channel.output_string oc msg;
  Out_channel.flush oc;
;;

(* Used to communicate acknowledgement of a remote call that returns nothing *)
let void_ackn_msg = "void"
;;


let create_proxy_player ic oc ~name ~age = object (self)
    inherit Player.t name age
    val inputs : S.t Stream.t = S.stream_from_channel ic
    val mutable first_setup : bool = true

    method place_penguin (state : GS.t) =
      if first_setup
      then
        (first_setup <- false;
         if self#play_with state
         then self#place_penguin_impl state
         else None)
      else self#place_penguin_impl state

    method take_turn (tree : GT.t) =
      self#send_call (Call.TakeTurn(GT.get_state tree, []));
      Option.bind ~f:(Fn.compose Result.ok S.to_action) @@ self#get_next_input()

    method! inform_tournament_start () =
    self#send_call Call.Start;
    self#expect_void_str ()

  method! assign_color (color : PC.t) =
    first_setup <- true; (* a new game has started *)
    self#send_call (Call.PlayAs color);
    self#expect_void_str ()

  method! inform_disqualified () =
    (* not defined in the remote protocol, so simulated with "losing" *)
    self#inform_tournament_result false

  method! inform_tournament_result (did_win : bool) =
    self#send_call (Call.End did_win);
    self#expect_void_str ()

  method! dispose () =
    (* Maybe channel is automatically closed if remote connection shut down? *)
    (try In_channel.close ic with _ -> ()); 
    (try Out_channel.close oc with _ -> ());

  method private place_penguin_impl (state : GS.t) : Pos.t option =
    self#send_call (Call.Setup state);
    Option.bind ~f:(Fn.compose Result.ok S.to_pos) @@ self#get_next_input()

  (* Our player interface doesn't have "play-with", so we simulate it *)
  method private play_with (state : GS.t) : bool =
    let my_color = GS.get_current_player state |> PS.get_color in
    let other_colors = GS.get_ordered_players state
                       |> List.map ~f:PS.get_color
                       |> List.filter ~f:(fun c -> not @@ PC.equal c my_color)
    in
    self#send_call (Call.PlayWith other_colors);
    self#expect_void_str ()

  (* return [None] _any_ exception is raised *)
  method private get_next_input () : S.t option =
    try Some(Stream.next inputs)
    with _ -> None

  (* Return [true] if we receive a void string back from [inputs] *)
  method private expect_void_str () : bool =
    match Option.bind ~f:S.to_string @@ self#get_next_input() with
    | Some(s) when String.(s = void_ackn_msg) -> true
    | _ -> false

  (* Write serialized [call] to [oc] immediately;
   * catch and ignore _any_ exception raised *)
  method private send_call (call : Call.t) : unit =
    let msg = S.to_json_string @@ Call.serialize call in
    try write_to_outchan_now oc msg;
    with _ -> ()
end


(* Return [false] if Call.End is received, i.e., the tournament has ended *)
let handle_remote_call
    (player : Player.t) (call : Call.t) (oc : Out_channel.t): bool =
  (* Serialize [Call.ackn_msg] and send it to [oc] immediately *)
  let send_void () : unit =
    write_to_outchan_now oc @@ (S.from_string void_ackn_msg |> S.to_json_string);
  in
  (match call with
   | Start -> if player#inform_tournament_start() then send_void ();
   | PlayAs(color) -> if player#assign_color color then send_void ();
   | PlayWith(_) -> send_void (); (* not implemented in our codebase *)
   | Setup(state) ->
     Option.iter (player#place_penguin state) ~f:(fun pos ->
         write_to_outchan_now oc @@ (S.from_pos pos |> S.to_json_string));
   | TakeTurn(state, _) ->
     Option.iter (player#take_turn @@ GT.create state) ~f:(fun act ->
         write_to_outchan_now oc @@ (S.from_action act |> S.to_json_string));
   | End(did_win) ->
     if player#inform_tournament_result did_win then send_void ());
  match call with
  | End(_) -> true
  | _ -> false
;;

let interact_with_proxy_chans (player : Player.t)
    (ic : In_channel.t) (oc : Out_channel.t) : unit =
  let inputs : S.t Stream.t = S.stream_from_channel ic in
  let rec loop () : unit =
    let next_s = Stream.next inputs in
    match Call.deserialize next_s with
    | None -> Printf.printf "Invalid remote call message: %s\n"
                (S.to_json_string next_s);
    | Some(call) -> if not (handle_remote_call player call oc) then loop ()
  in
  let name_str = player#get_name() |> S.from_string |> S.to_json_string in
  write_to_outchan_now oc name_str;
  loop ();
;;

let interact_with_proxy player ~ipaddr ~port =
  let server_addr = Unix.Inet_addr.of_string ipaddr in
  let sockaddr = Unix.ADDR_INET(server_addr, port) in 
  let ic, oc = Unix.open_connection sockaddr in
  interact_with_proxy_chans player ic oc;
  (* sends EOF to server, not sure why ic is input, also closes oc... *)
  Unix.shutdown_connection ic;
  In_channel.close ic;
;;
