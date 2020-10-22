(** A helper module to manage turns of player in a Fish game *)
module Player_Order = struct
  (* Essentially a 2 list based circular queue *)
  type t = 
    { nexts : Player_color.t list
    ; current : Player_color.t
    ; prevs : Player_color.t list
    }

  (** Create a player order based on [players], i.e. the first color in [players]
      is will be the current player in the resulting order *)
  let create (players : Player_color.t list) =
    match players with
    | [] -> failwith "Players must be non-empty"
    | p::ps -> { nexts = ps; current = p; prevs = [] }
  ;;

  let get_current (t : t) = t.current
  ;;

  (** Return a new order where the next player becomes the current player *)
  let rotate ({ nexts; current; prevs } : t) =
    match nexts with
    | [] -> create @@ List.rev (current::prevs)
    | n::ns -> { nexts = ns; current = n; prevs = current::prevs }
  ;;
end


type t =
  { state : Game_state.t
  ; order : Player_Order.t
  }

let create state start = 
  let players = state 
                |> Game_state.get_player_list |> Player_list.get_ordered_players
                |> List.map Player_state.get_player_color 
  in
  (** Find [start] in [players], and then create a [Player_Order.t].
      [prevs] contains the already checked players in reversed order.
      Return [None] if [start] is not found in [players]. *)
  let rec partition players prevs : Player_Order.t option =
    match players with
    | p::players ->
      if p = start
      then Some(Player_Order.create @@ start::players @ (List.rev prevs))
      else partition players (p::prevs)
    | [] -> None
  in
  match partition players [] with
  | None -> failwith "Starting player is not in the state"
  | Some(order) -> { state; order }
;;

let get_state t = t.state
;;

let get_current_player t = Player_Order.get_current t.order
;;

let get_next_nodes t =
  let board = Game_state.get_board_minus_penguins t.state
  and players = 
    t.state |> Game_state.get_player_list |> Player_list.get_ordered_players
  and current = get_current_player t
  in
  (* Return all legal action that can be taken starting from [src] on [board] *)
  let get_legal_actions_from (src : Position.t) : Action.t list =
    Board.get_reachable_from board src
    |> List.map (fun (_, dsts) -> dsts) |> List.flatten
    |> List.map (fun dst -> Action.Move(src, dst))
  in
  let next_order = Player_Order.rotate t.order in
  (* [create] guarantees current player is in the player list *)
  List.find (fun p -> current = Player_state.get_player_color p) players
  |> Player_state.get_penguins 
  |> List.map Penguin.get_position
  |> List.map get_legal_actions_from
  |> List.flatten
  |> List.map 
    (fun (Action.Move(src, dst) as act) -> 
       let next_state = Game_state.move_penguin t.state src dst in
       (act, { state = next_state; order = next_order }))
;;

let skip_to_moveable_tree t =
  let start = get_current_player t in
  (** [t]   is the current tree to be examined, [t = start] only for init call.
      [acc] is the previously skipped trees in reversed order. *)
  let rec skip_to_moveable_tree_until_start t acc : t list =
    match get_next_nodes t with
    | _::_ -> t::acc
    | [] -> 
      let next_t = { t with order = Player_Order.rotate t.order } in
      let next_player = get_current_player next_t in
      if next_player = start
      then [] (* no player was able to move *)
      else skip_to_moveable_tree_until_start next_t (t::acc)
  in 
  skip_to_moveable_tree_until_start t []
;;
