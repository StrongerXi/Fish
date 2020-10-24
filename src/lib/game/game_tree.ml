open !Core

module GS = Game_state
module PL = Player_list
module PS = Player_state
module Order = Util.Order

type t =
  { state : GS.t
  ; order : Player_color.t Order.t
  ; subtrees : (Action.t * t) list option ref
  }

let create state start_color = 
  let colors = state 
               |> GS.get_player_list |> PL.get_ordered_players
               |> List.map ~f:PS.get_player_color 
  in
  match Order.create_with_start colors start_color with
  | None -> failwith "Starting player is not in the state"
  | Some(order) -> { state; order; subtrees = ref None }
;;

let get_state t = t.state
;;

let get_current_player t = Order.get_current t.order
;;

(** A helper function that computes all legal moves for [t]'s current player,
    and associates them with the resulting game tree. *)
let compute_subtrees_with_moves t : (Action.t * t) list =
  let board = GS.get_board_minus_penguins t.state
  and players = t.state |> GS.get_player_list |> PL.get_ordered_players
  and current = Order.get_current t.order
  and next_order = Order.rotate t.order in
  (* Return all legal positions a penguin can move to from [src] on [board] *)
  let get_legal_move_dsts_from (src : Position.t) : Position.t list =
    Board.get_reachable_from board src
    |> List.map ~f:(fun (_, dsts) -> dsts) |> List.concat
  in
  let open List.Let_syntax in
  let%bind src = 
    (* [create] guarantees current player is in the player list *)
    List.find_exn players
      ~f:(fun p -> Core.phys_equal current (PS.get_player_color p)) 
    |> PS.get_penguins |> List.map ~f:Penguin.get_position
  in
  let%bind dst = get_legal_move_dsts_from src in
  let next_state = GS.move_penguin t.state src dst in
  let act = Action.Move(src, dst) in
  return (act, { state = next_state; order = next_order; subtrees = ref None })
;;

let rec get_subtrees t =
  match !(t.subtrees) with
  | Some(subtrees) -> subtrees
  | None -> generate_until_moveable_subtree t
(** Skip players without legal moves, until we reach a player that can move.
    If no player can move, return empty list.
    Else the first tree in the returned list is the one whose current player can
    make a move (i.e., [get_next_nodes] will return non-empty result). The other
    trees in the list are the ones skipped, in reveresed order. *)
and skip_to_moveable_tree t : t list =
  let start_color = get_current_player t in
  (** [t]   is the current tree to be examined, [t = start] only for init call.
      [acc] is the previously skipped trees in reversed order. *)
  let rec skip_to_moveable_tree_until_start t acc : t list =
    match compute_subtrees_with_moves t with
    | _::_ -> t::acc
    | [] -> 
      let next_t = { t with order = Order.rotate t.order } in
      let next_color = get_current_player next_t in
      if Core.phys_same next_color start_color
      then [] (* no player was able to move *)
      else skip_to_moveable_tree_until_start next_t (t::acc)
  in 
  skip_to_moveable_tree_until_start t []
(** In general, the subtrees of [t] has the form:
      t --skip--> t1 --skip--> ... --skip-->tk --moves-> ...
    or it has no subtrees if no player can move.
    [generate_until_moveable_subtree] compute and stitch together all the
    skipped subtrees until one that has legal moves. 
    EFFECT: Update the [subtrees] field for [t] all the way to [tk].
    Note that if [t] == [tk], there is no skipping at all *)
and generate_until_moveable_subtree t : (Action.t * t) list = 
  let () =
    match skip_to_moveable_tree t with
    | [] -> t.subtrees := Some([]);
    | moveable::ts ->
      Core.ignore @@
      List.fold_left ts (* Stitch the skipped subtrees together *)
        ~f:(fun subtree t -> t.subtrees := Some([(Action.Skip, subtree);]); t)
        ~init:moveable
  in
  match !(t.subtrees) with
  | None -> failwith "subtrees of t must have been initialized"
  | Some(subtrees) -> subtrees
;;