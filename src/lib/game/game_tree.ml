open !Core

module GS = Game_state
module PS = Player_state

type t =
  { state : GS.t
  ; subtrees : (Action.t * t) list option ref
  }

let create state = { state; subtrees = ref None }
;;

let get_state t = t.state
;;

(** A helper function that computes all legal moves for [t]'s current player,
    and associates them with the resulting game tree. *)
let compute_subtrees_with_moves t : (Action.t * t) list =
  let board = GS.get_board_minus_penguins t.state
  and current = GS.get_current_player t.state in
  (* Return all legal positions a penguin can move to from [src] on [board] *)
  let get_legal_move_dsts_from (src : Position.t) : Position.t list =
    Board.get_reachable_from board src
    |> List.map ~f:(fun (_, dsts) -> dsts) |> List.concat
  in
  let open List.Let_syntax in
  let%bind src = PS.get_penguins current |> List.map ~f:Penguin.get_position in
  let%bind dst = get_legal_move_dsts_from src in
  let next_state = GS.move_penguin t.state src dst in
  let act = Action.Move({ src; dst }) in
  return (act, { state = next_state; subtrees = ref None })
;;

let rec get_subtrees t =
  match !(t.subtrees) with
  | Some(subtrees) -> subtrees
  | None -> generate_until_moveable_subtree t

(** Skip subtrees without legal moves, until we reach a player that can move.
    The returned list has the form [[moveable; last-skipped; ...; t]]
    Return empty list if no player can move. *)
and skip_to_moveable_tree t : t list =
  let start_color = GS.get_current_player t.state |> PS.get_player_color in
  (* [t]   is the current tree to be examined, [t = start] only for init call.
     [acc] is the previously skipped trees in reversed order. *)
  let rec skip_to_moveable_tree_until_start t acc : t list =
    match compute_subtrees_with_moves t with
    | _::_ -> t::acc
    | [] -> (* current player in [t] can't move, skip it *)
      let next_gs = GS.rotate_to_next_player t.state in
      let next_color = GS.get_current_player next_gs |> PS.get_player_color in
      let next_t = { t with state = next_gs } in
      if Core.phys_same next_color start_color
      then [] (* no player was able to move *)
      else skip_to_moveable_tree_until_start next_t (t::acc)
  in 
  skip_to_moveable_tree_until_start t []

(** In general, the subtrees of [t] has the form:
      t --skip--> t1 --skip--> ... --skip--> tk --moves--> ...
    or it has no subtrees if no player can move.
    EFFECT: Update the [subtrees] field from [t] to [tk] to "stitch them up"
    Return [!t.subtrees], which is emptty if no moveable subtree exists *)
and generate_until_moveable_subtree t : (Action.t * t) list = 
  (match skip_to_moveable_tree t with
   | [] -> t.subtrees := Some([]);
   | moveable::ts ->
     Core.ignore @@
     List.fold_left ts (* Stitch the skipped subtrees together *)
       ~f:(fun subtree t -> t.subtrees := Some([(Action.Skip, subtree);]); t)
       ~init:moveable);
  match !(t.subtrees) with
  | None -> failwith "subtrees of t must have been initialized"
  | Some(subtrees) -> subtrees
;;
