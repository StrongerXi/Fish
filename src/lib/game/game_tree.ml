open !Core

module GS = Game_state
module PS = Player_state

type t =
  { state : GS.t
  ; mutable subtrees : (Action.t * t) list option
  }

let create state = { state; subtrees = None }
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
  let next_gs = GS.rotate_to_next_player t.state in (* player always rotates *)
  let%bind src = PS.get_penguins current |> List.map ~f:Penguin.get_position in
  let%bind dst = get_legal_move_dsts_from src in
  let moved_next_gs = GS.move_penguin next_gs src dst in
  let act = Action.Move({ src; dst }) in
  return (act, { state = moved_next_gs; subtrees = None })
;;

(** In general, the subtrees of [t] has the form:
      t --skip--> t1 --skip--> ... --skip--> tk --moves--> ...
    or it has no subtrees if no player can move.
    EFFECT: Update the [subtrees] field from [t] to [tk] to "stitch them up"
    If it didn't generate up to a moveable subtree, [t.subtree] is [Some[]] *)
let generate_until_moveable_subtree t : unit =
  let start_color = GS.get_current_player t.state |> PS.get_player_color in
  (* if _next_ (not current) player in [t] has color [start_color], stop. *)
  let rec generate_to_moveable_tree_until_start t : t option =
    match compute_subtrees_with_moves t with
    | _::_ as subtrees -> t.subtrees <- Some(subtrees); Some(t)
    | [] -> (* current player in [t] can't move, skip it *)
      let open Option.Let_syntax in
      let next_gs = GS.rotate_to_next_player t.state in
      let next_color = GS.get_current_player next_gs |> PS.get_player_color in
      let next_t = { t with state = next_gs } in
      let%bind subtree =
        if Core.phys_same next_color start_color
        then None (* no player was able to move *)
        else generate_to_moveable_tree_until_start next_t in
      t.subtrees <- Some[(Action.Skip, subtree);];
      return t
  in match generate_to_moveable_tree_until_start t with
  | None -> t.subtrees <- Some([]);
  | _ -> ()
;;

let get_subtrees t =
  match t.subtrees with
  | Some(subtrees) -> subtrees
  | None -> 
    generate_until_moveable_subtree t;
    match t.subtrees with
    | None -> failwith "subtrees of t must have been initialized"
    | Some(subtrees) -> subtrees
;;
