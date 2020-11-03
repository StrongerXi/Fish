open !Core

module Game_result = struct
  type t =
    { winners : Player.t list
    ; cheaters : Player.t list
    ; failed : Player.t list
    ; rest : Player.t list
    }
end

module Color = Player_state.Player_color

(** A [t] represents a referee which manages an entire fish game from start to
    end. A [t] manages exactly 1 game and becomes obselete after the game ends.
    It can:
    - Set up and run a game given an ordered list of [Player.t]s
    - Report final result of a game after it's finished *)
type t =
    (* current game state, updated during [run_game] *)
  { mutable state : Game_state.t option
  ; mutable color_to_player : (Color.t * Player.t) list
  ; mutable cheaters : Player.t list
  ; mutable failed : Player.t list
  }


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

let collect_result _ : Game_result.t =
  failwith "Unimplemented"
;;

(* TODO how does client query current game state during a [run_game]?
   Need some synchronization mechanism *)
(* TODO break this up into smaller mutually recursive functions that take in 
   [t] and [tree], e.g., [game_loop], [handle_cheater], etc. *)
let run_game t config =
  let board = Board.create config in
  let colors = List.map ~f:Tuple.T2.get1 t.color_to_player in
  let state = Game_state.create board colors in
  let rec loop tree : unit =
    match Game_tree.get_subtrees tree with
    | [] -> ()
    | [(Action.Skip, tree);] -> 
      loop tree (* TODO inform skip event to observers *)
    | subtrees -> 
      let state = Game_tree.get_state tree in
      let color = Game_state.get_current_player state
                  |> Player_state.get_player_color in
      let player = 
        List.Assoc.find_exn ~equal:Color.equal t.color_to_player color in
      (* TODO handle player unable_to_respond failure *)
      let action = Player.take_turn player tree in
      match List.Assoc.find ~equal:Action.equal subtrees action with
      | None -> 
        (* TODO remove player and add to cheater *)
        let next_state = Game_state.rotate_to_next_player state in
        loop @@ Game_tree.create next_state
      | Some(tree) -> loop tree
  in
  t.state <- (Some state);
  loop @@ Game_tree.create state;
  collect_result t
;;
