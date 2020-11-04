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
    (* current game state, updated during [run_game] *)
  { mutable state : Game_state.t option
  ; mutable color_to_player : (Color.t * Player.t) list
  ; mutable cheaters : Player.t list
  ; mutable failed : Player.t list
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

(* Some mutually recursive helper functions to implement the game loop *)
let rec game_loop (t : t) (tree : GT.t) : unit =
  match GT.get_subtrees tree with
  | [] -> () (* Game over *)
  (* TODO inform skip event to observers *)
  | [(Action.Skip, tree);] -> game_loop t tree
  | subtrees -> 
    let state = GT.get_state tree in
    let color = GS.get_current_player state
                |> Player_state.get_player_color in
    let player = get_player_with_color t color in
    (* TODO handle player unable_to_respond failure *)
    let action = Player.take_turn player tree in
    match List.Assoc.find ~equal:Action.equal subtrees action with
    | None -> remove_current_player t tree
    | Some(tree) -> game_loop t tree

and remove_current_player (t : t) (tree : GT.t) : unit =
  let state = GT.get_state tree in
  let state = GS.remove_current_player state in
  let next_state = GS.rotate_to_next_player state in
  game_loop t @@ GT.create next_state
;;

let collect_result t : Game_result.t =
  match t.state with
  | None -> failwith "Game state must be available in [collect_result]"
  | Some(state) ->
    let players = (* sorted from lower to higher score *)
      List.sort (GS.get_ordered_players state)
        ~compare:(fun p1 p2 -> Int.compare (PS.get_score p1) (PS.get_score p2))
    in
    let max_score = Option.value_map ~default:0 ~f:PS.get_score @@ List.hd players in
    let winners = 
      List.filter ~f:(fun p -> (PS.get_score p) = max_score) players
      |> List.map ~f:(fun p -> get_player_with_color t @@ PS.get_player_color p)
    in
    let rest = 
      List.filter ~f:(fun p -> (PS.get_score p) <> max_score) players
      |> List.map ~f:(fun p -> get_player_with_color t @@ PS.get_player_color p)
    in
    { winners; rest; failed = t.failed; cheaters = t.cheaters }
;;

let run_game t config =
  let board = Board.create config in
  let colors = List.map ~f:Tuple.T2.get1 t.color_to_player in
  let state = GS.create board colors in
  t.state <- (Some state);
  game_loop t (GT.create state);
  collect_result t
;;
