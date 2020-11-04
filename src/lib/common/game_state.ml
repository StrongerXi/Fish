open !Core

module Player_color = Player_state.Player_color
module PS = Player_state
module CQ = Util.Circular_queue

(* For implementation convenience *)
module Player_list = struct
  (** A [t] represents state of all the players in a fish game.
      It's agnostic to the board (boundary, etc.)
      It ensures: 
      - all players have distinct colors
      NOTE that it's immutable *)
  type t =
    { players : PS.t list
    }

  (** Create a [t] with 1 player for each of the given colors. *)
  let create (colors : Player_color.t list) : t =
    if List.contains_dup ~compare:Player_color.compare colors
    then failwith "Colors in a fish game must be unique"
    else { players = List.map ~f:PS.create colors }
  ;;

  (** Return [None] if no player has [color] in [t] *)
  let get_player_with_color t color : PS.t option  = 
    List.find t.players
      ~f:(fun p -> Player_color.equal color (PS.get_player_color p))
  ;;

  (** Return [None] if no player has [color] in [t] *)
  let remove_player_with_color t color : t option =
    let rec remove_in_players players (checked : PS.t list) : PS.t list option =
      match players with
      | [] -> None
      | p::players ->
        if Player_color.equal color (PS.get_player_color p)
        then Some(checked @ players)
        else remove_in_players players (p::checked)
    in
    Option.map ~f:(fun players -> { players }) (remove_in_players t.players [])
  ;;

  let any_player_has_penguin_at t (pos : Position.t) : bool =
    let player_has_penguin_at_pos (p : PS.t) : bool =
      PS.get_penguins p 
      |> List.map ~f:Penguin.get_position
      |> List.exists ~f:([%compare.equal: Position.t] pos)
    in
    List.exists t.players ~f:player_has_penguin_at_pos
  ;;

  (** Move the penguin at [src] to [dst]. The integer represents the # of fish
      on the tile at 1st position  Update player score based on this # of fish.
      Errors if no penguin is at [src], or a penguin exists at [dst] *)
  let move_penguin t (src: Position.t) (dst : Position.t) (fish : int) : t =
    let rec update_players players =
      match players with
      | [] -> failwith "No penguin resides at source position"
      | p::players ->
        match PS.move_penguin p src dst with
        | None    -> p::(update_players players)
        | Some(p) ->
          let new_score = fish + PS.get_score p in
          (PS.set_score p new_score)::players
    in
    if any_player_has_penguin_at t dst
    then failwith "Cannot move penguin to a tile occupied by another penguin"
    else { players = update_players t.players }
  ;;

  (** Place a new penguin with given color at given position on the board.
      Errors if the no the participating player has given color *)
  let place_penguin t (color : Player_color.t) (pos : Position.t) : t =
    let penguin = Penguin.create pos in
    let rec update_players players =
      match players with
      | [] -> failwith "No player has given color"
      | p::players ->
        if Player_color.equal color @@ PS.get_player_color p
        then (PS.add_penguin p penguin)::players
        else p::(update_players players)
    in
    if any_player_has_penguin_at t pos
    then failwith "Cannot place penguin onto a tile occupied by another penguin"
    else { players = update_players t.players }
  ;;

  (** Discouraged unless you have good reason and know what you are doing *)
  let from_players (players : PS.t list) : (t, string) result = 
    let colors = List.map ~f:PS.get_player_color players in
    if List.contains_dup ~compare:Player_color.compare colors
    then Result.fail "Players must have distinct colors"
    else Result.return { players }
end

module PL = Player_list

type t =
  { board : Board.t
  ; players : PL.t
  ; order : Player_color.t CQ.t
  }

let create board colors = 
  match colors with
  | [] -> failwith "There must be at least 1 player in a game"
  | start::nexts ->
    { board; players = PL.create colors; order = CQ.create start nexts }
;;

let get_board_copy t = Board.get_copy t.board
;;

let get_ordered_players t = 
  let opt_players = 
    CQ.to_list t.order
    |> List.map ~f:(PL.get_player_with_color t.players)
    |> Option.all in
  match opt_players with
  | None -> failwith "Some color(s) are missing in player list"
  | Some(players) -> players
;;

let get_current_player t = 
  match PL.get_player_with_color t.players @@ CQ.get_current t.order with
  | None -> failwith "Current player color is missing in player list"
  | Some(player) -> player
;;

let rotate_to_next_player t = 
  { t with order = CQ.rotate t.order }
;;

let remove_current_player t =
  let current_color = CQ.get_current t.order in
  match CQ.remove_current t.order with
  | None -> failwith "Cannot remove the last player in a game state"
  | Some(order) ->
    match (PL.remove_player_with_color t.players current_color) with
    | None -> failwith "Current color is missing in player list"
    | Some(players) -> { t with players; order }
  

let get_player_with_color t color = 
  match PL.get_player_with_color t.players color with
  | None -> failwith "No player has specified color in this game state"
  | Some(p) -> p
;;

let get_board_minus_penguins t =
  let board = ref @@ Board.get_copy t.board in
  let remove_penguin_tiles_of_player (p : PS.t) : unit =
    PS.get_penguins p |> List.iter ~f:(fun pg -> 
        board := Board.remove_tile_at !board (Penguin.get_position pg))
  in
  get_ordered_players t |> List.iter ~f:remove_penguin_tiles_of_player;
  !board
;;

let place_penguin t color pos =
  if Tile.is_hole @@ Board.get_tile_at t.board pos
  then failwith "Cannot place penguin onto a hole";
  { t with players = PL.place_penguin t.players color pos }
;;

let move_penguin t src dst =
  if Tile.is_hole @@ Board.get_tile_at t.board dst
  then failwith "Cannot move a penguin onto a hole";
  let fish = Board.get_tile_at t.board src |> Tile.get_fish in
  let players = PL.move_penguin t.players src dst fish in
  let board = Board.remove_tile_at (Board.get_copy t.board) src in
  { t with board; players }
;;

let from_board_players board players =
  match players with
  | [] -> Result.fail "There must be at least 1 player in a game"
  | start::nexts ->
    let open Result.Let_syntax in
    let%bind player_list = PL.from_players players in
    let penguin_positions = 
      List.concat_map ~f:PS.get_penguins players
      |> List.map ~f:Penguin.get_position in
    if List.contains_dup ~compare:Position.compare penguin_positions
    then Result.fail "Each tile must have at most 1 penguin"
    else
    if List.exists ~f:(Fun.negate @@ Board.within_board board) penguin_positions
    then Result.fail "All penguins must reside on tiles withint the board"
    else
    if List.map ~f:(Board.get_tile_at board) penguin_positions
       |> List.exists ~f:Tile.is_hole
    then Result.fail "No penguin should reside on a hole"
    else
    let start = PS.get_player_color start in
    let nexts = List.map ~f:PS.get_player_color nexts in
    let order = CQ.create start nexts in
    return { board; players = player_list; order }
;;
