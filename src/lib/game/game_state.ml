open !Core

(* For implementation convenience *)
module Player_list = struct
  (** A [t] represents state of all the players in a fish game, including their
      turn order.
      It's agnostic to the board (boundary, etc.)
      NOTE that it's immutable *)
  type t =
    { players : Player_state.t list
    }

  (** Create a [t] with 1 player for each of the given colors.
      Errors if there exists any duplicates *)
  let create (colors : Player_color.t list) : t = 
    { players = List.map ~f:Player_state.create colors }
  ;;

  (** Get states of all the players, ordered by how they take turns *)
  let get_ordered_players t : Player_state.t list = t.players
  ;;

  let get_player_with_color t color = 
    List.find t.players
      ~f:(fun p -> Core.phys_equal color (Player_state.get_player_color p))
  ;;

  let any_player_has_penguin_at t (pos : Position.t) : bool =
    let player_has_penguin_at_pos (p : Player_state.t) : bool =
      Player_state.get_penguins p 
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
        match Player_state.move_penguin p src dst with
        | None    -> p::(update_players players)
        | Some(p) ->
          let new_score = fish + Player_state.get_score p in
          (Player_state.set_score p new_score)::players
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
        if Core.phys_equal color @@ Player_state.get_player_color p
        then (Player_state.add_penguin p penguin)::players
        else p::(update_players players)
    in
    { players = update_players t.players }
  ;;

  (** Discouraged unless you have good reason and know what you are doing *)
  let from_players (players : Player_state.t list) : t = { players }
end


type t =
  { board : Board.t
  ; players : Player_list.t
  }

let create board colors = 
  if List.contains_dup ~compare:Player_color.compare colors
  then failwith "colors in a fish game must be unique"
  else { board; players = Player_list.create colors }
;;

let get_board_copy t = Board.get_copy t.board
;;

let get_ordered_players t = Player_list.get_ordered_players t.players
;;

let get_player_with_color t color = 
  match Player_list.get_player_with_color t.players color with
  | None -> failwith "No player has specified color in this game state"
  | Some(p) -> p
;;

let get_board_minus_penguins t =
  let board = ref @@ Board.get_copy t.board in
  let remove_penguin_tiles_of_player (p : Player_state.t) : unit =
    Player_state.get_penguins p |> List.iter ~f:(fun pg -> 
        board := Board.remove_tile_at !board (Penguin.get_position pg))
  in
  get_ordered_players t |> List.iter ~f:remove_penguin_tiles_of_player;
  !board
;;

let place_penguin t color pos =
  if Tile.is_hole @@ Board.get_tile_at t.board pos
  then failwith "Cannot place penguin onto a hole";
  { t with players = Player_list.place_penguin t.players color pos }
;;

let move_penguin t src dst =
  let fish = Board.get_tile_at t.board src |> Tile.get_fish in
  let players = Player_list.move_penguin t.players src dst fish in
  let board = Board.remove_tile_at (Board.get_copy t.board) src in
  { board; players }
;;

let from_board_players board players = 
  { board; players = Player_list.from_players players }
;;
