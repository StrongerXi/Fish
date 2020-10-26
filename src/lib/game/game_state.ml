open !Core

type t =
  { board : Board.t
  ; players : Player_list.t
  }

let create board colors = 
  if List.contains_dup ~compare:Player_color.compare colors
  then failwith "colors in a fish game must be unique"
  else { board; players = Player_list.create colors }

let get_board_copy t = Board.get_copy t.board
let get_ordered_players t = Player_list.get_ordered_players t.players

let get_board_minus_penguins t =
  let board = ref @@ Board.get_copy t.board in
  let remove_penguin_tiles_of_player (p : Player_state.t) : unit =
    Player_state.get_penguins p |> List.iter ~f:(fun pg -> 
        board := Board.remove_tile_at !board (Penguin.get_position pg))
  in
  get_ordered_players t |> List.iter ~f:remove_penguin_tiles_of_player;
  !board

let place_penguin t color pos =
  if Tile.is_hole @@ Board.get_tile_at t.board pos
  then failwith "Cannot place penguin into a hole";
  { t with players = Player_list.place_penguin t.players color pos }

let move_penguin t src dst =
  let fish = Board.get_tile_at t.board src |> Tile.get_fish in
  let players = Player_list.move_penguin t.players src dst fish in
  let board = Board.remove_tile_at (Board.get_copy t.board) src in
  { board; players }

let from_board_players board players = { board; players = Player_list.from_players players }
