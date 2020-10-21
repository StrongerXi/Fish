type t =
  { board : Board.t
  ; players : Player_list.t
  }

let create board colors = { board; players = Player_list.create colors }
let get_board t = t.board
let get_player_list t = t .players

let get_board_minus_penguins t =
  let board = ref @@ Board.get_copy t.board in
  Player_list.get_ordered_players t.players |> List.iter
    (fun p -> 
       Player_state.get_penguin_positions p |> List.iter
         (fun pos -> board := Board.remove_tile_at !board pos));
  !board

let place_penguin t color pos =
  if Board.within_board t.board pos
  then { t with players = Player_list.place_penguin t.players color pos }
  else failwith "Position is outside the board"

let move_penguin t src dst =
  let fish = Board.get_tile_at t.board src |> Tile.get_fish in
  let players = Player_list.move_penguin t.players src dst fish in
  let board = Board.remove_tile_at (Board.get_copy t.board) src in
  { board; players }
