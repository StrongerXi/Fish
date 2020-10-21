module C = Board_config

type t =
  { tiles : Tile.t array array
  }

module Direction = struct
  type t =
    | North
    | South
    | Northeast
    | Southeast
    | Northwest
    | Southwest

  (** The amount of offset in row and column to take 1 step in [dir],
      starting from any position on [row] *)
  let get_row_col_offset dir row =
    let even_row = row mod 2 == 0 in
    match dir with
    | North     -> (~-2, 0)
    | South     -> (  2, 0)
    | Northeast -> (~-1, if even_row then 0 else   1)
    | Southeast -> (  1, if even_row then 0 else   1)
    | Northwest -> (~-1, if even_row then ~-1 else 0)
    | Southwest -> (  1, if even_row then ~-1 else 0)

  let values = [North; South; Northeast; Southeast; Northwest; Southwest]
end

let create config =
  let width, height = C.get_width config, C.get_height config in
  if width <= 0 || height <= 0
  then failwith "Board dimension must be positive";

  let holes = C.get_holes config in
  let dft_fish = C.get_default_num_of_fish config in
  let one_fish_tile = ref @@ C.get_min_num_of_one_fish_tile config in

  let tiles = Array.make_matrix height width @@ Tile.create dft_fish in
  holes |> List.iter
    (fun {Position.row; col} -> tiles.(row).(col) <- Tile.empty_tile);
  Position.create_positions_within ~height ~width |> List.iter
    (fun {Position.row; col} ->
       if (not @@ Tile.is_empty tiles.(row).(col)) &&
          !one_fish_tile > 0
       then
         tiles.(row).(col) <- Tile.create 1;
         one_fish_tile := !one_fish_tile - 1);
  { tiles }

let get_width t = Array.length t.tiles.(0)

let get_height t = Array.length t.tiles

let within_board t { Position.row; col } =
  let width, height = get_width t, get_height t in
  0 <= row && row < height && 0 <= col && col < width

let get_tile_at t ({ Position.row; col } as pos) =
  if within_board t pos
  then t.tiles.(row).(col)
  else failwith "position is outside the board"

let remove_tile_at t ({ Position.row; col } as pos) =
  if within_board t pos
  then t.tiles.(row).(col) <- Tile.empty_tile
  else failwith "position is outside the board";
  t


let get_reachable_from t src =
  (* From (row, col), attempt to add AMAP position to `acc` following `dir` *)
  let rec add_until_cant row col dir acc =
    let pos = { Position.row; col } in
    if within_board t pos &&
       not @@ Tile.is_empty @@ (get_tile_at t pos)
    then
      let dr, dc = Direction.get_row_col_offset dir row in
      add_until_cant (row + dr) (col + dc) dir (pos::acc)
    else List.rev acc
  in
  if not @@ within_board t src
  then []
  else
    Direction.values |> List.map
      (fun dir ->
         (dir, List.tl @@ add_until_cant src.row src.col dir []))

let get_copy t = { tiles = Array.map Array.copy t.tiles }
