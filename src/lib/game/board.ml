open !Core

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

  (** Return the position of the adjust tile to ([row], [col])
      in direction [dir] *)
  let step_in_dir (row : int) (col : int) (dir : t) : (int * int) =
    let even_row = row mod 2 = 0 in
    match dir with
    | North     -> (row - 2, col)
    | South     -> (row + 2, col)
    | Northeast -> (row - 1, col + if even_row then 0 else   1)
    | Southeast -> (row + 1, col + if even_row then 0 else   1)
    | Northwest -> (row - 1, col + if even_row then ~-1 else 0)
    | Southwest -> (row + 1, col + if even_row then ~-1 else 0)
  ;;

  let values = [North; South; Northeast; Southeast; Northwest; Southwest]

  let step_from { Position.row; col } dir =
    let row, col = step_in_dir row col dir in
    { Position.row; col }
end


module Config = struct
  type t =
    { width : int
    ; height : int
    ; holes : Position.t list
    ; min_one_fish_tile : int
    ; default_num_of_fish : int
    }

  let create ~height ~width = { width; height;
                                holes = [];
                                min_one_fish_tile = 0;
                                default_num_of_fish = 1; }
  ;;

  let set_width width t = { t with width }
  let get_width  t = t.width

  let set_height height t = { t with height }
  let get_height  t = t.height

  let set_holes holes t = { t with holes }
  let get_holes  t = t.holes

  let set_min_num_of_one_fish_tile min_one_fish_tile t = 
    { t with min_one_fish_tile }
  let get_min_num_of_one_fish_tile  t = t.min_one_fish_tile

  let set_default_num_of_fish default_num_of_fish t = 
    { t with default_num_of_fish }
  let get_default_num_of_fish  t = t.default_num_of_fish
end

module C = Config

let create config =
  let width, height = Config.get_width config, Config.get_height config in
  if width <= 0 || height <= 0
  then failwith "Board dimension must be positive";
  let holes = Config.get_holes config in
  let dft_fish = Config.get_default_num_of_fish config in
  let one_fish_tiles_left = ref @@ Config.get_min_num_of_one_fish_tile config in
  let default_tile = Tile.create dft_fish in
  let tiles = Array.make_matrix ~dimy:width ~dimx:height default_tile in
  holes |> List.iter
    ~f:(fun {Position.row; col} -> tiles.(row).(col) <- Tile.hole);
  Position.create_positions_within ~height ~width |> List.iter
    ~f:(fun {Position.row; col} ->
        if (not @@ Tile.is_hole tiles.(row).(col)) &&
           !one_fish_tiles_left > 0
        then
          tiles.(row).(col) <- Tile.create 1;
        one_fish_tiles_left := !one_fish_tiles_left - 1);
  { tiles }
;;

let get_width t = Array.length t.tiles.(0)
;;

let get_height t = Array.length t.tiles
;;

let within_board t { Position.row; col } =
  let width, height = get_width t, get_height t in
  0 <= row && row < height && 0 <= col && col < width
;;

let get_tile_at t ({ Position.row; col } as pos) =
  if within_board t pos
  then t.tiles.(row).(col)
  else failwith "Position is outside the board"
;;

let remove_tile_at t ({ Position.row; col } as pos) =
  if within_board t pos
  then t.tiles.(row).(col) <- Tile.hole
  else failwith "Position is outside the board";
  t
;;


let get_reachable_from t src =
  (* From (row, col), attempt to add AMAP position to `acc` following `dir`
   * Assume (row, col) has been considered. *)
  let rec add_until_cant row col dir acc =
    let row, col = Direction.step_in_dir row col dir in
    let pos = { Position.row; col } in
    if (within_board t pos) && (not @@ Tile.is_hole @@ t.tiles.(row).(col))
    then add_until_cant row col dir (pos::acc)
    else List.rev acc
  in
  if not @@ within_board t src
  then []
  else List.map Direction.values
      ~f:(fun dir -> (dir, add_until_cant src.row src.col dir []))
;;

let get_copy t = { tiles = Array.map ~f:Array.copy t.tiles }
;;

let from_tiles tiles =
  let height = List.length tiles in
  match List.map ~f:List.length tiles |> List.max_elt ~compare:Int.compare with
  | None -> failwith "0 width means empty board"
  | Some(width) ->
    let arr = Array.make_matrix ~dimx:height ~dimy:width Tile.hole in
    List.iteri tiles ~f:(fun row tiles ->
        List.iteri tiles ~f:(fun col tile -> arr.(row).(col) <- tile));
    { tiles = arr }
;;
