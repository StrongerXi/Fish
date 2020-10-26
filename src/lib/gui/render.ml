module GS = Game.Game_state
module BD = Game.Board
module PC = Game.Player_state.Player_color
module PS = Game.Player_state
module Tile = Game.Tile
module Penguin = Game.Penguin
module Pos = Util.Position

module Constants = struct
  let tile_size = 30 (* In terms of pixels *)
  let window_title = "Fish!"
  let fish_color          = Graphics.black
  let tile_boundary_color = Graphics.black
  let hole_tile_color    = Graphics.rgb 200 200 200
  let occupied_tile_color = Graphics.rgb 255 165 0
end
(* -------------------------------------------------------------- *)
(* ----------------------- helper functions --------------------- *)
(* -------------------------------------------------------------- *)

(** For the hexagon located at (row, col), return the unscaled (row, col) of
    the topleft point of the smallest rectangle enclosing the hexagon. *)
let get_topleft_boundary_pos (row : int) (col : int) (height : int) : (int * int) =
  let colOffset = if row mod 2 == 1 then 2 else 0 in
  let row = height - row - 1 in (* [Graphics] has lower left corner as (0, 0) *)
  (row, colOffset + 4 * col)

(** A hexagon tile is rendered as
      A------B
     /        \
    F  fish... C
     \        /
      E------D
    Where the coordiates relative to top-left point of bounding rectangle are:
    A = (0, 1), B = (0, 2), C = (1, 3), D = (2, 2), E = (2, 1), F = (1, 0)
    where each coordinate is (row, col), and normalized by [tile_size] *)
let render_tile (tile : Tile.t) ({ Pos.row; col } : Pos.t) (height : int) : unit =
  let dr, dc = get_topleft_boundary_pos row col height in
  let open Constants in
  (* draw tile *)
  let vertices = 
    Array.map 
      (fun (r, c) -> (c + dc) * tile_size , (r + dr) * tile_size)
      [| (0, 1); (0, 2); (1, 3); (2, 2); (2, 1); (1, 0); |]
  in
  Graphics.set_color tile_boundary_color;
  Graphics.draw_poly vertices;
  if Tile.is_hole tile
  then Graphics.set_color hole_tile_color
  else Graphics.set_color occupied_tile_color;
  Graphics.fill_poly vertices;
  (* draw fish *)
  Graphics.moveto ((dc + 1) * tile_size) ((dr + 1) * tile_size);
  Graphics.set_color fish_color;
  Graphics.draw_string (string_of_int @@ Tile.get_fish tile);
;;

(** Render [penguin] at its position with [color] *)
let render_penguin (penguin : Penguin.t) (color : PC.t) (height : int) : unit =
  let { Pos.row; col } = Penguin.get_position penguin in
  let row, col = get_topleft_boundary_pos row col height in
  let c = match color with
    | White -> Graphics.white
    | Red   -> Graphics.red
    | Black -> Graphics.black
    | Brown -> Graphics.rgb 210 105 30
  in
  let open Constants in
  Graphics.set_color c;
  let x = (col + 1) * tile_size + tile_size / 2 in
  let y = (row + 1) * tile_size in
  let r = tile_size / 2 in
  Graphics.fill_circle x y r;
;;

(** Render relavent information associated with [player] *)
let render_player (player : PS.t) (height : int) : unit =
  let color = PS.get_player_color player in
  PS.get_penguins player |> List.iter
    (fun penguin -> render_penguin penguin color height);
;;

(** Render the entire board of hexagon tiles *)
let render_board (board : BD.t) : unit =
  let height, width = BD.get_height board, BD.get_width board in
  Pos.create_positions_within ~width ~height |> List.iter 
    (fun pos -> render_tile (BD.get_tile_at board pos) pos height);
;;

(** Resize the window based on game state *)
let resize_window (gs : GS.t) : unit =
  let board = GS.get_board_copy gs in
  let height, width = BD.get_height board, BD.get_width board in
  let min_height = (1 + height) * Constants.tile_size in
  let min_width  = (1 + 4 * width) * Constants.tile_size in
  Graphics.resize_window min_width min_height;
;;


(* -------------------------------------------------------------- *)
(* ---------------------- exported functions -------------------- *)
(* -------------------------------------------------------------- *)
let render =
  Printexc.record_backtrace true;
  let already_init = ref false in
  (fun gs -> 
     let init () = 
       Graphics.open_graph "";
       Graphics.set_window_title Constants.window_title;
       Graphics.resize_window 1 1;
       Graphics.auto_synchronize false; 
       already_init := true;
     in
     if not @@ !already_init
     then init ();
     resize_window gs;
     render_board @@ GS.get_board_copy gs;
     let height = gs |> GS.get_board_copy |> BD.get_height in
     gs |> GS.get_ordered_players |> List.iter 
       (fun p -> render_player p height);
     Graphics.display_mode true;
     Graphics.synchronize ();
  )
;;
