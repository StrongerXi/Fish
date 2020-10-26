open !Core

module YB = Yojson.Basic
module GS = Game_state

type t = YB.t

(* -------------------------------------------------------------- *)
(* ----------------------- helper functions --------------------- *)
(* -------------------------------------------------------------- *)
let rec assoc_opt (kvs : ('a * 'b) list) (key : 'a) : 'b option =
  match kvs with
  | (k, value)::_ when Stdlib.(=) k key -> Some(value)
  | _::kvs -> assoc_opt kvs key
  | _ -> None


let from_pos (pos : Position.t) : t =
  `List([`Int(pos.row); `Int(pos.col)])

let to_pos (t : t) : Position.t option =
  match t with
  | `List([`Int(row); `Int(col)]) -> Some({ Position.row; col })
  | _ -> None
;;

let from_tile (tile : Tile.t) : t = `Int(Tile.get_fish tile)

let to_tile (t : t) : Tile.t option =
  match t with
  | `Int(n) -> Some(if n = 0 then Tile.hole else Tile.create n)
  | _ -> None
;;

let from_board (board : Board.t) : t =
  let tile_ts_2d =
    let open List.Let_syntax in
    let%bind row = List.init (Board.get_height board) ~f:Fun.id in
    let row_t =
      let%bind col = List.init (Board.get_width board) ~f:Fun.id in
      [from_tile @@ Board.get_tile_at board { Position.row; col }]
    in [row_t]
  in
  `List(List.map ~f:(fun tile_ts -> `List(tile_ts)) tile_ts_2d)

let to_board (t : t) : Board.t option =
  let to_row t : Tile.t list option =
    match t with
    | `List(tile_ts) -> List.map ~f:to_tile tile_ts |> Option.all
    | _ -> None
  in
  match t with
  | `List(rows) -> 
    List.map ~f:to_row rows |> Option.all 
    |> Option.map ~f:Board.from_tiles
  | _ -> None

;;

let from_penguin (penguin : Penguin.t) : t =
  from_pos @@ Penguin.get_position penguin

let to_penguin (t : t) : Penguin.t option =
  Option.map ~f:Penguin.create @@ to_pos t
;;

let from_color (color : Player_color.t) : t =
  match color with
  | Red   -> `String("red")
  | White -> `String("white")
  | Black -> `String("black")
  | Brown -> `String("brown")

let to_color t : Player_color.t option =
  match t with
  | `String("red")   -> Some(Red)
  | `String("white") -> Some(White)
  | `String("black") -> Some(Black)
  | `String("brown") -> Some(Brown)
  | _ -> None
;;

let from_player (player : Player_state.t) : t =
  let color_t = from_color @@ Player_state.get_player_color player
  and score_t = `Int(Player_state.get_score player)
  and pengs_t = `List(List.map ~f:from_penguin @@ Player_state.get_penguins player)
  in
  `Assoc([("color", color_t); ("score", score_t); ("places", pengs_t);])

let to_player (t : t) : Player_state.t option =
  let open Option.Let_syntax in
  match t with
  | `Assoc(kv_pairs) ->
    let%bind 
      score_t = assoc_opt kv_pairs "score" and
      color_t = assoc_opt kv_pairs "color" in
    begin
      match%bind assoc_opt kv_pairs "places" with
      | `List(pengs_t) ->
        let%bind 
          pengs = List.map ~f:to_penguin pengs_t |> Option.all and 
        score = YB.Util.to_int_option score_t and
        color = to_color color_t in
        let player = Player_state.create color in
        let player = Player_state.set_score player score in
        let player = 
          List.fold_right ~init:player 
            ~f:(Fun.flip Player_state.add_penguin) pengs in
        Some(player)
      | _ -> None
    end
  | _ -> None
;;

let to_player_list (t : t) : Player_state.t list option =
  match t with
  | `List(player_ts) -> player_ts |> List.map ~f:to_player |> Option.all
  | _ -> None
;;

(* -------------------------------------------------------------- *)
(* ---------------------- exported functions -------------------- *)
(* -------------------------------------------------------------- *)
let from_board_posn (board, pos) =
  let bt = from_board board in
  let pt = from_pos pos in
  `Assoc([("board", bt); ("position", pt)])
;;

let to_board_posn (t : t) =
  let open Option.Let_syntax in
  match t with
  | `Assoc(kv_pairs) ->
    let%bind 
      board_t = assoc_opt kv_pairs "board" and
      pos_t = assoc_opt kv_pairs "position" in
    let%bind 
      board = to_board board_t and 
      pos = to_pos pos_t in
    Some(board, pos)
  | _ -> None
;;

let from_game_state gs = 
  let bt = from_board @@ GS.get_board_copy gs
  and plt = `List(List.map ~f:from_player @@ GS.get_ordered_players gs)
  in
  `Assoc([("board", bt); ("players", plt)])
;;

let to_game_state (t : t) =
  let open Option.Let_syntax in
  match t with
  | `Assoc(kv_pairs) -> 
    let%bind 
      board_t = assoc_opt kv_pairs "board" and
      players_t = assoc_opt kv_pairs "players" in
    let%bind 
      board = to_board board_t and
      players = to_player_list players_t in
    Some(GS.from_board_players board players)
  | _ -> None
;;

let to_string (t : t) = YB.to_string t
;;

let from_string str = 
  try 
    Some(YB.from_string str)
  with YB.Finally(_) -> None
;;

let stream_from_channel in_chan =
  YB.stream_from_channel in_chan
;;
