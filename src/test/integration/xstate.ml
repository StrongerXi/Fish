open !Core
module S = Fish.Serialize.Serialization
module B = Fish.Game.Board
module GS = Fish.Game.Game_state
module PS = Fish.Game.Player_state
module PN = Fish.Game.Penguin
module Pos = Fish.Util.Position

let preference =
  let open B.Direction in
  [ North; Northeast; Southeast; South; Southwest; Northwest; ]

(** Find the closest move in [dir_pos], prioritizing earlier directions in
    [preference] *)
let find_move (dir_pos : (B.Direction.t * (Pos.t list)) list) : Pos.t option =
  let rec go (prefs : B.Direction.t list) : Pos.t option =
    match prefs with
    | [] -> None
    | p::prefs ->
      match List.Assoc.find ~equal:phys_equal dir_pos p with
      | Some(pos::_) -> Some(pos)
      | _ -> go prefs
  in 
  go preference

(** find a move based on [find_move] from the 1st penguin of the 1st player,
    then return game state resulted from applying that move to the original 
    game state.  If such a move can't be found, return [None] *)
let find_move_and_apply (gs : GS.t) : GS.t option =
  let open Option.Let_syntax in
  let players = GS.get_ordered_players gs in
  let%bind player = List.hd players in
  let%bind penguin = List.hd @@ PS.get_penguins player in
  let src = PN.get_position penguin in
  let board = GS.get_board_minus_penguins gs in
  let%bind dst = find_move @@ B.get_reachable_from board src in
  return @@ GS.move_penguin gs src dst

(** deserialize a game state, and output the deserialized result of applying 
    it to [find_move_and_apply]. If the result is [None], then print "false". *)
let () =
  let input = Core.In_channel.input_all Core.In_channel.stdin in
  match S.from_string input |> Option.bind ~f:S.to_game_state with
  | Some(state) ->
    begin
      match find_move_and_apply state with
      | None -> print_string "false\n"
      | Some(state) -> 
        S.from_game_state state |> S.to_string |> Printf.printf "%s\n";
    end
  | None -> print_string "Invalid input\n"
