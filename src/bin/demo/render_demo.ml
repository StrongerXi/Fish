module Game_state = Fish.Common.Game_state
module Game_tree = Fish.Common.Game_tree
module Player = Fish.Common.Player
module Board = Fish.Common.Board
module Config = Fish.Common.Board.Config
module Player_color = Fish.Common.Player_state.Player_color
module Position = Fish.Util.Position
module Render = Fish.Gui.Render

let () =
  let board = 
    Config.create ~width:5 ~height:5
    |> Config.set_default_num_of_fish 3
    |> Config.set_min_num_of_one_fish_tile 3
    |> Config.set_holes [ { Position.row = 0; col = 0 }; 
                          { Position.row = 1; col = 0 }; 
                          { Position.row = 0; col = 1 }; 
                        ]
    |> Board.create in
  let players = [Player_color.Black; Player_color.White;] in
  let pos11 = { Position.row = 1; col = 1 } in
  let pos31 = { Position.row = 3; col = 1 } in
  let state = Game_state.create board players in
  let state = Game_state.place_penguin state Player_color.Black pos11 in
  let state = Game_state.place_penguin state Player_color.White pos31 in
  Render.render state;
  (* loop forever makes the canvas go blank after 0.1 sec,
   * this magic incantation keeps canvas there, not sure why... *)
  Core_kernel.never_returns (Async.Scheduler.go ())
