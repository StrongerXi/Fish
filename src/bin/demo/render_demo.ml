open Fish.Gui
open Fish.Game
open Fish.Util

let () =
  let board = 
    Board_config.create ~width:3 ~height:5
    |> Board_config.set_default_num_of_fish 3
    |> Board_config.set_min_num_of_one_fish_tile 3
    |> Board_config.set_holes [ { Position.row = 0; col = 0 }; ]
    |> Board.create
  in
  let players = [Player_color.Black; Player_color.White;] in
  let pos11 = { Position.row = 1; col = 1 } in
  let state = Game_state.create board players in
  let state = Game_state.place_penguin state Player_color.Black pos11 in
  Render.render state;
  (* loop forever makes the canvas go blank after 0.1 sec,
   * this magic incantation keeps canvas there, not sure why... *)
  Core_kernel.never_returns (Async.Scheduler.go ())
