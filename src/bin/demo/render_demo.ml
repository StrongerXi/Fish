open Fish.Gui
open Fish.Game

let () =
  let board = 
    Board_config.create ~width:5 ~height:5
    |> Board_config.set_default_num_of_fish 3
    |> Board_config.set_min_num_of_one_fish_tile 3
    |> Board.create
  in
  let players = [Player_color.Black; Player_color.White;] in
  let state = Game_state.create board players in
  Render.render state;
