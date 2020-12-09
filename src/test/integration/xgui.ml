open !Core
module Player = Fish.Player
module Config = Fish.Common.Board.Config
module Pos = Fish.Util.Position
module Render = Fish.Gui.Render
module Referee = Fish.Admin.Referee

let board_config = Config.create ~height:5 ~width:5
                   |> Config.set_default_num_of_fish 3
                   |> Config.set_min_num_of_one_fish_tile 7
                   |> Config.set_holes 
                     [{ Pos.row = 1; col = 1 };
                      { Pos.row = 0; col = 3 };
                      { Pos.row = 3; col = 2 };
                      { Pos.row = 4; col = 0 };
                      { Pos.row = 2; col = 4 }; ]
;;

let parse_int (args : string array) : int option = 
  if (Array.length args) <> 2
  then None
  else int_of_string_opt args.(1)
;;

let make_players (count : int) : Player.t list =
  List.init count ~f:(fun i ->
      Player.create_AI_player ~name:(string_of_int i)
        Player.Strategy.Penguin_placer.create_scanning_strategy
        (Player.Strategy.Turn_actor.create_minimax_strategy i))
;;

let () =
  match parse_int (Sys.get_argv()) with
  | None -> Printf.printf "Input: <# o ai players>";
  | Some(player_count) ->
    let players = make_players player_count in
    let referee = Referee.create () in
    let visualizer = Fish.Gui.Visualizers.get_observer_view ~debug:true () in
    Referee.add_game_observer referee visualizer;
    Core.ignore @@ Referee.run_game referee players board_config;
;;
