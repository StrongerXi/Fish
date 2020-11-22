module Conf = Fish.Common.Board.Config
module Pos = Fish.Util.Position
module Referee = Fish.Admin.Referee
module Player = Fish.Player

let get_default_ai_player _ =
  Player.create_AI_player
    Player.Strategy.Penguin_placer.create_scanning_strategy
    (Player.Strategy.Turn_actor.create_minimax_strategy 2)

let tests = OUnit2.(>:::) "referee_test" [
    OUnit2.(>::) "test_run_game_inputs_validation" (fun _ ->
        (* Use different board config and # of players to make sure [run_game]
         * properly errors on invalid inputs *)
        let holes = [ { Pos.row = 0; col = 1 };
                      { Pos.row = 0; col = 2 };
                      { Pos.row = 1; col = 0 }; ] in
        let conf = Conf.create ~width:3 ~height:3
                    |> Conf.set_default_num_of_fish 3
                    |> Conf.set_holes holes in
        let conf_no_hole = Conf.set_holes [] conf in
        let referee = Referee.create () in

        (* too few players *)
        OUnit.assert_raises (Failure "Invalid number of players: 0")
          (fun () -> Referee.run_game referee [] conf);
        OUnit.assert_raises (Failure "Invalid number of players: 1")
          (fun () -> Referee.run_game referee [get_default_ai_player ()] conf);

        (* these should pass without error *)
        Core.ignore @@ Referee.run_game referee
          (List.init 2 get_default_ai_player) conf_no_hole;
        Core.ignore @@ Referee.run_game referee
          (List.init 3 get_default_ai_player) conf_no_hole;
        Core.ignore @@ Referee.run_game referee
          (List.init 4 get_default_ai_player) conf_no_hole;

        (* too many players *)
        OUnit.assert_raises (Failure "Invalid number of players: 5")
          (fun () -> Referee.run_game referee
              (List.init 5 get_default_ai_player) conf_no_hole);
        OUnit.assert_raises (Failure "Invalid number of players: 6")
          (fun () -> Referee.run_game referee
              (List.init 6 get_default_ai_player) conf_no_hole);

        (* not enough open tiles for penguin placement *)
        let msg =
          "Board doesn't have enough non-hole tiles for penguin placement" in
        OUnit.assert_raises (Failure msg)
          (fun () -> Referee.run_game referee
              (List.init 4 get_default_ai_player) conf);
      );
  ]
let _ =
  OUnit2.run_test_tt_main tests
