open !Core
module Conf = Fish.Common.Board.Config
module Referee = Fish.Admin.Referee
module Manager = Fish.Admin.Manager
module Pos = Fish.Util.Position
module Player = Fish.Player
module MP = Mock_players

let board_config = Conf.create ~width:5 ~height:5
                   |> Conf.set_default_num_of_fish 3
                   |> Conf.set_holes []
                   |> Conf.set_min_num_of_one_fish_tile 0
;;

(* Mock and AI players are fast enough here anyway *)
let fast_referee_timeout_config = { Referee.default_timeout_config with
                                    turn_action_timeout_ms = 100;
                                    placement_timeout_ms = 100;
                                    assign_color_timeout_ms = 100;
                                    inform_disqualified_timeout_ms = 100; }
;;

let fast_timeout_config = { Manager.
                            inform_tournament_start_timeout_ms = 100;
                            inform_tournament_result_timeout_ms = 100; }
;;

let shuffle (xs : 'a list) : 'a list =
  let padded = List.map ~f:(fun x -> (Random.bits (), x)) xs in
  let sorted =
    List.sort ~compare:(fun (n1, _) (n2, _) -> Int.compare n1 n2) padded in
  List.map ~f:Tuple.T2.get2 sorted
;;

let tests = OUnit2.(>:::) "referee_test" [

    OUnit2.(>::) "test_run_tournament_mix_types_of_players" (fun _ ->
        let failed = [
          MP.get_player_fail_at_placement "failed-0";
          MP.get_player_fail_at_turn_action "failed-1";
          MP.get_player_fail_at_placement "failed-2";
          MP.get_player_fail_at_turn_action "failed-3";
          MP.get_player_hang_at_color_assignment_and_disqualification "failed-4";
          MP.get_player_hang_at_color_assignment_and_disqualification "failed-5";
          MP.get_player_fail_at_turn_action "failed-6";
          MP.get_player_fail_at_turn_action "failed-7";
          MP.get_player_hang_at_color_assignment_and_disqualification "failed-8";
        ] in
        let cheaters = [
          MP.get_player_cheat_at_placement "cheat-1";
          MP.get_player_cheat_at_turn_action "cheat-2";
          MP.get_player_cheat_at_placement "cheat-3";
          MP.get_player_cheat_at_turn_action "cheat-4";
          MP.get_player_cheat_at_placement "cheat-5";
          MP.get_player_cheat_at_turn_action "cheat-6";
        ] in
        let others = List.init 10 ~f:Util.get_default_ai_player in
        (* ages are the same, so order is based on initial order *)
        let players = failed @ cheaters @ others |> shuffle in
        let result =
          Manager.run_tournament
            ~timeout_conf:fast_timeout_config
            ~referee_timeout_conf:fast_referee_timeout_config
            players board_config
        in
        Util.check_same_set_of_players_by_names cheaters result.all_cheaters;
        Util.check_same_set_of_players_by_names failed result.all_failed_players;
        Util.check_same_set_of_players_by_names
          others (result.final_winners @ result.all_losers);
      );
  ]
let _ =
  OUnit2.run_test_tt_main tests
