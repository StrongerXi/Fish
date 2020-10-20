module Conf = Fish.Game.Board_config

let tests = OUnit2.(>:::) "board_config_tests" [

    OUnit2.(>::) "test_immutable" (fun _ ->
        (* TODO Make sure all update methods are immutable *)
        ()
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
