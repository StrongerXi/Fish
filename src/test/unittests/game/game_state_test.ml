module GS = Fish.Game.Game_state

let tests = OUnit2.(>:::) "board_config_tests" [

    OUnit2.(>::) "test_add_penguin" (fun _ ->
        (* TODO shouldn't affect the board *)
        ()
      );

    OUnit2.(>::) "test_move_penguin" (fun _ ->
        (* TODO score and board should be updated.
         * Make sure game state "wires the 2 components up" *)
        ()
      );

    OUnit2.(>::) "test_copy" (fun _ ->
        (* TODO update to copy shouldn't affect original state *)
        ()
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
