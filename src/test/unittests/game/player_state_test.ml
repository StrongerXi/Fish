module P = Fish.Game.Player_state
module PC = Fish.Game.Player_color
module PN = Fish.Game.Penguin
module Pos = Fish.Util.Position

let tests = OUnit2.(>:::) "player_state_tests" [

    OUnit2.(>::) "test_construction" (fun _ ->
        let p = P.create PC.Black in
        OUnit2.assert_equal PC.Black (P.get_player_color p);
        OUnit2.assert_equal 0 (P.get_score p);
        OUnit2.assert_equal [] (P.get_penguin_positions p);
      );

    OUnit2.(>::) "test_set_score" (fun _ ->
        (* 1. only penguin positions are updated, correctly
         * 2. no side effect *)
        let p1 = P.create PC.Black in
        let p2 = P.set_score p1 42 in
        OUnit2.assert_equal PC.Black (P.get_player_color p1);
        OUnit2.assert_equal 0 (P.get_score p1);
        OUnit2.assert_equal [] (P.get_penguin_positions p1);
        OUnit2.assert_equal PC.Black (P.get_player_color p2);
        OUnit2.assert_equal 42 (P.get_score p2);
        OUnit2.assert_equal [] (P.get_penguin_positions p2);
      );

    OUnit2.(>::) "test_add_penguin" (fun _ ->
        (* 1. only penguin positions are updated, correctly
         * 2. no side effect *)
        let p1 = P.create PC.Black in
        let pos = { Pos.row = 3; col = 5 } in
        let p2 = P.add_penguin p1 @@ PN.create pos in
        OUnit2.assert_equal PC.Black (P.get_player_color p1);
        OUnit2.assert_equal 0 (P.get_score p1);
        OUnit2.assert_equal [] (P.get_penguin_positions p1);
        OUnit2.assert_equal PC.Black (P.get_player_color p2);
        OUnit2.assert_equal 0 (P.get_score p2);
        OUnit2.assert_equal [pos;] (P.get_penguin_positions p2);
      );

    OUnit2.(>::) "test_move_penguin" (fun _ ->
        (* 1. only penguin positions and score are updated, correctly
         * 2. no side effect
         * 3. errors when no penguin is at 1st position *)
        let p1 = P.create PC.Red in
        let src = { Pos.row = 3; col = 5 } in
        let dst = { Pos.row = 3; col = 3 } in
        let p2 = P.add_penguin p1 @@ PN.create src in
        let p3 = P.move_penguin p2 src dst in
        OUnit2.assert_equal PC.Red (P.get_player_color p2);
        OUnit2.assert_equal 0 (P.get_score p2);
        OUnit2.assert_equal [src;] (P.get_penguin_positions p2);
        OUnit2.assert_equal PC.Red (P.get_player_color p3);
        OUnit2.assert_equal 0 (P.get_score p3);
        OUnit2.assert_equal [dst;] (P.get_penguin_positions p3);
      );

    OUnit2.(>::) "test_penguin_order" (fun _ ->
        (* order of penguins should be preserved by the time they were added. *)
        let p1 = P.create PC.White in
        let pos11 = { Pos.row = 1; col = 1 } in
        let pos23 = { Pos.row = 2; col = 3 } in
        let pos34 = { Pos.row = 3; col = 4 } in
        let pos77 = { Pos.row = 7; col = 7 } in
        let p2 = P.add_penguin p1 @@ PN.create pos11 in
        let p3 = P.add_penguin p2 @@ PN.create pos23 in
        let p4 = P.add_penguin p3 @@ PN.create pos34 in
        OUnit2.assert_equal PC.White (P.get_player_color p4);
        OUnit2.assert_equal 0 (P.get_score p4);
        OUnit2.assert_equal [pos11; pos23; pos34;] (P.get_penguin_positions p4);
        let p5 = P.move_penguin p3 pos11 pos77 in
        OUnit2.assert_equal PC.White (P.get_player_color p5);
        OUnit2.assert_equal 0 (P.get_score p5);
        OUnit2.assert_equal [pos11; pos77; pos34;] (P.get_penguin_positions p5);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
