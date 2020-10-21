module P = Fish.Game.Player_state
module PC = Fish.Game.Player_color
module PN = Fish.Game.Penguin
module PL = Fish.Game.Player_list
module Pos = Fish.Util.Position

let map_player_with_color (players : P.t list) (color : PC.t) (f : P.t -> P.t)
  : P.t list =
  List.map (fun p ->
      if color = P.get_player_color p
      then f p else p)
    players

let tests = OUnit2.(>:::) "player_list_tests" [

    OUnit2.(>::) "test_construction" (fun _ ->
        let colors = [PC.Black; PC.Brown; PC.Red] in
        let pl = PL.create colors in
        let players = List.map P.create colors in
        OUnit2.assert_equal players (PL.get_ordered_players pl);
      );

    OUnit2.(>::) "test_place_penguin" (fun _ ->
        (* 1. penguin is placed to the right player
         * 2. no side effect
         * 3. errors on bad input *)
        let colors = [PC.White; PC.Brown; PC.Red] in
        let pl = PL.create colors in
        let pos11 = { Pos.row = 1; col = 1 } in
        let pos23 = { Pos.row = 2; col = 3 } in
        let players = List.map P.create colors in

        let pl1 = PL.place_penguin pl PC.Brown pos11 in
        let new_players = map_player_with_color players PC.Brown
            (fun p -> P.add_penguin p @@ PN.create pos11)
        in
        OUnit2.assert_equal new_players (PL.get_ordered_players pl1);

        let new_players = map_player_with_color new_players PC.Red
            (fun p -> P.add_penguin p @@ PN.create pos23)
        in
        let pl2 = PL.place_penguin pl1 PC.Red pos23 in
        OUnit2.assert_equal new_players (PL.get_ordered_players pl2);

        let expect = Failure "No player has given color" in
        OUnit2.assert_raises
          expect (fun () -> PL.place_penguin pl2 PC.Black pos11);
      );

    OUnit2.(>::) "test_move_penguin" (fun _ ->
        (* 1. penguin is moved
         * 2. player score is updated
         * 3. no side effect
         * 4. errors on bad input *)
        let colors = [PC.Black; PC.White; PC.Red] in
        let pos10 = { Pos.row = 1; col = 0 } in
        let pos12 = { Pos.row = 1; col = 2 } in
        let pos42 = { Pos.row = 4; col = 2 } in
        let players = List.map P.create colors in
        let pl = PL.create colors in
        let pl = PL.place_penguin pl PC.White pos10 in
        let pl = PL.place_penguin pl PC.Black pos42 in
        let players = map_player_with_color players PC.White
            (fun p -> P.add_penguin p @@ PN.create pos10)
        in
        let players = map_player_with_color players PC.Black
            (fun p -> P.add_penguin p @@ PN.create pos42)
        in
        (* make 1 move *)
        let new_pl = PL.move_penguin pl pos10 pos12 3 in
        let new_players = map_player_with_color players PC.White
            (fun _ ->
               let peng = PN.create pos12 in
               let p = P.add_penguin (P.create PC.White) peng in
               P.set_score p 3)
        in
        OUnit2.assert_equal new_players (PL.get_ordered_players new_pl);
        (* make another move, score should be accumulated *)
        let new_pl = PL.move_penguin new_pl pos12 pos10 4 in
        let new_players = map_player_with_color players PC.White
            (fun _ ->
               let peng = PN.create pos10 in
               let p = P.add_penguin (P.create PC.White) peng in
               P.set_score p 7)
        in
        OUnit2.assert_equal new_players (PL.get_ordered_players new_pl);
        (* no penguin at source position *)
        let expect = Failure "No penguin resides at source position" in
        OUnit2.assert_raises expect (fun () -> PL.move_penguin new_pl pos12 pos10 4);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
