module GS = Fish.Game.Game_state
module B = Fish.Game.Board
module Conf = Fish.Game.Board_config
module Color = Fish.Game.Player_color
module Pos = Fish.Util.Position
module PL = Fish.Game.Player_list
module PS = Fish.Game.Player_state
module PN = Fish.Game.Penguin
module T = Fish.Game.Tile

let tests = OUnit2.(>:::) "game_state_tests" [

    OUnit2.(>::) "test_place_penguin" (fun _ ->
        let board = Conf.create ~width:3 ~height:3
                    |> Conf.set_default_num_of_fish 4
                    |> B.create
        in
        let colors = [Color.Black; Color.Brown; Color.Red;] in
        let state0 = GS.create board colors in
        let pos11 = { Pos.row = 1; col = 1 } in
        let state1 = GS.place_penguin state0 Color.Brown pos11 in
        let players = state1 |> GS.get_player_list |> PL.get_ordered_players in
        OUnit2.assert_equal (List.length colors) @@ List.length players;
        let p = List.nth players 1 in
        OUnit2.assert_equal [PN.create pos11;] @@ PS.get_penguins p;

        (* penguin placement shouldn't affect the board *)
        OUnit2.assert_equal (GS.get_board state0) (GS.get_board state1);

        (* place another one *)
        let pos02 = { Pos.row = 0; col = 2 } in
        let state1 = GS.place_penguin state1 Color.Brown pos02 in
        let players = state1 |> GS.get_player_list |> PL.get_ordered_players in
        OUnit2.assert_equal (List.length colors) @@ List.length players;
        let p = List.nth players 1 in
        OUnit2.assert_equal [PN.create pos02; PN.create pos11] @@ PS.get_penguins p;

        (* fails when input is bad *)
        let expect = Failure "Position is outside the board" in
        OUnit2.assert_raises expect (fun () -> 
            GS.place_penguin state0 Color.Brown { Pos.row = 3; col = 3 });
        let expect = Failure "No player has given color" in
        OUnit2.assert_raises expect (fun () -> 
            GS.place_penguin state0 Color.White pos11);
      );

    OUnit2.(>::) "test_move_penguin" (fun _ ->
        (* Score and board should be updated.
         * Make sure game state "wires the 2 components up" *)
        let board = Conf.create ~width:5 ~height:5
                    |> Conf.set_default_num_of_fish 3
                    |> B.create
        in
        let colors = [Color.Black; Color.Red;] in
        let pos11 = { Pos.row = 1; col = 1 } in
        let pos13 = { Pos.row = 1; col = 3 } in
        let state0 = GS.create board colors in
        let state0 = GS.place_penguin state0 Color.Red pos11 in
        let state1 = GS.move_penguin state0 pos11 pos13 in

        (* Board should be updated after move *)
        let board = GS.get_board state1 in
        let tile11 = B.get_tile_at board pos11 in
        let tile13 = B.get_tile_at board pos13 in
        OUnit2.assert_equal true @@ T.is_empty tile11;
        OUnit2.assert_equal false @@ T.is_empty tile13;
        OUnit2.assert_equal 3 @@ T.get_fish tile13; (* fish not changed yet *)

        (* Player should be updated after move *)
        let players = state1 |> GS.get_player_list |> PL.get_ordered_players in
        OUnit2.assert_equal (List.length colors) @@ List.length players;
        OUnit2.assert_equal 3 @@ PS.get_score @@ List.nth players 1;
        OUnit2.assert_equal 0 @@ PS.get_score @@ List.nth players 0;
      );

    OUnit2.(>::) "test_get_board_minus_penguins" (fun _ ->
        let board = Conf.create ~width:5 ~height:5
                    |> Conf.set_default_num_of_fish 3
                    |> B.create
        in
        let colors = [Color.Black; Color.Red;] in
        let pos11 = { Pos.row = 1; col = 1 } in
        let pos23 = { Pos.row = 2; col = 3 } in
        let state = GS.create board colors in
        let state = GS.place_penguin state Color.Red pos11 in
        let state = GS.place_penguin state Color.Red pos23 in
        let board = GS.get_board_minus_penguins state in
        Pos.create_positions_within ~width:5 ~height:5
        |> List.iter (fun pos ->
            let tile = B.get_tile_at board pos in
            if pos = pos11 || pos = pos23
            then OUnit2.assert_equal true @@ T.is_empty tile
            else OUnit2.assert_equal false @@ T.is_empty tile)
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
