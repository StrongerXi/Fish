module Pos = Fish.Util.Position
module B = Fish.Game.Board
module Conf = Fish.Game.Board_config
module T = Fish.Game.Tile

let tests = OUnit2.(>:::) "board_tests" [

    OUnit2.(>::) "test_construction" (fun _ ->
        let width, height = 3, 4 in
        let holes = [{ Pos.row = 2; col = 2 }; { Pos.row = 1; col = 0 }] in
        let min_one_fish_tile = 3 in
        let dflt_fish = 5 in
        let conf =
          Conf.create ~height ~width
          |> Conf.set_holes holes
          |> Conf.set_min_num_of_one_fish_tile min_one_fish_tile
          |> Conf.set_default_num_of_fish dflt_fish
        in
        let board = B.create conf in
        OUnit2.assert_equal width (B.get_width board);
        OUnit2.assert_equal height (B.get_height board);
        let fishes : int list =
          Pos.create_positions_within ~width ~height
          |> List.map (fun pos ->
              let tile = B.get_tile_at board pos in
              if List.mem pos holes
              then
                let _ = OUnit2.assert_equal true (T.is_empty tile) in
                0
              else
                let _ = OUnit2.assert_equal false (T.is_empty tile) in
                let fish = T.get_fish tile in
                let _ = OUnit2.assert_equal true
                  (fish = dflt_fish || fish = min_one_fish_tile) in
                fish)
        in
        let one_fish_tiles =
          List.filter (fun fish -> fish = 1) fishes |> List.length
        in
        OUnit2.assert_equal true (one_fish_tiles >= min_one_fish_tile);
      );

    OUnit2.(>::) "test_remove_tile_at" (fun _ ->
        (* 1. removes correct tile
         * 2. errors if pos is out of bound *)
        let width, height = 3, 4 in
        let holes = [] in
        let min_one_fish_tile = 0 in
        let dflt_fish = 3 in
        let conf =
          Conf.create ~height ~width
          |> Conf.set_holes holes
          |> Conf.set_min_num_of_one_fish_tile min_one_fish_tile
          |> Conf.set_default_num_of_fish dflt_fish
        in
        let board = B.create conf in
        let pos11 = { Pos.row = 1; col = 1 } in

        let board = B.remove_tile_at board pos11 in
        OUnit2.assert_equal width (B.get_width board);
        OUnit2.assert_equal height (B.get_height board);
        Pos.create_positions_within ~width ~height
        |> List.iter (fun pos ->
            let tile = B.get_tile_at board pos in
            if pos = pos11
            then OUnit2.assert_equal true @@ T.is_empty tile
            else OUnit2.assert_equal false @@ T.is_empty tile
          );

       let pos33 = { Pos.row = 3; col = 3 } in
       let expect = Failure "position is out of bound: (3, 3)" in
       OUnit2.assert_raises expect (fun _ -> B.get_tile_at board pos33);
       OUnit2.assert_raises expect (fun _ -> B.remove_tile_at board pos33);
      );

    OUnit2.(>::) "test_get_copy" (fun _ ->
        (* update copy has no effect on original *)
        let width, height = 3, 4 in
        let holes = [] in
        let min_one_fish_tile = 0 in
        let dflt_fish = 3 in
        let conf =
          Conf.create ~height ~width
          |> Conf.set_holes holes
          |> Conf.set_min_num_of_one_fish_tile min_one_fish_tile
          |> Conf.set_default_num_of_fish dflt_fish
        in
        let board = B.create conf in
        let pos11 = { Pos.row = 1; col = 1 } in
        let copy = B.get_copy board in
        let _ = B.remove_tile_at board pos11 in
        Pos.create_positions_within ~width ~height
        |> List.iter (fun pos ->
            let tile = B.get_tile_at copy pos in
            OUnit2.assert_equal 3 @@ T.get_fish tile
          );
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
