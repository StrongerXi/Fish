module GT = Fish.Common.Game_tree
module GS = Fish.Common.Game_state
module B = Fish.Common.Board
module Conf = Fish.Common.Board.Config
module Color = Fish.Common.Player_state.Player_color
module Pos = Fish.Util.Position
module Action = Fish.Common.Action

let tests = OUnit2.(>:::) "game_tree_test" [
    OUnit2.(>::) "test_construction" (fun _ ->
        let conf = Conf.create ~width:3 ~height:3
                    |> Conf.set_default_num_of_fish 3
                    |> Conf.set_holes [] in
        let board = B.create conf in
        let colors = [Color.Black; Color.Brown; Color.Red;] in
        let state = GS.create board colors in
        let tree = GT.create state in
        OUnit.assert_equal state @@ GT.get_state tree
      );

    OUnit2.(>::) "test_get_subtrees_end" (fun _ ->

        (* <red>   <0,1>   <--->
         *     <--->   <1,1>   <bro> *)
        let red_pos = { Pos.row = 0; col = 0 } in
        let brown_pos = { Pos.row = 1; col = 2 } in
        let hole1 = { Pos.row = 1; col = 0 } in
        let hole2 = { Pos.row = 0; col = 2 } in
        let conf = Conf.create ~width:3 ~height:2
                    |> Conf.set_default_num_of_fish 3
                    |> Conf.set_holes [hole1; hole2] in
        let board = B.create conf in
        let colors = [Color.Red; Color.Brown; Color.Black;] in
        let state = GS.create board colors in
        let state = GS.place_penguin state Color.Red red_pos in
        let state = GS.place_penguin state Color.Brown brown_pos in
        let tree = GT.create state in
        OUnit.assert_equal [] @@ GT.get_subtrees tree;
      );

    OUnit2.(>::) "test_get_subtrees_skip" (fun _ ->

        (* <red>   <0,1>   <0,2>
         *     <--->   <bro>   <1,2> *)
        let red_pos = { Pos.row = 0; col = 0 } in
        let brown_pos = { Pos.row = 1; col = 1 } in
        let hole_pos = { Pos.row = 1; col = 0 } in
        let conf = Conf.create ~width:3 ~height:2
                    |> Conf.set_default_num_of_fish 3
                    |> Conf.set_holes [hole_pos] in
        let board = B.create conf in
        let colors = [Color.Red; Color.Brown; Color.Black;] in
        let state = GS.create board colors in
        let state = GS.place_penguin state Color.Red red_pos in
        let state = GS.place_penguin state Color.Brown brown_pos in
        let tree = GT.create state in
        OUnit.assert_equal 
          [(Action.Skip, GS.rotate_to_next_player state);] 
          (List.map (fun (a, t) -> (a, GT.get_state t)) @@ GT.get_subtrees tree)
      );

    OUnit2.(>::) "test_get_subtrees" (fun _ ->

        (* <red>   <0,1>   <0,2>
         *     <--->   <bro>   <1,2> *)
        let red_pos = { Pos.row = 0; col = 0 } in
        let brown_pos = { Pos.row = 1; col = 1 } in
        let hole_pos = { Pos.row = 1; col = 0 } in
        let pos01 = { Pos.row = 0; col = 1 } in
        let pos02 = { Pos.row = 0; col = 2 } in
        let conf = Conf.create ~width:3 ~height:2
                    |> Conf.set_default_num_of_fish 3
                    |> Conf.set_holes [hole_pos] in
        let board = B.create conf in
        let colors = [Color.Brown; Color.Red; Color.Black;] in
        let state = GS.create board colors in
        let state = GS.place_penguin state Color.Red red_pos in
        let state = GS.place_penguin state Color.Brown brown_pos in
        let tree = GT.create state in
        let act_state_pairs = 
          List.map (fun (a, t) -> (a, GT.get_state t)) @@ GT.get_subtrees tree
        in
        OUnit.assert_equal 2 @@ List.length act_state_pairs;
        OUnit.assert_equal 
          (Some(GS.rotate_to_next_player @@ GS.move_penguin state brown_pos pos01))
          (List.assoc_opt 
             (Action.Move({ Action.Move.src = brown_pos; dst = pos01 }))
             act_state_pairs);
        OUnit.assert_equal 
          (Some(GS.rotate_to_next_player @@ GS.move_penguin state brown_pos pos02))
          (List.assoc_opt 
             (Action.Move({ Action.Move.src = brown_pos; dst = pos02 }))
             act_state_pairs);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
