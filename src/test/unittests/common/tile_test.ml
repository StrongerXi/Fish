module T = Fish.Common.Tile

let tests = OUnit2.(>:::) "tile_tests" [

    OUnit2.(>::) "test_construction" (fun _ ->
        let t3 = T.create 3 in
        OUnit2.assert_equal 3 (T.get_fish t3);
        OUnit2.assert_equal false (T.is_hole t3);
      );

    OUnit2.(>::) "test_hole" (fun _ ->
        let hole = T.hole in
        OUnit2.assert_equal 0 (T.get_fish hole);
        OUnit2.assert_equal true (T.is_hole hole);
      );

    OUnit2.(>::) "test_negative_fish_count" (fun _ ->
        let expect = Failure "fish count must be positive" in
        OUnit2.assert_raises expect (fun () -> T.create 0);
        OUnit2.assert_raises expect (fun () -> T.create ~-1);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
