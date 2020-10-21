module P = Fish.Util.Position

let tests = OUnit2.(>:::) "position_tests" [

    OUnit2.(>::) "test_compare" (fun _ ->
        let p11 = { P.row = 1; col = 1 } in
        let p03 = { P.row = 0; col = 3 } in
        let p20 = { P.row = 2; col = 0 } in
        let p33 = { P.row = 3; col = 3 } in
        OUnit2.assert_equal true @@ ((P.compare p03 p11) < 0);
        OUnit2.assert_equal true @@ ((P.compare p20 p11) > 0);
        OUnit2.assert_equal true @@ ((P.compare p20 p33) < 0);
        OUnit2.assert_equal true @@ ((P.compare p33 p33) = 0);
      );

    OUnit2.(>::) "test_create_positions" (fun _ ->
        let positions = P.create_positions_within ~height:2 ~width:3
                        |> List.sort P.compare
        in
        let expect = [ { P.row = 0; col = 0 };
                       { P.row = 0; col = 1 };
                       { P.row = 0; col = 2 };
                       { P.row = 1; col = 0 };
                       { P.row = 1; col = 1 };
                       { P.row = 1; col = 2 };
                     ]
                     |> List.sort P.compare
        in
        OUnit2.assert_equal expect positions
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
