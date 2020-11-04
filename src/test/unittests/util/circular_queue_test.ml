module CQ = Fish.Util.Circular_queue

let tests = OUnit2.(>:::) "circular_queue_tests" [

    OUnit2.(>::) "test_create" (fun _ ->
        let int_cq = CQ.create 42 [] in
        OUnit.assert_equal 42 @@ CQ.get_current int_cq;
        OUnit.assert_equal [42;] @@ CQ.to_list int_cq;

        let int_cq = CQ.create 2 [1; 3;] in
        OUnit.assert_equal 2 @@ CQ.get_current int_cq;
        OUnit.assert_equal [2; 1; 3;] @@ CQ.to_list int_cq;
      );

    OUnit2.(>::) "test_rotate" (fun _ ->
        let int_cq = CQ.create 17 [] |> CQ.rotate in
        OUnit.assert_equal 17 @@ CQ.get_current int_cq;
        OUnit.assert_equal [17;] @@ CQ.to_list int_cq;

        let int_cq = CQ.create 2 [1; 3;] in

        let int_cq = CQ.rotate int_cq in
        OUnit.assert_equal [1; 3; 2;] @@ CQ.to_list int_cq;
        OUnit.assert_equal 1 @@ CQ.get_current int_cq;

        let int_cq = CQ.rotate int_cq in
        OUnit.assert_equal [3; 2; 1;] @@ CQ.to_list int_cq;
        OUnit.assert_equal 3 @@ CQ.get_current int_cq;

        let int_cq = CQ.rotate int_cq in
        OUnit.assert_equal [2; 1; 3;] @@ CQ.to_list int_cq;
        OUnit.assert_equal 2 @@ CQ.get_current int_cq;
      );

    OUnit2.(>::) "test_remove_current" (fun _ ->
        let int_cq = CQ.create 42 [] in
        OUnit.assert_equal None @@ CQ.remove_current int_cq;

        let int_cq213 = CQ.create 2 [1; 3;] in
        let int_cq13 = CQ.remove_current int_cq213 in
        OUnit.assert_equal 
          (Some [1; 3;]) @@ (Option.map CQ.to_list int_cq13);
        OUnit.assert_equal 
          (Some 1) @@ (Option.map CQ.get_current int_cq13);
        OUnit.assert_equal 
          (Some [3; 1;]) 
          (Option.map CQ.to_list (Option.map CQ.rotate int_cq13));
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
