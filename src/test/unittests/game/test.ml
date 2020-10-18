(* This is a _template_ for unit tests using the OUnit2 framework *)
let oneIsOne _ = OUnit2.assert_equal 1 1

let tests = OUnit2.(>:::) "tests" [
    OUnit2.(>::) "oneIsOne" oneIsOne;
    OUnit2.(>::) "fooIsFoo" @@ fun _ -> OUnit2.assert_equal "foo" "foo";
  ]

let _ =
  OUnit2.run_test_tt_main tests
