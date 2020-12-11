open !Core
module T = Fish.Util.Timeout

let block_for_a_long_time () = Unix.sleep 1000000
;;

let tests = OUnit2.(>:::) "timeout_tests" [

    OUnit2.(>::) "test_call_with_timeout_ms" (fun _ ->
        let start = Unix.gettimeofday() in
        let result = T.call_with_timeout_ms block_for_a_long_time 1000 in
        let time_spent = (Unix.gettimeofday()) -. start in
        OUnit2.assert_bool
          "Time spent is not acceptable for 1000ms timeout call"
          (Float.between time_spent ~low:0.95 ~high:1.05);
        OUnit2.assert_equal None result;

        let start = Unix.gettimeofday() in
        let result = T.call_with_timeout_ms (fun () -> 42) 1000 in
        let time_spent = (Unix.gettimeofday()) -. start in
        OUnit2.assert_bool
          "Time spent is not acceptable for a timeout call with immediate return"
          (Float.between time_spent ~low:0.0 ~high:0.05);
        OUnit2.assert_equal (Some 42) result;
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
