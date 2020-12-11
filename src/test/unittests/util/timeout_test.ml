open !Core
module T = Fish.Util.Timeout

let block_for_a_long_time () = Unix.sleep 1000000
;;

let tests = OUnit2.(>:::) "timeout_tests" [

    OUnit2.(>::) "test_call_with_timeout_ms_single_thread" (fun _ ->
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

    (* TODO doesn't work because signal alarm is at process level, and
     * `Unix.sleep` puts a thread to sleep, so the alarm signal is handled by
     * the main thread, not the thread we created...
    OUnit2.(>::) "test_call_with_timeout_ms_multi_threads" (fun _ ->
        let t1 = Thread.create ~on_uncaught_exn:`Print_to_stderr
            (fun () ->
               let start = Unix.gettimeofday() in
               let result = T.call_with_timeout_ms block_for_a_long_time 1000 in
               let time_spent = (Unix.gettimeofday()) -. start in
               OUnit2.assert_bool
                 "Time spent is not acceptable for 1000ms timeout call"
                 (Float.between time_spent ~low:0.95 ~high:1.05);
               OUnit2.assert_equal None result;)
            ()
        in
        let t2 = Thread.create ~on_uncaught_exn:`Print_to_stderr
            (fun () ->
               let start = Unix.gettimeofday() in
               let result = T.call_with_timeout_ms block_for_a_long_time 3000 in
               let time_spent = (Unix.gettimeofday()) -. start in
               OUnit2.assert_bool
                 "Time spent is not acceptable for a timeout call with immediate return"
                 (Float.between time_spent ~low:0.95 ~high:1.05);
               OUnit2.assert_equal None result;)
            ()
        in
        Thread.join t1;
        Thread.join t2;
      );
       *)
  ]

let _ =
  OUnit2.run_test_tt_main tests
