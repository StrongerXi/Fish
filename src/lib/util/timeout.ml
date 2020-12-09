open !Core
module Sys = Stdlib.Sys

(* TODO This probably can't be used by multiple threads concurrently *)
exception Timeout
let call_with_timeout_ms (thunk : unit -> 'a) (ms : int) : 'a option =
  Core.ignore @@ (* time out once after [ms] ms *)
  Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.0;
                                    it_value = (float_of_int ms) };
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout));
  try 
    let res = Some(thunk()) in
    Core.ignore @@ (* cancel timeout since the computation is finished *)
    Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.0; it_value = 0.0};
    res
  with Timeout -> None
