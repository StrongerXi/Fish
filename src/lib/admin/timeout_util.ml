open !Core

let call_with_timeout_ms (f : unit -> 'a) (_ : int) : 'a option =
  (* TODO can't find a good solution. Tried Async, Lwt and Thread *)
  Option.some @@ f ()
;;
