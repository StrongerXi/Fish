(* Some utility functions commonly used in the admin package *)

(** Invoke the given thunk and return its result in optional. Terminate
    evaluation and return [None] if the it takes more than given # of seconds to
    evaluate *)
val call_with_timeout_ms : (unit -> 'a) -> int -> 'a option
