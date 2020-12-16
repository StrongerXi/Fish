(** Configurations for a sign-up phase *)
type config =
  { min_num_of_players     : int
  ; max_num_of_players     : int
  ; num_of_waiting_periods : int (* max # of waiting period to perform *)
  ; waiting_period_ms      : int (* max duration of individual waiting period *)
  ; name_reply_timeout_ms  : int (* how long to wait before player sends in its
                                    name after initial connection *)
  ; max_name_length        : int
  ; max_pending_reqs       : int (* max # of pending connection requests *)
  }

(** Starts a TCP server at given port on the executing machine. New connections
    will be used to create remote players once they send in their names.
    The sign up phase stops if
    1. the max # of players has been reached at any point
    2. the min # of players has been reached at the end of a waiting
    3. the # of waiting periods has been exhausted
    The players will be assigned distinct age (lower if it signs up earlier). *)
val sign_up : config -> port:int -> Player.t list
