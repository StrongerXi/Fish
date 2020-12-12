module Tournament_result : sig
  type t = (* each list of players is in no particular order *)
    { final_winners      : Player.t list 
    ; all_losers         : Player.t list
    ; all_cheaters       : Player.t list
    ; all_failed_players : Player.t list
    }
end

(* Enable clients to override default time out configurations *)
type timeout_config = 
  { inform_tournament_start_timeout_ms  : int
  ; inform_tournament_result_timeout_ms : int
  }
val default_timeout_config : timeout_config

(** The top finisher(s) of every game of round n move on to round n+1. 
    Termination Conditions (disjunctions) :
    1. two tournament rounds of games in a row produce the exact same winners
    2. when there are too few players for a single game, 
    3. when the number of participants has become small enough to run a single 
       final game (and yes this game is run). 
    The order of players in each game is based on their age (youngest first).
    NOTE all players will be disposed when this function call returns. *)
val run_tournament : ?timeout_conf:timeout_config ->
  ?referee_timeout_conf:Referee.timeout_config ->
  Player.t list -> Common.Board.Config.t -> Tournament_result.t
