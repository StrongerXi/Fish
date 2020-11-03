module Game_result : sig
  type t =
    { winners : Player.t list
    ; cheaters : Player.t list
    ; failed : Player.t list
    ; rest : Player.t list (* ranked by scores from lower to higher *)
    }
end

(** A [t] represents a referee which manages an entire fish game from start to
    end. A [t] manages exactly 1 game and becomes obselete after the game ends.
    It can:
    - Set up and run a game given an ordered list of [Player.t]s
    - Report final result of a game after it's finished *)
type t

(* Some constants *)
val min_num_of_players : int
val max_num_of_players : int

(** Create a referee with given list of players.
    Error if the # of players is invalid. *)
val create : Player.t list -> t

(** Set up and run a game with the list of players given in [create].
    The turn order is determined by the player order in the original list. 
    The initial board is constructed based on given config.
    Each player gets (6 - N) penguins to place, where N is the # of players.
    Error if we can't create a valid board or *)
val run_game : t -> Common.Board.Config.t -> Game_result.t
