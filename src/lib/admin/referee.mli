
module Game_result : sig
  type t =
    { winners : Player.t list
    ; cheaters : Player.t list
    ; failed : Player.t list
    ; rest : Player.t list
    }
end

(** A [t] represents a referee which manages an entire fish game from start to
    end. A [t] manages exactly 1 game and becomes obselete after the game ends.
    It can:
    - Set up and run a game given an ordered list of [Player.t]s
    - Report final result of a game after it's finished *)
type t

(** Create a referee with given list of players. *)
val create : Player.t list -> t

(** Set up and run a game with the list of players given in [create].
    The turn order is determined by the player order in the original list. 
    The initial board is constructed based on given config. *)
val run_game : t -> Common.Board.Config.t -> Game_result.t
