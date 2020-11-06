module Game_result : sig
  type t =
    { winners : Player.t list
    ; cheaters : Player.t list
    ; failed : Player.t list
    ; rest : Player.t list
    }
end

module Game_observer : sig
  type event =
    (* invoked when the game start, or when the observer first got added *)
    | Register of Game_state.t 
    | PenguinPlacement of Position.t
    | TurnAction of Action.t
    | Disqualify of Player_state.Player_color.t
    | EndOfGame of Game_result.t
  type t = event -> unit
end

(** A [t] represents a referee which manages an entire fish game from start to
    end. A [t] manages exactly 1 game and becomes obselete after the game ends.
    It can:
    - Set up and run a game given an ordered list of [Player.t]s
    - Add [Game_observer.t] before or during a game, and periodically report
      game events to the observers.
    - Report final result of a game after it's finished *)
type t

(* Some constants *)
val min_num_of_players : int
val max_num_of_players : int

(** Create a referee with given list of players.
    Error if 
    - the # of players is outside the range specified in constants above. *)
val create : Player.t list -> t

(** EFFECT: update [t] to keep track of given observer and inform it of ongoing
    game events (if a game is running) *)
val add_game_observer : t -> Game_observer.t -> unit

(** Set up and run a game with the list of players given in [create].
    The turn order is determined by the player order in the original list. 
    The initial board is constructed based on given config.
    Each player gets (6 - N) penguins to place, where N is the # of players.
    Error if 
    - we can't create a valid board based on given config.
    - the board doesn't have enough non-hole tiles for penguin placement. *)
val run_game : t -> Common.Board.Config.t -> Game_result.t
