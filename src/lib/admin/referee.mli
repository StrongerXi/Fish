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
    (* invoked when the game start, or [t] is registered during a game.
       Each event contains the most up-to-date game state *)
    | Register of Game_state.t 
    | PenguinPlacement of Game_state.t * Player_state.Player_color.t * Position.t
    | TurnAction of Game_state.t * Player_state.Player_color.t * Action.t
    | Disqualify of Game_state.t option * Player_state.Player_color.t
    | EndOfGame of Game_result.t
  type t = event -> unit
end

(** A [t] represents a referee which manages an entire fish game from start to
    end. A [t] manages exactly 1 game and becomes obselete after the game ends.
    It can:
    - Set up and run a game given a board config and ordered list of [Player.t]s
    - Add [Game_observer.t] before or during a game, and periodically report
      game events to the observers.
    - Report final result of a game after it's finished *)
type t

(* Constants *)
module C : sig
  val min_num_of_players : int
  val max_num_of_players : int
end

(* Enable clients to override default time out configurations *)
type timeout_config = 
  { assign_color_timeout_s : int
  ; placement_timeout_s : int
  ; turn_action_timeout_s : int
  ; inform_disqualified_timeout_s : int
  ; inform_observer_timeout_s : int
  }
val default_timeout_config : timeout_config

(** Create a referee, waiting to run a game *)
val create : ?config:timeout_config -> unit -> t

(** EFFECT: update [t] to keep track of given observer and inform it of ongoing
    game events (if a game is running) *)
val add_game_observer : t -> Game_observer.t -> unit

(** Set up and run a game with the given list of players and board config.
    The turn order is determined by the player order in the original list. 
    The initial board is constructed based on given config.
    Each player gets a fixed # of penguins to place, then the game continues
    until either no player can make a move, or everyone is kicked out.
    Error if 
    - the # of players is outside the range specified in constants above.
    - we can't create a valid board based on given config.
    - the board doesn't have enough non-hole tiles for penguin placement. *)
val run_game : t -> Player.t list -> Common.Board.Config.t -> Game_result.t
