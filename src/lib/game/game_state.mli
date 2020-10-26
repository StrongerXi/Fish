(** A [t] represents a snapshot of the game state at a certain time.
    It includes:
      - the board state
      - state of all participating players (with unique colors)
      - the order in which players take turn
    It excludes:
      - whose turn it is
      - how to communicate with the actual players
    It ensures:
      - all players have distinct colors
      - no penguin is on a hole
      - each tile has at most 1 penguin
    NOTE that it's immutable *)
type t

(** Create a game state with given board and participating players 
    Errors if there are duplicates in the colors *)
val create : Board.t -> Player_color.t list -> t

val get_board_copy : t -> Board.t

(** Return a list of players whose ordering conforms with the list of colors
    from game state creation. *)
val get_ordered_players : t -> Player_state.t list

(** Return the player that has the given color in [t]
    Errors if no player has the specified color *)
val get_player_with_color : t -> Player_color.t -> Player_state.t

(** Return a board after removing all tiles that have a penguin on it *)
val get_board_minus_penguins : t -> Board.t

(** Place a new penguin with given color at given position on the board.
    Errors if 
    - no the participating player has given color
    - position is out of bound or is a hole *)
val place_penguin : t -> Player_color.t -> Position.t -> t

(** Move the penguin at 1st position to the 2nd position, and update player
    score accordingly.
    Errors if 
    - any position is out of bound 
    - no penguin exists at source position
    - a penguin already exists at the target position *)
val move_penguin : t -> Position.t -> Position.t -> t

(** Discouraged unless you have good reason and know what you are doing *)
val from_board_players : Board.t -> Player_state.t list -> t
