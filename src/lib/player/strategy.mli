module Penguin_placer : sig
  (** A [t] represents a strategy for selecting a position to place a penguin on
      the board, during the initial penguin placement phase in a Fish game.
      It always acts on behalf of the current player in a game. *)
  type t

  (** Create a penguin placement strategy which always selects the first
      available position to place a penguin, starting from (0, 0), and
      scanning each row from left to right *)
  val create_scanning_strategy : t

  (** Use the given strategy to determine a position to place penguins in the
      given game state *)
  val use : t -> Game_state.t -> Position.t
end


module Turn_actor : sig
  (** A [t] represents a strategy for selecting a turn action for a player
      during the turn taking phase in a Fish game.
      It always acts on behalf of the current player in a game. *)
  type t

  (** Create a strategy to determine optimal turn action via the minimax search
      algorithm which looks ahead on a player's behalf as much as it needs for
      that player to take given # of turns.
      - It evaluates a game state solely based on the score of the player.
      - It breaks tie by selecting the lexicalgraphically smallest move, as if
        each move is (sr, sc, dr, dc) 
      Errors if the look ahead is non-positive. *)
  val create_minimax_strategy : int -> t

  (** Use the given strategy to determine an action in the current game state in
      given tree. The tree can be used for planning purposes. *)
  val use : t -> Game_tree.t -> Action.t
end
