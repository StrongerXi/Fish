(* This file serves both as an interface for the player, and package boundary
 * for the player package. *)
module Strategy = Strategy

(** A [t] represents an external player in a fish game. It's "external" because
    it holds little information about a player's state in a fish game.
    It's responsible for actions from a player, either taking turns or
    responding to certain game events 
    NOTE it's immutable *)
type t

(** Create an AI player with simplistic strategic planning.
    NOTE that it always respond on behalf of the current player in a game.
    - It selects the first available position to place a penguin, starting
      from (0, 0), and scanning each row from left to right
    - For turn taking, it always makes the move after looking ahead by given #
      of turns with a minimax algorithm. It breaks tie by selecting the
      lexicalgraphically smallest move, as if each move is (sr, sc, dr, dc)
    *)
val create_simple_player : int -> t

(** Assign given color to the given player.
    The player _may_ choose to ignore this information and play on behalf of
    the current player in the game state/tree being passed to it *)
val assign_color : t -> Player_state.Player_color.t -> t

(** Assuming it's this player's turn, return the action it chooses to perform
    in the state within given game tree. It can also use the tree for planning
    purposes, and implicitly take advantage of subtree caching. *)
val take_turn : t -> Game_tree.t -> Action.t

(** Assuming the game is in the initial penguin placement phase, return the
    position this player would like to place its next penguin *)
val place_penguin : t -> Game_state.t -> Position.t

(** Inform this player that it has been disqualified from a fish game 
    The player _may_ choose to ignore this event. *)
val inform_disqualified : t -> t
