(** A [t] represents an external player in a fish game. It's "external" because
    it holds little information about a player's state in a fish game.
    It's responsible for actions from a player, either taking turns or
    responding to certain game events *)
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

(** Assuming it's this player's turn, return the action it chooses to perform
    in the state within given game tree. It can also use the tree for planning
    purposes, and implicitly take advantage of subtree caching. *)
val take_turn : t -> Game_tree.t -> Action.t

(** Assuming the game is in the initial penguin placement phase, return the
    position this player would like to place its next penguin *)
val place_penguin : t -> Game_state.t -> Position.t

(** Inform this player that it has been disqualified from a fish game *)
val inform_disqualified : t -> unit
