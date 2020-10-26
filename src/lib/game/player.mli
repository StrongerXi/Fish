(** A [t] represents an external player in a fish game. It's "external" because
    it holds little information about a player's state in a fish game.
    It's responsible for actions from a player, either taking turns or
    responding to certain game events *)
type t
module Player_color = Player_state.Player_color

(** Return an AI player with simplistic strategic planning.
    - It always selects the first available position to place a penguin,
    starting from (0, 0), and scanning each row from left to right
    - For turn taking, it always makes the move after looking ahead by given #
    of turns with a minimax algorithm. It breaks tie by selecting the
    lexicalgraphically smallest move, i.e., as if each move is (sr, sc, dr, dc)
    *)
val get_simple_player : Player_color.t -> int -> t

(** Assuming it's this player's turn, return the action it chooses to perform *)
val take_turn : t -> Game_state.t -> Action.t

(** Assuming the game is in the initial penguin placement phase, return the
    position this player would like to place its next penguin *)
val place_penguin : t -> Game_state.t -> Position.t

(** Inform this player that it has been disqualified from a fish game *)
val inform_disqualified : t -> unit
