
module Game_result = struct
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
type t =
  { tree : Game_tree.t (* Root is the current game state *)
  ; players : Player.t list

let create players =
  { players }

val run_game : t -> Common.Board.Config.t -> Game_result.t
