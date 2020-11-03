open !Core
module Strategy = Strategy
open Strategy

module PS = Player_state
module GS = Game_state

type t =
  | AI of Penguin_placer.t * Turn_actor.t 

let create_simple_player turns = 
  let penguin_placer = Penguin_placer.create_scanning_strategy in
  let turn_action = Turn_actor.create_minimax_strategy turns in
  AI(penguin_placer, turn_action)
;;

let assign_color t _ = 
  match t with
  | AI(_) -> t
;;

let take_turn t gt =
  match t with
  | AI(_, turn_actor) -> Turn_actor.use turn_actor gt
;;

let place_penguin t gs =
  match t with
  | AI(penguin_placer, _) -> Penguin_placer.use penguin_placer gs
;;

let inform_disqualified t =
  match t with
  | AI(_) -> t
;;
