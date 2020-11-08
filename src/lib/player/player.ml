open !Core
module Strategy = Strategy
open Strategy

module PS = Player_state
module GS = Game_state

type t =
  | AI of Penguin_placer.t * Turn_actor.t 

let create_AI_player placer actor = 
  AI(placer, actor)
;;

let assign_color t _ = 
  match t with
  | AI(_) -> ()
;;

let take_turn t gt =
  match t with
  | AI(_, turn_actor) -> Option.some @@ Turn_actor.use turn_actor gt
;;

let place_penguin t gs =
  match t with
  | AI(penguin_placer, _) -> Option.some @@ Penguin_placer.use penguin_placer gs
;;

let inform_disqualified t =
  match t with
  | AI(_) -> ()
;;
