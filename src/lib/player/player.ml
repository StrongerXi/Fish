open !Core
module Strategy = Strategy
open Strategy

module PS = Player_state
module GS = Game_state

class virtual t (name : string) (age : int) = object
  method virtual place_penguin : Game_state.t -> Position.t option
  method virtual take_turn : Game_tree.t -> Action.t option
  method get_name () = name
  method get_age () = age
  method inform_tournament_start () = true
  method assign_color (_ : PS.Player_color.t) = true
  method inform_disqualified () = true
  method inform_tournament_result (_ : bool) = true
  method dispose () = ()
end

class ai_player 
    (name : string) 
    (age : int)
    (placer : Penguin_placer.t) 
    (actor : Turn_actor.t) = object
  inherit t name age
  val placer = placer
  val actor = actor
  method place_penguin gs =
    Option.some @@ Penguin_placer.use placer gs
  method take_turn gt =
    Option.some @@ Turn_actor.use actor gt
end

let create_AI_player ?(name = "AI") ?(age = 0) placer actor =
  new ai_player name age placer actor
;;
