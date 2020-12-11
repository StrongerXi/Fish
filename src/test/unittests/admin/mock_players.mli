(** Some mock player for testing purposes *)
val get_player_fail_at_placement : string -> Fish.Player.t
val get_player_cheat_at_placement : string -> Fish.Player.t
val get_player_hang_at_placement : string -> Fish.Player.t
val get_player_fail_at_turn_action : string -> Fish.Player.t
val get_player_cheat_at_turn_action : string -> Fish.Player.t
val get_player_hang_at_turn_action : string -> Fish.Player.t
val get_player_hang_at_color_assignment : string -> Fish.Player.t
val get_player_hang_at_inform_tournament_start : string -> Fish.Player.t
val get_player_hang_at_inform_tournament_result : string -> Fish.Player.t
val get_player_hang_at_color_assignment_and_disqualification :
  string -> Fish.Player.t
