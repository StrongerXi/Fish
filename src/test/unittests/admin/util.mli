
(** Uses Ounit to check whether given 2 list of players have the same set of
    names, i.e., order is irrelevant. First list is expected. *)
val check_same_set_of_players_by_names :
  Fish.Player.t list -> Fish.Player.t list -> unit
