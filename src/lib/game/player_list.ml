type t =
  { players : Player_state.t list
  }

let create colors = { players = List.map Player_state.create colors }

let move_penguin t src dst fish =
  let rec update_players players =
    match players with
    | [] -> failwith "No penguin resides at source position"
    | p::players ->
      match Player_state.move_penguin p src dst with
      | None    -> p::(update_players players)
      | Some(p) ->
        let new_score = fish + Player_state.get_score p in
        (Player_state.set_score p new_score)::players
  in
  { players = update_players t.players }

let place_penguin t color pos =
  let penguin = Penguin.create pos in
  let rec update_players players =
    match players with
    | [] -> failwith "No player has given color"
    | p::players ->
      if Player_state.get_player_color p = color
      then (Player_state.add_penguin p penguin)::players
      else p::(update_players players)
  in
  { players = update_players t.players }

let get_ordered_players t = t.players
