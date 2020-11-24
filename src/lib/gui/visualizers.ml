open !Core

let get_observer_view ?(debug=true) ?(delay=1.0) () =
  let debug_printf (s : string) : unit = 
    if debug 
    then (print_string s; Core.Out_channel.flush stdout);
  in
  let last_update = ref 0.0 in
  let open Referee.Game_observer in
  (fun event ->
     let delay_remain = delay -. ((Unix.time ()) -. !last_update) in
     if Float.(delay_remain > 0.0)
     then Thread.delay delay_remain;
     (match event with
      | Register(state) ->
        debug_printf @@ Printf.sprintf "registered\n";
        Render.render state
      | PenguinPlacement(state, _, _) -> Render.render state
      | TurnAction(state, color, act) -> 
        debug_printf @@ 
        Printf.sprintf "action from %s: %s\n"
          (Common.Player_state.Player_color.show color)
          (Common.Action.show act);
        Render.render state
      | Disqualify(opt_state, _) -> Option.iter ~f:Render.render opt_state
      | EndOfGame(result) ->
        let winners_str = List.map ~f:(fun p -> p#get_name ()) result.winners
                          |> List.to_string ~f:(Fun.id) in
        debug_printf @@ 
        Printf.sprintf "Game has ended. Winners are: %s" winners_str);
     last_update := Unix.time ())
