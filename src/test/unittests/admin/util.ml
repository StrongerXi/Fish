open !Core

module P = Fish.Player

let check_same_set_of_players_by_names ps1 ps2 =
  let cmp_by_name (p1 : P.t) (p2 : P.t) : int =
    String.compare (p1#get_name()) (p2#get_name())
  in
  let get_name (p : P.t) : string = p#get_name() in
  OUnit.assert_equal
    ~printer:(List.to_string ~f:Fn.id)
    (List.sort ~compare:cmp_by_name ps1 |> List.map ~f:get_name)
    (List.sort ~compare:cmp_by_name ps2 |> List.map ~f:get_name);
;;

let get_default_ai_player ?(depth=1) i =
  P.create_AI_player
    ~name:(string_of_int i)
    P.Strategy.Penguin_placer.create_scanning_strategy
    (P.Strategy.Turn_actor.create_minimax_strategy depth)
;;
