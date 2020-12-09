open !Core

module Remote_player = Fish.Remote.Remote_player
module Player = Fish.Player

type config =
  { num_clients  : int
  ; search_depth : int
  ; port         : int
  ; server_ip    : string
  }

let parse_config (args : string array) : config option = 
  let size = Array.length args in
  if size <> 3 && size <> 4
  then None
  else
    let open Option.Let_syntax in
    let%bind num_clients = int_of_string_opt args.(1) in
    let%bind port = int_of_string_opt args.(2) in
    let server_ip = if size = 3 then "127.0.0.1" else args.(3) in
    Option.return { num_clients; port; server_ip; search_depth = 2 }
;;

(* Start up a server for fish game at specified port *)
let () =
  match parse_config @@ Sys.get_argv () with
  | None ->
    Printf.printf "Input: <number-of-clients> <port-number> <ip-address>";
  | Some(conf) ->
    let my_ip = Unix.Inet_addr.localhost |> Unix.Inet_addr.to_string in
    let threads = 
      List.init conf.num_clients ~f:(fun i ->
          let player = Player.create_AI_player
              ~name:(Printf.sprintf "AI-%s-%d" my_ip i) ~age:i
              Player.Strategy.Penguin_placer.create_scanning_strategy
              (Player.Strategy.Turn_actor.create_minimax_strategy conf.search_depth)
          in
          Thread.create (fun () ->
              Remote_player.interact_with_proxy player conf.server_ip conf.port)
            ~on_uncaught_exn:`Print_to_stderr ())
    in
    List.iter ~f:Thread.join threads;
