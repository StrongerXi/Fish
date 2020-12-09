open !Core

module Timeout = Util.Timeout

type config =
  { min_num_of_players     : int
  ; max_num_of_players     : int
  ; num_of_waiting_periods : int
  ; waiting_period_ms      : int
  ; name_reply_timeout_ms  : int
  ; max_name_bytes         : int
  ; max_pending_reqs       : int
  }

(** Return a socket that is ready to accpet connections *)
let create_server (conf : config) (port : int) : Unix.File_descr.t =
  let sockaddr = Unix.ADDR_INET(Unix.Inet_addr.localhost, port) in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let sock = Unix.socket ~domain ~kind:SOCK_STREAM ~protocol:0 () in
  Unix.bind sock ~addr:sockaddr;
  Unix.listen sock ~backlog:conf.max_pending_reqs;
  sock
;;

let handle_waiting_period (server : Unix.File_descr.t)
    (conf : config) (players : Player.t list) : Player.t list =
  let buf = Core.Bytes.create conf.max_name_bytes in
  let rec loop
      (players : Player.t list) (time_left_ms : int) : Player.t list =
    if (List.length players) > conf.max_num_of_players
    then players
    else
      let start_ms = Unix.gettimeofday () in
      match Timeout.call_with_timeout_ms
              (fun () -> Unix.accept server) time_left_ms with
      | None -> players (* this waiting period is over *)
      | Some(client_sock, _) ->
        let opt_name = Timeout.call_with_timeout_ms (fun () ->
            Core.ignore @@ Unix.read ~pos:0 ~len:conf.max_name_bytes ~buf client_sock;
            Core.Bytes.to_string buf) conf.name_reply_timeout_ms in
        let time_passed_ms =
          int_of_float Float.(1000. * (Unix.gettimeofday()) - start_ms) in
        let time_left_ms = time_left_ms - time_passed_ms in
        match opt_name with
        | None -> loop players time_left_ms
        | Some(name) -> 
          let ic = Unix.in_channel_of_descr client_sock in
          let oc = Unix.out_channel_of_descr client_sock in
          let age = List.length players in
          let p = Remote_player.create_proxy_player ic oc name age in
          loop (p::players) time_left_ms
  in
  loop players conf.waiting_period_ms
;;

let sign_up conf port =
  let rec loop (server : Unix.File_descr.t)
      (num_of_wp_left : int) (players : Player.t list) : Player.t list =
    if num_of_wp_left = 0 then players
    else
      let players = handle_waiting_period server conf players in
      if (List.length players) > conf.min_num_of_players then players
      else loop server (num_of_wp_left - 1) players
  in
  let server = create_server conf port in
  loop server conf.num_of_waiting_periods []
;;
