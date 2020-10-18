

(* First argument must be a port number *)
let parse_args (args : string array) : int option =
  if Array.length args <> 2
  then None
  else Array.get args 1 |> int_of_string_opt

let _ = 
  match (parse_args Sys.argv) with
  | None -> Printf.printf "Please input an integer port number\n"
  | Some(port) -> 
    let my_addr = Unix.inet_addr_loopback in
    let sockaddr = (Unix.ADDR_INET(my_addr, port)) in
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 (* default protocol *) in
    Unix.bind sock sockaddr ;
    Unix.listen sock 1;
    let (client_sock, _) = Unix.accept sock in 
    let inchan = Unix.in_channel_of_descr client_sock in
    let outchan = Unix.out_channel_of_descr client_sock in
    (* do the work
    let jsonVal = JsonLib.Utils.json_from_channel inchan in
    (* TODO it would be better to use Yjson and parse a stream of json values *)
    let jsonStr = JsonLib.Utils.json_to_str jsonVal in *)
    let str = input_line inchan in
    output_string outchan str;
    (* clean up *)
    flush outchan;
    Unix.close client_sock;
    Unix.close sock
