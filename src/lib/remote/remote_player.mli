open !Core

(** A Proxy Player acts on behalf of another Player; it communicates with the
    latter via a pre-determined message protocol.
    Inputs: the input/output channel over which the proxy player communicates
    with the actual player, the name and age of this player *)
val create_proxy_player :
  In_channel.t -> Out_channel.t -> string -> int -> Player.t

(** Takes in the host ip address and port number, set up a connection to the
    remote proxy player to play an entire tournament from start to end.
    Use the input player for all decision making. *)
val interact_with_proxy : Player.t -> string -> int -> unit
