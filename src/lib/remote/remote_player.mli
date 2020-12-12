open !Core

(** A Proxy Player acts on behalf of another Player; it communicates with the
    latter via a pre-determined message protocol.
    The input/output channel will be used to communicate with the remote player.
    NOTE The channels will be closed once the player is informed of its
    tournament result or disqualification. *)
val create_proxy_player :
  In_channel.t -> Out_channel.t -> name:string -> age:int -> Player.t

(** Set up a connection to the remote proxy player at <ipaddr, port> to play an
    entire tournament from start to end. Use the input player for all decision
    making. *)
val interact_with_proxy : Player.t -> ipaddr:string -> port:int -> unit
