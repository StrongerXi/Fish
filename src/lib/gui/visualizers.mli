(** An observer which renders game state upon each update from referee 
    Print out debugging messages to STDOUT if [debug] is [true].
    [delay] represents the minimal delay between showing game state update *)
val get_observer_view : ?debug:bool -> ?delay:float -> unit -> Admin.Referee.Game_observer.t
