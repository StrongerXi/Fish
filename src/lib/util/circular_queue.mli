(** A ['a t] represents a circular queue (ordered collection) of ['a], with some 
    element being the "current" in the queue. *)
type 'a t

(** Create a circular queue with given element as the "current", and 
    elements from the given list, while preserving their order. *)
val create : 'a -> 'a list -> 'a t

(** Return the "current" element in this queue *)
val get_current : 'a t -> 'a

(** Rotate "current" element to the next one in the queue *)
val rotate : 'a t -> 'a t

(** Return an updated queue with current element removed.
    return [None] if the queue becomes empty after removal. *)
val remove_current : 'a t -> 'a t option

(** Return a list where the first element is the "current" element in the queue,
    and the rest are in order *)
val to_list : 'a t -> 'a list
