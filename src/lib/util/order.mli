(** A ['a t] represents an ordered collection of ['a], with some element being
    the "current" in the ordering. The order can also be rotated *)
type 'a t

(** Create an ordering with elements from the given list, with given element
    being the "current", while preserving their ordering in the list.
    Return [None] if list is empty or given element is not in the list *)
val create_with_start : 'a list -> 'a -> 'a t option

(** Return the "current" element in this ordering *)
val get_current : 'a t -> 'a

(** Rotate "current" element to the next one in the ordering *)
val rotate : 'a t -> 'a t
