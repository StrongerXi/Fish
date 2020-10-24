open !Core

(* Essentially a 2 list based circular queue *)
type 'a t = 
  { nexts : 'a list (* 1st element in [prevs] is the one after [current] *)
  ; current : 'a
  ; prevs : 'a list (* 1st element in [prevs] is the one before [current] *)
  }

(** Create an ordering via the order of elements in [xs] *)
let create_exn (xs : 'a list) =
  match xs with
  | [] -> failwith "An order must be non-empty"
  | x::xs -> { nexts = xs; current = x; prevs = [] }
;;

let create_with_start xs start =
  (** [prevs] contains the already checked elements in reversed order.
      Return [None] if [start] is not found in [xs]. *)
  let rec partition xs prevs : 'a t option =
    match xs with
    | x::xs ->
      if Core.phys_same x start
      then Some(create_exn @@ start::xs @ (List.rev prevs))
      else partition xs (x::prevs)
    | [] -> None
  in partition xs []
;;

let get_current t = t.current
;;

let rotate { nexts; current; prevs } =
  match nexts with
  | [] -> create_exn @@ List.rev (current::prevs)
  | n::ns -> { nexts = ns; current = n; prevs = current::prevs }
;;
