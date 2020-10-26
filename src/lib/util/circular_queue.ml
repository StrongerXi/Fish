open !Core

(* Essentially a 2 list based circular queue *)
type 'a t = 
  { nexts : 'a list (* 1st element in [prevs] is the one after [current] *)
  ; current : 'a
  ; prevs : 'a list (* 1st element in [prevs] is the one before [current] *)
  }

(** Create a circular queue following the order of elements in [xs] *)
let create_exn (xs : 'a list) =
  match xs with
  | [] -> failwith "An order must be non-empty"
  | x::xs -> { nexts = xs; current = x; prevs = [] }
;;

let create current nexts =
  { prevs = []; current; nexts }
;;

let get_current t = t.current
;;

let rotate { nexts; current; prevs } =
  match nexts with
  | [] -> create_exn @@ List.rev (current::prevs)
  | n::ns -> { nexts = ns; current = n; prevs = current::prevs }
;;

let to_list { nexts; current; prevs } =
  current::nexts @ (List.rev prevs) (* the one before [current] is the last *)
