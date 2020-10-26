type t = int

let hole = 0

let create fish = 
  if fish > 0
  then fish
  else failwith "fish count must be positive"

let get_fish t = t

let is_hole t = t = 0
