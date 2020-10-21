type t = 
  { fish : int
  }

let empty_tile = { fish = 0 }

let create fish = 
  if fish > 0
  then { fish }
  else failwith "fish count must be positive"

let get_fish t = t.fish

let is_empty t = t.fish = 0
