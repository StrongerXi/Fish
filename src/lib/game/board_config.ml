type t =
  { width : int
  ; height : int
  ; holes : Position.t list
  ; min_one_fish_tile : int
  ; default_num_of_fish : int
  }

let create ~height ~width = { width; height;
                              holes = [];
                              min_one_fish_tile = 0;
                              default_num_of_fish = 1; }

let set_width width t = { t with width }
let get_width  t = t.width

let set_height height t = { t with height }
let get_height  t = t.height

let set_holes holes t = { t with holes }
let get_holes  t = t.holes

let set_min_num_of_one_fish_tile min_one_fish_tile t = 
  { t with min_one_fish_tile }
let get_min_num_of_one_fish_tile  t = t.min_one_fish_tile

let set_default_num_of_fish default_num_of_fish t = 
  { t with default_num_of_fish }
let get_default_num_of_fish  t = t.default_num_of_fish
