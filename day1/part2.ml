let file = open_in "input.txt"

type direction = L | R

let direction_of_char = function
  | 'L' -> L
  | 'R' -> R
  | _ -> raise (Failure "direction_from_string")

let rec read_lines f =
  try
    let str = input_line f in
    let tuple =
      ( direction_of_char (String.get str 0),
        int_of_string (String.sub str 1 (String.length str - 1)) )
    in
    tuple :: read_lines f
  with End_of_file -> []

let lines = read_lines file

let move pos (dir, amount) =
  let real_amount = amount mod 100 in
  match dir with
  | R -> (pos + real_amount) mod 100
  | L ->
      if pos - real_amount < 0 then 100 + pos - real_amount
      else pos - real_amount

let rec clicks_in_0 pos (dir, amount) =
  match dir with
  | R -> (pos + amount) / 100
  | L ->
      if pos = 0 then clicks_in_0 0 (R, amount)
      else clicks_in_0 (100 - pos) (R, amount)

let way_to_fold (pos_list, clicks) mv =
  (move pos_list mv, clicks + clicks_in_0 pos_list mv)

let state, clicks = List.fold_left way_to_fold (50, 0) lines
let _ = print_int clicks
let _ = close_in file