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
    | L -> if pos - real_amount < 0 then 100 + pos - real_amount
    else pos - real_amount

let states = List.fold_left (fun acc a -> move (List.hd acc) a :: acc) [50] lines

let _ = print_int (List.length (List.filter ((=) 0) states))

let _ = close_in file