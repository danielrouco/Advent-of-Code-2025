let file = open_in "input.txt"

let range_of_string str =
  match String.split_on_char '-' str with
  | [ first; last ] -> (int_of_string first, int_of_string last)
  | _ -> raise (Invalid_argument "range_of_string")

let ranges =
  let rec aux () =
    let str = input_line file in
    match str with "" -> [] | str -> range_of_string str :: aux ()
  in
  aux ()

let ingredients =
  let rec aux () =
    try
      let str = input_line file in
      int_of_string str :: aux ()
    with End_of_file -> []
  in
  aux ()

let in_range ingredient (x, y) = ingredient >= x && ingredient <= y

let result =
  List.fold_left
    (fun acc ingredient ->
      if List.exists (in_range ingredient) ranges then acc + 1 else acc)
    0 ingredients

let _ = print_endline (string_of_int result)
let _ = close_in file
