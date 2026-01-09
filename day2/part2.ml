let file = open_in "input.txt"
let line = input_line file

let ranges =
  let range_of_string str =
    match String.split_on_char '-' str with
    | [ first; last ] -> (int_of_string first, int_of_string last)
    | _ -> raise (Invalid_argument "range_of_string")
  in
  List.map range_of_string (String.split_on_char ',' line)

let is_invalid str =
  let l = String.length str in
  let rec with_length n str =
    String.sub str 0 n = String.sub str n n
    &&
    let l = String.length str in
    l = 2 * n
    ||
      try with_length n (String.sub str n (l - n))
      with Invalid_argument _ -> false
  in
  let rec aux n = (not (n > l / 2)) && (with_length n str || aux (n + 1)) in
  aux 1

let rec invalid_ranges_sum (first, last) =
  if first > last then 0
  else
    let str = string_of_int first in
    if is_invalid str then first + invalid_ranges_sum (first + 1, last)
    else invalid_ranges_sum (first + 1, last)

let result = List.fold_left ( + ) 0 (List.map invalid_ranges_sum ranges)
let _ = print_endline (string_of_int result)
let _ = close_in file
