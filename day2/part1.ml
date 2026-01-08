let file = open_in "input.txt"
let line = input_line file

let ranges =
  let range_of_string str =
    match String.split_on_char '-' str with
    | [ first; last ] -> (int_of_string first, int_of_string last)
    | _ -> raise (Invalid_argument "range_of_string")
  in
  List.map range_of_string (String.split_on_char ',' line)

let rec invalid_ranges_sum (first, last) =
  if first > last then 0
  else
    let str = string_of_int first in
    let l = String.length str in
    if l mod 2 = 0 then
      let mid = l / 2 in
      let str1, str2 = (String.sub str 0 mid, String.sub str mid mid) in
      if str1 = str2 then first + invalid_ranges_sum (first + 1, last)
      else invalid_ranges_sum (first + 1, last)
    else invalid_ranges_sum (first + 1, last)

let result = List.fold_left ( + ) 0 (List.map invalid_ranges_sum ranges)
let _ = print_endline (string_of_int result)
let _ = close_in file
