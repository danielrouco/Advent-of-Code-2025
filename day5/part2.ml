let file = open_in "input.txt"

let range_of_string str =
  match String.split_on_char '-' str with
  | [ first; last ] -> (int_of_string first, int_of_string last)
  | _ -> raise (Invalid_argument "range_of_string")

let in_range ingredient (x, y) = ingredient >= x && ingredient <= y

let rec add (x, y) = function
  | [] -> [ (x, y) ]
  | (mi, ma) :: t ->
      if in_range x (mi, ma) then
        if in_range y (mi, ma) then (mi, ma) :: t else add (mi, y) t
      else if in_range y (mi, ma) then add (x, ma) t
      else if in_range mi (x, y) && in_range ma (x, y) then add (x, y) t
      else (mi, ma) :: add (x, y) t

let ranges =
  let rec aux () =
    let str = input_line file in
    match str with "" -> [] | str -> add (range_of_string str) (aux ())
  in
  aux ()

let n_in_range (x, y) = y - x + 1

let result = List.fold_left (fun acc a -> acc + n_in_range a) 0 ranges

let _ = print_endline (string_of_int result)
let _ = close_in file
