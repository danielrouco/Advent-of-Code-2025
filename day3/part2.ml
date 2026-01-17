let file = open_in "input.txt"

let banks =
  let rec aux () =
    try
      let str = input_line file in
      List.init (String.length str) (fun x ->
          int_of_string (String.sub str x 1))
      :: aux ()
    with End_of_file -> []
  in
  aux ()

let first_digit_and_rest digit = function
  | [] -> raise (Invalid_argument "first_digit_and_rest")
  | h :: t ->
      let rec aux max l =
        if List.length l = digit - 1 then max
        else
          match l with
          | x :: t -> if x > fst max then aux (x, t) t else aux max t
          | _ -> raise (Invalid_argument "first_digit_and_rest")
      in
      aux (h, t) t

let max_list = List.fold_left max 0

let max_power l =
  let rec aux l = function
    | 1 -> max_list l
    | n ->
        let first_digit, rest = first_digit_and_rest n l in
        (first_digit * int_of_float (10. ** float_of_int (n - 1)))
        + aux rest (n - 1)
  in
  aux l 12

let result = List.fold_left (fun acc a -> acc + max_power a) 0 banks
let _ = print_endline (string_of_int result)
let _ = close_in file