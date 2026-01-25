let file = open_in "input.txt"

let lines =
  let rec aux () =
    try
      let str = input_line file in
      str :: aux ()
    with End_of_file -> []
  in
  aux ()

let rec last_and_rest = function
  | [ h ] -> (h, [])
  | h :: t ->
      let last, rest = last_and_rest t in
      (last, h :: rest)
  | _ -> raise (Failure "last_and_rest")

(* This function does the same as List.map f (String.split_on_char ' ' str)
   but ignoring(removing) whitespaces *)
let split_and_map f str =
  List.fold_right
    (fun a acc -> match a with "" -> acc | a -> f a :: acc)
    (String.split_on_char ' ' str)
    []

let operation_of_string = function
  | "+" -> ( + )
  | "*" -> ( * )
  | _ -> raise (Failure "operation_of_string")

let operations, numbers = last_and_rest lines
let operations = split_and_map operation_of_string operations

let parse_column n strings =
  int_of_string_opt
    (String.trim
       (String.concat "" (List.map (fun str -> String.sub str n 1) strings)))

let parse_numbers strings =
  let l = String.length (List.hd strings) in
  let rec aux n =
    let rec parse_operands n =
      if n = l then ([], n + 1)
      else
        match parse_column n strings with
        | Some x ->
            let list, index = parse_operands (n + 1) in
            (x :: list, index)
        | None -> ([], n + 1)
    in
    let lis, x = parse_operands n in
    if x = l + 1 then [ lis ] else lis :: aux x
  in
  aux 0

let numbers = parse_numbers numbers

let result =
  List.fold_left2
    (fun acc list op ->
      match list with
      | h :: t -> acc + List.fold_left op h t
      | _ -> raise (Failure "result"))
    0 numbers operations

let _ = print_endline (string_of_int result)
let _ = close_in file
