let file = open_in "input.txt"

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

let numbers, operations =
  let rec aux () =
    let str = input_line file in
    try
      let line = split_and_map int_of_string str in
      let ns, ops = aux () in
      (line :: ns, ops)
    with Failure _ -> ([], split_and_map operation_of_string str)
  in
  aux ()

let rec calc numbers operations =
  match (numbers, operations) with
  | [] :: _, [] -> 0
  | numbers, op :: t -> (
      match (List.map List.hd numbers, List.map List.tl numbers) with
      | head :: heads, tails -> List.fold_left op head heads + calc tails t
      | _ -> raise (Failure "calc"))
  | _ -> raise (Failure "calc")

let result = calc numbers operations
let _ = print_endline (string_of_int result)
let _ = close_in file
