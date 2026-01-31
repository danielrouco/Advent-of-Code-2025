let file = open_in "input.txt"

let tuple_of_string str =
  match String.split_on_char ',' str with
  | [ x; y ] -> (int_of_string x, int_of_string y)
  | _ -> raise (Failure "tuple_of_string")

let red_tiles =
  let rec aux () =
    try
      let str = input_line file in
      tuple_of_string str :: aux ()
    with End_of_file -> []
  in
  aux ()

let area (x0, y0) (x1, y1) = abs ((x0 - x1 + 1) * (y0 - y1 + 1))

let largest_rectangle tiles =
  List.fold_left
    (fun ((tile_acc01, tile_acc02), a_acc0) tile0 ->
      let new_tiles, new_area =
        (List.fold_left (fun ((tile_acc11, tile_acc12), a_acc1) tile1 ->
             let a_1 = area tile0 tile1 in
             if a_1 > a_acc1 then ((tile0, tile1), a_1)
             else ((tile_acc11, tile_acc12), a_acc1)))
          (((0, 0), (0, 0)), -1)
          tiles
      in
      if new_area > a_acc0 then (new_tiles, new_area)
      else ((tile_acc01, tile_acc02), a_acc0))
    (((0, 0), (0, 0)), -1)
    tiles

let _, largest_area = largest_rectangle red_tiles
let _ = print_endline (string_of_int largest_area)
let _ = close_in file
