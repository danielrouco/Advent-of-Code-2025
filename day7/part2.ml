let file = open_in "input.txt"

let lines =
  let rec aux () =
    try
      let str = input_line file in
      str :: aux ()
    with End_of_file -> []
  in
  Array.of_list (aux ())

let horizontal_length = String.length lines.(0)
let vertical_length = Array.length lines
let beam_in_bounds beam = beam >= 0 && beam < horizontal_length
let tbl = Hashtbl.create 100_000

let rec timelines beam row =
  if not (beam_in_bounds beam) then 0
  else if row >= vertical_length - 1 then 1
  else
    match Hashtbl.find_opt tbl (beam, row) with
    | Some x -> x
    | None ->
        if String.get lines.(row) beam = '^' then (
          let left = timelines (beam - 1) (row + 1) in
          Hashtbl.add tbl (beam, row) left;
          let right = timelines (beam + 1) (row + 1) in
          Hashtbl.add tbl (beam, row) right;
          left + right)
        else
          let middle = timelines beam (row + 1) in
          Hashtbl.add tbl (beam, row) middle;
          middle

let result = timelines (String.index lines.(0) 'S') 1
let _ = print_endline (string_of_int result)
let _ = close_in file
