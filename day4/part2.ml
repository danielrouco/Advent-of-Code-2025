let file = open_in "input.txt"

let map =
  let rec aux () =
    try
      let str = input_line file in
      Array.init (String.length str) (fun x -> String.get str x = '@') :: aux ()
    with End_of_file -> []
  in
  Array.of_list (aux ())

let surroundings i j =
  [
    (i - 1, j - 1);
    (i - 1, j);
    (i - 1, j + 1);
    (i, j - 1);
    (i, j + 1);
    (i + 1, j - 1);
    (i + 1, j);
    (i + 1, j + 1);
  ]

let accesible i j map =
  List.fold_left
    (fun acc (x, y) ->
      acc + try if map.(x).(y) then 1 else 0 with Invalid_argument _ -> 0)
    0 (surroundings i j)
  < 4

let rec rolls () =
  let n = ref 0 in
  for i = 0 to Array.length map - 1 do
    for j = 0 to Array.length map.(0) - 1 do
      if map.(i).(j) && accesible i j map then (
        incr n;
        map.(i).(j) <- false)
      else ()
    done
  done;
  if !n = 0 then 0 else !n + rolls ()

let _ = print_endline (string_of_int (rolls ()))
let _ = close_in file