let file = open_in "input.txt"

let lines =
  let rec aux () =
    try
      let str = input_line file in
      str :: aux ()
    with End_of_file -> []
  in
  aux ()

(* Returns the next beams and how many divisions occured in this step *)
let next_beams_and_divisions beams line =
  let l = String.length line in
  let beam_is_valid beam beams l =
    beam >= 0 && beam < l && not (List.mem beam beams)
  in
  let add_beams actual_beams new_beams =
    List.fold_left
      (fun beams a ->
        if beam_is_valid a actual_beams l then a :: beams else beams)
      actual_beams new_beams
  in
  let rec remove_one_equal e = function
    | [] -> []
    | h :: t -> if e = h then t else h :: remove_one_equal e t
  in
  List.fold_left
    (fun (beams, divisions) beam ->
      if String.get line beam = '^' then
        ( add_beams (remove_one_equal beam beams) [ beam - 1; beam + 1 ],
          divisions + 1 )
      else (beams, divisions))
    (beams, 0) beams

let rec divisions beams = function
  | [] -> 0
  | line :: lines ->
      let bs, divi = next_beams_and_divisions beams line in
      divi + divisions bs lines

let result = divisions [ String.index (List.hd lines) 'S' ] (List.tl lines)
let _ = print_endline (string_of_int result)
let _ = close_in file
