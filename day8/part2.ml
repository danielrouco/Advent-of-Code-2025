let file = open_in "input.txt"

let triple_of_string str =
  match String.split_on_char ',' str with
  | [ x; y; z ] -> (int_of_string x, int_of_string y, int_of_string z)
  | _ -> raise (Failure "triple_of_string")

let jboxes =
  let rec aux () =
    try
      let str = input_line file in
      triple_of_string str :: aux ()
    with End_of_file -> []
  in
  aux ()

let rec distance triple1 (x2, y2, z2) =
  match triple1 with
  | 0, 0, 0 ->
      let x2, y2, z2 = (float_of_int x2, float_of_int y2, float_of_int z2) in
      sqrt ((x2 ** 2.) +. (y2 ** 2.) +. (z2 ** 2.))
  | x1, y1, z1 -> distance (0, 0, 0) (x2 - x1, y2 - y1, z2 - z1)

module IntTriple = struct
  type t = int * int * int

  let compare (x1, y1, z1) (x2, y2, z2) =
    match Stdlib.compare x1 x2 with
    | 0 -> (
        match Stdlib.compare y1 y2 with 0 -> Stdlib.compare z1 z2 | c -> c)
    | c -> c
end

module TripleSet = Set.Make (IntTriple)

(* This function returns the set in wich the new box is added and the list of
   sets without the set in wich the box is added *)
let rec add_box_to_set box = function
  | [] -> (TripleSet.singleton box, [])
  | h :: t ->
      if TripleSet.mem box h then (TripleSet.add box h, t)
      else
        let set, sets = add_box_to_set box t in
        (set, h :: sets)

let add_boxes_to_set box1 box2 sets =
  let new_set1, rest = add_box_to_set box1 sets in
  if TripleSet.mem box2 new_set1 then TripleSet.add box2 new_set1 :: rest
  else
    let new_set2, rest = add_box_to_set box2 rest in
    TripleSet.union new_set1 new_set2 :: rest

let all_edges_from triple triples =
  let triples = TripleSet.remove triple triples in
  TripleSet.fold
    (fun triple2 acc -> ((triple, triple2), distance triple triple2) :: acc)
    triples []

(* This function will generate "duplicates" for example the edge (1, 2) will be
   duplicated as (2, 1) but the cost of removing or not adding duplicates is 
   bigger than the extra cost of ordering a list twice as big. *)
let all_edges triples =
  TripleSet.fold
    (fun triple acc -> all_edges_from triple triples @ acc)
    triples []

(* As this list is ordered we will have duplicates (e.g. (1, 2) and (2, 1)) next
   to each other, so when traversing the list we will extract 2 elements at a 
   time ignoring one of them *)
let ordered_edges =
  List.sort
    (fun (_, d1) (_, d2) -> compare d1 d2)
    (all_edges (TripleSet.of_list jboxes))

let rec circuits sets edges last_edge boxes =
  if TripleSet.is_empty boxes then last_edge
  else
    match edges with
    | ((box1, box2), _) :: _ :: t ->
        circuits
          (add_boxes_to_set box1 box2 sets)
          t (box1, box2)
          (TripleSet.remove box2 (TripleSet.remove box1 boxes))
    | _ -> raise (Failure "circuits")

let result =
  let (x1, _, _), (x2, _, _) =
    circuits [] ordered_edges ((0, 0, 0), (0, 0, 0)) (TripleSet.of_list jboxes)
  in
  x1 * x2

let _ = print_endline (string_of_int result)
let _ = close_in file
