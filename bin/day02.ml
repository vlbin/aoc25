let ranges file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char ',' contents

let split_and_parse str = String.split_on_char '-' str |> List.map (int_of_string)

let parsed_ranges ranges = 
  List.map split_and_parse ranges

let input = ranges "inputs/day02"

let chunk string size =
  let rec chunk_next string acc =
    match string with
    | "" -> acc
    | str -> chunk_next (String.sub str size ((String.length str) - size)) ((String.sub str 0 size) :: acc) in
  chunk_next string []

let is_valid id =
  let id_str = Int.to_string(id) in
  let len = String.length id_str in
  match (len mod 2) with
  | 0 -> int_of_string (String.sub id_str 0 (len / 2)) != int_of_string (String.sub id_str (len / 2) (len / 2))
  | _ -> true


let invalid_in_range min max = 
  let rec loop curr acc =
    match curr with
    | n when n > max -> acc
    | _ -> loop (curr + 1) (if is_valid curr then acc else (curr :: acc)) in
  let invalid_nums = loop min [] in
  List.fold_left ( + ) 0 invalid_nums

let print_tuple (a,b) =
  Printf.printf "%s,%s" a b

let part1 =
  let parsed = parsed_ranges input in
  let invalid = List.map (fun (min :: max :: _) -> invalid_in_range min max) parsed in
  List.fold_left (+) 0 invalid

let () = 
  Printf.printf "%d" part1;
  Printf.printf "\n";
  
  