let ranges file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char ',' contents

let split_and_parse str = 
  match String.split_on_char '-' str |> List.map int_of_string with
  | [min; max] -> (min, max)
  | _ -> failwith "Invalid range"

let parsed_ranges ranges = 
  List.map split_and_parse ranges

let input = ranges "inputs/day02"

let chunk_invalid chunks = 
  match chunks with 
  | [] -> true
  | a :: _ -> List.length (List.filter (fun e -> e = a) chunks) = (List.length chunks)

let chunk string size =
  let rec chunk_next string acc =
    match string with
    | "" -> acc
    | str -> chunk_next (String.sub str size ((String.length str) - size)) ((String.sub str 0 size) :: acc) in
  chunk_next string []

let all_chunks id = 
  let id_str = Int.to_string id in
  let len = String.length id_str in
  let max = len / 2 in
  let rec chunk_loop chunks size =
    if size > max then chunks
    else if size mod len = 0 then chunk_loop (chunk id_str size :: chunks) (size + 1)
    else chunk_loop chunks (size + 1) in
  chunk_loop [] 1

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

let chunks_to_string =
  String.concat ", "


let is_invalid id =
  let rec is_invalid_loop chunk_size =
    if chunk_size > (String.length id) / 2 then false
    else if (String.length id) mod chunk_size <> 0 then is_invalid_loop (chunk_size + 1)
    else if chunk_invalid (chunk id chunk_size) then true
    else is_invalid_loop (chunk_size + 1) in
  is_invalid_loop 1

let rec expand_range (min, max) acc =
  if min > max then List.rev acc
  else expand_range (min+1, max) (min::acc)

let () = 
  let splitted = List.map split_and_parse input in
  let lst = List.map (fun (min, max) -> expand_range (min, max) []) splitted |> List.flatten |> List.map (Int.to_string) in
  let invalid = List.filter is_invalid lst |> List.map int_of_string in
  Printf.printf "%d \n" (List.fold_left (+) 0 invalid)