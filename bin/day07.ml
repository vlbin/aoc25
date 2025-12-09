let explode s = 
  List.init (String.length s) (String.get s) 

(* let join (list : int list) = 
  String.concat "" (List.map (Int.to_string) list) *)

let has_splitter line = 
  List.length line <> List.length (List.filter (Char.equal '.') line)

let unique list =
  let rec unique_helper list acc =
    match list with 
    | [] -> acc
    | x :: xs -> 
      let next_acc = if List.mem x acc then acc else x :: acc in
      unique_helper xs next_acc in
  unique_helper list []

let inputs file =
  In_channel.with_open_bin file In_channel.input_lines
  |> List.map explode
  |> List.filter has_splitter
  |> List.map (List.mapi (fun i a -> if a = '.' then -1 else i))
  |> List.map (List.filter (fun a -> a >= 0))

let split splitters beam =
  let matching_splitter = List.find_opt (fun splitter -> Int.equal beam splitter) splitters in
  match matching_splitter with
  | None -> [beam]
  | Some n -> [n-1; n+1]

(* let pow num = 
  let rec pow_helper num acc =
    match num with 
    | 0 -> 1
    | 1 -> 2
    | n -> pow_helper (num - 1) *)

let dec2bin num =
  match num with
  | 0 -> [0]
  | _ ->
    let rec loop n acc =
      match n with
      | 0 -> acc
      | n -> loop (n / 2) ((n mod 2) :: acc) in
    loop num []

(* let next_alg alg = *)


let get_next_beams beams splitters =
  let next_beams = List.map (split splitters) beams in
  let splits = List.filter (fun beam -> List.length beam = 2) next_beams in
  (unique (List.concat next_beams), List.length splits)


let rec split beams lines splits =
  match lines with 
  | [] -> splits
  | line :: rest -> 
    let (next_beams, next_splits) = get_next_beams beams line in
    split next_beams rest (splits + next_splits)

let part1 data =
  let starting_beam = List.hd (List.hd data) in
  split [starting_beam] (List.tl data) 0

let split_alg beam alg =
  match alg with
  | [] -> (beam - 1, [])
  | n :: rest -> if (n = 1) then (beam + 1, rest) else (beam - 1, rest) 

let rec walk_path beam alg lines acc =
  (* Printf.printf "beam: %d, alg: %s\n" beam (join alg); *)
  match lines with
  | [] -> List.rev (beam :: acc)
  | splitters :: rest -> 
    let matching_splitter = List.mem beam splitters in 
    match matching_splitter with
    | false -> walk_path beam alg rest (beam :: acc)
    | true -> 
      let (next_beam, next_alg) = split_alg beam alg in
      walk_path next_beam next_alg rest (beam :: acc)


let ways_to_x x prev_line =
  let score = List.filter (fun (idx, _) -> idx = x - 1 || idx = x + 1) prev_line
  |> List.map Pair.snd in
  match score with 
  | [] -> 1
  | list -> List.fold_left (+) 0 list


let count_ways start lines = 
  
  

let part2 data =
  let starting_beam = List.hd (List.hd data) in
  traverse_bin starting_beam (List.tl data)


let () = 
  let data = inputs "inputs/day07" in
  Printf.printf "%d \n" (part1 data);
  Printf.printf "%d " (List.length (part2 data))