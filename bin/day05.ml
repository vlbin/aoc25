let split_and_parse str = 
  match String.split_on_char '-' str |> List.map int_of_string with
  | [min; max] -> (min, max)
  | _ -> failwith "Invalid range"

let inputs file =
  let lines = In_channel.with_open_bin file In_channel.input_lines in
  let fresh_ranges = List.take_while (fun a -> a <> "") lines |> List.map split_and_parse in
  let drop = List.length fresh_ranges in
  let all_ingredients = List.drop (drop + 1) lines |> List.map (int_of_string) in
  (fresh_ranges, all_ingredients)

let in_range number range =
    number >= (fst range) && number <= (snd range)

let ranges_overlap (a1, b1) (a2, b2) =
  in_range a1 (a2, b2) || in_range a2 (a1, b1)

let merge_ranges (a1, b1) (a2, b2) =
  if ranges_overlap (a1, b1) (a2, b2) then
    let left = min a1 a2 in
    let right = max b1 b2 in
    [(left, right)]
  else 
    [(a1, b1); (a2, b2)]

let in_range_list fresh_ranges number =
  match (List.find_opt (in_range number) fresh_ranges) with 
  | None -> 0
  | _ -> 1  

let comp_pairs p1 p2 =
  compare (fst p1) (fst p2)

let sort_ranges ranges =
  List.sort comp_pairs ranges

let merge_first pair list =
  match list with
  | [] -> [pair]
  | (a :: rest) -> List.append (merge_ranges pair a) rest

let rec merge transformed pairs =
  match pairs with
  | [] -> transformed
  | (a :: rest) -> merge (merge_first a transformed) rest

let part1 fresh_ranges all =
  List.map (in_range_list fresh_ranges) all |> List.fold_left ( + ) 0

let part2 (fresh_ranges : (int * int) list) =
  sort_ranges fresh_ranges
  |> merge []
  |> List.map (fun (a,b) -> (b - a) + 1)
  |> List.fold_left (+) 0

let () =
  let (fresh, all) = inputs "inputs/day05" in
  let p1_res = part1 fresh all in
  let p2_res = part2 fresh in

  Printf.printf "%d\n" p1_res;
  Printf.printf "%d" p2_res;

