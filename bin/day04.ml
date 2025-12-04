let explode s = 
  List.init (String.length s) (String.get s) 
  |> List.map Char.escaped


let print_map map =
  List.iter (fun line -> Printf.printf "%s\n" (String.concat "" line)) map

let get_coord yi xi map = 
  let line = List.hd (List.drop yi map) in
  List.hd (List.drop xi line)

let get_line yi map = 
  List.hd (List.drop yi map)

let replace i list =
  List.mapi (fun j e -> if i = j then "." else e) list

let replace_coord yi xi map =
  let before = List.take yi map in
  let changed = replace xi (List.nth map yi) in
  let after = List.drop (yi + 1) map in
  List.concat (before :: (changed :: []) :: after :: [])

let dirs = [
  (-1, -1); (-1, 0); (-1, 1);
  (0, -1); (0, 1);
  (1, -1); (1, 0); (1, 1);
]

let get_neighbor_indices yi xi map = 
  let rows = List.length map in
  let cols = List.length (List.hd map) in 
  List.map (fun (a,b) -> (yi + a, xi + b)) dirs
  |> List.filter (fun (a,b) -> a >= 0 && b >= 0 && a < rows && b < cols)

let get_neighboring_rolls yi xi map =
  match get_coord yi xi map with 
  | "." -> ((yi,xi), 9)
  | _ -> 
    let indices = get_neighbor_indices yi xi map in
    let len = List.map (fun (a,b) -> get_coord a b map) indices
    |> List.filter (String.equal "@")
    |> List.length in
    ((yi, xi), len)

let inputs file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents |> List.map (explode)

let rec replace_map replace_coords map = 
  match replace_coords with 
  | [] -> map
  | (yi, xi) :: rest -> replace_map rest (replace_coord yi xi map)

let get_replaceable_indices map =
  List.mapi (fun yi line -> List.mapi (fun xi _ -> get_neighboring_rolls yi xi map) line) map
    |> List.flatten
    |> List.filter (fun ((a,b), e) -> e < 4)
    |> List.map (fun ((a,b), _) -> (a,b))


let rec replace_until_done (indices : (int * int) list) map replaced =
  match indices with
  | [] -> replaced
  | _indices -> 
      let new_map = replace_map _indices map in
      let replaceable = get_replaceable_indices new_map in
      replace_until_done replaceable new_map (replaced + List.length replaceable)


let () = 
  let map = inputs "inputs/day04" in
  let indices = get_replaceable_indices map in
  let map_after = replace_until_done indices map (List.length indices) in
  print_int map_after
  