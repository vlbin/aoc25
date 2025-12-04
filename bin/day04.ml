let get_coord yi xi map = 
  map.(xi).(yi)

let replace_coord yi xi map =
  map.(xi).(yi) <- '.'

let dirs = [
  (-1, -1); (-1, 0); (-1, 1);
  (0, -1); (0, 1);
  (1, -1); (1, 0); (1, 1);
]

let get_neighbor_indices yi xi map = 
  let rows = Array.length map in
  let cols = Array.length (Array.get map 0) in 
  List.map (fun (a,b) -> (yi + a, xi + b)) dirs
  |> List.filter (fun (a,b) -> a >= 0 && b >= 0 && a < rows && b < cols)

let get_neighboring_rolls yi xi map =
  match get_coord yi xi map with 
  | '.' -> ((yi,xi), 9)
  | _ -> 
    let indices = get_neighbor_indices yi xi map in
    let len = List.map (fun (a,b) -> get_coord a b map) indices
    |> List.filter (Char.equal '@')
    |> List.length in
    ((yi, xi), len)

let inputs file =
  let lines = In_channel.with_open_bin file In_channel.input_lines in
  let height = List.length lines in
  let width = String.length (List.hd lines) in
  let grid = Array.make_matrix width height '.' in
  List.iteri (
    fun i line -> String.iteri (
      fun j el -> 
        grid.(j).(i) <- el
    ) line
  ) lines;
  grid

let rec replace_map replace_coords map = 
  match replace_coords with 
  | [] -> map
  | (yi, xi) :: rest -> 
    replace_coord yi xi map;
    replace_map rest map

let get_replaceable_indices (map: char array array) =
  Array.mapi (fun yi line -> Array.mapi (fun xi _ -> get_neighboring_rolls yi xi map) line) map
  |> Array.map (Array.to_list)
  |> Array.to_list
  |> List.flatten
  |> List.filter (fun (_, e) -> e < 4)
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
  