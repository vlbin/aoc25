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
  | "." -> 99
  | _ -> 
    let indices = get_neighbor_indices yi xi map in
    List.map (fun (a,b) -> get_coord a b map) indices
    |> List.filter (String.equal "@")
    |> List.length
    

let inputs file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents |> List.map (explode)

let () = 
  let map = inputs "inputs/day04" in
   
  List.mapi (fun yi line -> List.mapi (fun xi _ -> get_neighboring_rolls yi xi map) line) map
    |> List.flatten
    |> List.filter (fun e -> e < 4)
    |> List.length
    |> print_int
  