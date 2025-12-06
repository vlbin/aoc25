let inputs file =
  In_channel.with_open_bin file In_channel.input_lines
  |> List.map (String.split_on_char ' ')
  |> List.map (List.filter (fun a -> a <> ""))

let join (list : string list) = 
  String.concat "" list

let explode s = 
  List.init (String.length s) (String.get s) 
  |> List.map Char.escaped

let heads matrix =
  try
    List.map (List.hd) matrix
  with Failure _ -> []

let heads_num (matrix : string list list) =
  let hd line =
    try List.hd line with Failure _ -> "" in
  List.map (hd) matrix |> List.filter (fun a -> a <> "") |> join

let get_op = function
  | "*" -> (1, Int.mul)
  | _ -> (0, Int.add)

let tl list =
  try List.tl list with Failure _ -> []

let hd (list : string list) = 
  try List.hd list with Failure _ -> ""

let rec max_length list max_el =
  match list with
  | [] -> max_el
  | a :: rest -> max_length rest (max (String.length a) max_el)

let calc_line line = 
  let line = List.rev line in
  let (start, op) = List.hd line |> get_op in
  let nums = (List.rev (List.drop 1 line)) in
  List.fold_left op start (List.map int_of_string nums)

let rec calc_matrix acc matrix =
  let line = heads matrix in
  match line with
  | [] -> acc
  | list -> 
    let new_matrix = List.map (List.tl) matrix in
    calc_matrix (acc + calc_line list) new_matrix
  
let part1 = 
  inputs "inputs/day06"
  |> calc_matrix 0

let rec sum_first_cols (lines : string list list) (acc : int list) op_sign =
  let col = List.map hd lines 
  |> List.filter (fun a -> a <> " ")
  |> join in

  match col with
  | "" -> 
    let (start, op) = get_op op_sign in
    (List.map (tl) lines, List.fold_left (op) start acc)
  | n -> 
    let num = int_of_string n in
    sum_first_cols (List.map (List.tl) lines) (num :: acc) op_sign

let rec sum2 lines ops acc =
  match ops with
  | [] -> acc
  | op :: rest -> 
    let (new_lines, cols_sum) = sum_first_cols lines [] op in
    sum2 new_lines rest acc + cols_sum

let inputs_2 file =
  In_channel.with_open_bin file In_channel.input_lines |> List.rev

let part2 = 
  let lines = inputs_2 "inputs/day06" in
  let ops = List.hd lines 
  |> String.split_on_char ' '
  |> List.filter (fun a -> a <> "") in
  let numeric_lines = List.tl lines
  |> List.rev
  |> List.map explode in
  
  sum2 numeric_lines ops 0

let () = 
  Printf.printf "%d " part2
