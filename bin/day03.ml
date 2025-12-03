let inputs file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let banks = inputs "inputs/day03"

let sublist b e list = List.take (e-b) (List.drop b list)

let find_max_int (list : int list) e = 
  let range = sublist 0 (List.length list - e) list in
  let rec find_num num =
    match List.find_index (Int.equal num) range with 
    | None -> find_num (num - 1)
    | Some i -> 
        let after_drop = List.drop i list in
        let num = List.hd after_drop in
        (num, List.drop 1 after_drop) in
  find_num 9

let find_max (size : int) (bank : int list) =
  let rec build_max_num acc list skip_from_end =
    match skip_from_end with 
    | n when n < 0 -> acc
    | n -> 
        let (max_i, rest_of_bank) = find_max_int list n in
        build_max_num (max_i :: acc) rest_of_bank (n-1) in

  build_max_num [] bank (size - 1)
  |> List.map (Int.to_string)
  |> List.rev
  |> String.concat ""
  |> int_of_string

let explode s = 
  List.init (String.length s) (String.get s) 
  |> List.map Char.escaped
  |> List.map int_of_string

let output_joltage batteries_on =
  List.map (explode) banks
  |> List.map (find_max batteries_on)
  |> List.fold_left ( + ) 0

let part1 = output_joltage 2

let part2 = output_joltage 12

let () = 
  Printf.printf "%d\n" part1;
  Printf.printf "%d\n" part2;