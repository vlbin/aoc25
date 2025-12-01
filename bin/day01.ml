let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let input = read_lines "inputs/day01"

let get_amount input = int_of_string (String.sub input 1 (String.length input - 1))
let get_dir input = String.get input 0

let turn dir amount start = 
  match dir with
  | 'L' -> (start + 100 - amount) mod 100
  | _ -> (start + amount) mod 100

let rec make_turns instrs position acc =
  match instrs with 
  | [] -> acc
  | (instr :: rest) -> make_turns rest (turn (get_dir instr) (get_amount instr) position) (position :: acc)

let () = 
  let turns = make_turns input 50 [] in
  let zeros = List.length (List.filter (fun x -> x == 0) turns) in
  Printf.printf "%d " zeros