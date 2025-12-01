let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let input = read_lines "inputs/day01"

let get_amount input = int_of_string (String.sub input 1 (String.length input - 1))

let get_amount_dir input = 
  match String.get input 0 with
  | 'L' -> -1 * (get_amount input)
  | _ -> get_amount input

let turn amount start = (start + amount)

let bounded amount = ((amount mod 100) + 100) mod 100

let rec make_turns instrs pos zeros =
  match instrs with 
  | [] -> zeros
  | (instr :: rest) -> 
    let amount = get_amount_dir instr in
    let new_pos = bounded (turn amount pos) in
    make_turns rest new_pos (if new_pos = 0 then zeros + 1 else zeros)

let passed_times pos amount new_pos =
  let laps = (abs amount) / 100 in
  let did_pass = pos != 0 && new_pos != 0 && ((pos > new_pos && amount >= 0) || (pos < new_pos && amount <= 0)) in
  laps + (if did_pass then 1 else 0)

let rec make_turns_new instrs pos zeros =
  match instrs with 
  | [] -> zeros
  | (instr :: rest) -> 
    let amount = get_amount_dir instr in
    let new_pos = bounded (turn amount pos) in
    let passed = passed_times pos amount new_pos in
    let end_zero = if new_pos = 0 then 1 else 0 in
    make_turns_new rest new_pos (zeros + passed + end_zero)

let part1 =
 make_turns input 50 0

let part2 =
  (* passed_times 0 (-105) 95 *)
 make_turns_new input 50 0
  

let () = 
  Printf.printf "%d \n" part1;
  Printf.printf "%d \n" part2;