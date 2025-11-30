let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents


let input = read_lines "inputs/day01"

let solution = String.concat " "

let () = print_endline (solution input)