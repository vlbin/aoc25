type point = float*float*float


type edge = {
  a : point;
  b : point;
  dist : float;
}

let euclidean ((p1,p2,p3): point) ((q1,q2,q3): point) = 
  sqrt (
    ((p1 -. q1) ** 2.0) +.
    ((p2 -. q2) ** 2.0) +.
    ((p3 -. q3) ** 2.0)
  )

let make_edge a b : edge = 
  let dist = euclidean a b in
  {
    dist = dist;
    a = a;
    b = b;
  }

let rec make_edges graph list = 
  match list with
  | [] -> List.sort (fun a b -> compare a.dist b.dist) graph
  | [_] -> List.sort (fun a b -> compare a.dist b.dist) graph
  | p :: ps -> 
    let new_edges = List.map (make_edge p) ps in
    make_edges (new_edges @ graph) ps

let make_point arr : point = 
  match arr with 
  | [a;b;c] -> (a,b,c)
  | _ -> failwith "invalid_arg"

let inputs =
  In_channel.with_open_bin "inputs/day08" In_channel.input_lines
  |> List.map (String.split_on_char ',')
  |> List.map (List.map float_of_string)
  |> List.map make_point

let add_edge edge circuits reverse =
  (* print_point edge.a;
  print_point edge.b; *)
  match (Hashtbl.find_opt circuits edge.a, Hashtbl.find_opt circuits edge.b) with
  | (Some a, Some b) when a = b -> 
    (circuits, reverse)
  | (Some a, Some b) ->
    Hashtbl.filter_map_inplace (fun _coord id -> if id = b then Some a else Some id) circuits;
    Hashtbl.replace reverse a (Hashtbl.find reverse b @ Hashtbl.find reverse a);
    Hashtbl.remove reverse b;
    (circuits, reverse)
  | (_, _) -> (circuits, reverse)

let add_to_max maxs num =
  let new_maxs = match maxs with
  | [] -> [num]
  | [a] -> [a; num]
  | [a;b] -> [a;b;num]
  | a :: b :: c :: _ -> if num > a then [num;b;c] else [a;b;c] in
  List.sort compare new_maxs

let keys tbl = Hashtbl.fold (fun key _val acc -> key :: acc) tbl []

let rec make_circuits edges circuits reverse iter max_iter =
  if iter = max_iter then
    let maxs = Hashtbl.fold (fun _id coords acc -> add_to_max acc (List.length coords)) reverse [] in
    List.fold_left (Int.mul) 1 maxs
  else
    let edge = List.hd edges in
    let (next_circuits, next_reverse) = add_edge edge circuits reverse in
    make_circuits (List.tl edges) next_circuits next_reverse (iter + 1) max_iter

let mul_x (x1, _, _) (x2, _, _) =
  Float.mul x1 x2

let rec make_circuits_until_done edges circuits reverse last_edge =
  let edge = List.hd edges in
  if List.length (keys reverse) = 1 then
    match last_edge with
    | None -> failwith "no edge"
    | Some e -> mul_x e.a e.b
  else
    let (next_circuits, next_reverse) = add_edge edge circuits reverse in
    make_circuits_until_done (List.tl edges) next_circuits next_reverse (Some edge)

let part1 iters =
  let points = inputs in
  let edges =  make_edges [] points in
  let circuits = Hashtbl.create 1000 in
  let reverse = Hashtbl.create 1000 in
  List.iteri (fun i point -> 
    Hashtbl.add circuits point i;
    Hashtbl.add reverse i [point];
  ) points;
  make_circuits edges circuits reverse 0 iters


let part2 =
  let points = inputs in
  let edges =  make_edges [] points in
  let circuits = Hashtbl.create 1000 in
  let reverse = Hashtbl.create 1000 in
  List.iteri (fun i point -> 
    Hashtbl.add circuits point i;
    Hashtbl.add reverse i [point];
  ) points;
  make_circuits_until_done edges circuits reverse None

let () =
    Printf.printf "Part 1: %d \n" (part1 1000);
    Printf.printf "Part 2: %d" (int_of_float part2);