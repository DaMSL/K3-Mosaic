
(*
 *
 *
 *)
open ShuffleHash
open Util

let fill_ring num =
  let range = create_range 1 num in
  let nodes = List.map (fun n -> "node_"^string_of_int n) range in
  List.iter (fun x -> Ring.add_node x) nodes

let test_route bmod num_nodes key =
  let results = route bmod num_nodes key in
  print_string "ips: ";
  List.iter (fun x -> print_string (x^" ")) results

let test_route1 () =
let m_key = [Some 200; Some 20; Some 35] in
let num_nodes = 32 in
let bmod = [0,8;1,4;2,10] in
test_route bmod num_nodes m_key

let test_route2 () =
  let m_key = [Some 5; None] in
  let num_nodes = 4 in
  let bmod = [0,8; 1,4] in
  test_route bmod num_nodes m_key

let test_shuffle1() =
  let n_key = [Some 5; None] in
  let num_nodes = 32 in
  let m_tuples = [[3; 12]; [5;20]; [100; 3]; [14; 12]] in
  let n_bmod = [0,8; 1,4] in
  let n_to_m_pat = [(0,1)] in
  shuffle_m_to_n n_bmod num_nodes n_to_m_pat n_key m_tuples false

let main() = fill_ring 32; (*test_route2 ()*) test_shuffle1();;

main()


