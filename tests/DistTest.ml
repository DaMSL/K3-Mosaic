open K3
open K3Util
open K3Helpers
open GenDist

let (rst_p:ProgInfo.prog_data_t) =
  (* trigger data *)
  ([1, "On_Insert_T", ["C", t_int; "D", t_int], [1;2;3]]
  , (* stmt data *)
  [1, 1, 1,  [], [5, ["C", 0]];
   2, 1, 2,  [], [4, ["C", 1]];
   3, 1, 3, ["C", 0], []]
  , (* map data *)
  [1, "Q", [];
   2, "QpR1", ["A", t_int];
   3, "QpR1pS1", ["A", t_int];
   4, "QpR1pT1", ["A", t_int; "B", t_int];
   5, "QpT1", ["A", t_int]
   ]
  )

let main() =
  let dist = gen_dist rst_p [] in
  print_endline(string_of_program dist ~print_id:true);
  K3Typechecker.deduce_program_type [] dist

let _ = Printexc.print main;;

main()


