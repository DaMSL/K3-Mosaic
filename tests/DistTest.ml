open K3.AST
open K3
open K3Util
open K3Helpers
open GenDist
open K3Typechecker
open K3Printing

let (rst_p:ProgInfo.prog_data_t) =
  (* trigger data *)
  ([1, "On_Insert_T", ["C", t_int; "D", t_int], [1;2;3]]
  , (* stmt data *)
  [1, 1, 1,  [], [5, ["C", 0]];
   2, 1, 2,  [], [4, ["C", 1]];
   3, 1, 3, ["C", 0], []]
  , (* map data *)
  [1, "Q", [canonical TFloat];
    2, "QpR1", [t_int; canonical TFloat ];
    3, "QpR1pS1", [t_int; canonical TFloat];
    4, "QpR1pT1", [t_int; t_int; canonical TFloat];
    5, "QpT1", [t_int; canonical TFloat]
   ]
  )

let main() =
  let dist = gen_dist rst_p [] in
  print_endline(string_of_program dist ~print_id:true);
  try K3Typechecker.deduce_program_type dist; ()
  with TypeError(id, str) -> print_endline("\nType error:\n"^string_of_int id^",\n"^str)

let _ = Printexc.print main;;

main()


