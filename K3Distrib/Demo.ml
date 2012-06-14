open K3
open GenDist

let (rst_p:ProgInfo.prog_data_t) =
  (* trigger data *)
  ([1, "On_Insert_T", ["C", BaseT(TInt); "D", BaseT(TInt)], [1;2;3]]
  , (* stmt data *)
  [1, 1, 1,  [], [5, ["C", 0]];
   2, 1, 2,  [], [4, ["C", 1]];
   3, 1, 3, ["C", 0], []]
  , (* map data *)
  [1, "Q", [];
   2, "QpR1", ["A", BaseT(TInt)];
   3, "QpR1pS1", ["A", BaseT(TInt)];
   4, "QpR1pT1", ["A", BaseT(TInt); "B", BaseT(TInt)];
   5, "QpT1", ["A", BaseT(TInt)]
   ]
  )

let main() =
  let dist = gen_dist rst_p 0 in
  let prog = List.map (fun x -> Declaration(x)) dist in
  print_endline(string_of_program prog)

let _ = Printexc.print main;;

main()


