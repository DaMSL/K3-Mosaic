
open K3Helpers
open Tree
open K3

(* rewriting in the new K3 *)

(*
Global(ValueT("pT1",TCollection(TSet, RefT(TTuple([TFloat])))))
Global(ValueT("pR1",TCollection(TSet, RefT(TTuple([TFloat])))))
Global(ValueT("pS1",TCollection(TSet, RefT(TTuple([TFloat])))))
Global(ValueT("pR1pS1",TCollection(TSet, RefT(TTuple([TFloat)))))
Global(ValueT("pR1pT1",TCollection(TSet, RefT(TTuple([TFloat; TFloat])))))
Global(ValueT("Q1", TValueT(TRef(TFloat)))
*)

let = ();;
let mk_has_member col pat = mk_neg (mk_eq (mk_slice col pat) (mk_empty (BaseT(TFloat))));;

let t = Trigger("On_Insert_T", ATuple([("pR1_pS1T_C", BaseT(TFloat));("pR1_pS1T_D", BaseT(TFloat))]), [], 
mk_block  
	[(mk_ifthenelse 
		(mk_has_member 
			(mk_var "pT1") (mk_tuple [(mk_var "C")]) 
		)
		(mk_assigntoref 
			(mk_var "Q1") 
			(mk_add 
				(mk_var "Q1") 
				(mk_mult 
					(mk_slice
						(mk_var "pT1") 
						(mk_tuple [(mk_var "C")])
					)
					(mk_var "D")
				)
			)
		)
		(mk_block
			[mk_update 
				(mk_var "pT1") 
				(mk_slice 
					(mk_var "pT1") 
					(mk_tuple [(mk_var "C")])
				)
				(mk_const (CFloat(0.)))
			;
			mk_assigntoref (mk_var "Q1") 
				(mk_add 
					(mk_var "Q1") 
					(mk_mult 
						(mk_const (CFloat(0.)))
						(mk_var "D")
					)
				)
			]
		)
	)
    ;	
	mk_apply
		(mk_lambda
			(AVar("__cse1", TRef(TCollection(TList, BaseT(TTuple([TRef(TFloat);
                TRef(TFloat)]))))))
			(mk_iterate
				(mk_lambda
					(ATuple(["B", BaseT(TFloat); "dv", BaseT(TFloat)]))
					(mk_ifthenelse
						(mk_has_member 
							(mk_var "pR1") (mk_tuple [(mk_var "B")]) 
						)
						(mk_update
							(mk_var "pR1")
							(mk_slice 
								(mk_var "pR1") 
                (mk_tuple [mk_var "B"])
							)
							(mk_add
								(mk_slice 
									(mk_var "pR1") 
                  (mk_tuple ([mk_var "B"]))
							  )
								(mk_var "dv")
							)
						)
            (mk_update
              (mk_var "pR1")
              (mk_slice
                (mk_var "pR1")
                (mk_tuple ([mk_var "B"]))
              )
              (mk_var "dv")
            )
					)
				)
				(mk_var "__cse1")
			)
		)
		(mk_groupbyaggregate
			(mk_lambda
        (ATuple(["B",BaseT(TFloat);"__t1",BaseT(TFloat);
          "v2",BaseT(TFloat);"accv_3",BaseT(TFloat)])
        )
				(mk_add
					(mk_mult
						(mk_var "D")
						(mk_var "v2")
					)
					(mk_var "accv_3")
				)
			)
			(mk_lambda
				(ATuple(["QUERY_1_1R_R__B",BaseT(TFloat);"__t1",BaseT(TFloat);"v2",BaseT(TFloat)]))
                (mk_tuple [mk_var "B"])
			)
			(mk_const (CFloat(0.)))
			(mk_slice
				(mk_var "pT1")
				(mk_tuple [mk_var "_"; mk_var "C"])
			)
		)
	;
	mk_ifthenelse 
		(mk_has_member (mk_var "pR1pS1") (mk_tuple [mk_var "C"]))
		(mk_update 
			(mk_var "pR1pS1")
			(mk_slice (mk_var "pR1pS1") (mk_tuple [mk_var "C"]))
			(mk_add
				(mk_slice (mk_var "pR1pS1") (mk_tuple [mk_var "C"]))
				(mk_var "D")
			)
		)
		(mk_update
			(mk_var "pR1pS1")
			(mk_slice (mk_var "pR1pS1") (mk_tuple [mk_var "C"]))
			(mk_var "D")
		)
	]
)
