
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

let a = ();;
let mk_has_member a col pat = mk_neg a (mk_eq a (mk_slice a col pat) (mk_empty a (BaseT(TFloat))));;

let t = Trigger("On_Insert_T", ATuple([("pR1_pS1T_C", BaseT(TFloat));("pR1_pS1T_D", BaseT(TFloat))]), [], 
mk_block a  
	[(mk_ifthenelse a 
		(mk_has_member a 
			(mk_var a "pT1") (mk_tuple a [(mk_var a "C")]) 
		)
		(mk_assigntoref a 
			(mk_var a "Q1") 
			(mk_add a 
				(mk_var a "Q1") 
				(mk_mult a 
					(mk_slice a
						(mk_var a "pT1") 
						(mk_tuple a [(mk_var a "C")])
					)
					(mk_var a "D")
				)
			)
		)
		(mk_block a
			[mk_update a 
				(mk_var a "pT1") 
				(mk_slice a 
					(mk_var a "pT1") 
					(mk_tuple a [(mk_var a "C")])
				)
				(mk_const a (CFloat(0.)))
			;
			mk_assigntoref a (mk_var a "Q1") 
				(mk_add a 
					(mk_var a "Q1") 
					(mk_mult a 
						(mk_const a (CFloat(0.)))
						(mk_var a "D")
					)
				)
			]
		)
	)
    ;	
	mk_apply a
		(mk_lambda a
			(AVar("__cse1", TRef(TCollection(TList, BaseT(TTuple([TRef(TFloat);
                TRef(TFloat)]))))))
			(mk_iterate a
				(mk_lambda a
					(ATuple(["B", BaseT(TFloat); "dv", BaseT(TFloat)]))
					(mk_ifthenelse a
						(mk_has_member a 
							(mk_var a "pR1") (mk_tuple a [(mk_var a "B")]) 
						)
						(mk_update a
							(mk_var a "pR1")
							(mk_slice a 
								(mk_var a "pR1") 
                                (mk_tuple a ([mk_var a "B"]))
							)
							(mk_add a
								(mk_slice a 
									(mk_var a "pR1") 
                                    (mk_tuple a ([mk_var a "B"]))
								)
								(mk_var a "dv")
							)
						)
                        (mk_update a
                            (mk_var a "pR1")
                            (mk_slice a
                                (mk_var a "pR1")
                                (mk_tuple a ([mk_var a "B"]))
                            )
                            (mk_var a "dv")
                        )
					)
				)
				(mk_var a "__cse1")
			)
		)
		(mk_groupbyaggregate a
			(mk_lambda a
                (ATuple(["B",BaseT(TFloat);"__t1",BaseT(TFloat);"v2",BaseT(TFloat);"accv_3",BaseT(TFloat)]))
				(mk_add a
					(mk_mult a
						(mk_var a "D")
						(mk_var a "v2")
					)
					(mk_var a "accv_3")
				)
			)
			(mk_lambda a
				(ATuple(["QUERY_1_1R_R__B",BaseT(TFloat);"__t1",BaseT(TFloat);"v2",BaseT(TFloat)]))
                (mk_tuple a [mk_var a "B"])
			)
			(mk_const a (CFloat(0.)))
			(mk_slice a
				(mk_var a "pT1")
				(mk_tuple a [mk_var a "_"; mk_var a "C"])
			)
		)
	;
	mk_ifthenelse a 
		(mk_has_member a (mk_var a "pR1pS1") (mk_tuple a [mk_var a "C"]))
		(mk_update a 
			(mk_var a "pR1pS1")
			(mk_slice a (mk_var a "pR1pS1") (mk_tuple a [mk_var a "C"]))
			(mk_add a
				(mk_slice a (mk_var a "pR1pS1") (mk_tuple a [mk_var a "C"]))
				(mk_var a "D")
			)
		)
		(mk_update a
			(mk_var a "pR1pS1")
			(mk_slice a (mk_var a "pR1pS1") (mk_tuple a [mk_var a "C"]))
			(mk_var a "D")
		)
	]
)
