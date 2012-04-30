open K3Helper

(* rewriting in the new K3 *)
on_insert_T = 
let rt = recompose_tree

Global(ValueT("pT1",TCollection(TSet, RefT(TTuple([TInt])))))
Global(ValueT("pR1",TCollection(TSet, RefT(TTuple([TInt])))))
Global(ValueT("pS1",TCollection(TSet, RefT(TTuple([TInt])))))
Global(ValueT("pR1pS1",TCollection(TSet, RefT(TTuple([TInt)))))
Global(ValueT("pR1pT1",TCollection(TSet, RefT(TTuple([TInt; TInt])))))
Global(ValueT("Q1", TValueT(TRef(TFloat)))

let mk_has_member col pat = mk_eq a (mk_slice a col pattern) (mk_const a CNothing)
let a = ()

let t = Trigger("On_Insert_T", ATuple([("pR1_pS1T_C", TFloat);("pR1_pS1T_D", TFloat)), [], 
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
					(mk_var "D")
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
				(mk_const a CFloat(0))
			;
			mk_assigntoref a (mk_var a "Q1") 
				(mk_add a 
					(mk_var a "Q1") 
					(mk_mult a 
						(mk_const a (CFloat(0)))
						(mk_var a "D")
					)
				)
			]
		)
	);
	mk_apply a
		(mk_lambda a
			Avar("__cse1", TRef(TCollection(TTuple([TFloat; TFloat]))))
			(mk_iterate a
				(mk_lambda a
					ATuple(["B", TFloat; "dv", Tfloat])
					(mk_ifthenelse a
						(mk_has_member a 
							(mk_var a "pR1") (mk_tuple a [(mk_var a "B")]) 
						)
						(mk_update a
							(mk_var a "pR1")
							(mk_slice a 
								(mk_var a "pR1") 
								(mk_tuple a (mk_var a "B"))
							)
							(mk_add a
								(mk_slice a 
									(mk_var a "pR1") 
									(mk_tuple a (mk_var a "B"))
								)
								(mk_var a "dv")
							)
						)
					)
				)
				(mk_var a "__cse1")
			)
		)
		(mk_groupbyaggregate a
			(mk_lambda a
				(ATuple(["B",TFloat;"__t1",TFloat;"v2",TFloat]),AVar("accv_3",TFloat))
				(mk_add a
					(mk_mult a
						(mk_var a "D")
						(mk_var a "v2")
					)
					(mk_var a "accv_3")
				)
			)
			(Const(CFloat(0.)))
			(mk_lambda a
				(ATuple(["QUERY_1_1R_R__B",TFloat;"__t1",TFloat;"v2",TFloat]))
				(mk_tuple a (mk_var a "B"))
			)
			(mk_slice a
				(mk_val a "pT1")
				(mk_tuple a [mk_var a "_"; mk_var a "C"])
			)
		)
	;
	mk_ifthenelse a 
		(mk_has_member a (mk_var a "pR1pS1") [mk_var a "C"])
		(mk_update a 
			(mk_var a "pR1pS1")
			(mk_slice a "pR1pS1" [mk_var a "C"])
			(mk_add a
				(mk_slice a "pR1pS1" [mk_var a "C"])
				(mk_var a "D")
			)
		)
		(mk_update a
			(mk_var a "pR1pS1")
			(mk_slice a "pR1pS1" [mk_var a "C"])
			(mk_var a "D")
		)
	]
