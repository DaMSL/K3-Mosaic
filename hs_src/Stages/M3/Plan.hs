module Plan where
{-
   A plan is the third representation in the compiler pipeline 
   (SQL -> Calc -> Plan)
   
   Each plan consists of a set of compiled datastructures, which consist of 
   {ul
       A datastructure description, including a name and a definition of the
         the query which the datastructure is responsible for maintaining}
       A set of triggers which are used to maintain the datastructure.}
   }

   ++author Oliver Kennedy
-}

import Calculus

------------------- Datastructures -------------------
-- A datastructure description --
data Ds = Ds {
   ds_name       :: Expr, {- The name of the datastructure.  The content of
                               this expr_t must always be a single external leaf
                               and can always be simply (assuming its schema
                               matches) dropped into an existing expression in
                               order to access this datastructure.  
                               {b mk_ds_name} and {b expand_ds_name} are 
                               utility methods for interacting with ds_names -}
   
   ds_definition :: Expr  {- The definition of the datastructure.  This is the
                               query that the map will be maintaining the result
                               of -}
}

-- Construct a ds_name for a Ds
mk_ds_name = mk_ds_name_ivc Nothing

mk_ds_name_ivc ivc name schema t =
   Calculus.mk_external name (fst schema) (snd schema) t ivc

-- Extract the name, schema, type, and ivc code from a ds_name
expand_ds_name CalcRing.Val(External(e)) = e
expand_ds_name _ = error "Error:: invalid datastructure name"

-- Stringify a datastructure description.  The string conforms to the grammar 
--    of Calculusparser
string_of_ds ds =
   (string_of_expr $ ds_name ds)++" ::= "++(string_of_expr ds_definition ds)

------------------- Statements -------------------

-- A statement can either update (increment) the existing value of the key that
--  it is writing to, or replace the value of the key that it is writing to 
--  entirely
data StmtType = UpdateStmt | ReplaceStmt

-- A statement which alters the contents of a datastructure when executed
data Stmt = Stmt {
   target_map  :: Expr,     -- The datastructure to be modified
   update_type :: StmtType, -- The type of alteration to be performed
   update_expr :: Expr      -- The calculus expression defining the new 
                            --      value or update
}

-- Stringify a statement.  This string conforms to the grammar of Calculusparser
string_of_statement stmt =
   let expr_string = CalculusPrinter.string_of_expr $ stmt.update_expr in
   string_of_expr $ target_map stmt++
     if update_map stmt == UpdateStmt
     then " += "
     else " ::= "++
        (if '\n' `elem` expr_string then "\n  " else "")++
        expr_string
   

------------------- Compiled Datastructures -------------------
-- A compiled datastructure --
data CompiledDs = CompiledDs {
   description :: Ds,
   ds_triggers :: [Schema.Event, Stmt]
}

-- An incremental view maintenance plan (produced by Compiler)
type Plan = [CompiledDs]

