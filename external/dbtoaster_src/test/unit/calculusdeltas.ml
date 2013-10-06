open Schema
open Calculus
open Type
open UnitTest
;;
Debug.activate "PARSE-CALC-WITH-FLOAT-VARS"
;;
let test ?(opt_in=false) ?(opt_out=false) msg event input output = 
   let input_calc = parse_calc input in
   let map_schema = (event_vars event, snd (schema_of_expr input_calc)) in
   log_test ("Delta of Expression ( "^msg^" : "^(string_of_event event)^" )")
      CalculusPrinter.string_of_expr
      (( if opt_out 
         then CalculusTransforms.optimize_expr map_schema
         else (fun x -> x))
         (CalculusDeltas.delta_of_expr event (
            if opt_in 
            then CalculusTransforms.optimize_expr map_schema input_calc
            else input_calc
         ))
      )
      (parse_calc output)

;;

test "TPCH17 simple" (InsertEvent(schema_rel "P" ["dPK"]))
   "AggSum([], P(PK) * L(PK, QTY) * (nested ^= AggSum([PK], 
       L(PK, QTY2) * QTY2)))"
   "AggSum([], (PK ^= dPK) * L(PK, QTY) * (nested ^= AggSum([PK], 
       L(PK, QTY2) * QTY2)))"
;;

test "TPCH17 simple" (InsertEvent(schema_rel "L" ["dPK"; "dQTY"]))
   "AggSum([], P(PK) * L(PK, QTY) * (nested ^= AggSum([PK], 
       L(PK, QTY2) * QTY2)*0.5))"
   "AggSum([], P(PK) * (((PK ^= dPK) * (QTY ^= dQTY) * 
       (nested ^= AggSum([PK], L(PK, QTY2) * QTY2)*0.5)) + 
       ((L(PK, QTY) + (PK ^= dPK) * (QTY ^= dQTY)) * 
        (PK ^= dPK) * (
            (nested ^= (AggSum([PK], L(PK, QTY2) * QTY2)*0.5) + 
                (AggSum([], (QTY2 ^= dQTY)* QTY2) * 0.5)) - 
            (nested ^= AggSum([PK], L(PK, QTY2) * QTY2)*0.5)
   ))))"
;;

test "TPCH11" (InsertEvent(schema_rel "PARTSUPP" ["dPK"; "dSK"; "dAQ"; "dSC"]))
   "AggSum([P_NATIONKEY, P_PARTKEY], 
       ((N_VALUE ^=
           AggSum([P_NATIONKEY], 
              (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST) *
               SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, S_PHONE,
                        S_ACCTBAL, S_COMMENT) *
               (P_NATIONKEY ^= S_NATIONKEY) * PS_SUPPLYCOST * PS_AVAILQTY))) *
        (P_VALUE ^=
           AggSum([P_PARTKEY, P_NATIONKEY], 
              (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST) *
               (P_PARTKEY ^= PS_PARTKEY) *
               SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, S_PHONE,
                        S_ACCTBAL, S_COMMENT) *
               (P_NATIONKEY ^= S_NATIONKEY) * PS_SUPPLYCOST * PS_AVAILQTY))) *
        AggSum([], 
           ((__sql_inline_agg_1 ^= (0.001 * N_VALUE)) *
            {P_VALUE > __sql_inline_agg_1})) *
        P_VALUE))"
   "AggSum([P_NATIONKEY, P_PARTKEY], 
       ((( 
          ((N_VALUE ^=
               (AggSum([P_NATIONKEY], 
                   (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, 
                             PS_SUPPLYCOST) *
                    SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY,
                             S_PHONE, S_ACCTBAL, S_COMMENT) *
                   (P_NATIONKEY ^= S_NATIONKEY) * 
                   PS_SUPPLYCOST * PS_AVAILQTY)) +
                AggSum([P_NATIONKEY], 
                   (PS_PARTKEY ^= dPK) *  
                   (PS_SUPPKEY ^= dSK) *
                   (PS_AVAILQTY ^= dAQ) *
                   (PS_SUPPLYCOST ^= dSC) *
                   (SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY,
                             S_PHONE, S_ACCTBAL, S_COMMENT) *
                   (P_NATIONKEY ^= S_NATIONKEY) * 
                   PS_SUPPLYCOST * PS_AVAILQTY)))) +
           (-1 *
            (N_VALUE ^= AggSum([P_NATIONKEY], 
                (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST) *
                 SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY,
                          S_PHONE, S_ACCTBAL, S_COMMENT) *
                 (P_NATIONKEY ^= S_NATIONKEY) * PS_SUPPLYCOST *
                 PS_AVAILQTY)))))) *
         (P_VALUE ^=
             AggSum([P_PARTKEY, P_NATIONKEY], 
                (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST) *
                (P_PARTKEY ^= PS_PARTKEY) *
                SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, S_PHONE,
                         S_ACCTBAL, S_COMMENT) *
                (P_NATIONKEY ^= S_NATIONKEY) * 
                PS_SUPPLYCOST * PS_AVAILQTY))) *
         AggSum([], 
            ((__sql_inline_agg_1 ^= (0.001 * N_VALUE)) *
             {P_VALUE > __sql_inline_agg_1})) *
         P_VALUE) +
        (((N_VALUE ^= AggSum([P_NATIONKEY], 
              (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST) *
               SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, S_PHONE,
                        S_ACCTBAL, S_COMMENT) *
               (P_NATIONKEY ^= S_NATIONKEY) * PS_SUPPLYCOST * PS_AVAILQTY))) +
          (((N_VALUE ^=
                (AggSum([P_NATIONKEY], 
                   (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, 
                             PS_SUPPLYCOST) *
                    SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY,
                             S_PHONE, S_ACCTBAL, S_COMMENT) *
                    (P_NATIONKEY ^= S_NATIONKEY) * 
                    PS_SUPPLYCOST * PS_AVAILQTY)) +
                 AggSum([P_NATIONKEY], 
                    (PS_PARTKEY ^= dPK) * 
                    (PS_SUPPKEY ^= dSK) *
                    (PS_AVAILQTY ^= dAQ) * 
                    (PS_SUPPLYCOST ^= dSC) *
                    (SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, 
                              S_PHONE, S_ACCTBAL, S_COMMENT) *
                    (P_NATIONKEY ^= S_NATIONKEY) * 
                    PS_SUPPLYCOST * PS_AVAILQTY)))) +
            (-1 *
             (N_VALUE ^=
                 AggSum([P_NATIONKEY], 
                    (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, 
                              PS_SUPPLYCOST) *
                     SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY,
                              S_PHONE, S_ACCTBAL, S_COMMENT) *
                     (P_NATIONKEY ^= S_NATIONKEY) * PS_SUPPLYCOST *
                     PS_AVAILQTY))))))) *
          (((P_VALUE ^=
                (AggSum([P_PARTKEY, P_NATIONKEY], 
                    (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY,
                              PS_SUPPLYCOST) *
                     (P_PARTKEY ^= PS_PARTKEY) *
                     SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY,
                                S_PHONE, S_ACCTBAL, S_COMMENT) *
                     (P_NATIONKEY ^= S_NATIONKEY) * 
                     PS_SUPPLYCOST * PS_AVAILQTY)) +
                 AggSum([P_PARTKEY, P_NATIONKEY], 
                    (PS_SUPPLYCOST ^= dSC) *
                    (PS_AVAILQTY ^= dAQ) * 
                    (PS_SUPPKEY ^= dSK) *
                    (PS_PARTKEY ^= dPK) * 
                    (P_PARTKEY ^= PS_PARTKEY) *
                    (SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, 
                              S_PHONE, S_ACCTBAL, S_COMMENT) *
                    (P_NATIONKEY ^= S_NATIONKEY) * 
                    PS_SUPPLYCOST * PS_AVAILQTY)))) +
            (-1 *
             (P_VALUE ^=
                 AggSum([P_PARTKEY, P_NATIONKEY], 
                    (PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY,
                              PS_SUPPLYCOST) *
                    (P_PARTKEY ^= PS_PARTKEY) *
                    SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY,
                             S_PHONE, S_ACCTBAL, S_COMMENT) *
                    (P_NATIONKEY ^= S_NATIONKEY) * PS_SUPPLYCOST *
                    PS_AVAILQTY)))))) *
        AggSum([], 
           ((__sql_inline_agg_1 ^= (0.001 * N_VALUE)) *
            {P_VALUE > __sql_inline_agg_1})) *
            P_VALUE)))"
;;

test "Employee37 dEmployee" 
   (InsertEvent("LOCATION", [var "dLID"; "dRG", Type.TString], 
                Schema.StreamRel))
   "AggSum([COUNT_DID], 
      ((__sql_inline_agg_1 ^=
         AggSum([D_LOCATION_ID], 
            ((L_REGIONAL_GROUP:string ^= 'CHICAGO') *
             LOCATION(D_LOCATION_ID, L_REGIONAL_GROUP:string)))) *
      DEPARTMENT(COUNT_DID, D_NAME, D_LOCATION_ID) *
      {__sql_inline_agg_1 > 0}))"
   "AggSum([COUNT_DID, D_LOCATION_ID], 
      ((D_LOCATION_ID ^= dLID) *
       ((__sql_inline_agg_1 ^=
          (AggSum([D_LOCATION_ID], 
             ((L_REGIONAL_GROUP:string ^= 'CHICAGO') *
              LOCATION(D_LOCATION_ID, L_REGIONAL_GROUP:string))) + 
          (AggSum([],
             (L_REGIONAL_GROUP:string ^= dRG:string) * 
             (L_REGIONAL_GROUP:string ^= 'CHICAGO'))))) +
        (-1 *
         (__sql_inline_agg_1 ^=
            AggSum([D_LOCATION_ID], 
               ((L_REGIONAL_GROUP:string ^= 'CHICAGO') *
                LOCATION(D_LOCATION_ID, L_REGIONAL_GROUP:string)))))) *
       DEPARTMENT(COUNT_DID, D_NAME, D_LOCATION_ID) * 
       {__sql_inline_agg_1 > 0}))"
;;

test "SumADivB"
   (InsertEvent(schema_rel "R" ["dA"; "dB"]))
   "(AggSum([], (R(R_A, R_B) * R_A)) *
       AggSum([], 
          ((__sql_inline_agg_2 ^= (AggSum([], (R(R_A, R_B) * R_B)) + 1)) *
           {[/:float](__sql_inline_agg_2)})))"
   "(AggSum([], ((R_A ^= dA) * (R_B ^= dB) * R_A)) *
       (AggSum([], 
          ((__sql_inline_agg_2 ^= (AggSum([], (R(R_A, R_B) * R_B)) + 1)) *
           {[/:float](__sql_inline_agg_2)})))) +
    ((AggSum([], (R(R_A, R_B) * R_A)) + 
         AggSum([], ((R_A ^= dA) * (R_B ^= dB) * R_A))) *
     (AggSum([], 
         (
            (__sql_inline_agg_2 ^= (
               AggSum([], (R(R_A, R_B) * R_B)) + 1 + 
               AggSum([], (R_A ^= dA) * (R_B ^= dB) * R_B)))
          - (__sql_inline_agg_2 ^= (
               AggSum([], (R(R_A, R_B) * R_B)) + 1))
         ) *
         {[/:float](__sql_inline_agg_2)}
    )))"
;;
