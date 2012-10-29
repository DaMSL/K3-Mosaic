-------------------- SOURCES --------------------
CREATE STREAM SUPPLIER(SUPPLIER_SUPPKEY int, SUPPLIER_NAME string, SUPPLIER_ADDRESS string, SUPPLIER_NATIONKEY int, SUPPLIER_PHONE string, SUPPLIER_ACCTBAL float, SUPPLIER_COMMENT string)
  FROM FILE '../../experiments/data/tpch/standard/supplier.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM PARTSUPP(PARTSUPP_PARTKEY int, PARTSUPP_SUPPKEY int, PARTSUPP_AVAILQTY int, PARTSUPP_SUPPLYCOST float, PARTSUPP_COMMENT string)
  FROM FILE '../../experiments/data/tpch/standard/partsupp.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE TABLE NATION(NATION_NATIONKEY int, NATION_NAME string, NATION_REGIONKEY int, NATION_COMMENT string)
  FROM FILE '../../experiments/data/tpch/standard/nation.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP QUERY11[][P_PARTKEY:int] := 
(EXISTS(
   AggSum([P_PARTKEY:int], 
     ((N_NAME:string ^= 'GERMANY') *
       PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                  PS_SUPPLYCOST:float, PS_COMMENT:string) *
       (P_PARTKEY:int ^= PS_PARTKEY:int) *
       SUPPLIER(PS_SUPPKEY:int, S_NAME:string, S_ADDRESS:string,
                  S_NATIONKEY:int, S_PHONE:string, S_ACCTBAL:float,
                  S_COMMENT:string) *
       NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int,
                N_COMMENT:string) *
       PS_SUPPLYCOST:float * PS_AVAILQTY:int))) *
  AggSum([P_PARTKEY:int], 
    ((P_VALUE:float ^=
       AggSum([P_PARTKEY:int], 
         ((N_NAME:string ^= 'GERMANY') *
           PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                      PS_SUPPLYCOST:float, PS_COMMENT:string) *
           (P_PARTKEY:int ^= PS_PARTKEY:int) *
           SUPPLIER(PS_SUPPKEY:int, S_NAME:string, S_ADDRESS:string,
                      S_NATIONKEY:int, S_PHONE:string, S_ACCTBAL:float,
                      S_COMMENT:string) *
           NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int,
                    N_COMMENT:string) *
           PS_SUPPLYCOST:float * PS_AVAILQTY:int))) *
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           (AggSum([], 
              ((N_NAME:string ^= 'GERMANY') *
                PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                           PS_SUPPLYCOST:float, PS_COMMENT:string) *
                SUPPLIER(PS_SUPPKEY:int, S_NAME:string, S_ADDRESS:string,
                           S_NATIONKEY:int, S_PHONE:string, S_ACCTBAL:float,
                           S_COMMENT:string) *
                NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int,
                         N_COMMENT:string) *
                PS_SUPPLYCOST:float * PS_AVAILQTY:int)) *
             0.001)) *
          {P_VALUE:float > __sql_inline_agg_1:float})) *
      P_VALUE:float)));

DECLARE MAP QUERY11_mSUPPLIER1_E2_2(int)[][QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
AggSum([QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((N_NAME:string ^= 'GERMANY') *
    NATION(QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int, N_NAME:string,
             N_REGIONKEY:int, N_COMMENT:string)));

DECLARE MAP QUERY11_mSUPPLIER1_E2_3[]
[P_PARTKEY:int, QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] := 
AggSum([P_PARTKEY:int, QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int], 
  (PARTSUPP(PS_PARTKEY:int, QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int,
              PS_AVAILQTY:int, PS_SUPPLYCOST:float, PS_COMMENT:string) *
    (P_PARTKEY:int ^= PS_PARTKEY:int) * PS_SUPPLYCOST:float *
    PS_AVAILQTY:int));

DECLARE MAP QUERY11_mSUPPLIER7_L1_4[][QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] := 
AggSum([QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int], 
  (PARTSUPP(PS_PARTKEY:int, QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int,
              PS_AVAILQTY:int, PS_SUPPLYCOST:float, PS_COMMENT:string) *
    PS_SUPPLYCOST:float * PS_AVAILQTY:int));

DECLARE MAP QUERY11_mPARTSUPP1_L1_1[][] := 
AggSum([], 
  ((N_NAME:string ^= 'GERMANY') *
    PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
               PS_SUPPLYCOST:float, PS_COMMENT:string) *
    SUPPLIER(PS_SUPPKEY:int, S_NAME:string, S_ADDRESS:string,
               S_NATIONKEY:int, S_PHONE:string, S_ACCTBAL:float,
               S_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string) *
    PS_SUPPLYCOST:float * PS_AVAILQTY:int));

DECLARE MAP QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] := 
AggSum([P_PARTKEY:int], 
  ((N_NAME:string ^= 'GERMANY') *
    PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
               PS_SUPPLYCOST:float, PS_COMMENT:string) *
    (P_PARTKEY:int ^= PS_PARTKEY:int) *
    SUPPLIER(PS_SUPPKEY:int, S_NAME:string, S_ADDRESS:string,
               S_NATIONKEY:int, S_PHONE:string, S_ACCTBAL:float,
               S_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string) *
    PS_SUPPLYCOST:float * PS_AVAILQTY:int));

DECLARE MAP QUERY11_mPARTSUPP1_E2_3(int)[][QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] := 
AggSum([QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int], 
  ((N_NAME:string ^= 'GERMANY') *
    SUPPLIER(QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int, S_NAME:string,
               S_ADDRESS:string, S_NATIONKEY:int, S_PHONE:string,
               S_ACCTBAL:float, S_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string)));

-------------------- QUERIES --------------------
DECLARE QUERY QUERY11 := QUERY11(float)[][P_PARTKEY:int];

------------------- TRIGGERS --------------------
ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
      (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
        QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
           QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int]);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int];
}

ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
      (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
        QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 
      -1))) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
           QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
           -1))) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -1);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] * -1);
}

ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
      ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
        QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
        PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
         ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mSUPPLIER1_E2_3(float)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] += (PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int);
   QUERY11_mSUPPLIER7_L1_4(float)[][PARTSUPP_SUPPKEY:int] += (PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY:int] += 
  (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int);
}

ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
      ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
        QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
        PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
             -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -0.001 *
              PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
             -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
         ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mSUPPLIER1_E2_3(float)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] += (-1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int);
   QUERY11_mSUPPLIER7_L1_4(float)[][PARTSUPP_SUPPKEY:int] += (-1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY:int] += 
  (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int);
}

ON SYSTEM READY {
   QUERY11_mSUPPLIER1_E2_2(int)[][QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
  AggSum([QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((N_NAME:string ^= 'GERMANY') *
    NATION(QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int, N_NAME:string,
             N_REGIONKEY:int, N_COMMENT:string)));
}

CORRECT QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] += delta_QUERY11_mPARTSUPP1_E2_1:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][delta_P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| AggSum([delta_P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((EXISTS(
     (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
       (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
         QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]) +
       delta_QUERY11_mPARTSUPP1_E2_1:float)) +
    (EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
           QUERY11_mSUPPLIER1_E2_3[]
           [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
      -1)) *
   (AggSum([delta_P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
     EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
           QUERY11_mSUPPLIER1_E2_3[]
           [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]) +
         delta_QUERY11_mPARTSUPP1_E2_1:float)) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[]
            [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
       -1)) *
    (AggSum([], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         ((P_VALUE:float ^=
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
              delta_QUERY11_mPARTSUPP1_E2_1:float)) +
           ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]) +
               delta_QUERY11_mPARTSUPP1_E2_1:float)) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
             0.001)) *
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]) +
               delta_QUERY11_mPARTSUPP1_E2_1:float)) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[]
            [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]) +
          delta_QUERY11_mPARTSUPP1_E2_1:float)) +
       (EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER1_E2_3[]
              [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         -1)) *
      (AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([delta_P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) * -1)) *
       AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER1_E2_3[]
              [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]) +
            delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
          -1)) *
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 delta_QUERY11_mPARTSUPP1_E2_1:float)) +
              ((P_VALUE:float ^=
                 QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
                -1)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]) +
                  delta_QUERY11_mPARTSUPP1_E2_1:float)) +
               ((P_VALUE:float ^=
                  (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                      QUERY11_mSUPPLIER1_E2_3[]
                      [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
                 -1)) *
             {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) * -1)) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                delta_QUERY11_mPARTSUPP1_E2_1:float)) +
             ((P_VALUE:float ^=
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_NATIONKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_2:int)) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
       -1)) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
     ((SUPPLIER_NATIONKEY:int ^=
        delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_2:int)) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
           -1)))) *
    (AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_2:int)) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([P_PARTKEY:int], 
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             ((__sql_inline_agg_1:float ^=
                (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                    0.001)) *
                 -1))) +
            (((__sql_inline_agg_1:float ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] +
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                     QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                  0.001)) +
               ((SUPPLIER_NATIONKEY:int ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
                 ((__sql_inline_agg_1:float ^=
                    (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                        (QUERY11_mSUPPLIER1_E2_2(int)[]
                         [SUPPLIER_NATIONKEY:int] +
                          delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                       QUERY11_mPARTSUPP1_L1_1[][]) *
                      0.001)) +
                   ((__sql_inline_agg_1:float ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] +
                         (QUERY11_mSUPPLIER1_E2_2(int)[]
                          [SUPPLIER_NATIONKEY:int] *
                           QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                        0.001)) *
                     -1)))) *
              ((P_VALUE:float ^=
                 ((QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER1_E2_3[]
                       [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_NATIONKEY:int ^=
       delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_2:int)) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_2:int)) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)))) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
}

CORRECT QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int,delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER1_E2_3:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][delta_P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| AggSum([delta_P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
         (QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_3:float)) +
        QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[]
            [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
       -1)) *
   (AggSum([delta_P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
     ((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             (QUERY11_mSUPPLIER1_E2_3[]
              [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_3:float)) +
            QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
           -1)))) *
    (AggSum([], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_SUPPKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (QUERY11_mSUPPLIER1_E2_3[]
                [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_3:float)) +
              QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
             0.001)) *
          (SUPPLIER_SUPPKEY:int ^=
            delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
          ((P_VALUE:float ^=
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                (QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_3:float)) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            (QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_3:float)) +
           QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
          -1)) *
      (AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([delta_P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
        ((SUPPLIER_SUPPKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                (QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_3:float)) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)))) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_SUPPKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_3:float)) +
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_3:float);
}

CORRECT QUERY11_mPARTSUPP1_L1_1[][] += delta_QUERY11_mPARTSUPP1_L1_1:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
      (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
        QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
   (AggSum([P_PARTKEY:int], 
      ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              delta_QUERY11_mPARTSUPP1_L1_1:float) *
             0.001)) +
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            -1)) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((P_VALUE:float ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         ((__sql_inline_agg_1:float ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] +
               delta_QUERY11_mPARTSUPP1_L1_1:float) *
              0.001)) +
           ((__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((P_VALUE:float ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         ((__sql_inline_agg_1:float ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int]) +
               delta_QUERY11_mPARTSUPP1_L1_1:float) *
              0.001)) +
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                0.001)) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
           QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
      (AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 delta_QUERY11_mPARTSUPP1_L1_1:float) *
                0.001)) +
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 QUERY11_mSUPPLIER1_E2_3[]
                 [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
            ((__sql_inline_agg_1:float ^=
               ((QUERY11_mPARTSUPP1_L1_1[][] +
                  delta_QUERY11_mPARTSUPP1_L1_1:float) *
                 0.001)) +
              ((__sql_inline_agg_1:float ^=
                 (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
                -1)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
       AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 delta_QUERY11_mPARTSUPP1_L1_1:float) *
                0.001)) +
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
}

CORRECT QUERY11_mSUPPLIER7_L1_4[][delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER7_L1_4:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (EXISTS(
   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
  AggSum([P_PARTKEY:int], 
    ((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
      (P_VALUE:float ^=
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
      ((__sql_inline_agg_1:float ^=
         (((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             (QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] +
               delta_QUERY11_mSUPPLIER7_L1_4:float)) +
            QUERY11_mPARTSUPP1_L1_1[][]) *
           0.001)) +
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
             0.001)) *
          -1)) *
      {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_SUPPKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_QUERY11_mSUPPLIER7_L1_4:float);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_NATIONKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_2:int)) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
       -1)) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
     ((SUPPLIER_NATIONKEY:int ^=
        delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_2:int)) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
           -1)))) *
    (AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_2:int)) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([P_PARTKEY:int], 
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             ((__sql_inline_agg_1:float ^=
                (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                    0.001)) *
                 -1))) +
            (((__sql_inline_agg_1:float ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] +
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                     QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                  0.001)) +
               ((SUPPLIER_NATIONKEY:int ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
                 ((__sql_inline_agg_1:float ^=
                    (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                        (QUERY11_mSUPPLIER1_E2_2(int)[]
                         [SUPPLIER_NATIONKEY:int] +
                          delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                       QUERY11_mPARTSUPP1_L1_1[][]) *
                      0.001)) +
                   ((__sql_inline_agg_1:float ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] +
                         (QUERY11_mSUPPLIER1_E2_2(int)[]
                          [SUPPLIER_NATIONKEY:int] *
                           QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                        0.001)) *
                     -1)))) *
              ((P_VALUE:float ^=
                 ((QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER1_E2_3[]
                       [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_NATIONKEY:int ^=
       delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_2:int)) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_2:int)) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)))) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
}

CORRECT QUERY11_mSUPPLIER7_L1_4[][delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER7_L1_4:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (EXISTS(
   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
  AggSum([P_PARTKEY:int], 
    ((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
      (P_VALUE:float ^=
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
      ((__sql_inline_agg_1:float ^=
         (((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             (QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] +
               delta_QUERY11_mSUPPLIER7_L1_4:float)) +
            QUERY11_mPARTSUPP1_L1_1[][]) *
           0.001)) +
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
             0.001)) *
          -1)) *
      {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_SUPPKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_QUERY11_mSUPPLIER7_L1_4:float);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_NATIONKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_2:int)) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
       -1)) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
     ((SUPPLIER_NATIONKEY:int ^=
        delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_2:int)) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
           -1)))) *
    (AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_2:int)) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([P_PARTKEY:int], 
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             ((__sql_inline_agg_1:float ^=
                (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                    0.001)) *
                 -1))) +
            (((__sql_inline_agg_1:float ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] +
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                     QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                  0.001)) +
               ((SUPPLIER_NATIONKEY:int ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
                 ((__sql_inline_agg_1:float ^=
                    (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                        (QUERY11_mSUPPLIER1_E2_2(int)[]
                         [SUPPLIER_NATIONKEY:int] +
                          delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                       QUERY11_mPARTSUPP1_L1_1[][]) *
                      0.001)) +
                   ((__sql_inline_agg_1:float ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] +
                         (QUERY11_mSUPPLIER1_E2_2(int)[]
                          [SUPPLIER_NATIONKEY:int] *
                           QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                        0.001)) *
                     -1)))) *
              ((P_VALUE:float ^=
                 ((QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER1_E2_3[]
                       [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_NATIONKEY:int ^=
       delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_2:int)) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_2:int)) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)))) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
}

CORRECT QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int,delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER1_E2_3:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][delta_P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| AggSum([delta_P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
         (QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_3:float)) +
        QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[]
            [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
       -1)) *
   (AggSum([delta_P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
     ((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             (QUERY11_mSUPPLIER1_E2_3[]
              [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_3:float)) +
            QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
           -1)))) *
    (AggSum([], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_SUPPKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (QUERY11_mSUPPLIER1_E2_3[]
                [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_3:float)) +
              QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
             0.001)) *
          (SUPPLIER_SUPPKEY:int ^=
            delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
          ((P_VALUE:float ^=
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                (QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_3:float)) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            (QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_3:float)) +
           QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
          -1)) *
      (AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([delta_P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
        ((SUPPLIER_SUPPKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                (QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_3:float)) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)))) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_SUPPKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_3:float)) +
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_3:float);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_NATIONKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_2:int)) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
       -1)) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
     ((SUPPLIER_NATIONKEY:int ^=
        delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_2:int)) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
           -1)))) *
    (AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_2:int)) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([P_PARTKEY:int], 
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
             ((__sql_inline_agg_1:float ^=
                (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                    0.001)) *
                 -1))) +
            (((__sql_inline_agg_1:float ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] +
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                     QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                  0.001)) +
               ((SUPPLIER_NATIONKEY:int ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
                 ((__sql_inline_agg_1:float ^=
                    (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                        (QUERY11_mSUPPLIER1_E2_2(int)[]
                         [SUPPLIER_NATIONKEY:int] +
                          delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                       QUERY11_mPARTSUPP1_L1_1[][]) *
                      0.001)) +
                   ((__sql_inline_agg_1:float ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] +
                         (QUERY11_mSUPPLIER1_E2_2(int)[]
                          [SUPPLIER_NATIONKEY:int] *
                           QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int])) *
                        0.001)) *
                     -1)))) *
              ((P_VALUE:float ^=
                 ((QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER1_E2_3[]
                       [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_NATIONKEY:int ^=
       delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_2:int)) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) +
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_2:int)) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
              -1)))) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_2:int)) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int]))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
}

CORRECT QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] += delta_QUERY11_mPARTSUPP1_E2_1:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][delta_P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| AggSum([delta_P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((EXISTS(
     (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
       (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
         QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
         -1) +
       delta_QUERY11_mPARTSUPP1_E2_1:float)) +
    (EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
           QUERY11_mSUPPLIER1_E2_3[]
           [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
      -1)) *
   (AggSum([delta_P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) +
     EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
           QUERY11_mSUPPLIER1_E2_3[]
           [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1) +
         delta_QUERY11_mPARTSUPP1_E2_1:float)) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[]
            [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
       -1)) *
    (AggSum([], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         ((P_VALUE:float ^=
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
              delta_QUERY11_mPARTSUPP1_E2_1:float)) +
           ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1) +
               delta_QUERY11_mPARTSUPP1_E2_1:float)) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1) +
               delta_QUERY11_mPARTSUPP1_E2_1:float)) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[]
            [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1) +
          delta_QUERY11_mPARTSUPP1_E2_1:float)) +
       (EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER1_E2_3[]
              [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
         -1)) *
      (AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([delta_P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) * -1)) *
       AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER1_E2_3[]
              [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1) +
            delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
          -1)) *
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 delta_QUERY11_mPARTSUPP1_E2_1:float)) +
              ((P_VALUE:float ^=
                 QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
                -1)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1) +
                  delta_QUERY11_mPARTSUPP1_E2_1:float)) +
               ((P_VALUE:float ^=
                  (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                      QUERY11_mSUPPLIER1_E2_3[]
                      [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
                 -1)) *
             {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) * -1)) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                delta_QUERY11_mPARTSUPP1_E2_1:float)) +
             ((P_VALUE:float ^=
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_NATIONKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_2:int) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            -1))) *
       -1)) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
          -1))) +
     ((SUPPLIER_NATIONKEY:int ^=
        delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_2:int) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
           -1)))) *
    (AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_2:int) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([P_PARTKEY:int], 
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             ((__sql_inline_agg_1:float ^=
                ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                     delta_QUERY11_mSUPPLIER1_E2_2:int) *
                   -0.001) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                      QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                      -0.001))) *
                 -1))) +
            (((__sql_inline_agg_1:float ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * 
                  -0.001))) +
               ((SUPPLIER_NATIONKEY:int ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
                 ((__sql_inline_agg_1:float ^=
                    ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                       (QUERY11_mSUPPLIER1_E2_2(int)[]
                        [SUPPLIER_NATIONKEY:int] +
                         delta_QUERY11_mSUPPLIER1_E2_2:int) *
                       -0.001) +
                      (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                   ((__sql_inline_agg_1:float ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                        (QUERY11_mSUPPLIER1_E2_2(int)[]
                         [SUPPLIER_NATIONKEY:int] *
                          QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                          -0.001))) *
                     -1)))) *
              ((P_VALUE:float ^=
                 ((QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int) *
                    -1) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER1_E2_3[]
                       [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_NATIONKEY:int ^=
       delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_2:int) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             -1))) +
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_2:int) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)))) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_2:int) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_QUERY11_mSUPPLIER1_E2_2:int);
}

CORRECT QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int,delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER1_E2_3:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][delta_P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| AggSum([delta_P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
         (QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_3:float) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[]
            [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
       -1)) *
   (AggSum([delta_P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) +
     ((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             (QUERY11_mSUPPLIER1_E2_3[]
              [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_3:float) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
           -1)))) *
    (AggSum([], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_SUPPKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (QUERY11_mSUPPLIER1_E2_3[]
                [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_3:float) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
          (SUPPLIER_SUPPKEY:int ^=
            delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
          ((P_VALUE:float ^=
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                (QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_3:float) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            (QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_3:float) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
          -1)) *
      (AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([delta_P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) +
        ((SUPPLIER_SUPPKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                (QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_3:float) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)))) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_SUPPKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_3:float) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_3:float);
}

CORRECT QUERY11_mPARTSUPP1_L1_1[][] += delta_QUERY11_mPARTSUPP1_L1_1:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
      (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
        QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 
      -1))) *
   (AggSum([P_PARTKEY:int], 
      ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              delta_QUERY11_mPARTSUPP1_L1_1:float) *
             0.001)) +
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            -1)) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((P_VALUE:float ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
              -1))) *
         ((__sql_inline_agg_1:float ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] +
               delta_QUERY11_mPARTSUPP1_L1_1:float) *
              0.001)) +
           ((__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((P_VALUE:float ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
              -1))) *
         ((__sql_inline_agg_1:float ^=
            (((QUERY11_mPARTSUPP1_L1_1[][] +
                delta_QUERY11_mPARTSUPP1_L1_1:float) *
               0.001) +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) +
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
           QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
           -1))) *
      (AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 delta_QUERY11_mPARTSUPP1_L1_1:float) *
                0.001)) +
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 QUERY11_mSUPPLIER1_E2_3[]
                 [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
            ((__sql_inline_agg_1:float ^=
               ((QUERY11_mPARTSUPP1_L1_1[][] +
                  delta_QUERY11_mPARTSUPP1_L1_1:float) *
                 0.001)) +
              ((__sql_inline_agg_1:float ^=
                 (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
                -1)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
       AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 delta_QUERY11_mPARTSUPP1_L1_1:float) *
                0.001)) +
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
}

CORRECT QUERY11_mSUPPLIER7_L1_4[][delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER7_L1_4:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (EXISTS(
   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 
     -1))) *
  AggSum([P_PARTKEY:int], 
    ((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
      (P_VALUE:float ^=
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            -1))) *
      ((__sql_inline_agg_1:float ^=
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            (QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] +
              delta_QUERY11_mSUPPLIER7_L1_4:float) *
            -0.001) +
           (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
          -1)) *
      {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_SUPPKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER7_L1_4:float);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_NATIONKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_2:int) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            -1))) *
       -1)) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
          -1))) +
     ((SUPPLIER_NATIONKEY:int ^=
        delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_2:int) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
           -1)))) *
    (AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_2:int) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([P_PARTKEY:int], 
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             ((__sql_inline_agg_1:float ^=
                ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                     delta_QUERY11_mSUPPLIER1_E2_2:int) *
                   -0.001) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                      QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                      -0.001))) *
                 -1))) +
            (((__sql_inline_agg_1:float ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * 
                  -0.001))) +
               ((SUPPLIER_NATIONKEY:int ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
                 ((__sql_inline_agg_1:float ^=
                    ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                       (QUERY11_mSUPPLIER1_E2_2(int)[]
                        [SUPPLIER_NATIONKEY:int] +
                         delta_QUERY11_mSUPPLIER1_E2_2:int) *
                       -0.001) +
                      (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                   ((__sql_inline_agg_1:float ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                        (QUERY11_mSUPPLIER1_E2_2(int)[]
                         [SUPPLIER_NATIONKEY:int] *
                          QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                          -0.001))) *
                     -1)))) *
              ((P_VALUE:float ^=
                 ((QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int) *
                    -1) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER1_E2_3[]
                       [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_NATIONKEY:int ^=
       delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_2:int) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             -1))) +
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_2:int) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)))) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_2:int) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_QUERY11_mSUPPLIER1_E2_2:int);
}

CORRECT QUERY11_mSUPPLIER7_L1_4[][delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER7_L1_4:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (EXISTS(
   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 
     -1))) *
  AggSum([P_PARTKEY:int], 
    ((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
      (P_VALUE:float ^=
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            -1))) *
      ((__sql_inline_agg_1:float ^=
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            (QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] +
              delta_QUERY11_mSUPPLIER7_L1_4:float) *
            -0.001) +
           (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
          -1)) *
      {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_SUPPKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER7_L1_4:float);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_NATIONKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_2:int) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            -1))) *
       -1)) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
          -1))) +
     ((SUPPLIER_NATIONKEY:int ^=
        delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_2:int) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
           -1)))) *
    (AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_2:int) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([P_PARTKEY:int], 
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             ((__sql_inline_agg_1:float ^=
                ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                     delta_QUERY11_mSUPPLIER1_E2_2:int) *
                   -0.001) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                      QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                      -0.001))) *
                 -1))) +
            (((__sql_inline_agg_1:float ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * 
                  -0.001))) +
               ((SUPPLIER_NATIONKEY:int ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
                 ((__sql_inline_agg_1:float ^=
                    ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                       (QUERY11_mSUPPLIER1_E2_2(int)[]
                        [SUPPLIER_NATIONKEY:int] +
                         delta_QUERY11_mSUPPLIER1_E2_2:int) *
                       -0.001) +
                      (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                   ((__sql_inline_agg_1:float ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                        (QUERY11_mSUPPLIER1_E2_2(int)[]
                         [SUPPLIER_NATIONKEY:int] *
                          QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                          -0.001))) *
                     -1)))) *
              ((P_VALUE:float ^=
                 ((QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int) *
                    -1) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER1_E2_3[]
                       [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_NATIONKEY:int ^=
       delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_2:int) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             -1))) +
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_2:int) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)))) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_2:int) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_QUERY11_mSUPPLIER1_E2_2:int);
}

CORRECT QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int,delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER1_E2_3:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][delta_P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| AggSum([delta_P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
         (QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_3:float) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[]
            [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
       -1)) *
   (AggSum([delta_P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[]
          [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) +
     ((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             (QUERY11_mSUPPLIER1_E2_3[]
              [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_3:float) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
           -1)))) *
    (AggSum([], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_SUPPKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (QUERY11_mSUPPLIER1_E2_3[]
                [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_3:float) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
          (SUPPLIER_SUPPKEY:int ^=
            delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
          ((P_VALUE:float ^=
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                (QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_3:float) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_SUPPKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            (QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_3:float) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
          -1)) *
      (AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([delta_P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[]
             [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) +
        ((SUPPLIER_SUPPKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                (QUERY11_mSUPPLIER1_E2_3[]
                 [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_3:float) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)))) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_SUPPKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_3:float) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_3:float);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((SUPPLIER_NATIONKEY:int ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
           delta_QUERY11_mSUPPLIER1_E2_2:int) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            -1))) *
       -1)) *
   (AggSum([P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -0.001))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
          -1))) +
     ((SUPPLIER_NATIONKEY:int ^=
        delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
               delta_QUERY11_mSUPPLIER1_E2_2:int) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
           -1)))) *
    (AggSum([P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
         ((P_VALUE:float ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_QUERY11_mSUPPLIER1_E2_2:int) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([P_PARTKEY:int], 
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
             ((__sql_inline_agg_1:float ^=
                ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                     delta_QUERY11_mSUPPLIER1_E2_2:int) *
                   -0.001) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                      QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                      -0.001))) *
                 -1))) +
            (((__sql_inline_agg_1:float ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * 
                  -0.001))) +
               ((SUPPLIER_NATIONKEY:int ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
                 ((__sql_inline_agg_1:float ^=
                    ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                       (QUERY11_mSUPPLIER1_E2_2(int)[]
                        [SUPPLIER_NATIONKEY:int] +
                         delta_QUERY11_mSUPPLIER1_E2_2:int) *
                       -0.001) +
                      (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                   ((__sql_inline_agg_1:float ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                        (QUERY11_mSUPPLIER1_E2_2(int)[]
                         [SUPPLIER_NATIONKEY:int] *
                          QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] *
                          -0.001))) *
                     -1)))) *
              ((P_VALUE:float ^=
                 ((QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                      delta_QUERY11_mSUPPLIER1_E2_2:int) *
                    -1) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                       QUERY11_mSUPPLIER1_E2_3[]
                       [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((SUPPLIER_NATIONKEY:int ^=
       delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
              delta_QUERY11_mSUPPLIER1_E2_2:int) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
               -1))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
             -1))) +
        ((SUPPLIER_NATIONKEY:int ^=
           delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                  delta_QUERY11_mSUPPLIER1_E2_2:int) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
              -1)))) *
       AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY:int ^=
             delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int) *
           ((P_VALUE:float ^=
              ((QUERY11_mSUPPLIER1_E2_3[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] +
                   delta_QUERY11_mSUPPLIER1_E2_2:int) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY:int] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY:int, SUPPLIER_SUPPKEY:int] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2:int);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_QUERY11_mSUPPLIER1_E2_2:int);
}

CORRECT QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] += delta_QUERY11_mPARTSUPP1_E2_1:float FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][delta_P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| AggSum([delta_P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((EXISTS(
     (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
       ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
         QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
         PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
       delta_QUERY11_mPARTSUPP1_E2_1:float)) +
    (EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
         ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
      -1)) *
   (AggSum([delta_P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
            0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
        ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
          PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
     EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
         ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
         delta_QUERY11_mPARTSUPP1_E2_1:float)) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
            QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
            PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
       -1)) *
    (AggSum([], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         ((P_VALUE:float ^=
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
              delta_QUERY11_mPARTSUPP1_E2_1:float)) +
           ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
               ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
               delta_QUERY11_mPARTSUPP1_E2_1:float)) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                   PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
             0.001)) *
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
               ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
               delta_QUERY11_mPARTSUPP1_E2_1:float)) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                   PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
            QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
            PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
          delta_QUERY11_mPARTSUPP1_E2_1:float)) +
       (EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
              PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         -1)) *
      (AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([delta_P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) * -1)) *
       AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
             QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
             PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
              PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
            delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
          -1)) *
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 delta_QUERY11_mPARTSUPP1_E2_1:float)) +
              ((P_VALUE:float ^=
                 QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
                -1)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                  ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
                  delta_QUERY11_mPARTSUPP1_E2_1:float)) +
               ((P_VALUE:float ^=
                  (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                    ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                 -1)) *
             {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) * -1)) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                delta_QUERY11_mPARTSUPP1_E2_1:float)) +
             ((P_VALUE:float ^=
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
   (((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
      (EXISTS(
         (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
             delta_QUERY11_mPARTSUPP1_E2_3:int) *
            PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           (__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            (__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            (__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                   PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                0.001)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
         (EXISTS(
            (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                delta_QUERY11_mPARTSUPP1_E2_3:int) *
               PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             -1)) *
         (AggSum([P_PARTKEY:int], 
            ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
              (__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
           AggSum([P_PARTKEY:int], 
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               (__sql_inline_agg_1:float ^=
                 (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
           ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
             (PARTSUPP_SUPPKEY:int ^=
               delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
             (EXISTS(
                (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                    delta_QUERY11_mPARTSUPP1_E2_3:int) *
                   PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                    ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                 -1)))) *
          AggSum([], 
            ((PARTSUPP_SUPPKEY:int ^=
               delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
              (__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                  -1)) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
          PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       (PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
       (EXISTS(
          (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
              delta_QUERY11_mPARTSUPP1_E2_3:int) *
             PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
           -1)))) *
    (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       AggSum([], 
         ((PARTSUPP_SUPPKEY:int ^=
            delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
           (__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE:float ^=
              (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                  delta_QUERY11_mPARTSUPP1_E2_3:int) *
                 PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
      AggSum([P_PARTKEY:int], 
        ((PARTSUPP_SUPPKEY:int ^=
           delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             ((__sql_inline_agg_1:float ^=
                ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                    0.001)) *
                 -1))) +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              ((__sql_inline_agg_1:float ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                   0.001)) +
                ((PARTSUPP_SUPPKEY:int ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
                  ((__sql_inline_agg_1:float ^=
                     ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                          delta_QUERY11_mPARTSUPP1_E2_3:int) *
                         PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                        QUERY11_mPARTSUPP1_L1_1[][]) *
                       0.001)) +
                    ((__sql_inline_agg_1:float ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] +
                          (QUERY11_mPARTSUPP1_E2_3(int)[]
                           [PARTSUPP_SUPPKEY:int] *
                            PARTSUPP_SUPPLYCOST:float *
                            PARTSUPP_AVAILQTY:int)) *
                         0.001)) *
                      -1)))) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY:int] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
}

CORRECT QUERY11_mPARTSUPP1_L1_1[][] += delta_QUERY11_mPARTSUPP1_L1_1:float FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
      ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
        QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
        PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
   (AggSum([P_PARTKEY:int], 
      ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              delta_QUERY11_mPARTSUPP1_L1_1:float) *
             0.001)) +
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            -1)) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((P_VALUE:float ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
              PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         ((__sql_inline_agg_1:float ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] +
               delta_QUERY11_mPARTSUPP1_L1_1:float) *
              0.001)) +
           ((__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((P_VALUE:float ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
              PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         ((__sql_inline_agg_1:float ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] +
               (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
               delta_QUERY11_mPARTSUPP1_L1_1:float) *
              0.001)) +
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                   PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                0.001)) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
         ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
      (AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 delta_QUERY11_mPARTSUPP1_L1_1:float) *
                0.001)) +
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            ((__sql_inline_agg_1:float ^=
               ((QUERY11_mPARTSUPP1_L1_1[][] +
                  delta_QUERY11_mPARTSUPP1_L1_1:float) *
                 0.001)) +
              ((__sql_inline_agg_1:float ^=
                 (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
                -1)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
       AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 delta_QUERY11_mPARTSUPP1_L1_1:float) *
                0.001)) +
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
   (((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
      (EXISTS(
         (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
             delta_QUERY11_mPARTSUPP1_E2_3:int) *
            PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           (__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            (__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            (__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                   PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                0.001)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
         (EXISTS(
            (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                delta_QUERY11_mPARTSUPP1_E2_3:int) *
               PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             -1)) *
         (AggSum([P_PARTKEY:int], 
            ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
              (__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
           AggSum([P_PARTKEY:int], 
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               (__sql_inline_agg_1:float ^=
                 (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
           ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
             (PARTSUPP_SUPPKEY:int ^=
               delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
             (EXISTS(
                (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                    delta_QUERY11_mPARTSUPP1_E2_3:int) *
                   PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                    ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                 -1)))) *
          AggSum([], 
            ((PARTSUPP_SUPPKEY:int ^=
               delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
              (__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                  -1)) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
          PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       (PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
       (EXISTS(
          (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
              delta_QUERY11_mPARTSUPP1_E2_3:int) *
             PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
           -1)))) *
    (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       AggSum([], 
         ((PARTSUPP_SUPPKEY:int ^=
            delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
           (__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE:float ^=
              (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                  delta_QUERY11_mPARTSUPP1_E2_3:int) *
                 PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
      AggSum([P_PARTKEY:int], 
        ((PARTSUPP_SUPPKEY:int ^=
           delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             ((__sql_inline_agg_1:float ^=
                ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                    0.001)) *
                 -1))) +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              ((__sql_inline_agg_1:float ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                   0.001)) +
                ((PARTSUPP_SUPPKEY:int ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
                  ((__sql_inline_agg_1:float ^=
                     ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                          delta_QUERY11_mPARTSUPP1_E2_3:int) *
                         PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                        QUERY11_mPARTSUPP1_L1_1[][]) *
                       0.001)) +
                    ((__sql_inline_agg_1:float ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] +
                          (QUERY11_mPARTSUPP1_E2_3(int)[]
                           [PARTSUPP_SUPPKEY:int] *
                            PARTSUPP_SUPPLYCOST:float *
                            PARTSUPP_AVAILQTY:int)) *
                         0.001)) *
                      -1)))) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY:int] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
   (((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
      (EXISTS(
         (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
             delta_QUERY11_mPARTSUPP1_E2_3:int) *
            PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
               PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           (__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            (__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            (__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                   PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                0.001)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
         (EXISTS(
            (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                delta_QUERY11_mPARTSUPP1_E2_3:int) *
               PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             -1)) *
         (AggSum([P_PARTKEY:int], 
            ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
              (__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
           AggSum([P_PARTKEY:int], 
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               (__sql_inline_agg_1:float ^=
                 (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
           ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
             (PARTSUPP_SUPPKEY:int ^=
               delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
             (EXISTS(
                (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                    delta_QUERY11_mPARTSUPP1_E2_3:int) *
                   PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                    ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                 -1)))) *
          AggSum([], 
            ((PARTSUPP_SUPPKEY:int ^=
               delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
              (__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                  -1)) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
          PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       (PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
       (EXISTS(
          (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
              delta_QUERY11_mPARTSUPP1_E2_3:int) *
             PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
           -1)))) *
    (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       AggSum([], 
         ((PARTSUPP_SUPPKEY:int ^=
            delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
           (__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE:float ^=
              (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                  delta_QUERY11_mPARTSUPP1_E2_3:int) *
                 PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
      AggSum([P_PARTKEY:int], 
        ((PARTSUPP_SUPPKEY:int ^=
           delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                  PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             ((__sql_inline_agg_1:float ^=
                ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                    0.001)) *
                 -1))) +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              ((__sql_inline_agg_1:float ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int)) *
                   0.001)) +
                ((PARTSUPP_SUPPKEY:int ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
                  ((__sql_inline_agg_1:float ^=
                     ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                          delta_QUERY11_mPARTSUPP1_E2_3:int) *
                         PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                        QUERY11_mPARTSUPP1_L1_1[][]) *
                       0.001)) +
                    ((__sql_inline_agg_1:float ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] +
                          (QUERY11_mPARTSUPP1_E2_3(int)[]
                           [PARTSUPP_SUPPKEY:int] *
                            PARTSUPP_SUPPLYCOST:float *
                            PARTSUPP_AVAILQTY:int)) *
                         0.001)) *
                      -1)))) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY:int] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
}

CORRECT QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] += delta_QUERY11_mPARTSUPP1_E2_1:float FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][delta_P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| AggSum([delta_P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((EXISTS(
     (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
       ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
         QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
         PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
       delta_QUERY11_mPARTSUPP1_E2_1:float)) +
    (EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
         ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
      -1)) *
   (AggSum([delta_P_PARTKEY:int], 
      ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
             -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([delta_P_PARTKEY:int], 
       ((__sql_inline_agg_1:float ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -0.001 *
              PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         (P_VALUE:float ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
             -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
        ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
          PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
     EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
         ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
         delta_QUERY11_mPARTSUPP1_E2_1:float)) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
            QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
            PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
       -1)) *
    (AggSum([], 
       ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         ((P_VALUE:float ^=
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
              delta_QUERY11_mPARTSUPP1_E2_1:float)) +
           ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
               ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
               -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
               delta_QUERY11_mPARTSUPP1_E2_1:float)) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                   -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
             -0.001 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
               ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
               -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
               delta_QUERY11_mPARTSUPP1_E2_1:float)) +
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                   -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
              -1)) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  ((((EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
          ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
            QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
            PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
          delta_QUERY11_mPARTSUPP1_E2_1:float)) +
       (EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
            -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         -1)) *
      (AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([delta_P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) * -1)) *
       AggSum([delta_P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
           ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
             QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
             PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
            -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
            delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
             ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
             -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
          -1)) *
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            ((P_VALUE:float ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                 delta_QUERY11_mPARTSUPP1_E2_1:float)) +
              ((P_VALUE:float ^=
                 QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
                -1)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                  ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int) +
                  delta_QUERY11_mPARTSUPP1_E2_1:float)) +
               ((P_VALUE:float ^=
                  (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                    ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                 -1)) *
             {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
            delta_QUERY11_mPARTSUPP1_E2_1:float)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) * -1)) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] +
                delta_QUERY11_mPARTSUPP1_E2_1:float)) +
             ((P_VALUE:float ^=
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int]) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
   (((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
      (EXISTS(
         (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
             delta_QUERY11_mPARTSUPP1_E2_3:int) *
            -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
             -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
               (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 -0.001 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
         (EXISTS(
            (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                delta_QUERY11_mPARTSUPP1_E2_3:int) *
               -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             -1)) *
         (AggSum([P_PARTKEY:int], 
            ((__sql_inline_agg_1:float ^=
               (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
           AggSum([P_PARTKEY:int], 
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               (P_VALUE:float ^=
                 (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                   ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                     -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
              -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
           ((PARTSUPP_SUPPKEY:int ^=
              delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
             (P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
             (EXISTS(
                (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                    delta_QUERY11_mPARTSUPP1_E2_3:int) *
                   -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                    ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                 -1)))) *
          AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (PARTSUPP_SUPPKEY:int ^=
                delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       -1 * PARTSUPP_SUPPLYCOST:float *
                       PARTSUPP_AVAILQTY:int))) *
                  -1)) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
          PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
     ((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
       (P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       (EXISTS(
          (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
              delta_QUERY11_mPARTSUPP1_E2_3:int) *
             -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
              -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
           -1)))) *
    (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (PARTSUPP_SUPPKEY:int ^=
             delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
           ((P_VALUE:float ^=
              (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                  delta_QUERY11_mPARTSUPP1_E2_3:int) *
                 -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
      AggSum([P_PARTKEY:int], 
        ((PARTSUPP_SUPPKEY:int ^=
           delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             ((__sql_inline_agg_1:float ^=
                (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                    delta_QUERY11_mPARTSUPP1_E2_3:int) *
                   -0.001 * PARTSUPP_AVAILQTY:int *
                   PARTSUPP_SUPPLYCOST:float) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      -0.001 * PARTSUPP_SUPPLYCOST:float *
                      PARTSUPP_AVAILQTY:int))) *
                 -1))) +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              ((__sql_inline_agg_1:float ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                   (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                     -0.001 * PARTSUPP_SUPPLYCOST:float *
                     PARTSUPP_AVAILQTY:int))) +
                ((PARTSUPP_SUPPKEY:int ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
                  ((__sql_inline_agg_1:float ^=
                     (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                         delta_QUERY11_mPARTSUPP1_E2_3:int) *
                        -0.001 * PARTSUPP_AVAILQTY:int *
                        PARTSUPP_SUPPLYCOST:float) +
                       (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                    ((__sql_inline_agg_1:float ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                         (QUERY11_mPARTSUPP1_E2_3(int)[]
                          [PARTSUPP_SUPPKEY:int] * -0.001 *
                           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                      -1)))) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       -1 * PARTSUPP_SUPPLYCOST:float *
                       PARTSUPP_AVAILQTY:int))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY:int] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
}

CORRECT QUERY11_mPARTSUPP1_L1_1[][] += delta_QUERY11_mPARTSUPP1_L1_1:float FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
      ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
        QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
        PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
   (AggSum([P_PARTKEY:int], 
      ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
        ((__sql_inline_agg_1:float ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              delta_QUERY11_mPARTSUPP1_L1_1:float) *
             0.001)) +
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            -1)) *
        {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((P_VALUE:float ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
            -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         ((__sql_inline_agg_1:float ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] +
               delta_QUERY11_mPARTSUPP1_L1_1:float) *
              0.001)) +
           ((__sql_inline_agg_1:float ^=
              (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
     AggSum([P_PARTKEY:int], 
       ((P_VALUE:float ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
            -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
         ((__sql_inline_agg_1:float ^=
            (((QUERY11_mPARTSUPP1_L1_1[][] +
                delta_QUERY11_mPARTSUPP1_L1_1:float) *
               0.001) +
              (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                -0.001 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                  -0.001 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             -1)) *
         {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
         ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
      (AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 delta_QUERY11_mPARTSUPP1_L1_1:float) *
                0.001)) +
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((P_VALUE:float ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
               ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
               -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            ((__sql_inline_agg_1:float ^=
               ((QUERY11_mPARTSUPP1_L1_1[][] +
                  delta_QUERY11_mPARTSUPP1_L1_1:float) *
                 0.001)) +
              ((__sql_inline_agg_1:float ^=
                 (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
                -1)) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
       AggSum([P_PARTKEY:int], 
         ((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           ((__sql_inline_agg_1:float ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 delta_QUERY11_mPARTSUPP1_L1_1:float) *
                0.001)) +
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
    -1));
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
   (((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
      (EXISTS(
         (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
             delta_QUERY11_mPARTSUPP1_E2_3:int) *
            -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
             -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
               (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 -0.001 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
         (EXISTS(
            (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                delta_QUERY11_mPARTSUPP1_E2_3:int) *
               -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             -1)) *
         (AggSum([P_PARTKEY:int], 
            ((__sql_inline_agg_1:float ^=
               (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
           AggSum([P_PARTKEY:int], 
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               (P_VALUE:float ^=
                 (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                   ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                     -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
              -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
           ((PARTSUPP_SUPPKEY:int ^=
              delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
             (P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
             (EXISTS(
                (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                    delta_QUERY11_mPARTSUPP1_E2_3:int) *
                   -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                    ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                 -1)))) *
          AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (PARTSUPP_SUPPKEY:int ^=
                delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       -1 * PARTSUPP_SUPPLYCOST:float *
                       PARTSUPP_AVAILQTY:int))) *
                  -1)) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
          PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
     ((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
       (P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       (EXISTS(
          (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
              delta_QUERY11_mPARTSUPP1_E2_3:int) *
             -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
              -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
           -1)))) *
    (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (PARTSUPP_SUPPKEY:int ^=
             delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
           ((P_VALUE:float ^=
              (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                  delta_QUERY11_mPARTSUPP1_E2_3:int) *
                 -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
      AggSum([P_PARTKEY:int], 
        ((PARTSUPP_SUPPKEY:int ^=
           delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             ((__sql_inline_agg_1:float ^=
                (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                    delta_QUERY11_mPARTSUPP1_E2_3:int) *
                   -0.001 * PARTSUPP_AVAILQTY:int *
                   PARTSUPP_SUPPLYCOST:float) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      -0.001 * PARTSUPP_SUPPLYCOST:float *
                      PARTSUPP_AVAILQTY:int))) *
                 -1))) +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              ((__sql_inline_agg_1:float ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                   (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                     -0.001 * PARTSUPP_SUPPLYCOST:float *
                     PARTSUPP_AVAILQTY:int))) +
                ((PARTSUPP_SUPPKEY:int ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
                  ((__sql_inline_agg_1:float ^=
                     (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                         delta_QUERY11_mPARTSUPP1_E2_3:int) *
                        -0.001 * PARTSUPP_AVAILQTY:int *
                        PARTSUPP_SUPPLYCOST:float) +
                       (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                    ((__sql_inline_agg_1:float ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                         (QUERY11_mPARTSUPP1_E2_3(int)[]
                          [PARTSUPP_SUPPKEY:int] * -0.001 *
                           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                      -1)))) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       -1 * PARTSUPP_SUPPLYCOST:float *
                       PARTSUPP_AVAILQTY:int))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY:int] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   QUERY11(float)[][P_PARTKEY:int]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| AggSum([P_PARTKEY:int],(((P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY:int]) |><| (__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] |><| 0.001)) |><| {P_VALUE:float > __sql_inline_agg_1:float} |><| P_VALUE:float))))) += 
  (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
   (((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
      (EXISTS(
         (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
             delta_QUERY11_mPARTSUPP1_E2_3:int) *
            -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
             ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
             -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
          -1)) *
      (AggSum([P_PARTKEY:int], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
        AggSum([P_PARTKEY:int], 
          ((__sql_inline_agg_1:float ^=
             ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
               (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                 -0.001 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            (P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
            {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
     ((((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
         (EXISTS(
            (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                delta_QUERY11_mPARTSUPP1_E2_3:int) *
               -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             -1)) *
         (AggSum([P_PARTKEY:int], 
            ((__sql_inline_agg_1:float ^=
               (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (P_VALUE:float ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int]) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)) +
           AggSum([P_PARTKEY:int], 
             ((__sql_inline_agg_1:float ^=
                (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               (P_VALUE:float ^=
                 (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                   ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                     -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
              -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
           ((PARTSUPP_SUPPKEY:int ^=
              delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
             (P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
             (EXISTS(
                (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                    delta_QUERY11_mPARTSUPP1_E2_3:int) *
                   -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                    ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                 -1)))) *
          AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (PARTSUPP_SUPPKEY:int ^=
                delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       -1 * PARTSUPP_SUPPLYCOST:float *
                       PARTSUPP_AVAILQTY:int))) *
                  -1)) *
              {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
        ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * -1 *
          PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) +
     ((PARTSUPP_SUPPKEY:int ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
       (P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       (EXISTS(
          (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
              delta_QUERY11_mPARTSUPP1_E2_3:int) *
             -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
              ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
              -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
           -1)))) *
    (((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
       AggSum([], 
         ((__sql_inline_agg_1:float ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (PARTSUPP_SUPPKEY:int ^=
             delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
           ((P_VALUE:float ^=
              (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                  delta_QUERY11_mPARTSUPP1_E2_3:int) *
                 -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
             ((P_VALUE:float ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                  ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                    -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
               -1)) *
           {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float))) +
      AggSum([P_PARTKEY:int], 
        ((PARTSUPP_SUPPKEY:int ^=
           delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
          (((P_VALUE:float ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] * 
                -1 * PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
             ((__sql_inline_agg_1:float ^=
                (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                    delta_QUERY11_mPARTSUPP1_E2_3:int) *
                   -0.001 * PARTSUPP_AVAILQTY:int *
                   PARTSUPP_SUPPLYCOST:float) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1:float ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                      -0.001 * PARTSUPP_SUPPLYCOST:float *
                      PARTSUPP_AVAILQTY:int))) *
                 -1))) +
            ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
              ((__sql_inline_agg_1:float ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                   (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                     -0.001 * PARTSUPP_SUPPLYCOST:float *
                     PARTSUPP_AVAILQTY:int))) +
                ((PARTSUPP_SUPPKEY:int ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int) *
                  ((__sql_inline_agg_1:float ^=
                     (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] +
                         delta_QUERY11_mPARTSUPP1_E2_3:int) *
                        -0.001 * PARTSUPP_AVAILQTY:int *
                        PARTSUPP_SUPPLYCOST:float) +
                       (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                    ((__sql_inline_agg_1:float ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                         (QUERY11_mPARTSUPP1_E2_3(int)[]
                          [PARTSUPP_SUPPKEY:int] * -0.001 *
                           PARTSUPP_SUPPLYCOST:float * PARTSUPP_AVAILQTY:int))) *
                      -1)))) *
              ((P_VALUE:float ^=
                 (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int]) +
                     delta_QUERY11_mPARTSUPP1_E2_3:int) *
                    -1 * PARTSUPP_AVAILQTY:int * PARTSUPP_SUPPLYCOST:float) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int])) +
                ((P_VALUE:float ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] +
                     ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY:int] *
                       -1 * PARTSUPP_SUPPLYCOST:float *
                       PARTSUPP_AVAILQTY:int))) *
                  -1)))) *
          {P_VALUE:float > __sql_inline_agg_1:float} * P_VALUE:float)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY:int] += 
  ({PARTSUPP_SUPPKEY:int = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_QUERY11_mPARTSUPP1_E2_3:int * PARTSUPP_SUPPLYCOST:float *
  PARTSUPP_AVAILQTY:int);
}
