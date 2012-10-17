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
   AggSum([P_PARTKEY], 
     ((N_NAME ^= 'GERMANY') *
       PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST,
                  PS_COMMENT) *
       (P_PARTKEY ^= PS_PARTKEY) *
       SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, S_PHONE,
                  S_ACCTBAL, S_COMMENT) *
       NATION(S_NATIONKEY, N_NAME, N_REGIONKEY, N_COMMENT) * PS_SUPPLYCOST *
       PS_AVAILQTY))) *
  AggSum([P_PARTKEY], 
    ((P_VALUE ^=
       AggSum([P_PARTKEY], 
         ((N_NAME ^= 'GERMANY') *
           PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST,
                      PS_COMMENT) *
           (P_PARTKEY ^= PS_PARTKEY) *
           SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, S_PHONE,
                      S_ACCTBAL, S_COMMENT) *
           NATION(S_NATIONKEY, N_NAME, N_REGIONKEY, N_COMMENT) *
           PS_SUPPLYCOST * PS_AVAILQTY))) *
      AggSum([], 
        ((__sql_inline_agg_1 ^=
           (AggSum([], 
              ((N_NAME ^= 'GERMANY') *
                PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST,
                           PS_COMMENT) *
                SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, S_PHONE,
                           S_ACCTBAL, S_COMMENT) *
                NATION(S_NATIONKEY, N_NAME, N_REGIONKEY, N_COMMENT) *
                PS_SUPPLYCOST * PS_AVAILQTY)) *
             0.001)) *
          {P_VALUE > __sql_inline_agg_1})) *
      P_VALUE)));

DECLARE MAP QUERY11_mSUPPLIER1_E2_2(int)[][QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
AggSum([QUERY11_mSUPPLIERSUPPLIER_NATIONKEY], 
  ((N_NAME ^= 'GERMANY') *
    NATION(QUERY11_mSUPPLIERSUPPLIER_NATIONKEY, N_NAME, N_REGIONKEY,
             N_COMMENT)));

DECLARE MAP QUERY11_mSUPPLIER1_E2_3[]
[P_PARTKEY:int, QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] := 
AggSum([P_PARTKEY, QUERY11_mSUPPLIERSUPPLIER_SUPPKEY], 
  (PARTSUPP(PS_PARTKEY, QUERY11_mSUPPLIERSUPPLIER_SUPPKEY, PS_AVAILQTY,
              PS_SUPPLYCOST, PS_COMMENT) *
    (P_PARTKEY ^= PS_PARTKEY) * PS_SUPPLYCOST * PS_AVAILQTY));

DECLARE MAP QUERY11_mSUPPLIER7_L1_4[][QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] := 
AggSum([QUERY11_mSUPPLIERSUPPLIER_SUPPKEY], 
  (PARTSUPP(PS_PARTKEY, QUERY11_mSUPPLIERSUPPLIER_SUPPKEY, PS_AVAILQTY,
              PS_SUPPLYCOST, PS_COMMENT) *
    PS_SUPPLYCOST * PS_AVAILQTY));

DECLARE MAP QUERY11_mPARTSUPP1_L1_1[][] := 
AggSum([], 
  ((N_NAME ^= 'GERMANY') *
    PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST, PS_COMMENT) *
    SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, S_PHONE, S_ACCTBAL,
               S_COMMENT) *
    NATION(S_NATIONKEY, N_NAME, N_REGIONKEY, N_COMMENT) * PS_SUPPLYCOST *
    PS_AVAILQTY));

DECLARE MAP QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY:int] := 
AggSum([P_PARTKEY], 
  ((N_NAME ^= 'GERMANY') *
    PARTSUPP(PS_PARTKEY, PS_SUPPKEY, PS_AVAILQTY, PS_SUPPLYCOST, PS_COMMENT) *
    (P_PARTKEY ^= PS_PARTKEY) *
    SUPPLIER(PS_SUPPKEY, S_NAME, S_ADDRESS, S_NATIONKEY, S_PHONE, S_ACCTBAL,
               S_COMMENT) *
    NATION(S_NATIONKEY, N_NAME, N_REGIONKEY, N_COMMENT) * PS_SUPPLYCOST *
    PS_AVAILQTY));

DECLARE MAP QUERY11_mPARTSUPP1_E2_3(int)[][QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] := 
AggSum([QUERY11_mPARTSUPPPARTSUPP_SUPPKEY], 
  ((N_NAME ^= 'GERMANY') *
    SUPPLIER(QUERY11_mPARTSUPPPARTSUPP_SUPPKEY, S_NAME, S_ADDRESS,
               S_NATIONKEY, S_PHONE, S_ACCTBAL, S_COMMENT) *
    NATION(S_NATIONKEY, N_NAME, N_REGIONKEY, N_COMMENT)));

-------------------- QUERIES --------------------
DECLARE QUERY QUERY11 := QUERY11(float)[][P_PARTKEY];

------------------- TRIGGERS --------------------
ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
      (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
        QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
           QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY]);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY];
}

ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
      (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
        QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
           QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -1);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] * -1);
}

ON + PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
      ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
        QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
        PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
         ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
           PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mSUPPLIER1_E2_3(float)[][PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY] += (PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mSUPPLIER7_L1_4(float)[][PARTSUPP_SUPPKEY] += (PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * PARTSUPP_SUPPLYCOST *
  PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY] += 
  (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * PARTSUPP_SUPPLYCOST *
  PARTSUPP_AVAILQTY);
}

ON - PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
      ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
        QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
        PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -0.001 *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
         ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
           PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mSUPPLIER1_E2_3(float)[][PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY] += (-1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mSUPPLIER7_L1_4(float)[][PARTSUPP_SUPPKEY] += (-1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY] += 
  (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
}

ON SYSTEM READY {
   QUERY11_mSUPPLIER1_E2_2(int)[][QUERY11_mSUPPLIERSUPPLIER_NATIONKEY] := 
  AggSum([QUERY11_mSUPPLIERSUPPLIER_NATIONKEY], 
  ((N_NAME ^= 'GERMANY') *
    NATION(QUERY11_mSUPPLIERSUPPLIER_NATIONKEY, N_NAME, N_REGIONKEY,
             N_COMMENT)));
}

CORRECT QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] += delta_QUERY11_mPARTSUPP1_E2_1:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][delta_P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * AggSum([delta_P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((EXISTS(
     (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
       (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
         QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]) +
       delta_QUERY11_mPARTSUPP1_E2_1)) +
    (EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
           QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
      -1)) *
   (AggSum([delta_P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) +
     EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
           QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]) +
         delta_QUERY11_mPARTSUPP1_E2_1)) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
       -1)) *
    (AggSum([], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         ((P_VALUE ^=
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
              delta_QUERY11_mPARTSUPP1_E2_1)) +
           ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]) +
               delta_QUERY11_mPARTSUPP1_E2_1)) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
             0.001)) *
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]) +
               delta_QUERY11_mPARTSUPP1_E2_1)) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]) +
          delta_QUERY11_mPARTSUPP1_E2_1)) +
       (EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         -1)) *
      (AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([delta_P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
       AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]) +
            delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
          -1)) *
       (AggSum([], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 delta_QUERY11_mPARTSUPP1_E2_1)) +
              ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
         AggSum([], 
           ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY, SUPPLIER_SUPPKEY]) +
                  delta_QUERY11_mPARTSUPP1_E2_1)) +
               ((P_VALUE ^=
                  (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                      QUERY11_mSUPPLIER1_E2_3[]
                      [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
                 -1)) *
             {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                delta_QUERY11_mPARTSUPP1_E2_1)) +
             ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
           delta_QUERY11_mSUPPLIER1_E2_2)) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
       -1)) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) +
     ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
               delta_QUERY11_mSUPPLIER1_E2_2)) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
           -1)))) *
    (AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_2)) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([P_PARTKEY], 
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             ((__sql_inline_agg_1 ^=
                (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2)) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                    0.001)) *
                 -1))) +
            (((__sql_inline_agg_1 ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] +
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                     QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                  0.001)) +
               ((SUPPLIER_NATIONKEY ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
                 ((__sql_inline_agg_1 ^=
                    (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                          delta_QUERY11_mSUPPLIER1_E2_2)) +
                       QUERY11_mPARTSUPP1_L1_1[][]) *
                      0.001)) +
                   ((__sql_inline_agg_1 ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] +
                         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                           QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                        0.001)) *
                     -1)))) *
              ((P_VALUE ^=
                 ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2)) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
              delta_QUERY11_mSUPPLIER1_E2_2)) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) +
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_2)) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)))) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_2)) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  delta_QUERY11_mSUPPLIER1_E2_2);
}

CORRECT QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int,delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER1_E2_3:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][delta_P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * AggSum([delta_P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
         (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
           delta_QUERY11_mSUPPLIER1_E2_3)) +
        QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
       -1)) *
   (AggSum([delta_P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) +
     ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
               delta_QUERY11_mSUPPLIER1_E2_3)) +
            QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
           -1)))) *
    (AggSum([], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_3)) +
              QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
             0.001)) *
          (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
          ((P_VALUE ^=
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_3)) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
              delta_QUERY11_mSUPPLIER1_E2_3)) +
           QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
          -1)) *
      (AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([delta_P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) +
        ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_3)) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)))) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 (QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_3)) +
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY] += 
  ({SUPPLIER_SUPPKEY = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
  delta_QUERY11_mSUPPLIER1_E2_3);
}

CORRECT QUERY11_mPARTSUPP1_L1_1[][] += delta_QUERY11_mPARTSUPP1_L1_1:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
      (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
        QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
   (AggSum([P_PARTKEY], 
      ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
             0.001)) +
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            -1)) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((P_VALUE ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         ((__sql_inline_agg_1 ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
              0.001)) +
           ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((P_VALUE ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         ((__sql_inline_agg_1 ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY]) +
               delta_QUERY11_mPARTSUPP1_L1_1) *
              0.001)) +
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                0.001)) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
           QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
      (AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                0.001)) +
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
            ((__sql_inline_agg_1 ^=
               ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                 0.001)) +
              ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
                -1)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
       AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                0.001)) +
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
}

CORRECT QUERY11_mSUPPLIER7_L1_4[][delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER7_L1_4:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (EXISTS(
   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
  AggSum([P_PARTKEY], 
    ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
      (P_VALUE ^=
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
      ((__sql_inline_agg_1 ^=
         (((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             (QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] +
               delta_QUERY11_mSUPPLIER7_L1_4)) +
            QUERY11_mPARTSUPP1_L1_1[][]) *
           0.001)) +
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
             0.001)) *
          -1)) *
      {P_VALUE > __sql_inline_agg_1} * P_VALUE)));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_SUPPKEY = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
  delta_QUERY11_mSUPPLIER7_L1_4);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
           delta_QUERY11_mSUPPLIER1_E2_2)) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
       -1)) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) +
     ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
               delta_QUERY11_mSUPPLIER1_E2_2)) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
           -1)))) *
    (AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_2)) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([P_PARTKEY], 
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             ((__sql_inline_agg_1 ^=
                (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2)) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                    0.001)) *
                 -1))) +
            (((__sql_inline_agg_1 ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] +
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                     QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                  0.001)) +
               ((SUPPLIER_NATIONKEY ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
                 ((__sql_inline_agg_1 ^=
                    (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                          delta_QUERY11_mSUPPLIER1_E2_2)) +
                       QUERY11_mPARTSUPP1_L1_1[][]) *
                      0.001)) +
                   ((__sql_inline_agg_1 ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] +
                         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                           QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                        0.001)) *
                     -1)))) *
              ((P_VALUE ^=
                 ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2)) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
              delta_QUERY11_mSUPPLIER1_E2_2)) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) +
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_2)) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)))) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_2)) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  delta_QUERY11_mSUPPLIER1_E2_2);
}

CORRECT QUERY11_mSUPPLIER7_L1_4[][delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER7_L1_4:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (EXISTS(
   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
  AggSum([P_PARTKEY], 
    ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
      (P_VALUE ^=
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
      ((__sql_inline_agg_1 ^=
         (((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             (QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] +
               delta_QUERY11_mSUPPLIER7_L1_4)) +
            QUERY11_mPARTSUPP1_L1_1[][]) *
           0.001)) +
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
             0.001)) *
          -1)) *
      {P_VALUE > __sql_inline_agg_1} * P_VALUE)));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_SUPPKEY = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
  delta_QUERY11_mSUPPLIER7_L1_4);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
           delta_QUERY11_mSUPPLIER1_E2_2)) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
       -1)) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) +
     ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
               delta_QUERY11_mSUPPLIER1_E2_2)) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
           -1)))) *
    (AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_2)) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([P_PARTKEY], 
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             ((__sql_inline_agg_1 ^=
                (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2)) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                    0.001)) *
                 -1))) +
            (((__sql_inline_agg_1 ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] +
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                     QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                  0.001)) +
               ((SUPPLIER_NATIONKEY ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
                 ((__sql_inline_agg_1 ^=
                    (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                          delta_QUERY11_mSUPPLIER1_E2_2)) +
                       QUERY11_mPARTSUPP1_L1_1[][]) *
                      0.001)) +
                   ((__sql_inline_agg_1 ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] +
                         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                           QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                        0.001)) *
                     -1)))) *
              ((P_VALUE ^=
                 ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2)) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
              delta_QUERY11_mSUPPLIER1_E2_2)) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) +
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_2)) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)))) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_2)) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  delta_QUERY11_mSUPPLIER1_E2_2);
}

CORRECT QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int,delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER1_E2_3:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][delta_P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * AggSum([delta_P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
         (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
           delta_QUERY11_mSUPPLIER1_E2_3)) +
        QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
       -1)) *
   (AggSum([delta_P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) +
     ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
               delta_QUERY11_mSUPPLIER1_E2_3)) +
            QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
           -1)))) *
    (AggSum([], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_3)) +
              QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
             0.001)) *
          (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
          ((P_VALUE ^=
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_3)) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
              delta_QUERY11_mSUPPLIER1_E2_3)) +
           QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
          -1)) *
      (AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([delta_P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) +
        ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_3)) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)))) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 (QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_3)) +
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY, SUPPLIER_SUPPKEY]))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY] += 
  ({SUPPLIER_SUPPKEY = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
  delta_QUERY11_mSUPPLIER1_E2_3);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
           delta_QUERY11_mSUPPLIER1_E2_2)) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
       -1)) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) +
     ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
               delta_QUERY11_mSUPPLIER1_E2_2)) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
           -1)))) *
    (AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_2)) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([P_PARTKEY], 
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
             ((__sql_inline_agg_1 ^=
                (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2)) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                    0.001)) *
                 -1))) +
            (((__sql_inline_agg_1 ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] +
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                     QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                  0.001)) +
               ((SUPPLIER_NATIONKEY ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
                 ((__sql_inline_agg_1 ^=
                    (((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                          delta_QUERY11_mSUPPLIER1_E2_2)) +
                       QUERY11_mPARTSUPP1_L1_1[][]) *
                      0.001)) +
                   ((__sql_inline_agg_1 ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] +
                         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                           QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY])) *
                        0.001)) *
                     -1)))) *
              ((P_VALUE ^=
                 ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2)) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
              delta_QUERY11_mSUPPLIER1_E2_2)) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) +
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_2)) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
              -1)))) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_2)) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY]))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  delta_QUERY11_mSUPPLIER1_E2_2);
}

CORRECT QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] += delta_QUERY11_mPARTSUPP1_E2_1:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][delta_P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * AggSum([delta_P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((EXISTS(
     (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
       (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
         QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] * 
       -1) + delta_QUERY11_mPARTSUPP1_E2_1)) +
    (EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
           QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] * 
         -1))) *
      -1)) *
   (AggSum([delta_P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
               -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
               -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] * 
        -1))) +
     EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
           QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] * 
         -1) + delta_QUERY11_mPARTSUPP1_E2_1)) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
            -1))) *
       -1)) *
    (AggSum([], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         ((P_VALUE ^=
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
              delta_QUERY11_mPARTSUPP1_E2_1)) +
           ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
                 -1) +
               delta_QUERY11_mPARTSUPP1_E2_1)) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
                 -1) +
               delta_QUERY11_mPARTSUPP1_E2_1)) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
            -1) +
          delta_QUERY11_mPARTSUPP1_E2_1)) +
       (EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
              -1))) *
         -1)) *
      (AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([delta_P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
       AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
             -1))) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
              -1) +
            delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
               -1))) *
          -1)) *
       (AggSum([], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 delta_QUERY11_mPARTSUPP1_E2_1)) +
              ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
         AggSum([], 
           ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1) +
                  delta_QUERY11_mPARTSUPP1_E2_1)) +
               ((P_VALUE ^=
                  (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                      QUERY11_mSUPPLIER1_E2_3[]
                      [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
                 -1)) *
             {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                delta_QUERY11_mPARTSUPP1_E2_1)) +
             ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
           delta_QUERY11_mSUPPLIER1_E2_2) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
          -1))) *
       -1)) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) +
     ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
               delta_QUERY11_mSUPPLIER1_E2_2) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
              -1))) *
           -1)))) *
    (AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_2) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([P_PARTKEY], 
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
             ((__sql_inline_agg_1 ^=
                ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                     delta_QUERY11_mSUPPLIER1_E2_2) *
                   -0.001) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                      QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
                 -1))) +
            (((__sql_inline_agg_1 ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) +
               ((SUPPLIER_NATIONKEY ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
                 ((__sql_inline_agg_1 ^=
                    ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                       (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                         delta_QUERY11_mSUPPLIER1_E2_2) *
                       -0.001) +
                      (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                   ((__sql_inline_agg_1 ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                          QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                          -0.001))) *
                     -1)))) *
              ((P_VALUE ^=
                 ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2) *
                    -1) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                       -1))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
              delta_QUERY11_mSUPPLIER1_E2_2) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
           -1))) +
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_2) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                   -1))) *
              -1)))) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_2) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    -1))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
}

CORRECT QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int,delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER1_E2_3:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][delta_P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * AggSum([delta_P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
         (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
           delta_QUERY11_mSUPPLIER1_E2_3) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
            -1))) *
       -1)) *
   (AggSum([delta_P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
               -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
               -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] * 
        -1))) +
     ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
               delta_QUERY11_mSUPPLIER1_E2_3) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
                -1))) *
           -1)))) *
    (AggSum([], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_3) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
          (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
          ((P_VALUE ^=
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_3) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
              delta_QUERY11_mSUPPLIER1_E2_3) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
               -1))) *
          -1)) *
      (AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([delta_P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
             -1))) +
        ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_3) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
              -1)))) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 (QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_3) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY] += 
  ({SUPPLIER_SUPPKEY = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_3);
}

CORRECT QUERY11_mPARTSUPP1_L1_1[][] += delta_QUERY11_mPARTSUPP1_L1_1:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
      (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
        QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
   (AggSum([P_PARTKEY], 
      ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
             0.001)) +
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            -1)) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((P_VALUE ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
            -1))) *
         ((__sql_inline_agg_1 ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
              0.001)) +
           ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((P_VALUE ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
            -1))) *
         ((__sql_inline_agg_1 ^=
            (((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
               0.001) +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) +
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
           QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
      (AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                0.001)) +
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
               -1))) *
            ((__sql_inline_agg_1 ^=
               ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                 0.001)) +
              ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
                -1)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
       AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                0.001)) +
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
}

CORRECT QUERY11_mSUPPLIER7_L1_4[][delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER7_L1_4:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (EXISTS(
   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
  AggSum([P_PARTKEY], 
    ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
      (P_VALUE ^=
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
          -1))) *
      ((__sql_inline_agg_1 ^=
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            (QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] +
              delta_QUERY11_mSUPPLIER7_L1_4) *
            -0.001) +
           (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
          -1)) *
      {P_VALUE > __sql_inline_agg_1} * P_VALUE)));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_SUPPKEY = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] * -1 *
  delta_QUERY11_mSUPPLIER7_L1_4);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
           delta_QUERY11_mSUPPLIER1_E2_2) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
          -1))) *
       -1)) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) +
     ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
               delta_QUERY11_mSUPPLIER1_E2_2) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
              -1))) *
           -1)))) *
    (AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_2) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([P_PARTKEY], 
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
             ((__sql_inline_agg_1 ^=
                ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                     delta_QUERY11_mSUPPLIER1_E2_2) *
                   -0.001) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                      QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
                 -1))) +
            (((__sql_inline_agg_1 ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) +
               ((SUPPLIER_NATIONKEY ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
                 ((__sql_inline_agg_1 ^=
                    ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                       (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                         delta_QUERY11_mSUPPLIER1_E2_2) *
                       -0.001) +
                      (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                   ((__sql_inline_agg_1 ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                          QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                          -0.001))) *
                     -1)))) *
              ((P_VALUE ^=
                 ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2) *
                    -1) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                       -1))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
              delta_QUERY11_mSUPPLIER1_E2_2) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
           -1))) +
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_2) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                   -1))) *
              -1)))) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_2) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    -1))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
}

CORRECT QUERY11_mSUPPLIER7_L1_4[][delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER7_L1_4:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (EXISTS(
   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
  AggSum([P_PARTKEY], 
    ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
      (P_VALUE ^=
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
          -1))) *
      ((__sql_inline_agg_1 ^=
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            (QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] +
              delta_QUERY11_mSUPPLIER7_L1_4) *
            -0.001) +
           (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
          -1)) *
      {P_VALUE > __sql_inline_agg_1} * P_VALUE)));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_SUPPKEY = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] * -1 *
  delta_QUERY11_mSUPPLIER7_L1_4);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
           delta_QUERY11_mSUPPLIER1_E2_2) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
          -1))) *
       -1)) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) +
     ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
               delta_QUERY11_mSUPPLIER1_E2_2) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
              -1))) *
           -1)))) *
    (AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_2) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([P_PARTKEY], 
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
             ((__sql_inline_agg_1 ^=
                ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                     delta_QUERY11_mSUPPLIER1_E2_2) *
                   -0.001) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                      QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
                 -1))) +
            (((__sql_inline_agg_1 ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) +
               ((SUPPLIER_NATIONKEY ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
                 ((__sql_inline_agg_1 ^=
                    ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                       (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                         delta_QUERY11_mSUPPLIER1_E2_2) *
                       -0.001) +
                      (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                   ((__sql_inline_agg_1 ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                          QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                          -0.001))) *
                     -1)))) *
              ((P_VALUE ^=
                 ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2) *
                    -1) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                       -1))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
              delta_QUERY11_mSUPPLIER1_E2_2) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
           -1))) +
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_2) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                   -1))) *
              -1)))) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_2) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    -1))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
}

CORRECT QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY:int,delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_QUERY11_mSUPPLIER1_E2_3:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][delta_P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * AggSum([delta_P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
         (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
           delta_QUERY11_mSUPPLIER1_E2_3) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
            -1))) *
       -1)) *
   (AggSum([delta_P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
               -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
               -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] * 
        -1))) +
     ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
               delta_QUERY11_mSUPPLIER1_E2_3) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
                -1))) *
           -1)))) *
    (AggSum([], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_3) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
          (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
          ((P_VALUE ^=
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_3) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
              delta_QUERY11_mSUPPLIER1_E2_3) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
               -1))) *
          -1)) *
      (AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([delta_P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] *
             -1))) +
        ((SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                (QUERY11_mSUPPLIER1_E2_3[][delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_3) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[]
                   [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
              -1)))) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_SUPPKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                 (QUERY11_mSUPPLIER1_E2_3[]
                  [delta_P_PARTKEY, SUPPLIER_SUPPKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_3) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[]
                    [delta_P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY] += 
  ({SUPPLIER_SUPPKEY = delta_QUERY11_mSUPPLIERSUPPLIER_SUPPKEY} *
  QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_3);
}

CORRECT QUERY11_mSUPPLIER1_E2_2[][delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_QUERY11_mSUPPLIER1_E2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY, SUPPLIER_NAME, SUPPLIER_ADDRESS, SUPPLIER_NATIONKEY, SUPPLIER_PHONE, SUPPLIER_ACCTBAL, SUPPLIER_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
   (EXISTS(
      ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
         (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
           delta_QUERY11_mSUPPLIER1_E2_2) *
         -1) +
        QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
          (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
            QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
          -1))) *
       -1)) *
   (AggSum([P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
              QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
          QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1))) +
     ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
       (EXISTS(
          ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
               delta_QUERY11_mSUPPLIER1_E2_2) *
             -1) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
              -1))) *
           -1)))) *
    (AggSum([P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
         ((P_VALUE ^=
            ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
               (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                 delta_QUERY11_mSUPPLIER1_E2_2) *
               -1) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([P_PARTKEY], 
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
             ((__sql_inline_agg_1 ^=
                ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                   (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                     delta_QUERY11_mSUPPLIER1_E2_2) *
                   -0.001) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                      QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) *
                 -1))) +
            (((__sql_inline_agg_1 ^=
                ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -0.001))) +
               ((SUPPLIER_NATIONKEY ^=
                  delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
                 ((__sql_inline_agg_1 ^=
                    ((QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                       (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                         delta_QUERY11_mSUPPLIER1_E2_2) *
                       -0.001) +
                      (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                   ((__sql_inline_agg_1 ^=
                      ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                        (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                          QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] *
                          -0.001))) *
                     -1)))) *
              ((P_VALUE ^=
                 ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                      delta_QUERY11_mSUPPLIER1_E2_2) *
                    -1) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                       QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                       -1))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
      (EXISTS(
         ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
            (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
              delta_QUERY11_mSUPPLIER1_E2_2) *
            -1) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
               QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
             -1))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                  -1))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
           (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
             QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * 
           -1))) +
        ((SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
          (EXISTS(
             ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                  delta_QUERY11_mSUPPLIER1_E2_2) *
                -1) +
               QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
            (EXISTS(
               (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                   QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                   -1))) *
              -1)))) *
       AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (SUPPLIER_NATIONKEY ^= delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY) *
           ((P_VALUE ^=
              ((QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                 (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] +
                   delta_QUERY11_mSUPPLIER1_E2_2) *
                 -1) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  (QUERY11_mSUPPLIER1_E2_2(int)[][SUPPLIER_NATIONKEY] *
                    QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] *
                    -1))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER7_L1_4[][SUPPLIER_SUPPKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} *
  QUERY11_mSUPPLIER1_E2_3[][P_PARTKEY, SUPPLIER_SUPPKEY] * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
   QUERY11_mPARTSUPP1_E2_3(int)[][SUPPLIER_SUPPKEY] += 
  ({SUPPLIER_NATIONKEY = delta_QUERY11_mSUPPLIERSUPPLIER_NATIONKEY} * -1 *
  delta_QUERY11_mSUPPLIER1_E2_2);
}

CORRECT QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] += delta_QUERY11_mPARTSUPP1_E2_1:float FOR ON + PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][delta_P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * AggSum([delta_P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((EXISTS(
     (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
       ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
         QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
         PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
       delta_QUERY11_mPARTSUPP1_E2_1)) +
    (EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
         ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
           PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
      -1)) *
   (AggSum([delta_P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] +
             (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
            0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
        ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
          PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
     EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
         ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
           PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
         delta_QUERY11_mPARTSUPP1_E2_1)) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
            QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
            PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
       -1)) *
    (AggSum([], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         ((P_VALUE ^=
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
              delta_QUERY11_mPARTSUPP1_E2_1)) +
           ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
               ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
               delta_QUERY11_mPARTSUPP1_E2_1)) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                   PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] +
              (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
             0.001)) *
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
               ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
               delta_QUERY11_mPARTSUPP1_E2_1)) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                   PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
            QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
            PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
          delta_QUERY11_mPARTSUPP1_E2_1)) +
       (EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         -1)) *
      (AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([delta_P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
       AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
             QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
             PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
            delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
          -1)) *
       (AggSum([], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 delta_QUERY11_mPARTSUPP1_E2_1)) +
              ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
         AggSum([], 
           ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                  ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                    PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
                  delta_QUERY11_mPARTSUPP1_E2_1)) +
               ((P_VALUE ^=
                  (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                    ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1)) *
             {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                delta_QUERY11_mPARTSUPP1_E2_1)) +
             ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
   (((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
      (EXISTS(
         (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
             delta_QUERY11_mPARTSUPP1_E2_3) *
            PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            (__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                   PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                0.001)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
         (EXISTS(
            (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                delta_QUERY11_mPARTSUPP1_E2_3) *
               PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             -1)) *
         (AggSum([P_PARTKEY], 
            ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
              (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
           AggSum([P_PARTKEY], 
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                    PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
           ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
             (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
             (EXISTS(
                (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                    delta_QUERY11_mPARTSUPP1_E2_3) *
                   PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                    ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1)))) *
          AggSum([], 
            ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
              (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
          PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
       (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
       (EXISTS(
          (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
              delta_QUERY11_mPARTSUPP1_E2_3) *
             PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
           -1)))) *
    (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
       AggSum([], 
         ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
           (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE ^=
              (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                  delta_QUERY11_mPARTSUPP1_E2_3) *
                 PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                    PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
      AggSum([P_PARTKEY], 
        ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             ((__sql_inline_agg_1 ^=
                ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                    0.001)) *
                 -1))) +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              ((__sql_inline_agg_1 ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                   0.001)) +
                ((PARTSUPP_SUPPKEY ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
                  ((__sql_inline_agg_1 ^=
                     ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                          delta_QUERY11_mPARTSUPP1_E2_3) *
                         PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                        QUERY11_mPARTSUPP1_L1_1[][]) *
                       0.001)) +
                    ((__sql_inline_agg_1 ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] +
                          (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                            PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                         0.001)) *
                      -1)))) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
}

CORRECT QUERY11_mPARTSUPP1_L1_1[][] += delta_QUERY11_mPARTSUPP1_L1_1:float FOR ON + PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
      ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
        QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
        PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
   (AggSum([P_PARTKEY], 
      ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
             0.001)) +
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            -1)) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((P_VALUE ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         ((__sql_inline_agg_1 ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
              0.001)) +
           ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((P_VALUE ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         ((__sql_inline_agg_1 ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] +
               (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
               delta_QUERY11_mPARTSUPP1_L1_1) *
              0.001)) +
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                   PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                0.001)) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
         ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
           PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
      (AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                0.001)) +
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            ((__sql_inline_agg_1 ^=
               ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                 0.001)) +
              ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
                -1)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
       AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                0.001)) +
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
   (((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
      (EXISTS(
         (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
             delta_QUERY11_mPARTSUPP1_E2_3) *
            PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            (__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                   PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                0.001)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
         (EXISTS(
            (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                delta_QUERY11_mPARTSUPP1_E2_3) *
               PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             -1)) *
         (AggSum([P_PARTKEY], 
            ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
              (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
           AggSum([P_PARTKEY], 
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                    PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
           ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
             (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
             (EXISTS(
                (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                    delta_QUERY11_mPARTSUPP1_E2_3) *
                   PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                    ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1)))) *
          AggSum([], 
            ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
              (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
          PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
       (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
       (EXISTS(
          (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
              delta_QUERY11_mPARTSUPP1_E2_3) *
             PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
           -1)))) *
    (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
       AggSum([], 
         ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
           (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE ^=
              (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                  delta_QUERY11_mPARTSUPP1_E2_3) *
                 PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                    PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
      AggSum([P_PARTKEY], 
        ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             ((__sql_inline_agg_1 ^=
                ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                    0.001)) *
                 -1))) +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              ((__sql_inline_agg_1 ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                   0.001)) +
                ((PARTSUPP_SUPPKEY ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
                  ((__sql_inline_agg_1 ^=
                     ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                          delta_QUERY11_mPARTSUPP1_E2_3) *
                         PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                        QUERY11_mPARTSUPP1_L1_1[][]) *
                       0.001)) +
                    ((__sql_inline_agg_1 ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] +
                          (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                            PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                         0.001)) *
                      -1)))) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
   (((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
      (EXISTS(
         (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
             delta_QUERY11_mPARTSUPP1_E2_3) *
            PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            (__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] +
                 (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                   PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                0.001)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
         (EXISTS(
            (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                delta_QUERY11_mPARTSUPP1_E2_3) *
               PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             -1)) *
         (AggSum([P_PARTKEY], 
            ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
              (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
           AggSum([P_PARTKEY], 
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                    PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
           ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
             (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
             (EXISTS(
                (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                    delta_QUERY11_mPARTSUPP1_E2_3) *
                   PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                    ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1)))) *
          AggSum([], 
            ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
              (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
          PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
       (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
       (EXISTS(
          (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
              delta_QUERY11_mPARTSUPP1_E2_3) *
             PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
           -1)))) *
    (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
       AggSum([], 
         ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
           (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE ^=
              (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                  delta_QUERY11_mPARTSUPP1_E2_3) *
                 PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                    PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
      AggSum([P_PARTKEY], 
        ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             ((__sql_inline_agg_1 ^=
                ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_L1_1[][]) *
                  0.001)) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] +
                     (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                    0.001)) *
                 -1))) +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              ((__sql_inline_agg_1 ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                   0.001)) +
                ((PARTSUPP_SUPPKEY ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
                  ((__sql_inline_agg_1 ^=
                     ((((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                          delta_QUERY11_mPARTSUPP1_E2_3) *
                         PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                        QUERY11_mPARTSUPP1_L1_1[][]) *
                       0.001)) +
                    ((__sql_inline_agg_1 ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] +
                          (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                            PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY)) *
                         0.001)) *
                      -1)))) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
}

CORRECT QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY:int] += delta_QUERY11_mPARTSUPP1_E2_1:float FOR ON - PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][delta_P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * AggSum([delta_P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][delta_P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((EXISTS(
     (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
       ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
         QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
         PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
       delta_QUERY11_mPARTSUPP1_E2_1)) +
    (EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
         ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
           PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
      -1)) *
   (AggSum([delta_P_PARTKEY], 
      ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
        (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([delta_P_PARTKEY], 
       ((__sql_inline_agg_1 ^=
          ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
            (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -0.001 *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         (P_VALUE ^=
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
        ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
          PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
     EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
         ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
           PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
         delta_QUERY11_mPARTSUPP1_E2_1)) +
     (EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
            QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
            PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
       -1)) *
    (AggSum([], 
       ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
         ((P_VALUE ^=
            (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
              delta_QUERY11_mPARTSUPP1_E2_1)) +
           ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
               ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
               delta_QUERY11_mPARTSUPP1_E2_1)) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                 -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
      AggSum([], 
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
             (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -0.001 *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
               ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
               delta_QUERY11_mPARTSUPP1_E2_1)) +
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                 -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
              -1)) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  ((((EXISTS(
        (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
          ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
            QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
            PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
          delta_QUERY11_mPARTSUPP1_E2_1)) +
       (EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         -1)) *
      (AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([delta_P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
       AggSum([delta_P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
     ((EXISTS(
         (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
           ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
             QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
             PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
            delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
             ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
          -1)) *
       (AggSum([], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            ((P_VALUE ^=
               (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                 delta_QUERY11_mPARTSUPP1_E2_1)) +
              ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
         AggSum([], 
           ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                  ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                  -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY) +
                  delta_QUERY11_mPARTSUPP1_E2_1)) +
               ((P_VALUE ^=
                  (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                    ((delta_P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                    -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1)) *
             {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) +
        EXISTS(
          (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
            delta_QUERY11_mPARTSUPP1_E2_1)) +
        (EXISTS( QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           ((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY] +
                delta_QUERY11_mPARTSUPP1_E2_1)) +
             ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][delta_P_PARTKEY]) * -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
   (((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
      (EXISTS(
         (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
             delta_QUERY11_mPARTSUPP1_E2_3) *
            -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^=
             ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
               (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -0.001 *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
         (EXISTS(
            (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                delta_QUERY11_mPARTSUPP1_E2_3) *
               -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             -1)) *
         (AggSum([P_PARTKEY], 
            ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
           AggSum([P_PARTKEY], 
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               (P_VALUE ^=
                 (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                   ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                   -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
           ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
             (P_PARTKEY ^= PARTSUPP_PARTKEY) *
             (EXISTS(
                (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                    delta_QUERY11_mPARTSUPP1_E2_3) *
                   -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                    ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                    -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1)))) *
          AggSum([], 
            ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
          PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
     ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
       (P_PARTKEY ^= PARTSUPP_PARTKEY) *
       (EXISTS(
          (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
              delta_QUERY11_mPARTSUPP1_E2_3) *
             -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
           -1)))) *
    (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
           ((P_VALUE ^=
              (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                  delta_QUERY11_mPARTSUPP1_E2_3) *
                 -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                  -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
      AggSum([P_PARTKEY], 
        ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             ((__sql_inline_agg_1 ^=
                (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                    delta_QUERY11_mPARTSUPP1_E2_3) *
                   -0.001 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      -0.001 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1))) +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              ((__sql_inline_agg_1 ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                   (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                     -0.001 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
                ((PARTSUPP_SUPPKEY ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
                  ((__sql_inline_agg_1 ^=
                     (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                         delta_QUERY11_mPARTSUPP1_E2_3) *
                        -0.001 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                       (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                    ((__sql_inline_agg_1 ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                         (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                           -0.001 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                      -1)))) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} * -1 *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} * -1 *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
}

CORRECT QUERY11_mPARTSUPP1_L1_1[][] += delta_QUERY11_mPARTSUPP1_L1_1:float FOR ON - PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  ((EXISTS(
    (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
      ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
        QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
        PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
   (AggSum([P_PARTKEY], 
      ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
        ((__sql_inline_agg_1 ^=
           ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
             0.001)) +
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            -1)) *
        {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((P_VALUE ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         ((__sql_inline_agg_1 ^=
            ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
              0.001)) +
           ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
     AggSum([P_PARTKEY], 
       ((P_VALUE ^=
          (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
              PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
         ((__sql_inline_agg_1 ^=
            (((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
               0.001) +
              (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -0.001 *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -0.001 *
                  PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             -1)) *
         {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
  (((EXISTS(
       (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
         ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
           QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
           PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
      (AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                0.001)) +
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((P_VALUE ^=
             (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
               ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            ((__sql_inline_agg_1 ^=
               ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                 0.001)) +
              ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
                -1)) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     (EXISTS( QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
       AggSum([P_PARTKEY], 
         ((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           ((__sql_inline_agg_1 ^=
              ((QUERY11_mPARTSUPP1_L1_1[][] + delta_QUERY11_mPARTSUPP1_L1_1) *
                0.001)) +
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
    -1));
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
   (((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
      (EXISTS(
         (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
             delta_QUERY11_mPARTSUPP1_E2_3) *
            -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^=
             ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
               (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -0.001 *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
         (EXISTS(
            (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                delta_QUERY11_mPARTSUPP1_E2_3) *
               -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             -1)) *
         (AggSum([P_PARTKEY], 
            ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
           AggSum([P_PARTKEY], 
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               (P_VALUE ^=
                 (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                   ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                   -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
           ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
             (P_PARTKEY ^= PARTSUPP_PARTKEY) *
             (EXISTS(
                (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                    delta_QUERY11_mPARTSUPP1_E2_3) *
                   -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                    ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                    -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1)))) *
          AggSum([], 
            ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
          PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
     ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
       (P_PARTKEY ^= PARTSUPP_PARTKEY) *
       (EXISTS(
          (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
              delta_QUERY11_mPARTSUPP1_E2_3) *
             -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
           -1)))) *
    (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
           ((P_VALUE ^=
              (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                  delta_QUERY11_mPARTSUPP1_E2_3) *
                 -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                  -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
      AggSum([P_PARTKEY], 
        ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             ((__sql_inline_agg_1 ^=
                (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                    delta_QUERY11_mPARTSUPP1_E2_3) *
                   -0.001 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      -0.001 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1))) +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              ((__sql_inline_agg_1 ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                   (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                     -0.001 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
                ((PARTSUPP_SUPPKEY ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
                  ((__sql_inline_agg_1 ^=
                     (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                         delta_QUERY11_mPARTSUPP1_E2_3) *
                        -0.001 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                       (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                    ((__sql_inline_agg_1 ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                         (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                           -0.001 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                      -1)))) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} * -1 *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} * -1 *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
}

CORRECT QUERY11_mPARTSUPP1_E2_3[][delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_QUERY11_mPARTSUPP1_E2_3:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY, PARTSUPP_SUPPKEY, PARTSUPP_AVAILQTY, PARTSUPP_SUPPLYCOST, PARTSUPP_COMMENT) {
   QUERY11(float)[][P_PARTKEY]:((Exists(QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * AggSum([P_PARTKEY],(((P_VALUE ^= QUERY11_mPARTSUPP1_E2_1(float)[][P_PARTKEY]) * (__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1(float)[][] * 0.001)) * {P_VALUE > __sql_inline_agg_1} * P_VALUE))))) += 
  (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
   (((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
      (EXISTS(
         (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
             delta_QUERY11_mPARTSUPP1_E2_3) *
            -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
           QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
        (EXISTS(
           (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
             ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
               PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
          -1)) *
      (AggSum([P_PARTKEY], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
        AggSum([P_PARTKEY], 
          ((__sql_inline_agg_1 ^=
             ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
               (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -0.001 *
                 PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            (P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
            {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
     ((((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
         (EXISTS(
            (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                 QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                delta_QUERY11_mPARTSUPP1_E2_3) *
               -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
              QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
           (EXISTS(
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             -1)) *
         (AggSum([P_PARTKEY], 
            ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (P_VALUE ^= QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY]) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)) +
           AggSum([P_PARTKEY], 
             ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
               (P_VALUE ^=
                 (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                   ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                   -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) +
        ((EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
           ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
             (P_PARTKEY ^= PARTSUPP_PARTKEY) *
             (EXISTS(
                (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                     QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                    delta_QUERY11_mPARTSUPP1_E2_3) *
                   -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                  QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
               (EXISTS(
                  (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                    ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                    -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1)))) *
          AggSum([], 
            ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
              (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)) *
              {P_VALUE > __sql_inline_agg_1} * P_VALUE)))) *
       -1))) +
  ((EXISTS(
      (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
        ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
          QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
          PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
     ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
       (P_PARTKEY ^= PARTSUPP_PARTKEY) *
       (EXISTS(
          (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
               QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
              delta_QUERY11_mPARTSUPP1_E2_3) *
             -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
            QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
         (EXISTS(
            (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
              ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * -1 *
                PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
           -1)))) *
    (((P_PARTKEY ^= PARTSUPP_PARTKEY) *
       AggSum([], 
         ((__sql_inline_agg_1 ^= (QUERY11_mPARTSUPP1_L1_1[][] * 0.001)) *
           (PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
           ((P_VALUE ^=
              (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                   QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                  delta_QUERY11_mPARTSUPP1_E2_3) *
                 -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
             ((P_VALUE ^=
                (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                  ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                    QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                  -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
               -1)) *
           {P_VALUE > __sql_inline_agg_1} * P_VALUE))) +
      AggSum([P_PARTKEY], 
        ((PARTSUPP_SUPPKEY ^= delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
          (((P_VALUE ^=
              (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                  QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] * 
                -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
             ((__sql_inline_agg_1 ^=
                (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                    delta_QUERY11_mPARTSUPP1_E2_3) *
                   -0.001 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                  (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
               ((__sql_inline_agg_1 ^=
                  ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                    (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                      -0.001 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                 -1))) +
            ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
              ((__sql_inline_agg_1 ^=
                 ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                   (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                     -0.001 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) +
                ((PARTSUPP_SUPPKEY ^=
                   delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY) *
                  ((__sql_inline_agg_1 ^=
                     (((QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] +
                         delta_QUERY11_mPARTSUPP1_E2_3) *
                        -0.001 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                       (QUERY11_mPARTSUPP1_L1_1[][] * 0.001))) +
                    ((__sql_inline_agg_1 ^=
                       ((QUERY11_mPARTSUPP1_L1_1[][] * 0.001) +
                         (QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                           -0.001 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                      -1)))) *
              ((P_VALUE ^=
                 (((((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                      QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY]) +
                     delta_QUERY11_mPARTSUPP1_E2_3) *
                    -1 * PARTSUPP_AVAILQTY * PARTSUPP_SUPPLYCOST) +
                   QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY])) +
                ((P_VALUE ^=
                   (QUERY11_mPARTSUPP1_E2_1[][P_PARTKEY] +
                     ((P_PARTKEY ^= PARTSUPP_PARTKEY) *
                       QUERY11_mPARTSUPP1_E2_3(int)[][PARTSUPP_SUPPKEY] *
                       -1 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY))) *
                  -1)))) *
          {P_VALUE > __sql_inline_agg_1} * P_VALUE)))));
   QUERY11_mPARTSUPP1_L1_1(float)[][] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} * -1 *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
   QUERY11_mPARTSUPP1_E2_1(float)[][PARTSUPP_PARTKEY] += 
  ({PARTSUPP_SUPPKEY = delta_QUERY11_mPARTSUPPPARTSUPP_SUPPKEY} * -1 *
  delta_QUERY11_mPARTSUPP1_E2_3 * PARTSUPP_SUPPLYCOST * PARTSUPP_AVAILQTY);
}
