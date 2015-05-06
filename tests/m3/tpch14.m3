-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem_small.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM PART(PART_PARTKEY int, PART_NAME string, PART_MFGR string, PART_BRAND string, PART_TYPE string, PART_SIZE int, PART_CONTAINER string, PART_RETAILPRICE float, PART_COMMENT string)
  FROM FILE 'data/tpch/part.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP PROMO_REVENUE(float)[][] := 
(AggSum([], 
   (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
               L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
               L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
               L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
               L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
     PART(L_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
            P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
            P_RETAILPRICE:float, P_COMMENT:string) *
     {L_SHIPDATE:date >= DATE('1995-9-1')} *
     {L_SHIPDATE:date < DATE('1995-10-1')} *
     {0 != [regexp_match:int]('^PROMO.*$', P_TYPE:string)} *
     ((-1 * L_DISCOUNT:float) + 1) * L_EXTENDEDPRICE:float)) *
  AggSum([], 
    ((__sql_inline_agg_2:float ^=
       AggSum([], 
         ((__sql_inline_agg_1:float ^=
            AggSum([], 
              (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int,
                          L_LINENUMBER:int, L_QUANTITY:float,
                          L_EXTENDEDPRICE:float, L_DISCOUNT:float,
                          L_TAX:float, L_RETURNFLAG:string,
                          L_LINESTATUS:string, L_SHIPDATE:date,
                          L_COMMITDATE:date, L_RECEIPTDATE:date,
                          L_SHIPINSTRUCT:string, L_SHIPMODE:string,
                          L_COMMENT:string) *
                PART(L_PARTKEY:int, P_NAME:string, P_MFGR:string,
                       P_BRAND:string, P_TYPE:string, P_SIZE:int,
                       P_CONTAINER:string, P_RETAILPRICE:float,
                       P_COMMENT:string) *
                {L_SHIPDATE:date >= DATE('1995-9-1')} *
                {L_SHIPDATE:date < DATE('1995-10-1')} *
                ((-1 * L_DISCOUNT:float) + 1) * L_EXTENDEDPRICE:float))) *
           {[listmax:float](1, __sql_inline_agg_1:float)}))) *
      {[/:float](__sql_inline_agg_2:float)})) *
  100.);

DECLARE MAP PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] := 
AggSum([], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    PART(L_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    {L_SHIPDATE:date >= DATE('1995-9-1')} *
    {L_SHIPDATE:date < DATE('1995-10-1')} * L_DISCOUNT:float *
    L_EXTENDEDPRICE:float));

DECLARE MAP PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1(float)[]
[PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int] := 
AggSum([PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int], 
  (LINEITEM(L_ORDERKEY:int,
              PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int,
              L_SUPPKEY:int, L_LINENUMBER:int, L_QUANTITY:float,
              L_EXTENDEDPRICE:float, L_DISCOUNT:float, L_TAX:float,
              L_RETURNFLAG:string, L_LINESTATUS:string, L_SHIPDATE:date,
              L_COMMITDATE:date, L_RECEIPTDATE:date, L_SHIPINSTRUCT:string,
              L_SHIPMODE:string, L_COMMENT:string) *
    {L_SHIPDATE:date >= DATE('1995-9-1')} *
    {L_SHIPDATE:date < DATE('1995-10-1')} * L_DISCOUNT:float *
    L_EXTENDEDPRICE:float));

DECLARE MAP PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3(int)[]
[PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int] := 
AggSum([PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int], 
  PART(PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int,
         P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
         P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
         P_COMMENT:string));

DECLARE MAP PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] := 
AggSum([], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    PART(L_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    {L_SHIPDATE:date >= DATE('1995-9-1')} *
    {L_SHIPDATE:date < DATE('1995-10-1')} * L_EXTENDEDPRICE:float));

DECLARE MAP PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1(float)[]
[PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int] := 
AggSum([PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int], 
  (LINEITEM(L_ORDERKEY:int,
              PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int,
              L_SUPPKEY:int, L_LINENUMBER:int, L_QUANTITY:float,
              L_EXTENDEDPRICE:float, L_DISCOUNT:float, L_TAX:float,
              L_RETURNFLAG:string, L_LINESTATUS:string, L_SHIPDATE:date,
              L_COMMITDATE:date, L_RECEIPTDATE:date, L_SHIPINSTRUCT:string,
              L_SHIPMODE:string, L_COMMENT:string) *
    {L_SHIPDATE:date >= DATE('1995-9-1')} *
    {L_SHIPDATE:date < DATE('1995-10-1')} * L_EXTENDEDPRICE:float));

DECLARE MAP PROMO_REVENUE_mLINEITEM2(float)[][] := 
AggSum([], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    PART(L_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    {L_SHIPDATE:date >= DATE('1995-9-1')} *
    {L_SHIPDATE:date < DATE('1995-10-1')} *
    {0 != [regexp_match:int]('^PROMO.*$', P_TYPE:string)} *
    L_DISCOUNT:float * L_EXTENDEDPRICE:float));

DECLARE MAP PROMO_REVENUE_mLINEITEM2_mLINEITEM3(int)[]
[PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int] := 
AggSum([PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int], 
  (PART(PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int,
          P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
          P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
          P_COMMENT:string) *
    {0 != [regexp_match:int]('^PROMO.*$', P_TYPE:string)}));

DECLARE MAP PROMO_REVENUE_mLINEITEM5(float)[][] := 
AggSum([], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    PART(L_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    {L_SHIPDATE:date >= DATE('1995-9-1')} *
    {L_SHIPDATE:date < DATE('1995-10-1')} *
    {0 != [regexp_match:int]('^PROMO.*$', P_TYPE:string)} *
    L_EXTENDEDPRICE:float));

-------------------- QUERIES --------------------
DECLARE QUERY PROMO_REVENUE := PROMO_REVENUE(float)[][];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3(int)[][LINEITEM_PARTKEY:int] *
  LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1(float)[][LINEITEM_PARTKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * LINEITEM_DISCOUNT:float *
  LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3(int)[][LINEITEM_PARTKEY:int] *
  LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1(float)[][LINEITEM_PARTKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  PROMO_REVENUE_mLINEITEM2_mLINEITEM3(int)[][LINEITEM_PARTKEY:int] *
  LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  PROMO_REVENUE_mLINEITEM2_mLINEITEM3(int)[][LINEITEM_PARTKEY:int] *
  LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3(int)[][LINEITEM_PARTKEY:int] *
  -1 * LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1(float)[][LINEITEM_PARTKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3(int)[][LINEITEM_PARTKEY:int] *
  -1 * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1(float)[][LINEITEM_PARTKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  PROMO_REVENUE_mLINEITEM2_mLINEITEM3(int)[][LINEITEM_PARTKEY:int] * 
-1 * LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  PROMO_REVENUE_mLINEITEM2_mLINEITEM3(int)[][LINEITEM_PARTKEY:int] * 
-1 * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1(float)[][PART_PARTKEY:int];
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3(int)[][PART_PARTKEY:int] += 1;
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1(float)[][PART_PARTKEY:int];
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} *
  PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1(float)[][PART_PARTKEY:int]);
   PROMO_REVENUE_mLINEITEM2_mLINEITEM3(int)[][PART_PARTKEY:int] += {0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)};
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} *
  PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1(float)[][PART_PARTKEY:int]);
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += (PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1(float)[][PART_PARTKEY:int] * -1);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3(int)[][PART_PARTKEY:int] += -1;
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += (PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1(float)[][PART_PARTKEY:int] * -1);
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} *
  PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1(float)[][PART_PARTKEY:int] * 
-1);
   PROMO_REVENUE_mLINEITEM2_mLINEITEM3(int)[][PART_PARTKEY:int] += ({0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} * -1);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} *
  PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1(float)[][PART_PARTKEY:int] * 
-1);
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

ON SYSTEM READY {
   PROMO_REVENUE(float)[][] := 0.;
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] := 0.;
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] := 0.;
   PROMO_REVENUE_mLINEITEM2(float)[][] := 0.;
   PROMO_REVENUE_mLINEITEM5(float)[][] := 0.;
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int *
  LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int *
  LINEITEM_EXTENDEDPRICE:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int *
  LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int *
  LINEITEM_EXTENDEDPRICE:float);
}

CORRECT PROMO_REVENUE_mLINEITEM2_mLINEITEM3[][delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int * LINEITEM_DISCOUNT:float *
  LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int *
  LINEITEM_EXTENDEDPRICE:float);
}

CORRECT PROMO_REVENUE_mLINEITEM2_mLINEITEM3[][delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int * LINEITEM_DISCOUNT:float *
  LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} *
  delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int *
  LINEITEM_EXTENDEDPRICE:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1[][] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   (((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
               PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
            {[listmax:float](1, __sql_inline_agg_1:float)})) +
         AggSum([], 
           (((__sql_inline_agg_1:float ^=
               (((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] +
                   delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1:float) *
                  -1) +
                 PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) +
              ((__sql_inline_agg_1:float ^=
                 ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
                -1)) *
             {[listmax:float](1, __sql_inline_agg_1:float)})))) +
      ((__sql_inline_agg_2:float ^=
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
             {[listmax:float](1, __sql_inline_agg_1:float)}))) *
        -1)) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_3[][] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   (((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
               PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
            {[listmax:float](1, __sql_inline_agg_1:float)})) +
         AggSum([], 
           (((__sql_inline_agg_1:float ^=
               ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                 PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] +
                 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3:float)) +
              ((__sql_inline_agg_1:float ^=
                 ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
                -1)) *
             {[listmax:float](1, __sql_inline_agg_1:float)})))) +
      ((__sql_inline_agg_2:float ^=
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
             {[listmax:float](1, __sql_inline_agg_1:float)}))) *
        -1)) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

CORRECT PROMO_REVENUE_mLINEITEM2[][] += delta_PROMO_REVENUE_mLINEITEM2:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  -100. * delta_PROMO_REVENUE_mLINEITEM2:float);
}

CORRECT PROMO_REVENUE_mLINEITEM5[][] += delta_PROMO_REVENUE_mLINEITEM5:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  100. * delta_PROMO_REVENUE_mLINEITEM5:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int *
  LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int *
  LINEITEM_EXTENDEDPRICE:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int *
  LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mLINEITEM3:int *
  LINEITEM_EXTENDEDPRICE:float);
}

CORRECT PROMO_REVENUE_mLINEITEM2_mLINEITEM3[][delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int * LINEITEM_DISCOUNT:float *
  LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int *
  LINEITEM_EXTENDEDPRICE:float);
}

CORRECT PROMO_REVENUE_mLINEITEM2_mLINEITEM3[][delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int * LINEITEM_DISCOUNT:float *
  LINEITEM_EXTENDEDPRICE:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({LINEITEM_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM2_mLINEITEMLINEITEM_PARTKEY:int} *
  {LINEITEM_SHIPDATE:date >= DATE('1995-9-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-10-1')} * -1 *
  delta_PROMO_REVENUE_mLINEITEM2_mLINEITEM3:int *
  LINEITEM_EXTENDEDPRICE:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1[][] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   (((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
               PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
            {[listmax:float](1, __sql_inline_agg_1:float)})) +
         AggSum([], 
           (((__sql_inline_agg_1:float ^=
               (((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] +
                   delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1:float) *
                  -1) +
                 PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) +
              ((__sql_inline_agg_1:float ^=
                 ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
                -1)) *
             {[listmax:float](1, __sql_inline_agg_1:float)})))) +
      ((__sql_inline_agg_2:float ^=
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
             {[listmax:float](1, __sql_inline_agg_1:float)}))) *
        -1)) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_3[][] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   (((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
               PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
            {[listmax:float](1, __sql_inline_agg_1:float)})) +
         AggSum([], 
           (((__sql_inline_agg_1:float ^=
               ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                 PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] +
                 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3:float)) +
              ((__sql_inline_agg_1:float ^=
                 ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
                -1)) *
             {[listmax:float](1, __sql_inline_agg_1:float)})))) +
      ((__sql_inline_agg_2:float ^=
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
             {[listmax:float](1, __sql_inline_agg_1:float)}))) *
        -1)) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

CORRECT PROMO_REVENUE_mLINEITEM2[][] += delta_PROMO_REVENUE_mLINEITEM2:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  -100. * delta_PROMO_REVENUE_mLINEITEM2:float);
}

CORRECT PROMO_REVENUE_mLINEITEM5[][] += delta_PROMO_REVENUE_mLINEITEM5:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  100. * delta_PROMO_REVENUE_mLINEITEM5:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float);
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int} *
  {0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int} *
  {0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float);
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int} *
  {0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int} *
  {0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1[][] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   (((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
               PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
            {[listmax:float](1, __sql_inline_agg_1:float)})) +
         AggSum([], 
           (((__sql_inline_agg_1:float ^=
               (((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] +
                   delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1:float) *
                  -1) +
                 PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) +
              ((__sql_inline_agg_1:float ^=
                 ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
                -1)) *
             {[listmax:float](1, __sql_inline_agg_1:float)})))) +
      ((__sql_inline_agg_2:float ^=
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
             {[listmax:float](1, __sql_inline_agg_1:float)}))) *
        -1)) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_3[][] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   (((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
               PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
            {[listmax:float](1, __sql_inline_agg_1:float)})) +
         AggSum([], 
           (((__sql_inline_agg_1:float ^=
               ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                 PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] +
                 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3:float)) +
              ((__sql_inline_agg_1:float ^=
                 ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
                -1)) *
             {[listmax:float](1, __sql_inline_agg_1:float)})))) +
      ((__sql_inline_agg_2:float ^=
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
             {[listmax:float](1, __sql_inline_agg_1:float)}))) *
        -1)) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

CORRECT PROMO_REVENUE_mLINEITEM2[][] += delta_PROMO_REVENUE_mLINEITEM2:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  -100. * delta_PROMO_REVENUE_mLINEITEM2:float);
}

CORRECT PROMO_REVENUE_mLINEITEM5[][] += delta_PROMO_REVENUE_mLINEITEM5:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  100. * delta_PROMO_REVENUE_mLINEITEM5:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float);
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int} *
  {0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int} *
  {0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float);
   PROMO_REVENUE_mLINEITEM2(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPARTPART_PARTKEY:int} *
  {0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1_mPART1:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1[][delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float);
   PROMO_REVENUE_mLINEITEM5(float)[][] += 
  ({PART_PARTKEY:int =
 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPARTPART_PARTKEY:int} *
  {0 != [regexp_match:int]('^PROMO.*$', PART_TYPE:string)} * -1 *
  delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3_mPART1:float);
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_1[][] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   (((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
               PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
            {[listmax:float](1, __sql_inline_agg_1:float)})) +
         AggSum([], 
           (((__sql_inline_agg_1:float ^=
               (((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] +
                   delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_1:float) *
                  -1) +
                 PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) +
              ((__sql_inline_agg_1:float ^=
                 ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
                -1)) *
             {[listmax:float](1, __sql_inline_agg_1:float)})))) +
      ((__sql_inline_agg_2:float ^=
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
             {[listmax:float](1, __sql_inline_agg_1:float)}))) *
        -1)) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

CORRECT PROMO_REVENUE_mLINEITEM1_L1_1_L1_3[][] += delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   (((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
               PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
            {[listmax:float](1, __sql_inline_agg_1:float)})) +
         AggSum([], 
           (((__sql_inline_agg_1:float ^=
               ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                 PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][] +
                 delta_PROMO_REVENUE_mLINEITEM1_L1_1_L1_3:float)) +
              ((__sql_inline_agg_1:float ^=
                 ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                   PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
                -1)) *
             {[listmax:float](1, __sql_inline_agg_1:float)})))) +
      ((__sql_inline_agg_2:float ^=
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
                PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
             {[listmax:float](1, __sql_inline_agg_1:float)}))) *
        -1)) *
     {[/:float](__sql_inline_agg_2:float)})) *
  ((PROMO_REVENUE_mLINEITEM2(float)[][] * -100.) +
    (PROMO_REVENUE_mLINEITEM5(float)[][] * 100.)));
}

CORRECT PROMO_REVENUE_mLINEITEM2[][] += delta_PROMO_REVENUE_mLINEITEM2:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  -100. * delta_PROMO_REVENUE_mLINEITEM2:float);
}

CORRECT PROMO_REVENUE_mLINEITEM5[][] += delta_PROMO_REVENUE_mLINEITEM5:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   PROMO_REVENUE(float)[][] := 
  (AggSum([], 
   ((__sql_inline_agg_2:float ^=
      AggSum([], 
        ((__sql_inline_agg_1:float ^=
           ((PROMO_REVENUE_mLINEITEM1_L1_1_L1_1(float)[][] * -1) +
             PROMO_REVENUE_mLINEITEM1_L1_1_L1_3(float)[][])) *
          {[listmax:float](1, __sql_inline_agg_1:float)}))) *
     {[/:float](__sql_inline_agg_2:float)})) *
  100. * delta_PROMO_REVENUE_mLINEITEM5:float);
}
