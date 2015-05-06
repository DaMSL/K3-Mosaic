-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP SUM_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
  (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
              LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
              LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
              LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
              LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
              LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
              LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
              LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
    {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_QUANTITY:float));

DECLARE MAP SUM_BASE_PRICE(float)[]
[LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
  (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
              LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
              LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
              LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
              LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
              LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
              LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
              LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
    {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
    LINEITEM_EXTENDEDPRICE:float));

DECLARE MAP SUM_DISC_PRICE(float)[]
[LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
  (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
              LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
              LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
              LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
              LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
              LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
              LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
              LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
    {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
    ((-1 * LINEITEM_DISCOUNT:float) + 1) * LINEITEM_EXTENDEDPRICE:float));

DECLARE MAP SUM_CHARGE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
  (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
              LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
              LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
              LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
              LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
              LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
              LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
              LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
    {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
    ((-1 * LINEITEM_DISCOUNT:float) + 1) * LINEITEM_EXTENDEDPRICE:float *
    {(1 + LINEITEM_TAX:float)}));

DECLARE MAP AVG_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
(AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
   (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
               LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
               LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
               LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
               LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
               LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
               LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
               LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_QUANTITY:float)) *
  AggSum([], 
    ((__sql_inline_average_count_1:int ^=
       AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
         (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
                     LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
                     LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
                     LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
                     LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
                     LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
                     LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
                     LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
           {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')}))) *
      {0 != __sql_inline_average_count_1:int} *
      {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})));

DECLARE MAP AVG_QTY_mLINEITEM1_L3_1(int)[]
[LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
  (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
              LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
              LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
              LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
              LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
              LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
              LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
              LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
    {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')}));

DECLARE MAP AVG_QTY_mLINEITEM5(float)[]
[LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
  (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
              LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
              LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
              LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
              LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
              LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
              LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
              LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
    {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_QUANTITY:float));

DECLARE MAP AVG_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
(AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
   (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
               LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
               LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
               LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
               LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
               LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
               LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
               LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     LINEITEM_EXTENDEDPRICE:float)) *
  AggSum([], 
    ((__sql_inline_average_count_2:int ^=
       AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
         (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
                     LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
                     LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
                     LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
                     LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
                     LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
                     LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
                     LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
           {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')}))) *
      {0 != __sql_inline_average_count_2:int} *
      {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})));

DECLARE MAP AVG_PRICE_mLINEITEM5(float)[]
[LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
  (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
              LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
              LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
              LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
              LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
              LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
              LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
              LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
    {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
    LINEITEM_EXTENDEDPRICE:float));

DECLARE MAP AVG_DISC(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
(AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
   (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
               LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
               LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
               LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
               LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
               LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
               LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
               LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_DISCOUNT:float)) *
  AggSum([], 
    ((__sql_inline_average_count_3:int ^=
       AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
         (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
                     LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
                     LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
                     LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
                     LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
                     LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
                     LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
                     LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
           {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')}))) *
      {0 != __sql_inline_average_count_3:int} *
      {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})));

DECLARE MAP AVG_DISC_mLINEITEM5(float)[]
[LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
  (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
              LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
              LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
              LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
              LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
              LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
              LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
              LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
    {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_DISCOUNT:float));

DECLARE MAP COUNT_ORDER(int)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] := 
AggSum([LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string], 
  (LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int,
              LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int,
              LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float,
              LINEITEM_DISCOUNT:float, LINEITEM_TAX:float,
              LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string,
              LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date,
              LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string,
              LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) *
    {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')}));

-------------------- QUERIES --------------------
DECLARE QUERY SUM_QTY := SUM_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string];

DECLARE QUERY SUM_BASE_PRICE := SUM_BASE_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string];

DECLARE QUERY SUM_DISC_PRICE := SUM_DISC_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string];

DECLARE QUERY SUM_CHARGE := SUM_CHARGE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string];

DECLARE QUERY AVG_QTY := AVG_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string];

DECLARE QUERY AVG_PRICE := AVG_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string];

DECLARE QUERY AVG_DISC := AVG_DISC(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string];

DECLARE QUERY COUNT_ORDER := COUNT_ORDER(int)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   SUM_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_QUANTITY:float);
   SUM_BASE_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_EXTENDEDPRICE:float);
   SUM_DISC_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
  ((-1 * LINEITEM_DISCOUNT:float) + 1) * LINEITEM_EXTENDEDPRICE:float);
   SUM_CHARGE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
  ((-1 * LINEITEM_DISCOUNT:float) + 1) * {(1 + LINEITEM_TAX:float)} *
  LINEITEM_EXTENDEDPRICE:float);
   AVG_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
   AggSum([], 
     ((__sql_inline_average_count_1:int ^=
        (AVG_QTY_mLINEITEM1_L3_1(int)[]
         [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
          {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
       {0 != __sql_inline_average_count_1:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
   LINEITEM_QUANTITY:float) +
  (AVG_QTY_mLINEITEM5(float)[]
   [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
    (AggSum([], 
       ((__sql_inline_average_count_1:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
            {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
         {0 != __sql_inline_average_count_1:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) +
      (AggSum([], 
         ((__sql_inline_average_count_1:int ^=
            AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
           {0 != __sql_inline_average_count_1:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
        -1))));
   AVG_QTY_mLINEITEM5(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_QUANTITY:float);
   AVG_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
   AggSum([], 
     ((__sql_inline_average_count_2:int ^=
        (AVG_QTY_mLINEITEM1_L3_1(int)[]
         [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
          {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
       {0 != __sql_inline_average_count_2:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
   LINEITEM_EXTENDEDPRICE:float) +
  (AVG_PRICE_mLINEITEM5(float)[]
   [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
    (AggSum([], 
       ((__sql_inline_average_count_2:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
            {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
         {0 != __sql_inline_average_count_2:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) +
      (AggSum([], 
         ((__sql_inline_average_count_2:int ^=
            AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
           {0 != __sql_inline_average_count_2:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
        -1))));
   AVG_PRICE_mLINEITEM5(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_EXTENDEDPRICE:float);
   AVG_DISC(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
   AggSum([], 
     ((__sql_inline_average_count_3:int ^=
        (AVG_QTY_mLINEITEM1_L3_1(int)[]
         [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
          {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
       {0 != __sql_inline_average_count_3:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
   LINEITEM_DISCOUNT:float) +
  (AVG_DISC_mLINEITEM5(float)[]
   [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
    (AggSum([], 
       ((__sql_inline_average_count_3:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
            {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
         {0 != __sql_inline_average_count_3:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) +
      (AggSum([], 
         ((__sql_inline_average_count_3:int ^=
            AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
           {0 != __sql_inline_average_count_3:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
        -1))));
   AVG_QTY_mLINEITEM1_L3_1(int)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')};
   AVG_DISC_mLINEITEM5(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * LINEITEM_DISCOUNT:float);
   COUNT_ORDER(int)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')};
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   SUM_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1 * LINEITEM_QUANTITY:float);
   SUM_BASE_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1 *
  LINEITEM_EXTENDEDPRICE:float);
   SUM_DISC_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
  LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)});
   SUM_CHARGE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * {(1 + LINEITEM_TAX:float)} *
  LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)});
   AVG_QTY(float)[][LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] += 
  (((((LINEITEM_RETURNFLAG_1:string ^= LINEITEM_RETURNFLAG:string) *
     (LINEITEM_LINESTATUS_1:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       ((__sql_inline_average_count_1:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] +
            ((LINEITEM_RETURNFLAG_1:string ^= LINEITEM_RETURNFLAG:string) *
              (LINEITEM_LINESTATUS_1:string ^= LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
         {0 != __sql_inline_average_count_1:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
     LINEITEM_QUANTITY:float) +
    (AVG_QTY_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] *
      AggSum([], 
        ((__sql_inline_average_count_1:int ^=
           AVG_QTY_mLINEITEM1_L3_1(int)[]
           [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string]) *
          {0 != __sql_inline_average_count_1:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})))) *
   -1) +
  (AVG_QTY_mLINEITEM5(float)[]
   [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] *
    AggSum([], 
      ((__sql_inline_average_count_1:int ^=
         (AVG_QTY_mLINEITEM1_L3_1(int)[]
          [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] +
           ((LINEITEM_RETURNFLAG_1:string ^= LINEITEM_RETURNFLAG:string) *
             (LINEITEM_LINESTATUS_1:string ^= LINEITEM_LINESTATUS:string) *
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
        {0 != __sql_inline_average_count_1:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))}))));
   AVG_QTY_mLINEITEM5(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1 * LINEITEM_QUANTITY:float);
   AVG_PRICE(float)[][LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] += 
  (((((LINEITEM_RETURNFLAG_1:string ^= LINEITEM_RETURNFLAG:string) *
     (LINEITEM_LINESTATUS_1:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       ((__sql_inline_average_count_2:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] +
            ((LINEITEM_RETURNFLAG_1:string ^= LINEITEM_RETURNFLAG:string) *
              (LINEITEM_LINESTATUS_1:string ^= LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
         {0 != __sql_inline_average_count_2:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
     LINEITEM_EXTENDEDPRICE:float) +
    (AVG_PRICE_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] *
      AggSum([], 
        ((__sql_inline_average_count_2:int ^=
           AVG_QTY_mLINEITEM1_L3_1(int)[]
           [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string]) *
          {0 != __sql_inline_average_count_2:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})))) *
   -1) +
  (AVG_PRICE_mLINEITEM5(float)[]
   [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] *
    AggSum([], 
      ((__sql_inline_average_count_2:int ^=
         (AVG_QTY_mLINEITEM1_L3_1(int)[]
          [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] +
           ((LINEITEM_RETURNFLAG_1:string ^= LINEITEM_RETURNFLAG:string) *
             (LINEITEM_LINESTATUS_1:string ^= LINEITEM_LINESTATUS:string) *
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
        {0 != __sql_inline_average_count_2:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))}))));
   AVG_PRICE_mLINEITEM5(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1 *
  LINEITEM_EXTENDEDPRICE:float);
   AVG_DISC(float)[][LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] += 
  (((((LINEITEM_RETURNFLAG_1:string ^= LINEITEM_RETURNFLAG:string) *
     (LINEITEM_LINESTATUS_1:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       ((__sql_inline_average_count_3:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] +
            ((LINEITEM_RETURNFLAG_1:string ^= LINEITEM_RETURNFLAG:string) *
              (LINEITEM_LINESTATUS_1:string ^= LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
         {0 != __sql_inline_average_count_3:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
     LINEITEM_DISCOUNT:float) +
    (AVG_DISC_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] *
      AggSum([], 
        ((__sql_inline_average_count_3:int ^=
           AVG_QTY_mLINEITEM1_L3_1(int)[]
           [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string]) *
          {0 != __sql_inline_average_count_3:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})))) *
   -1) +
  (AVG_DISC_mLINEITEM5(float)[]
   [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] *
    AggSum([], 
      ((__sql_inline_average_count_3:int ^=
         (AVG_QTY_mLINEITEM1_L3_1(int)[]
          [LINEITEM_RETURNFLAG_1:string, LINEITEM_LINESTATUS_1:string] +
           ((LINEITEM_RETURNFLAG_1:string ^= LINEITEM_RETURNFLAG:string) *
             (LINEITEM_LINESTATUS_1:string ^= LINEITEM_LINESTATUS:string) *
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
        {0 != __sql_inline_average_count_3:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))}))));
   AVG_QTY_mLINEITEM1_L3_1(int)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1);
   AVG_DISC_mLINEITEM5(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1 * LINEITEM_DISCOUNT:float);
   COUNT_ORDER(int)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += ({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1);
}

ON SYSTEM READY {
}

CORRECT AVG_QTY_mLINEITEM1_L3_1[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_QTY_mLINEITEM1_L3_1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_1:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_1:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
            -1)) *
         {0 != __sql_inline_average_count_1:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
     LINEITEM_QUANTITY:float) +
    (AVG_QTY_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
      (AggSum([], 
         (((__sql_inline_average_count_1:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
               delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
            ((__sql_inline_average_count_1:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
              -1)) *
           {0 != __sql_inline_average_count_1:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) +
        (AggSum([], 
           (((__sql_inline_average_count_1:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
              ((__sql_inline_average_count_1:int ^=
                 AVG_QTY_mLINEITEM1_L3_1(int)[]
                 [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
                -1)) *
             {0 != __sql_inline_average_count_1:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
          -1)))));
   AVG_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_2:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_2:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
            -1)) *
         {0 != __sql_inline_average_count_2:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
     LINEITEM_EXTENDEDPRICE:float) +
    (AVG_PRICE_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
      (AggSum([], 
         (((__sql_inline_average_count_2:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
               delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
            ((__sql_inline_average_count_2:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
              -1)) *
           {0 != __sql_inline_average_count_2:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) +
        (AggSum([], 
           (((__sql_inline_average_count_2:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
              ((__sql_inline_average_count_2:int ^=
                 AVG_QTY_mLINEITEM1_L3_1(int)[]
                 [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
                -1)) *
             {0 != __sql_inline_average_count_2:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
          -1)))));
   AVG_DISC(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_3:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_3:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
            -1)) *
         {0 != __sql_inline_average_count_3:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
     LINEITEM_DISCOUNT:float) +
    (AVG_DISC_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
      (AggSum([], 
         (((__sql_inline_average_count_3:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
               delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
            ((__sql_inline_average_count_3:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
              -1)) *
           {0 != __sql_inline_average_count_3:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) +
        (AggSum([], 
           (((__sql_inline_average_count_3:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
              ((__sql_inline_average_count_3:int ^=
                 AVG_QTY_mLINEITEM1_L3_1(int)[]
                 [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
                -1)) *
             {0 != __sql_inline_average_count_3:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
          -1)))));
}

CORRECT AVG_QTY_mLINEITEM5[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_QTY_mLINEITEM5:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (AggSum([], 
     ((__sql_inline_average_count_1:int ^=
        (AVG_QTY_mLINEITEM1_L3_1(int)[]
         [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
          {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
       {0 != __sql_inline_average_count_1:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) +
    (AggSum([], 
       ((__sql_inline_average_count_1:int ^=
          AVG_QTY_mLINEITEM1_L3_1(int)[]
          [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
         {0 != __sql_inline_average_count_1:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
      -1)) *
  delta_AVG_QTY_mLINEITEM5:float);
}

CORRECT AVG_QTY_mLINEITEM1_L3_1[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_QTY_mLINEITEM1_L3_1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_1:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_1:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
            -1)) *
         {0 != __sql_inline_average_count_1:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
     LINEITEM_QUANTITY:float) +
    (AVG_QTY_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
      (AggSum([], 
         (((__sql_inline_average_count_1:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
               delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
            ((__sql_inline_average_count_1:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
              -1)) *
           {0 != __sql_inline_average_count_1:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) +
        (AggSum([], 
           (((__sql_inline_average_count_1:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
              ((__sql_inline_average_count_1:int ^=
                 AVG_QTY_mLINEITEM1_L3_1(int)[]
                 [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
                -1)) *
             {0 != __sql_inline_average_count_1:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
          -1)))));
   AVG_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_2:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_2:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
            -1)) *
         {0 != __sql_inline_average_count_2:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
     LINEITEM_EXTENDEDPRICE:float) +
    (AVG_PRICE_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
      (AggSum([], 
         (((__sql_inline_average_count_2:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
               delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
            ((__sql_inline_average_count_2:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
              -1)) *
           {0 != __sql_inline_average_count_2:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) +
        (AggSum([], 
           (((__sql_inline_average_count_2:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
              ((__sql_inline_average_count_2:int ^=
                 AVG_QTY_mLINEITEM1_L3_1(int)[]
                 [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
                -1)) *
             {0 != __sql_inline_average_count_2:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
          -1)))));
   AVG_DISC(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_3:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_3:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
            -1)) *
         {0 != __sql_inline_average_count_3:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
     LINEITEM_DISCOUNT:float) +
    (AVG_DISC_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
      (AggSum([], 
         (((__sql_inline_average_count_3:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
               delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
            ((__sql_inline_average_count_3:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
              -1)) *
           {0 != __sql_inline_average_count_3:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) +
        (AggSum([], 
           (((__sql_inline_average_count_3:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
              ((__sql_inline_average_count_3:int ^=
                 AVG_QTY_mLINEITEM1_L3_1(int)[]
                 [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
                -1)) *
             {0 != __sql_inline_average_count_3:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
          -1)))));
}

CORRECT AVG_PRICE_mLINEITEM5[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_PRICE_mLINEITEM5:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (AggSum([], 
     ((__sql_inline_average_count_2:int ^=
        (AVG_QTY_mLINEITEM1_L3_1(int)[]
         [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
          {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
       {0 != __sql_inline_average_count_2:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) +
    (AggSum([], 
       ((__sql_inline_average_count_2:int ^=
          AVG_QTY_mLINEITEM1_L3_1(int)[]
          [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
         {0 != __sql_inline_average_count_2:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
      -1)) *
  delta_AVG_PRICE_mLINEITEM5:float);
}

CORRECT AVG_QTY_mLINEITEM1_L3_1[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_QTY_mLINEITEM1_L3_1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_QTY(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_1:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_1:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
            -1)) *
         {0 != __sql_inline_average_count_1:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
     LINEITEM_QUANTITY:float) +
    (AVG_QTY_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
      (AggSum([], 
         (((__sql_inline_average_count_1:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
               delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
            ((__sql_inline_average_count_1:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
              -1)) *
           {0 != __sql_inline_average_count_1:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) +
        (AggSum([], 
           (((__sql_inline_average_count_1:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
              ((__sql_inline_average_count_1:int ^=
                 AVG_QTY_mLINEITEM1_L3_1(int)[]
                 [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
                -1)) *
             {0 != __sql_inline_average_count_1:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
          -1)))));
   AVG_PRICE(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_2:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_2:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
            -1)) *
         {0 != __sql_inline_average_count_2:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
     LINEITEM_EXTENDEDPRICE:float) +
    (AVG_PRICE_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
      (AggSum([], 
         (((__sql_inline_average_count_2:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
               delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
            ((__sql_inline_average_count_2:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
              -1)) *
           {0 != __sql_inline_average_count_2:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) +
        (AggSum([], 
           (((__sql_inline_average_count_2:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
              ((__sql_inline_average_count_2:int ^=
                 AVG_QTY_mLINEITEM1_L3_1(int)[]
                 [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
                -1)) *
             {0 != __sql_inline_average_count_2:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
          -1)))));
   AVG_DISC(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (({LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_3:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
             {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_3:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
            -1)) *
         {0 != __sql_inline_average_count_3:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
     LINEITEM_DISCOUNT:float) +
    (AVG_DISC_mLINEITEM5(float)[]
     [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] *
      (AggSum([], 
         (((__sql_inline_average_count_3:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} +
               delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
            ((__sql_inline_average_count_3:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
              -1)) *
           {0 != __sql_inline_average_count_3:int} *
           {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) +
        (AggSum([], 
           (((__sql_inline_average_count_3:int ^=
               (AVG_QTY_mLINEITEM1_L3_1(int)[]
                [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
                 delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
              ((__sql_inline_average_count_3:int ^=
                 AVG_QTY_mLINEITEM1_L3_1(int)[]
                 [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
                -1)) *
             {0 != __sql_inline_average_count_3:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
          -1)))));
}

CORRECT AVG_DISC_mLINEITEM5[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_DISC_mLINEITEM5:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_DISC(float)[][LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] += 
  ({LINEITEM_LINESTATUS:string = delta_LINEITEM_LINESTATUS:string} *
  {LINEITEM_RETURNFLAG:string = delta_LINEITEM_RETURNFLAG:string} *
  (AggSum([], 
     ((__sql_inline_average_count_3:int ^=
        (AVG_QTY_mLINEITEM1_L3_1(int)[]
         [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string] +
          {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')})) *
       {0 != __sql_inline_average_count_3:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) +
    (AggSum([], 
       ((__sql_inline_average_count_3:int ^=
          AVG_QTY_mLINEITEM1_L3_1(int)[]
          [LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string]) *
         {0 != __sql_inline_average_count_3:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
      -1)) *
  delta_AVG_DISC_mLINEITEM5:float);
}

CORRECT AVG_QTY_mLINEITEM1_L3_1[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_QTY_mLINEITEM1_L3_1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_QTY(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
     (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_1:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [delta_LINEITEM_RETURNFLAG:string,
               delta_LINEITEM_LINESTATUS:string] +
             ((delta_LINEITEM_RETURNFLAG:string ^=
                LINEITEM_RETURNFLAG:string) *
               (delta_LINEITEM_LINESTATUS:string ^=
                 LINEITEM_LINESTATUS:string) *
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_1:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string] +
               ((delta_LINEITEM_RETURNFLAG:string ^=
                  LINEITEM_RETURNFLAG:string) *
                 (delta_LINEITEM_LINESTATUS:string ^=
                   LINEITEM_LINESTATUS:string) *
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
            -1)) *
         {0 != __sql_inline_average_count_1:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
     LINEITEM_QUANTITY:float) +
    (AVG_QTY_mLINEITEM5(float)[]
     [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
      AggSum([], 
        (((__sql_inline_average_count_1:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
           ((__sql_inline_average_count_1:int ^=
              AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string]) *
             -1)) *
          {0 != __sql_inline_average_count_1:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})))) *
   -1) +
  (AVG_QTY_mLINEITEM5(float)[]
   [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
    AggSum([], 
      (((__sql_inline_average_count_1:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [delta_LINEITEM_RETURNFLAG:string,
              delta_LINEITEM_LINESTATUS:string] +
            ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
              (delta_LINEITEM_LINESTATUS:string ^=
                LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
            delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
         ((__sql_inline_average_count_1:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              ((delta_LINEITEM_RETURNFLAG:string ^=
                 LINEITEM_RETURNFLAG:string) *
                (delta_LINEITEM_LINESTATUS:string ^=
                  LINEITEM_LINESTATUS:string) *
                {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
           -1)) *
        {0 != __sql_inline_average_count_1:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))}))));
   AVG_PRICE(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
     (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_2:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [delta_LINEITEM_RETURNFLAG:string,
               delta_LINEITEM_LINESTATUS:string] +
             ((delta_LINEITEM_RETURNFLAG:string ^=
                LINEITEM_RETURNFLAG:string) *
               (delta_LINEITEM_LINESTATUS:string ^=
                 LINEITEM_LINESTATUS:string) *
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_2:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string] +
               ((delta_LINEITEM_RETURNFLAG:string ^=
                  LINEITEM_RETURNFLAG:string) *
                 (delta_LINEITEM_LINESTATUS:string ^=
                   LINEITEM_LINESTATUS:string) *
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
            -1)) *
         {0 != __sql_inline_average_count_2:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
     LINEITEM_EXTENDEDPRICE:float) +
    (AVG_PRICE_mLINEITEM5(float)[]
     [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
      AggSum([], 
        (((__sql_inline_average_count_2:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
           ((__sql_inline_average_count_2:int ^=
              AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string]) *
             -1)) *
          {0 != __sql_inline_average_count_2:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})))) *
   -1) +
  (AVG_PRICE_mLINEITEM5(float)[]
   [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
    AggSum([], 
      (((__sql_inline_average_count_2:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [delta_LINEITEM_RETURNFLAG:string,
              delta_LINEITEM_LINESTATUS:string] +
            ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
              (delta_LINEITEM_LINESTATUS:string ^=
                LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
            delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
         ((__sql_inline_average_count_2:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              ((delta_LINEITEM_RETURNFLAG:string ^=
                 LINEITEM_RETURNFLAG:string) *
                (delta_LINEITEM_LINESTATUS:string ^=
                  LINEITEM_LINESTATUS:string) *
                {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
           -1)) *
        {0 != __sql_inline_average_count_2:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))}))));
   AVG_DISC(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
     (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_3:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [delta_LINEITEM_RETURNFLAG:string,
               delta_LINEITEM_LINESTATUS:string] +
             ((delta_LINEITEM_RETURNFLAG:string ^=
                LINEITEM_RETURNFLAG:string) *
               (delta_LINEITEM_LINESTATUS:string ^=
                 LINEITEM_LINESTATUS:string) *
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_3:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string] +
               ((delta_LINEITEM_RETURNFLAG:string ^=
                  LINEITEM_RETURNFLAG:string) *
                 (delta_LINEITEM_LINESTATUS:string ^=
                   LINEITEM_LINESTATUS:string) *
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
            -1)) *
         {0 != __sql_inline_average_count_3:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
     LINEITEM_DISCOUNT:float) +
    (AVG_DISC_mLINEITEM5(float)[]
     [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
      AggSum([], 
        (((__sql_inline_average_count_3:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
           ((__sql_inline_average_count_3:int ^=
              AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string]) *
             -1)) *
          {0 != __sql_inline_average_count_3:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})))) *
   -1) +
  (AVG_DISC_mLINEITEM5(float)[]
   [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
    AggSum([], 
      (((__sql_inline_average_count_3:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [delta_LINEITEM_RETURNFLAG:string,
              delta_LINEITEM_LINESTATUS:string] +
            ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
              (delta_LINEITEM_LINESTATUS:string ^=
                LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
            delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
         ((__sql_inline_average_count_3:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              ((delta_LINEITEM_RETURNFLAG:string ^=
                 LINEITEM_RETURNFLAG:string) *
                (delta_LINEITEM_LINESTATUS:string ^=
                  LINEITEM_LINESTATUS:string) *
                {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
           -1)) *
        {0 != __sql_inline_average_count_3:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))}))));
}

CORRECT AVG_QTY_mLINEITEM5[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_QTY_mLINEITEM5:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_QTY(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((AggSum([], 
     ((__sql_inline_average_count_1:int ^=
        AVG_QTY_mLINEITEM1_L3_1(int)[]
        [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string]) *
       {0 != __sql_inline_average_count_1:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
    -1) +
   AggSum([], 
     ((__sql_inline_average_count_1:int ^=
        (AVG_QTY_mLINEITEM1_L3_1(int)[]
         [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] +
          ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
            (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
            {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
       {0 != __sql_inline_average_count_1:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))}))) *
  delta_AVG_QTY_mLINEITEM5:float);
}

CORRECT AVG_QTY_mLINEITEM1_L3_1[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_QTY_mLINEITEM1_L3_1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_QTY(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
     (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_1:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [delta_LINEITEM_RETURNFLAG:string,
               delta_LINEITEM_LINESTATUS:string] +
             ((delta_LINEITEM_RETURNFLAG:string ^=
                LINEITEM_RETURNFLAG:string) *
               (delta_LINEITEM_LINESTATUS:string ^=
                 LINEITEM_LINESTATUS:string) *
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_1:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string] +
               ((delta_LINEITEM_RETURNFLAG:string ^=
                  LINEITEM_RETURNFLAG:string) *
                 (delta_LINEITEM_LINESTATUS:string ^=
                   LINEITEM_LINESTATUS:string) *
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
            -1)) *
         {0 != __sql_inline_average_count_1:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
     LINEITEM_QUANTITY:float) +
    (AVG_QTY_mLINEITEM5(float)[]
     [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
      AggSum([], 
        (((__sql_inline_average_count_1:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
           ((__sql_inline_average_count_1:int ^=
              AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string]) *
             -1)) *
          {0 != __sql_inline_average_count_1:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})))) *
   -1) +
  (AVG_QTY_mLINEITEM5(float)[]
   [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
    AggSum([], 
      (((__sql_inline_average_count_1:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [delta_LINEITEM_RETURNFLAG:string,
              delta_LINEITEM_LINESTATUS:string] +
            ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
              (delta_LINEITEM_LINESTATUS:string ^=
                LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
            delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
         ((__sql_inline_average_count_1:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              ((delta_LINEITEM_RETURNFLAG:string ^=
                 LINEITEM_RETURNFLAG:string) *
                (delta_LINEITEM_LINESTATUS:string ^=
                  LINEITEM_LINESTATUS:string) *
                {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
           -1)) *
        {0 != __sql_inline_average_count_1:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))}))));
   AVG_PRICE(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
     (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_2:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [delta_LINEITEM_RETURNFLAG:string,
               delta_LINEITEM_LINESTATUS:string] +
             ((delta_LINEITEM_RETURNFLAG:string ^=
                LINEITEM_RETURNFLAG:string) *
               (delta_LINEITEM_LINESTATUS:string ^=
                 LINEITEM_LINESTATUS:string) *
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_2:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string] +
               ((delta_LINEITEM_RETURNFLAG:string ^=
                  LINEITEM_RETURNFLAG:string) *
                 (delta_LINEITEM_LINESTATUS:string ^=
                   LINEITEM_LINESTATUS:string) *
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
            -1)) *
         {0 != __sql_inline_average_count_2:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
     LINEITEM_EXTENDEDPRICE:float) +
    (AVG_PRICE_mLINEITEM5(float)[]
     [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
      AggSum([], 
        (((__sql_inline_average_count_2:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
           ((__sql_inline_average_count_2:int ^=
              AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string]) *
             -1)) *
          {0 != __sql_inline_average_count_2:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})))) *
   -1) +
  (AVG_PRICE_mLINEITEM5(float)[]
   [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
    AggSum([], 
      (((__sql_inline_average_count_2:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [delta_LINEITEM_RETURNFLAG:string,
              delta_LINEITEM_LINESTATUS:string] +
            ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
              (delta_LINEITEM_LINESTATUS:string ^=
                LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
            delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
         ((__sql_inline_average_count_2:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              ((delta_LINEITEM_RETURNFLAG:string ^=
                 LINEITEM_RETURNFLAG:string) *
                (delta_LINEITEM_LINESTATUS:string ^=
                  LINEITEM_LINESTATUS:string) *
                {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
           -1)) *
        {0 != __sql_inline_average_count_2:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))}))));
   AVG_DISC(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
     (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_3:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [delta_LINEITEM_RETURNFLAG:string,
               delta_LINEITEM_LINESTATUS:string] +
             ((delta_LINEITEM_RETURNFLAG:string ^=
                LINEITEM_RETURNFLAG:string) *
               (delta_LINEITEM_LINESTATUS:string ^=
                 LINEITEM_LINESTATUS:string) *
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_3:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string] +
               ((delta_LINEITEM_RETURNFLAG:string ^=
                  LINEITEM_RETURNFLAG:string) *
                 (delta_LINEITEM_LINESTATUS:string ^=
                   LINEITEM_LINESTATUS:string) *
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
            -1)) *
         {0 != __sql_inline_average_count_3:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
     LINEITEM_DISCOUNT:float) +
    (AVG_DISC_mLINEITEM5(float)[]
     [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
      AggSum([], 
        (((__sql_inline_average_count_3:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
           ((__sql_inline_average_count_3:int ^=
              AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string]) *
             -1)) *
          {0 != __sql_inline_average_count_3:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})))) *
   -1) +
  (AVG_DISC_mLINEITEM5(float)[]
   [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
    AggSum([], 
      (((__sql_inline_average_count_3:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [delta_LINEITEM_RETURNFLAG:string,
              delta_LINEITEM_LINESTATUS:string] +
            ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
              (delta_LINEITEM_LINESTATUS:string ^=
                LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
            delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
         ((__sql_inline_average_count_3:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              ((delta_LINEITEM_RETURNFLAG:string ^=
                 LINEITEM_RETURNFLAG:string) *
                (delta_LINEITEM_LINESTATUS:string ^=
                  LINEITEM_LINESTATUS:string) *
                {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
           -1)) *
        {0 != __sql_inline_average_count_3:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))}))));
}

CORRECT AVG_PRICE_mLINEITEM5[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_PRICE_mLINEITEM5:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_PRICE(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((AggSum([], 
     ((__sql_inline_average_count_2:int ^=
        AVG_QTY_mLINEITEM1_L3_1(int)[]
        [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string]) *
       {0 != __sql_inline_average_count_2:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
    -1) +
   AggSum([], 
     ((__sql_inline_average_count_2:int ^=
        (AVG_QTY_mLINEITEM1_L3_1(int)[]
         [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] +
          ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
            (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
            {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
       {0 != __sql_inline_average_count_2:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))}))) *
  delta_AVG_PRICE_mLINEITEM5:float);
}

CORRECT AVG_QTY_mLINEITEM1_L3_1[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_QTY_mLINEITEM1_L3_1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_QTY(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
     (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_1:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [delta_LINEITEM_RETURNFLAG:string,
               delta_LINEITEM_LINESTATUS:string] +
             ((delta_LINEITEM_RETURNFLAG:string ^=
                LINEITEM_RETURNFLAG:string) *
               (delta_LINEITEM_LINESTATUS:string ^=
                 LINEITEM_LINESTATUS:string) *
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_1:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string] +
               ((delta_LINEITEM_RETURNFLAG:string ^=
                  LINEITEM_RETURNFLAG:string) *
                 (delta_LINEITEM_LINESTATUS:string ^=
                   LINEITEM_LINESTATUS:string) *
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
            -1)) *
         {0 != __sql_inline_average_count_1:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
     LINEITEM_QUANTITY:float) +
    (AVG_QTY_mLINEITEM5(float)[]
     [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
      AggSum([], 
        (((__sql_inline_average_count_1:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
           ((__sql_inline_average_count_1:int ^=
              AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string]) *
             -1)) *
          {0 != __sql_inline_average_count_1:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})))) *
   -1) +
  (AVG_QTY_mLINEITEM5(float)[]
   [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
    AggSum([], 
      (((__sql_inline_average_count_1:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [delta_LINEITEM_RETURNFLAG:string,
              delta_LINEITEM_LINESTATUS:string] +
            ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
              (delta_LINEITEM_LINESTATUS:string ^=
                LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
            delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
         ((__sql_inline_average_count_1:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              ((delta_LINEITEM_RETURNFLAG:string ^=
                 LINEITEM_RETURNFLAG:string) *
                (delta_LINEITEM_LINESTATUS:string ^=
                  LINEITEM_LINESTATUS:string) *
                {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
           -1)) *
        {0 != __sql_inline_average_count_1:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))}))));
   AVG_PRICE(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
     (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_2:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [delta_LINEITEM_RETURNFLAG:string,
               delta_LINEITEM_LINESTATUS:string] +
             ((delta_LINEITEM_RETURNFLAG:string ^=
                LINEITEM_RETURNFLAG:string) *
               (delta_LINEITEM_LINESTATUS:string ^=
                 LINEITEM_LINESTATUS:string) *
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_2:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string] +
               ((delta_LINEITEM_RETURNFLAG:string ^=
                  LINEITEM_RETURNFLAG:string) *
                 (delta_LINEITEM_LINESTATUS:string ^=
                   LINEITEM_LINESTATUS:string) *
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
            -1)) *
         {0 != __sql_inline_average_count_2:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})) *
     LINEITEM_EXTENDEDPRICE:float) +
    (AVG_PRICE_mLINEITEM5(float)[]
     [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
      AggSum([], 
        (((__sql_inline_average_count_2:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
           ((__sql_inline_average_count_2:int ^=
              AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string]) *
             -1)) *
          {0 != __sql_inline_average_count_2:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))})))) *
   -1) +
  (AVG_PRICE_mLINEITEM5(float)[]
   [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
    AggSum([], 
      (((__sql_inline_average_count_2:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [delta_LINEITEM_RETURNFLAG:string,
              delta_LINEITEM_LINESTATUS:string] +
            ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
              (delta_LINEITEM_LINESTATUS:string ^=
                LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
            delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
         ((__sql_inline_average_count_2:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              ((delta_LINEITEM_RETURNFLAG:string ^=
                 LINEITEM_RETURNFLAG:string) *
                (delta_LINEITEM_LINESTATUS:string ^=
                  LINEITEM_LINESTATUS:string) *
                {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
           -1)) *
        {0 != __sql_inline_average_count_2:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_2:int))}))));
   AVG_DISC(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
     (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
     {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} *
     AggSum([], 
       (((__sql_inline_average_count_3:int ^=
           (AVG_QTY_mLINEITEM1_L3_1(int)[]
            [delta_LINEITEM_RETURNFLAG:string,
               delta_LINEITEM_LINESTATUS:string] +
             ((delta_LINEITEM_RETURNFLAG:string ^=
                LINEITEM_RETURNFLAG:string) *
               (delta_LINEITEM_LINESTATUS:string ^=
                 LINEITEM_LINESTATUS:string) *
               {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
             delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
          ((__sql_inline_average_count_3:int ^=
             (AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string] +
               ((delta_LINEITEM_RETURNFLAG:string ^=
                  LINEITEM_RETURNFLAG:string) *
                 (delta_LINEITEM_LINESTATUS:string ^=
                   LINEITEM_LINESTATUS:string) *
                 {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
            -1)) *
         {0 != __sql_inline_average_count_3:int} *
         {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
     LINEITEM_DISCOUNT:float) +
    (AVG_DISC_mLINEITEM5(float)[]
     [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
      AggSum([], 
        (((__sql_inline_average_count_3:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
           ((__sql_inline_average_count_3:int ^=
              AVG_QTY_mLINEITEM1_L3_1(int)[]
              [delta_LINEITEM_RETURNFLAG:string,
                 delta_LINEITEM_LINESTATUS:string]) *
             -1)) *
          {0 != __sql_inline_average_count_3:int} *
          {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})))) *
   -1) +
  (AVG_DISC_mLINEITEM5(float)[]
   [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] *
    AggSum([], 
      (((__sql_inline_average_count_3:int ^=
          (AVG_QTY_mLINEITEM1_L3_1(int)[]
           [delta_LINEITEM_RETURNFLAG:string,
              delta_LINEITEM_LINESTATUS:string] +
            ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
              (delta_LINEITEM_LINESTATUS:string ^=
                LINEITEM_LINESTATUS:string) *
              {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1) +
            delta_AVG_QTY_mLINEITEM1_L3_1:int)) +
         ((__sql_inline_average_count_3:int ^=
            (AVG_QTY_mLINEITEM1_L3_1(int)[]
             [delta_LINEITEM_RETURNFLAG:string,
                delta_LINEITEM_LINESTATUS:string] +
              ((delta_LINEITEM_RETURNFLAG:string ^=
                 LINEITEM_RETURNFLAG:string) *
                (delta_LINEITEM_LINESTATUS:string ^=
                  LINEITEM_LINESTATUS:string) *
                {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
           -1)) *
        {0 != __sql_inline_average_count_3:int} *
        {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))}))));
}

CORRECT AVG_DISC_mLINEITEM5[][delta_LINEITEM_RETURNFLAG:string,delta_LINEITEM_LINESTATUS:string] += delta_AVG_DISC_mLINEITEM5:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_DISC(float)[][delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] += 
  (((AggSum([], 
     ((__sql_inline_average_count_3:int ^=
        AVG_QTY_mLINEITEM1_L3_1(int)[]
        [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string]) *
       {0 != __sql_inline_average_count_3:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))})) *
    -1) +
   AggSum([], 
     ((__sql_inline_average_count_3:int ^=
        (AVG_QTY_mLINEITEM1_L3_1(int)[]
         [delta_LINEITEM_RETURNFLAG:string, delta_LINEITEM_LINESTATUS:string] +
          ((delta_LINEITEM_RETURNFLAG:string ^= LINEITEM_RETURNFLAG:string) *
            (delta_LINEITEM_LINESTATUS:string ^= LINEITEM_LINESTATUS:string) *
            {LINEITEM_SHIPDATE:date <= DATE('1997-9-1')} * -1))) *
       {0 != __sql_inline_average_count_3:int} *
       {[/:float]([listmax:int](1, __sql_inline_average_count_3:int))}))) *
  delta_AVG_DISC_mLINEITEM5:float);
}
