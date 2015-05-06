-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM PART(PART_PARTKEY int, PART_NAME string, PART_MFGR string, PART_BRAND string, PART_TYPE string, PART_SIZE int, PART_CONTAINER string, PART_RETAILPRICE float, PART_COMMENT string)
  FROM FILE 'data/tpch/part.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM SUPPLIER(SUPPLIER_SUPPKEY int, SUPPLIER_NAME string, SUPPLIER_ADDRESS string, SUPPLIER_NATIONKEY int, SUPPLIER_PHONE string, SUPPLIER_ACCTBAL float, SUPPLIER_COMMENT string)
  FROM FILE 'data/tpch/supplier.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM PARTSUPP(PARTSUPP_PARTKEY int, PARTSUPP_SUPPKEY int, PARTSUPP_AVAILQTY int, PARTSUPP_SUPPLYCOST float, PARTSUPP_COMMENT string)
  FROM FILE 'data/tpch/partsupp.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE TABLE NATION(NATION_NATIONKEY int, NATION_NAME string, NATION_REGIONKEY int, NATION_COMMENT string)
  FROM FILE 'data/tpch/nation.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP COUNT(int)[][S_NAME:string, S_ADDRESS:string] := 
AggSum([S_NAME:string, S_ADDRESS:string], 
  ((N_NAME:string ^= 'CANADA') *
    SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
               S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string) *
    EXISTS(
      AggSum([S_SUPPKEY:int], 
        (PARTSUPP(PS_PARTKEY:int, S_SUPPKEY:int, PS_AVAILQTY:int,
                    PS_SUPPLYCOST:float, PS_COMMENT:string) *
          EXISTS(
            AggSum([PS_PARTKEY:int], 
              (PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string,
                      P_BRAND:string, P_TYPE:string, P_SIZE:int,
                      P_CONTAINER:string, P_RETAILPRICE:float,
                      P_COMMENT:string) *
                {0 != [regexp_match:int]('^forest.*$', P_NAME:string)}))) *
          AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (AggSum([PS_PARTKEY:int, S_SUPPKEY:int], 
                  (LINEITEM(L_ORDERKEY:int, PS_PARTKEY:int, S_SUPPKEY:int,
                              L_LINENUMBER:int, L_QUANTITY:float,
                              L_EXTENDEDPRICE:float, L_DISCOUNT:float,
                              L_TAX:float, L_RETURNFLAG:string,
                              L_LINESTATUS:string, L_SHIPDATE:date,
                              L_COMMITDATE:date, L_RECEIPTDATE:date,
                              L_SHIPINSTRUCT:string, L_SHIPMODE:string,
                              L_COMMENT:string) *
                    {L_SHIPDATE:date >= DATE('1994-1-1')} *
                    {L_SHIPDATE:date < DATE('1995-1-1')} * L_QUANTITY:float)) *
                 0.5)) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})))))));

DECLARE MAP COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] := 
AggSum([S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int], 
  ((N_NAME:string ^= 'CANADA') *
    SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
               S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string)));

DECLARE MAP COUNT_mSUPPLIER1(int)[][COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
AggSum([COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((N_NAME:string ^= 'CANADA') *
    NATION(COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int, N_NAME:string,
             N_REGIONKEY:int, N_COMMENT:string)));

DECLARE MAP COUNT_mSUPPLIER2_E1_1(int)[]
[COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] := 
AggSum([COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int], 
  PARTSUPP(PS_PARTKEY:int, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
             PS_AVAILQTY:int, PS_SUPPLYCOST:float, PS_COMMENT:string));

DECLARE MAP COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
[PS_PARTKEY:int, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] := 
AggSum([PS_PARTKEY:int, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
  (LINEITEM(L_ORDERKEY:int, PS_PARTKEY:int,
              COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    {L_SHIPDATE:date >= DATE('1994-1-1')} *
    {L_SHIPDATE:date < DATE('1995-1-1')} * L_QUANTITY:float));

DECLARE MAP COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int] := 
AggSum([PS_PARTKEY:int], 
  (PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
          P_TYPE:string, P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
          P_COMMENT:string) *
    {0 != [regexp_match:int]('^forest.*$', P_NAME:string)}));

-------------------- QUERIES --------------------
DECLARE QUERY COUNT := COUNT(int)[][S_NAME:string, S_ADDRESS:string];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, LINEITEM_SUPPKEY:int] *
  (EXISTS(
     ((EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) *
        (AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                 ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                   {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                   LINEITEM_QUANTITY:float)) *
                0.5)) *
             COUNT_mSUPPLIER2_E1_1(int)[]
             [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
               COUNT_mSUPPLIER2_E1_1(int)[]
               [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
            -1))) +
       AggSum([LINEITEM_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float})))) +
    (EXISTS(
       AggSum([LINEITEM_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)));
   COUNT_mSUPPLIER2_E1_1_L1_1(float)[][LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * LINEITEM_QUANTITY:float);
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (AggSum([S_NAME:string, S_ADDRESS:string], 
   (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
     EXISTS(
       AggSum([S_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5) +
               ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 (PS_PARTKEY:int ^= LINEITEM_PARTKEY:int) *
                 {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                 {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                 LINEITEM_QUANTITY:float))) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
  (AggSum([S_NAME:string, S_ADDRESS:string], 
     (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
       EXISTS(
         AggSum([S_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) *
    -1));
   COUNT_mSUPPLIER2_E1_1_L1_1(float)[][LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -1 * LINEITEM_QUANTITY:float);
}

ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (AggSum([S_NAME:string, S_ADDRESS:string], 
   (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
     EXISTS(
       ((AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PART_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
             COUNT_mSUPPLIER2_E1_1(int)[]
             [S_SUPPKEY:int, PART_PARTKEY:int, PS_AVAILQTY:int] *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
          (EXISTS(
             (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
               {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
            (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) *
              -1))) +
         AggSum([S_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})))))) +
  (AggSum([S_NAME:string, S_ADDRESS:string], 
     (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
       EXISTS(
         AggSum([S_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) *
    -1));
   COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] += {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)};
}

ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (AggSum([S_NAME:string, S_ADDRESS:string], 
   (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
     EXISTS(
       AggSum([S_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
           EXISTS(
             (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int] +
               ((PS_PARTKEY:int ^= PART_PARTKEY:int) *
                 {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                 -1))) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
  (AggSum([S_NAME:string, S_ADDRESS:string], 
     (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
       EXISTS(
         AggSum([S_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) *
    -1));
   COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] += ({0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} * -1);
}

ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  (COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int] *
  EXISTS(
    AggSum([SUPPLIER_SUPPKEY:int], 
      (COUNT_mSUPPLIER2_E1_1(int)[]
       [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
        (__sql_inline_agg_1:float ^=
          (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
           [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
        EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
        {PS_AVAILQTY:int > __sql_inline_agg_1:float}))));
   COUNT_mPARTSUPP1(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_SUPPKEY:int] += COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int];
}

ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  (COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int] *
  EXISTS(
    AggSum([SUPPLIER_SUPPKEY:int], 
      (COUNT_mSUPPLIER2_E1_1(int)[]
       [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
        (__sql_inline_agg_1:float ^=
          (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
           [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
        EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
        {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
  -1);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_SUPPKEY:int] += (COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int] * -1);
}

ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, PARTSUPP_SUPPKEY:int] *
  (EXISTS(
     (AggSum([PARTSUPP_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
            {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
         EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int])))) +
    (EXISTS(
       AggSum([PARTSUPP_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)));
   COUNT_mSUPPLIER2_E1_1(int)[][PARTSUPP_SUPPKEY:int, PARTSUPP_PARTKEY:int, PARTSUPP_AVAILQTY:int] += 1;
}

ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (AggSum([S_NAME:string, S_ADDRESS:string], 
   (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
     EXISTS(
       (AggSum([S_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
         ((S_SUPPKEY:int ^= PARTSUPP_SUPPKEY:int) *
           AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [PARTSUPP_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
               {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
           -1))))) +
  (AggSum([S_NAME:string, S_ADDRESS:string], 
     (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
       EXISTS(
         AggSum([S_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) *
    -1));
   COUNT_mSUPPLIER2_E1_1(int)[][PARTSUPP_SUPPKEY:int, PARTSUPP_PARTKEY:int, PARTSUPP_AVAILQTY:int] += -1;
}

ON SYSTEM READY {
   COUNT_mSUPPLIER1(int)[][COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
  AggSum([COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((N_NAME:string ^= 'CANADA') *
    NATION(COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int, N_NAME:string,
             N_REGIONKEY:int, N_COMMENT:string)));
}

CORRECT COUNT_mPARTSUPP1[][delta_S_NAME:string,delta_S_ADDRESS:string,delta_S_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][delta_S_NAME:string, delta_S_ADDRESS:string] += 
  ({LINEITEM_SUPPKEY:int = delta_S_SUPPKEY:int} *
  (EXISTS(
     ((EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) *
        (AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                 ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                   {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                   LINEITEM_QUANTITY:float)) *
                0.5)) *
             COUNT_mSUPPLIER2_E1_1(int)[]
             [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
               COUNT_mSUPPLIER2_E1_1(int)[]
               [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
            -1))) +
       AggSum([LINEITEM_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float})))) +
    (EXISTS(
       AggSum([LINEITEM_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)) *
  delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mSUPPLIER2_E1_1_E2_1[][delta_PS_PARTKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_E2_1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, LINEITEM_SUPPKEY:int] *
  (((EXISTS(
       ((EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) *
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                   ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                     {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                     LINEITEM_QUANTITY:float)) *
                  0.5)) *
               COUNT_mSUPPLIER2_E1_1(int)[]
               [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
            (AggSum([], 
               ((__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                 COUNT_mSUPPLIER2_E1_1(int)[]
                 [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
                 {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
              -1))) +
         AggSum([LINEITEM_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})))) +
      EXISTS(
        (AggSum([LINEITEM_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          ((EXISTS(
              (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
             (EXISTS(
                COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
               -1)) *
            AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [delta_PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [LINEITEM_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
      (EXISTS(
         AggSum([LINEITEM_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      (((AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                 ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                   {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                   LINEITEM_QUANTITY:float)) *
                0.5)) *
             COUNT_mSUPPLIER2_E1_1(int)[]
             [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
               COUNT_mSUPPLIER2_E1_1(int)[]
               [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
            -1)) *
         (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) +
           ((LINEITEM_PARTKEY:int ^= delta_PS_PARTKEY:int) *
             (EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int] +
                  delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
               (EXISTS(
                  COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) *
                 -1))))) +
        AggSum([LINEITEM_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        ((EXISTS(
            (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
              delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
           (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
             -1)) *
          AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [delta_PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
              COUNT_mSUPPLIER2_E1_1(int)[]
              [LINEITEM_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})))))));
}

CORRECT COUNT_mSUPPLIER2_E1_1_L1_1[][delta_PS_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_L1_1:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  ({LINEITEM_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mPARTSUPP1(int)[]
  [S_NAME:string, S_ADDRESS:string, LINEITEM_SUPPKEY:int] *
  (((EXISTS(
       ((EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) *
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                   ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                     {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                     LINEITEM_QUANTITY:float)) *
                  0.5)) *
               COUNT_mSUPPLIER2_E1_1(int)[]
               [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
            (AggSum([], 
               ((__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                 COUNT_mSUPPLIER2_E1_1(int)[]
                 [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
                 {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
              -1))) +
         AggSum([LINEITEM_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})))) +
      EXISTS(
        (AggSum([LINEITEM_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            AggSum([LINEITEM_SUPPKEY:int, delta_PS_PARTKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [LINEITEM_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                ((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [delta_PS_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                      delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                     0.5)) +
                  ((__sql_inline_agg_1:float ^=
                     (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [delta_PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 
                     0.5)) *
                    -1)) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
      (EXISTS(
         AggSum([LINEITEM_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      ((EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) *
         (AggSum([], 
            ((__sql_inline_agg_1:float ^=
               ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                  ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                    {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                    LINEITEM_QUANTITY:float)) *
                 0.5)) *
              COUNT_mSUPPLIER2_E1_1(int)[]
              [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
           (AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
             -1) +
           ((LINEITEM_PARTKEY:int ^= delta_PS_PARTKEY:int) *
             (AggSum([], 
                (((__sql_inline_agg_1:float ^=
                    ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                       ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                         {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                         LINEITEM_QUANTITY:float) +
                       delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                      0.5)) +
                   ((__sql_inline_agg_1:float ^=
                      ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                        [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                         ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                           {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                           LINEITEM_QUANTITY:float)) *
                        0.5)) *
                     -1)) *
                  COUNT_mSUPPLIER2_E1_1(int)[]
                  [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int,
                     PS_AVAILQTY:int] *
                  {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
               (AggSum([], 
                  (((__sql_inline_agg_1:float ^=
                      ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                        [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                         delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                        0.5)) +
                     ((__sql_inline_agg_1:float ^=
                        (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                         [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 
                        0.5)) *
                       -1)) *
                    COUNT_mSUPPLIER2_E1_1(int)[]
                    [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int,
                       PS_AVAILQTY:int] *
                    {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
                 -1))))) +
        AggSum([LINEITEM_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
          AggSum([LINEITEM_SUPPKEY:int, delta_PS_PARTKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [LINEITEM_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
              ((__sql_inline_agg_1:float ^=
                 ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [delta_PS_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                    delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                   0.5)) +
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                  -1)) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})))))));
}

CORRECT COUNT_mSUPPLIER2_E1_1[][delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_PARTKEY:int,delta_PS_AVAILQTY:int] += delta_COUNT_mSUPPLIER2_E1_1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, LINEITEM_SUPPKEY:int] *
  (((EXISTS(
       ((EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) *
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                   ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                     {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                     LINEITEM_QUANTITY:float)) *
                  0.5)) *
               COUNT_mSUPPLIER2_E1_1(int)[]
               [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
            (AggSum([], 
               ((__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                 COUNT_mSUPPLIER2_E1_1(int)[]
                 [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
                 {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
              -1))) +
         AggSum([LINEITEM_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})))) +
      ((LINEITEM_SUPPKEY:int ^= delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int) *
        (EXISTS(
           (AggSum([LINEITEM_SUPPKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
                (__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
             (AggSum([], 
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                  EXISTS(
                    COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
                  {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
               delta_COUNT_mSUPPLIER2_E1_1:int))) +
          (EXISTS(
             AggSum([LINEITEM_SUPPKEY:int], 
               (COUNT_mSUPPLIER2_E1_1(int)[]
                [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
                 (__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                 EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
                 {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
            -1)))) *
     -1) +
    EXISTS(
      ((EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) *
         (AggSum([], 
            ((__sql_inline_agg_1:float ^=
               ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                  ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                    {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                    LINEITEM_QUANTITY:float)) *
                 0.5)) *
              COUNT_mSUPPLIER2_E1_1(int)[]
              [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
           (AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [LINEITEM_SUPPKEY:int, LINEITEM_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
             -1))) +
        AggSum([LINEITEM_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [LINEITEM_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        (((EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][LINEITEM_PARTKEY:int]) *
            (AggSum([], 
               ((__sql_inline_agg_1:float ^=
                  ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] +
                     ({LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                       {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} *
                       LINEITEM_QUANTITY:float)) *
                    0.5)) *
                 (LINEITEM_SUPPKEY:int ^=
                   delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int) *
                 (LINEITEM_PARTKEY:int ^= delta_PS_PARTKEY:int) *
                 {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
              (AggSum([], 
                 ((__sql_inline_agg_1:float ^=
                    (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                   (LINEITEM_SUPPKEY:int ^=
                     delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int) *
                   (LINEITEM_PARTKEY:int ^= delta_PS_PARTKEY:int) *
                   {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
                -1))) +
           ((LINEITEM_SUPPKEY:int ^=
              delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int) *
             AggSum([], 
               ((__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [delta_PS_PARTKEY:int, LINEITEM_SUPPKEY:int] * 0.5)) *
                 EXISTS(
                   COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
                 {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})))) *
          delta_COUNT_mSUPPLIER2_E1_1:int)))));
}

CORRECT COUNT_mPARTSUPP1[][delta_S_NAME:string,delta_S_ADDRESS:string,delta_S_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][delta_S_NAME:string, delta_S_ADDRESS:string] += 
  ((EXISTS(
    AggSum([delta_S_SUPPKEY:int], 
      (COUNT_mSUPPLIER2_E1_1(int)[]
       [delta_S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
        (__sql_inline_agg_1:float ^=
          ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
            [PS_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5) +
            ((PS_PARTKEY:int ^= LINEITEM_PARTKEY:int) *
              {LINEITEM_SUPPKEY:int = delta_S_SUPPKEY:int} *
              {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
              {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
              LINEITEM_QUANTITY:float))) *
        EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
        {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) +
   (EXISTS(
      AggSum([delta_S_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [delta_S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
     -1)) *
  delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mSUPPLIER2_E1_1[][delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_PARTKEY:int,delta_PS_AVAILQTY:int] += delta_COUNT_mSUPPLIER2_E1_1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
  (((EXISTS(
       AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
             PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5) +
               ((PS_PARTKEY:int ^= LINEITEM_PARTKEY:int) *
                 {LINEITEM_SUPPKEY:int =
                 delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
                 {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                 {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                 LINEITEM_QUANTITY:float))) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) +
      EXISTS(
        (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [delta_PS_PARTKEY:int,
                    delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                  0.5)) *
               EXISTS(
                 COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
               {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
            delta_COUNT_mSUPPLIER2_E1_1:int))) +
      (EXISTS(
         AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
             PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5) +
               ((PS_PARTKEY:int ^= LINEITEM_PARTKEY:int) *
                 {LINEITEM_SUPPKEY:int =
                 delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
                 {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                 {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                 LINEITEM_QUANTITY:float))) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        (AggSum([], 
           ((__sql_inline_agg_1:float ^=
              ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [delta_PS_PARTKEY:int,
                   delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5) +
                ({LINEITEM_PARTKEY:int = delta_PS_PARTKEY:int} *
                  {LINEITEM_SUPPKEY:int =
                  delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
                  {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                  {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                  LINEITEM_QUANTITY:float))) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
             {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
          delta_COUNT_mSUPPLIER2_E1_1:int)))));
}

CORRECT COUNT_mSUPPLIER2_E1_1_L1_1[][delta_PS_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_L1_1:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
  (((EXISTS(
       AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
             PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5) +
               ((PS_PARTKEY:int ^= LINEITEM_PARTKEY:int) *
                 {LINEITEM_SUPPKEY:int =
                 delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
                 {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                 {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                 LINEITEM_QUANTITY:float))) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) +
      EXISTS(
        (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                      delta_PS_PARTKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                  delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                ((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [delta_PS_PARTKEY:int,
                        delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                      delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                     0.5)) +
                  ((__sql_inline_agg_1:float ^=
                     (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [delta_PS_PARTKEY:int,
                         delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                       0.5)) *
                    -1)) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
      (EXISTS(
         AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
             PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5) +
               ((PS_PARTKEY:int ^= LINEITEM_PARTKEY:int) *
                 {LINEITEM_SUPPKEY:int =
                 delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
                 {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                 {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                 LINEITEM_QUANTITY:float))) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
          AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                    delta_PS_PARTKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
              ((__sql_inline_agg_1:float ^=
                 (((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int,
                       delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                     delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                    0.5) +
                   ({LINEITEM_PARTKEY:int = delta_PS_PARTKEY:int} *
                     {LINEITEM_SUPPKEY:int =
                     delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
                     {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                     {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                     LINEITEM_QUANTITY:float))) +
                ((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [delta_PS_PARTKEY:int,
                        delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                      0.5) +
                     ({LINEITEM_PARTKEY:int = delta_PS_PARTKEY:int} *
                       {LINEITEM_SUPPKEY:int =
                       delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
                       {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                       {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * 
                     -0.5 * LINEITEM_QUANTITY:float))) *
                  -1)) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})))))));
}

CORRECT COUNT_mSUPPLIER2_E1_1_E2_1[][delta_PS_PARTKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_E2_1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (AggSum([S_NAME:string, S_ADDRESS:string], 
   (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
     (EXISTS(
        (AggSum([S_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5) +
                 ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                   (PS_PARTKEY:int ^= LINEITEM_PARTKEY:int) *
                   {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                   {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                   LINEITEM_QUANTITY:float))) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          ((EXISTS(
              (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
             (EXISTS(
                COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
               -1)) *
            AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [delta_PS_PARTKEY:int, S_SUPPKEY:int] * 0.5) +
                   ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                     {LINEITEM_PARTKEY:int = delta_PS_PARTKEY:int} *
                     {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                     {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                     LINEITEM_QUANTITY:float))) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [S_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
       (EXISTS(
          AggSum([S_SUPPKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
              (__sql_inline_agg_1:float ^=
                ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5) +
                  ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                    (PS_PARTKEY:int ^= LINEITEM_PARTKEY:int) *
                    {LINEITEM_SHIPDATE:date >= DATE('1994-1-1')} *
                    {LINEITEM_SHIPDATE:date < DATE('1995-1-1')} * -0.5 *
                    LINEITEM_QUANTITY:float))) *
              EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
         -1)))) +
  (AggSum([S_NAME:string, S_ADDRESS:string], 
     (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
       (EXISTS(
          (AggSum([S_SUPPKEY:int], 
             (COUNT_mSUPPLIER2_E1_1(int)[]
              [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
               (__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
               EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
            ((EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                  delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
               (EXISTS(
                  COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
                 -1)) *
              AggSum([], 
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                  COUNT_mSUPPLIER2_E1_1(int)[]
                  [S_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                  {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
         (EXISTS(
            AggSum([S_SUPPKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
                (__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
           -1)))) *
    -1));
}

CORRECT COUNT_mPARTSUPP1[][delta_S_NAME:string,delta_S_ADDRESS:string,delta_S_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][delta_S_NAME:string, delta_S_ADDRESS:string] += 
  ((EXISTS(
    ((AggSum([], 
        ((__sql_inline_agg_1:float ^=
           (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
            [PART_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5)) *
          COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_S_SUPPKEY:int, PART_PARTKEY:int, PS_AVAILQTY:int] *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
       (EXISTS(
          (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
            {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
         (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) * -1))) +
      AggSum([delta_S_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [delta_S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})))) +
   (EXISTS(
      AggSum([delta_S_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [delta_S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
     -1)) *
  delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mSUPPLIER2_E1_1_L1_1[][delta_PS_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_L1_1:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
  (((EXISTS(
       ((AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PART_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5)) *
             COUNT_mSUPPLIER2_E1_1(int)[]
             [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PART_PARTKEY:int,
                PS_AVAILQTY:int] *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
          (EXISTS(
             (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
               {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
            (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) *
              -1))) +
         AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})))) +
      EXISTS(
        (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                      delta_PS_PARTKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                  delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                ((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [delta_PS_PARTKEY:int,
                        delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                      delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                     0.5)) +
                  ((__sql_inline_agg_1:float ^=
                     (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [delta_PS_PARTKEY:int,
                         delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                       0.5)) *
                    -1)) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
      (EXISTS(
         AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      (((EXISTS(
           (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
             {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
          (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) * -1)) *
         (AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PART_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
              COUNT_mSUPPLIER2_E1_1(int)[]
              [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PART_PARTKEY:int,
                 PS_AVAILQTY:int] *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
           ((PART_PARTKEY:int ^= delta_PS_PARTKEY:int) *
             AggSum([], 
               (((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [PART_PARTKEY:int,
                        delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                      delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                     0.5)) +
                  ((__sql_inline_agg_1:float ^=
                     (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [PART_PARTKEY:int,
                         delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                       0.5)) *
                    -1)) *
                 COUNT_mSUPPLIER2_E1_1(int)[]
                 [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                    PART_PARTKEY:int, PS_AVAILQTY:int] *
                 {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
        AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
              PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
          AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                    delta_PS_PARTKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
              ((__sql_inline_agg_1:float ^=
                 ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [delta_PS_PARTKEY:int,
                      delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                    delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                   0.5)) +
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int,
                       delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                     0.5)) *
                  -1)) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})))))));
}

CORRECT COUNT_mSUPPLIER2_E1_1[][delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_PARTKEY:int,delta_PS_AVAILQTY:int] += delta_COUNT_mSUPPLIER2_E1_1:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (AggSum([S_NAME:string, S_ADDRESS:string], 
   (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
     (EXISTS(
        ((AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PART_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
              COUNT_mSUPPLIER2_E1_1(int)[]
              [S_SUPPKEY:int, PART_PARTKEY:int, PS_AVAILQTY:int] *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
           (EXISTS(
              (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
                {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
             (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) *
               -1))) +
          AggSum([S_SUPPKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
              (__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
              EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (((AggSum([], 
               ((S_SUPPKEY:int ^= delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int) *
                 (__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [PART_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                 (PART_PARTKEY:int ^= delta_PS_PARTKEY:int) *
                 {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
              (EXISTS(
                 (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
                   {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
                (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) *
                  -1))) +
             ((S_SUPPKEY:int ^= delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int) *
               AggSum([], 
                 ((__sql_inline_agg_1:float ^=
                    (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [delta_PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                   EXISTS(
                     COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
                   {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})))) *
            delta_COUNT_mSUPPLIER2_E1_1:int))) +
       (EXISTS(
          ((AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [PART_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [S_SUPPKEY:int, PART_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
             (EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
                  {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
               (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) *
                 -1))) +
            AggSum([S_SUPPKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
                (__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float})))) *
         -1)))) +
  (COUNT_mPARTSUPP1(int)[]
   [S_NAME:string, S_ADDRESS:string,
      delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
    (EXISTS(
       (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
              PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
         (AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [delta_PS_PARTKEY:int,
                   delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
              EXISTS(
                COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
              {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
           delta_COUNT_mSUPPLIER2_E1_1:int))) +
      (EXISTS(
         AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
    -1));
}

CORRECT COUNT_mSUPPLIER2_E1_1_E2_1[][delta_PS_PARTKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_E2_1:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (AggSum([S_NAME:string, S_ADDRESS:string], 
   (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
     (EXISTS(
        ((AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PART_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
              COUNT_mSUPPLIER2_E1_1(int)[]
              [S_SUPPKEY:int, PART_PARTKEY:int, PS_AVAILQTY:int] *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
           (EXISTS(
              (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
                {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
             (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) *
               -1) +
             ((PART_PARTKEY:int ^= delta_PS_PARTKEY:int) *
               (((EXISTS(
                    (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
                      {0 !=
                      [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
                   EXISTS(
                     (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
                       delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
                   (EXISTS(
                      COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) *
                     -1)) *
                  -1) +
                 EXISTS(
                   (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
                     {0 !=
                     [regexp_match:int]('^forest.*$', PART_NAME:string)} +
                     delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)))))) +
          AggSum([S_SUPPKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
              (__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
              EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          ((EXISTS(
              (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
             (EXISTS(
                COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
               -1)) *
            AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [delta_PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [S_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
       (EXISTS(
          ((AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [PART_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [S_SUPPKEY:int, PART_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
             (EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int] +
                  {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)})) +
               (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PART_PARTKEY:int]) *
                 -1))) +
            AggSum([S_SUPPKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
                (__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float})))) *
         -1)))) +
  (AggSum([S_NAME:string, S_ADDRESS:string], 
     (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
       (EXISTS(
          (AggSum([S_SUPPKEY:int], 
             (COUNT_mSUPPLIER2_E1_1(int)[]
              [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
               (__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
               EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
            ((EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                  delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
               (EXISTS(
                  COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
                 -1)) *
              AggSum([], 
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                  COUNT_mSUPPLIER2_E1_1(int)[]
                  [S_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                  {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
         (EXISTS(
            AggSum([S_SUPPKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
                (__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
           -1)))) *
    -1));
}

CORRECT COUNT_mPARTSUPP1[][delta_S_NAME:string,delta_S_ADDRESS:string,delta_S_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][delta_S_NAME:string, delta_S_ADDRESS:string] += 
  ((EXISTS(
    AggSum([delta_S_SUPPKEY:int], 
      (COUNT_mSUPPLIER2_E1_1(int)[]
       [delta_S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
        (__sql_inline_agg_1:float ^=
          (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
           [PS_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5)) *
        EXISTS(
          (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int] +
            ((PS_PARTKEY:int ^= PART_PARTKEY:int) *
              {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} * 
            -1))) *
        {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) +
   (EXISTS(
      AggSum([delta_S_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [delta_S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
     -1)) *
  delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mSUPPLIER2_E1_1[][delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_PARTKEY:int,delta_PS_AVAILQTY:int] += delta_COUNT_mSUPPLIER2_E1_1:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
  (((EXISTS(
       AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
             PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
               0.5)) *
           EXISTS(
             (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int] +
               ((PS_PARTKEY:int ^= PART_PARTKEY:int) *
                 {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                 -1))) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) +
      EXISTS(
        (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [delta_PS_PARTKEY:int,
                    delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                  0.5)) *
               EXISTS(
                 COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
               {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
            delta_COUNT_mSUPPLIER2_E1_1:int))) +
      (EXISTS(
         AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
             PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
               0.5)) *
           EXISTS(
             (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int] +
               ((PS_PARTKEY:int ^= PART_PARTKEY:int) *
                 {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                 -1))) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        (EXISTS(
           (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
             ({PART_PARTKEY:int = delta_PS_PARTKEY:int} *
               {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
               -1))) *
          AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [delta_PS_PARTKEY:int,
                   delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
              {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
          delta_COUNT_mSUPPLIER2_E1_1:int)))));
}

CORRECT COUNT_mSUPPLIER2_E1_1_L1_1[][delta_PS_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_L1_1:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
  (((EXISTS(
       AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
             PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
               0.5)) *
           EXISTS(
             (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int] +
               ((PS_PARTKEY:int ^= PART_PARTKEY:int) *
                 {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                 -1))) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) +
      EXISTS(
        (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                      delta_PS_PARTKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                  delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                ((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [delta_PS_PARTKEY:int,
                        delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                      delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                     0.5)) +
                  ((__sql_inline_agg_1:float ^=
                     (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [delta_PS_PARTKEY:int,
                         delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                       0.5)) *
                    -1)) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
      (EXISTS(
         AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
             PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
               0.5)) *
           EXISTS(
             (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int] +
               ((PS_PARTKEY:int ^= PART_PARTKEY:int) *
                 {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                 -1))) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                  delta_PS_PARTKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, delta_PS_PARTKEY:int,
              PS_AVAILQTY:int] *
            ((__sql_inline_agg_1:float ^=
               ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [delta_PS_PARTKEY:int,
                    delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                  delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                 0.5)) +
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [delta_PS_PARTKEY:int,
                     delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                   0.5)) *
                -1)) *
            EXISTS(
              (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                ({PART_PARTKEY:int = delta_PS_PARTKEY:int} *
                  {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                  -1))) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))));
}

CORRECT COUNT_mSUPPLIER2_E1_1_E2_1[][delta_PS_PARTKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_E2_1:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (AggSum([S_NAME:string, S_ADDRESS:string], 
   (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
     (EXISTS(
        (AggSum([S_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
             EXISTS(
               (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int] +
                 ((PS_PARTKEY:int ^= PART_PARTKEY:int) *
                   {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                   -1))) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          ((EXISTS(
              (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                ({PART_PARTKEY:int = delta_PS_PARTKEY:int} *
                  {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                  -1) +
                delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
             (EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                  ({PART_PARTKEY:int = delta_PS_PARTKEY:int} *
                    {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                    -1))) *
               -1)) *
            AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [delta_PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [S_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
       (EXISTS(
          AggSum([S_SUPPKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
              (__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
              EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int] +
                  ((PS_PARTKEY:int ^= PART_PARTKEY:int) *
                    {0 != [regexp_match:int]('^forest.*$', PART_NAME:string)} *
                    -1))) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
         -1)))) +
  (AggSum([S_NAME:string, S_ADDRESS:string], 
     (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
       (EXISTS(
          (AggSum([S_SUPPKEY:int], 
             (COUNT_mSUPPLIER2_E1_1(int)[]
              [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
               (__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
               EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
            ((EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                  delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
               (EXISTS(
                  COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
                 -1)) *
              AggSum([], 
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                  COUNT_mSUPPLIER2_E1_1(int)[]
                  [S_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                  {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
         (EXISTS(
            AggSum([S_SUPPKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
                (__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
           -1)))) *
    -1));
}

CORRECT COUNT_mSUPPLIER1[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  EXISTS(
    AggSum([SUPPLIER_SUPPKEY:int], 
      (COUNT_mSUPPLIER2_E1_1(int)[]
       [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
        (__sql_inline_agg_1:float ^=
          (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
           [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
        EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
        {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER2_E1_1[][delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_PARTKEY:int,delta_PS_AVAILQTY:int] += delta_COUNT_mSUPPLIER2_E1_1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int] *
  (EXISTS(
     (AggSum([SUPPLIER_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [delta_PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
         delta_COUNT_mSUPPLIER2_E1_1:int))) +
    (EXISTS(
       AggSum([SUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)));
}

CORRECT COUNT_mSUPPLIER2_E1_1_L1_1[][delta_PS_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_L1_1:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int] *
  (EXISTS(
     (AggSum([SUPPLIER_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
       (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
         AggSum([SUPPLIER_SUPPKEY:int, delta_PS_PARTKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [SUPPLIER_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
             ((__sql_inline_agg_1:float ^=
                ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [delta_PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                   delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                  0.5)) +
               ((__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [delta_PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
                 -1)) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
    (EXISTS(
       AggSum([SUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)));
}

CORRECT COUNT_mSUPPLIER2_E1_1_E2_1[][delta_PS_PARTKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_E2_1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  (COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int] *
  (EXISTS(
     (AggSum([SUPPLIER_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
       ((EXISTS(
           (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
             delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
          (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            -1)) *
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [delta_PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
             COUNT_mSUPPLIER2_E1_1(int)[]
             [SUPPLIER_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
    (EXISTS(
       AggSum([SUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)));
}

CORRECT COUNT_mSUPPLIER1[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  EXISTS(
    AggSum([SUPPLIER_SUPPKEY:int], 
      (COUNT_mSUPPLIER2_E1_1(int)[]
       [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
        (__sql_inline_agg_1:float ^=
          (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
           [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
        EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
        {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER1[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  EXISTS(
    AggSum([SUPPLIER_SUPPKEY:int], 
      (COUNT_mSUPPLIER2_E1_1(int)[]
       [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
        (__sql_inline_agg_1:float ^=
          (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
           [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
        EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
        {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER2_E1_1[][delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_PARTKEY:int,delta_PS_AVAILQTY:int] += delta_COUNT_mSUPPLIER2_E1_1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int] *
  (EXISTS(
     (AggSum([SUPPLIER_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [delta_PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
         delta_COUNT_mSUPPLIER2_E1_1:int))) +
    (EXISTS(
       AggSum([SUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)) *
  -1);
}

CORRECT COUNT_mSUPPLIER2_E1_1_L1_1[][delta_PS_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_L1_1:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int] *
  (EXISTS(
     (AggSum([SUPPLIER_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
       (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
         AggSum([SUPPLIER_SUPPKEY:int, delta_PS_PARTKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [SUPPLIER_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
             ((__sql_inline_agg_1:float ^=
                ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [delta_PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] +
                   delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                  0.5)) +
               ((__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [delta_PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
                 -1)) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
    (EXISTS(
       AggSum([SUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)) *
  -1);
}

CORRECT COUNT_mSUPPLIER2_E1_1_E2_1[][delta_PS_PARTKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_E2_1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  (COUNT_mSUPPLIER1(int)[][SUPPLIER_NATIONKEY:int] *
  (EXISTS(
     (AggSum([SUPPLIER_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
       ((EXISTS(
           (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
             delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
          (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            -1)) *
         AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [delta_PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
             COUNT_mSUPPLIER2_E1_1(int)[]
             [SUPPLIER_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
    (EXISTS(
       AggSum([SUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)) *
  -1);
}

CORRECT COUNT_mSUPPLIER1[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  EXISTS(
    AggSum([SUPPLIER_SUPPKEY:int], 
      (COUNT_mSUPPLIER2_E1_1(int)[]
       [SUPPLIER_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
        (__sql_inline_agg_1:float ^=
          (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
           [PS_PARTKEY:int, SUPPLIER_SUPPKEY:int] * 0.5)) *
        EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
        {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mPARTSUPP1[][delta_S_NAME:string,delta_S_ADDRESS:string,delta_S_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_NAME:string, delta_S_ADDRESS:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_S_SUPPKEY:int} *
  (EXISTS(
     (AggSum([PARTSUPP_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
       (AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
            {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
         EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int])))) +
    (EXISTS(
       AggSum([PARTSUPP_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
      -1)) *
  delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mSUPPLIER2_E1_1[][delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_PARTKEY:int,delta_PS_AVAILQTY:int] += delta_COUNT_mSUPPLIER2_E1_1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mPARTSUPP1(int)[]
  [S_NAME:string, S_ADDRESS:string, PARTSUPP_SUPPKEY:int] *
  (((EXISTS(
       (AggSum([PARTSUPP_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
         (AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
              {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int])))) +
      EXISTS(
        (AggSum([PARTSUPP_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [delta_PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
               EXISTS(
                 COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
               {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
            delta_COUNT_mSUPPLIER2_E1_1:int))) +
      (EXISTS(
         AggSum([PARTSUPP_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      (AggSum([PARTSUPP_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        (AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
             {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int])) +
        (AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [delta_PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
             {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
          delta_COUNT_mSUPPLIER2_E1_1:int)))));
}

CORRECT COUNT_mSUPPLIER2_E1_1_L1_1[][delta_PS_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_L1_1:float FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mPARTSUPP1(int)[]
  [S_NAME:string, S_ADDRESS:string, PARTSUPP_SUPPKEY:int] *
  (((EXISTS(
       (AggSum([PARTSUPP_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
         (AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
              {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int])))) +
      EXISTS(
        (AggSum([PARTSUPP_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            AggSum([PARTSUPP_SUPPKEY:int, delta_PS_PARTKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [PARTSUPP_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                ((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [delta_PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] +
                      delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                     0.5)) +
                  ((__sql_inline_agg_1:float ^=
                     (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [delta_PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 
                     0.5)) *
                    -1)) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
      (EXISTS(
         AggSum([PARTSUPP_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      ((EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
         (AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
              {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) +
           ((PARTSUPP_PARTKEY:int ^= delta_PS_PARTKEY:int) *
             AggSum([], 
               (((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] +
                      delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                     0.5)) +
                  ((__sql_inline_agg_1:float ^=
                     (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 
                     0.5)) *
                    -1)) *
                 {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
        AggSum([PARTSUPP_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
          AggSum([PARTSUPP_SUPPKEY:int, delta_PS_PARTKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [PARTSUPP_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
              ((__sql_inline_agg_1:float ^=
                 ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [delta_PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] +
                    delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                   0.5)) +
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
                  -1)) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})))))));
}

CORRECT COUNT_mSUPPLIER2_E1_1_E2_1[][delta_PS_PARTKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_E2_1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, PARTSUPP_SUPPKEY:int] *
  (((EXISTS(
       (AggSum([PARTSUPP_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
         (AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
              {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int])))) +
      EXISTS(
        (AggSum([PARTSUPP_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          ((EXISTS(
              (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
             (EXISTS(
                COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
               -1)) *
            AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [delta_PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [PARTSUPP_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
      (EXISTS(
         AggSum([PARTSUPP_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      ((AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
            {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
         (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) +
           ((PARTSUPP_PARTKEY:int ^= delta_PS_PARTKEY:int) *
             (EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int] +
                  delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
               (EXISTS(
                  COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
                 -1))))) +
        AggSum([PARTSUPP_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [PARTSUPP_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        ((EXISTS(
            (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
              delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
           (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
             -1)) *
          AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [delta_PS_PARTKEY:int, PARTSUPP_SUPPKEY:int] * 0.5)) *
              COUNT_mSUPPLIER2_E1_1(int)[]
              [PARTSUPP_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})))))));
}

CORRECT COUNT_mPARTSUPP1[][delta_S_NAME:string,delta_S_ADDRESS:string,delta_S_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_NAME:string, delta_S_ADDRESS:string] += 
  ((EXISTS(
    (AggSum([delta_S_SUPPKEY:int], 
       (COUNT_mSUPPLIER2_E1_1(int)[]
        [delta_S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
         (__sql_inline_agg_1:float ^=
           (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
            [PS_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5)) *
         EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
         {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
      ({PARTSUPP_SUPPKEY:int = delta_S_SUPPKEY:int} *
        AggSum([], 
          ((__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PARTSUPP_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5)) *
            {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
        EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
        -1))) +
   (EXISTS(
      AggSum([delta_S_SUPPKEY:int], 
        (COUNT_mSUPPLIER2_E1_1(int)[]
         [delta_S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
          (__sql_inline_agg_1:float ^=
            (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
             [PS_PARTKEY:int, delta_S_SUPPKEY:int] * 0.5)) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
          {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
     -1)) *
  delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mSUPPLIER2_E1_1[][delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_PARTKEY:int,delta_PS_AVAILQTY:int] += delta_COUNT_mSUPPLIER2_E1_1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
  (((EXISTS(
       (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
              PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
         ({PARTSUPP_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
           AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [PARTSUPP_PARTKEY:int,
                    delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                  0.5)) *
               {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
           -1))) +
      EXISTS(
        (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [delta_PS_PARTKEY:int,
                    delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                  0.5)) *
               EXISTS(
                 COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
               {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
            delta_COUNT_mSUPPLIER2_E1_1:int))) +
      (EXISTS(
         AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
         (COUNT_mSUPPLIER2_E1_1(int)[]
          [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
             PS_AVAILQTY:int] *
           (__sql_inline_agg_1:float ^=
             (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
              [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
               0.5)) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
           {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        ({PARTSUPP_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
          AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PARTSUPP_PARTKEY:int,
                   delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
              {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
          EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
          -1) +
        (AggSum([], 
           ((__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [delta_PS_PARTKEY:int,
                  delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
             {delta_PS_AVAILQTY:int > __sql_inline_agg_1:float})) *
          delta_COUNT_mSUPPLIER2_E1_1:int)))));
}

CORRECT COUNT_mSUPPLIER2_E1_1_L1_1[][delta_PS_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_L1_1:float FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_NAME:string, S_ADDRESS:string, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
  (((EXISTS(
       (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
              PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
         ({PARTSUPP_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
           AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [PARTSUPP_PARTKEY:int,
                    delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                  0.5)) *
               {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
           EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
           -1))) +
      EXISTS(
        (AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
            AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                      delta_PS_PARTKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                  delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                ((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [delta_PS_PARTKEY:int,
                        delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                      delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                     0.5)) +
                  ((__sql_inline_agg_1:float ^=
                     (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [delta_PS_PARTKEY:int,
                         delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                       0.5)) *
                    -1)) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
      (EXISTS(
         AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
           (COUNT_mSUPPLIER2_E1_1(int)[]
            [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
               PS_AVAILQTY:int] *
             (__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
             EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
             {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
        -1)) *
     -1) +
    EXISTS(
      (({PARTSUPP_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
         EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
         (AggSum([], 
            ((__sql_inline_agg_1:float ^=
               (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                [PARTSUPP_PARTKEY:int,
                   delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                 0.5)) *
              {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) +
           ((PARTSUPP_PARTKEY:int ^= delta_PS_PARTKEY:int) *
             AggSum([], 
               (((__sql_inline_agg_1:float ^=
                   ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                     [PARTSUPP_PARTKEY:int,
                        delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                      delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                     0.5)) +
                  ((__sql_inline_agg_1:float ^=
                     (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                      [PARTSUPP_PARTKEY:int,
                         delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                       0.5)) *
                    -1)) *
                 {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})))) *
         -1) +
        AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int], 
          (COUNT_mSUPPLIER2_E1_1(int)[]
           [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_PARTKEY:int,
              PS_AVAILQTY:int] *
            (__sql_inline_agg_1:float ^=
              (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
               [PS_PARTKEY:int, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                0.5)) *
            EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
            {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
        (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
          AggSum([delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                    delta_PS_PARTKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
                delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
              ((__sql_inline_agg_1:float ^=
                 ((COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [delta_PS_PARTKEY:int,
                      delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] +
                    delta_COUNT_mSUPPLIER2_E1_1_L1_1:float) *
                   0.5)) +
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int,
                       delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int] *
                     0.5)) *
                  -1)) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})))))));
}

CORRECT COUNT_mSUPPLIER2_E1_1_E2_1[][delta_PS_PARTKEY:int] += delta_COUNT_mSUPPLIER2_E1_1_E2_1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_NAME:string, S_ADDRESS:string] += 
  (AggSum([S_NAME:string, S_ADDRESS:string], 
   (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
     (EXISTS(
        (((S_SUPPKEY:int ^= PARTSUPP_SUPPKEY:int) *
           AggSum([], 
             ((__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [PARTSUPP_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
               {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
           (EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) +
             ((PARTSUPP_PARTKEY:int ^= delta_PS_PARTKEY:int) *
               (EXISTS(
                  (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int] +
                    delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
                 (EXISTS(
                    COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
                   -1)))) *
           -1) +
          AggSum([S_SUPPKEY:int], 
            (COUNT_mSUPPLIER2_E1_1(int)[]
             [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
              (__sql_inline_agg_1:float ^=
                (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                 [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
              EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
              {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
          ((EXISTS(
              (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
             (EXISTS(
                COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
               -1)) *
            AggSum([], 
              ((__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [delta_PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                COUNT_mSUPPLIER2_E1_1(int)[]
                [S_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
       (EXISTS(
          (AggSum([S_SUPPKEY:int], 
             (COUNT_mSUPPLIER2_E1_1(int)[]
              [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
               (__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
               EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
            ((S_SUPPKEY:int ^= PARTSUPP_SUPPKEY:int) *
              AggSum([], 
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [PARTSUPP_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                  {PARTSUPP_AVAILQTY:int > __sql_inline_agg_1:float})) *
              EXISTS(
                COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PARTSUPP_PARTKEY:int]) *
              -1))) *
         -1)))) +
  (AggSum([S_NAME:string, S_ADDRESS:string], 
     (COUNT_mPARTSUPP1(int)[][S_NAME:string, S_ADDRESS:string, S_SUPPKEY:int] *
       (EXISTS(
          (AggSum([S_SUPPKEY:int], 
             (COUNT_mSUPPLIER2_E1_1(int)[]
              [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
               (__sql_inline_agg_1:float ^=
                 (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                  [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
               EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
               {PS_AVAILQTY:int > __sql_inline_agg_1:float})) +
            ((EXISTS(
                (COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int] +
                  delta_COUNT_mSUPPLIER2_E1_1_E2_1:int)) +
               (EXISTS(
                  COUNT_mSUPPLIER2_E1_1_E2_1(int)[][delta_PS_PARTKEY:int]) *
                 -1)) *
              AggSum([], 
                ((__sql_inline_agg_1:float ^=
                   (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                    [delta_PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                  COUNT_mSUPPLIER2_E1_1(int)[]
                  [S_SUPPKEY:int, delta_PS_PARTKEY:int, PS_AVAILQTY:int] *
                  {PS_AVAILQTY:int > __sql_inline_agg_1:float}))))) +
         (EXISTS(
            AggSum([S_SUPPKEY:int], 
              (COUNT_mSUPPLIER2_E1_1(int)[]
               [S_SUPPKEY:int, PS_PARTKEY:int, PS_AVAILQTY:int] *
                (__sql_inline_agg_1:float ^=
                  (COUNT_mSUPPLIER2_E1_1_L1_1(float)[]
                   [PS_PARTKEY:int, S_SUPPKEY:int] * 0.5)) *
                EXISTS( COUNT_mSUPPLIER2_E1_1_E2_1(int)[][PS_PARTKEY:int]) *
                {PS_AVAILQTY:int > __sql_inline_agg_1:float}))) *
           -1)))) *
    -1));
}
