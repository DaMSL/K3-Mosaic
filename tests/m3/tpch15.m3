-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM SUPPLIER(SUPPLIER_SUPPKEY int, SUPPLIER_NAME string, SUPPLIER_ADDRESS string, SUPPLIER_NATIONKEY int, SUPPLIER_PHONE string, SUPPLIER_ACCTBAL float, SUPPLIER_COMMENT string)
  FROM FILE 'data/tpch/supplier.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP COUNT(int)[]
[S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string,
   R1_TOTAL_REVENUE:float] := 
(EXISTS(
   AggSum([S_SUPPKEY:int], 
     (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, S_SUPPKEY:int,
                 L_LINENUMBER:int, L_QUANTITY:float, L_EXTENDEDPRICE:float,
                 L_DISCOUNT:float, L_TAX:float, L_RETURNFLAG:string,
                 L_LINESTATUS:string, L_SHIPDATE:date, L_COMMITDATE:date,
                 L_RECEIPTDATE:date, L_SHIPINSTRUCT:string,
                 L_SHIPMODE:string, L_COMMENT:string) *
       {L_SHIPDATE:date >= DATE('1996-1-1')} *
       {L_SHIPDATE:date < DATE('1996-4-1')} * ((-1 * L_DISCOUNT:float) + 1) *
       L_EXTENDEDPRICE:float))) *
  (R1_TOTAL_REVENUE:float ^=
    AggSum([S_SUPPKEY:int], 
      (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, S_SUPPKEY:int,
                  L_LINENUMBER:int, L_QUANTITY:float, L_EXTENDEDPRICE:float,
                  L_DISCOUNT:float, L_TAX:float, L_RETURNFLAG:string,
                  L_LINESTATUS:string, L_SHIPDATE:date, L_COMMITDATE:date,
                  L_RECEIPTDATE:date, L_SHIPINSTRUCT:string,
                  L_SHIPMODE:string, L_COMMENT:string) *
        {L_SHIPDATE:date >= DATE('1996-1-1')} *
        {L_SHIPDATE:date < DATE('1996-4-1')} *
        ((-1 * L_DISCOUNT:float) + 1) * L_EXTENDEDPRICE:float))) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([], 
          (EXISTS(
             AggSum([R2_SUPPKEY:int], 
               (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int,
                           L_LINENUMBER:int, L_QUANTITY:float,
                           L_EXTENDEDPRICE:float, L_DISCOUNT:float,
                           L_TAX:float, L_RETURNFLAG:string,
                           L_LINESTATUS:string, L_SHIPDATE:date,
                           L_COMMITDATE:date, L_RECEIPTDATE:date,
                           L_SHIPINSTRUCT:string, L_SHIPMODE:string,
                           L_COMMENT:string) *
                 (R2_SUPPKEY:int ^= L_SUPPKEY:int) *
                 {L_SHIPDATE:date >= DATE('1996-1-1')} *
                 {L_SHIPDATE:date < DATE('1996-4-1')} *
                 ((-1 * L_DISCOUNT:float) + 1) * L_EXTENDEDPRICE:float))) *
            (R2_TOTAL_REVENUE:float ^=
              AggSum([R2_SUPPKEY:int], 
                (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int,
                            L_LINENUMBER:int, L_QUANTITY:float,
                            L_EXTENDEDPRICE:float, L_DISCOUNT:float,
                            L_TAX:float, L_RETURNFLAG:string,
                            L_LINESTATUS:string, L_SHIPDATE:date,
                            L_COMMITDATE:date, L_RECEIPTDATE:date,
                            L_SHIPINSTRUCT:string, L_SHIPMODE:string,
                            L_COMMENT:string) *
                  (R2_SUPPKEY:int ^= L_SUPPKEY:int) *
                  {L_SHIPDATE:date >= DATE('1996-1-1')} *
                  {L_SHIPDATE:date < DATE('1996-4-1')} *
                  ((-1 * L_DISCOUNT:float) + 1) * L_EXTENDEDPRICE:float))) *
            {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))) *
  AggSum([S_NAME:string, S_ADDRESS:string, S_PHONE:string], 
    SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
               S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string)));

DECLARE MAP COUNT_mLINEITEM1(int)[]
[S_NAME:string, S_ADDRESS:string, S_PHONE:string, S_SUPPKEY:int] := 
AggSum([S_NAME:string, S_ADDRESS:string, S_PHONE:string, S_SUPPKEY:int], 
  SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
             S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string));

DECLARE MAP COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] := 
AggSum([S_SUPPKEY:int], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, S_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    {L_SHIPDATE:date >= DATE('1996-1-1')} *
    {L_SHIPDATE:date < DATE('1996-4-1')} * L_DISCOUNT:float *
    L_EXTENDEDPRICE:float));

DECLARE MAP COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int] := 
AggSum([S_SUPPKEY:int], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, S_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    {L_SHIPDATE:date >= DATE('1996-1-1')} *
    {L_SHIPDATE:date < DATE('1996-4-1')} * L_EXTENDEDPRICE:float));

DECLARE MAP COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] := 
AggSum([R2_SUPPKEY:int], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    (R2_SUPPKEY:int ^= L_SUPPKEY:int) *
    {L_SHIPDATE:date >= DATE('1996-1-1')} *
    {L_SHIPDATE:date < DATE('1996-4-1')} * L_DISCOUNT:float *
    L_EXTENDEDPRICE:float));

DECLARE MAP COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int] := 
AggSum([R2_SUPPKEY:int], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    (R2_SUPPKEY:int ^= L_SUPPKEY:int) *
    {L_SHIPDATE:date >= DATE('1996-1-1')} *
    {L_SHIPDATE:date < DATE('1996-4-1')} * L_EXTENDEDPRICE:float));

-------------------- QUERIES --------------------
DECLARE QUERY COUNT := COUNT(int)[][S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, S_SUPPKEY:int] *
  (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
     (((((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
          ((EXISTS(
              ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
             2) +
            EXISTS(
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
         ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
           ((EXISTS(
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              ((R1_TOTAL_REVENUE:float ^=
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                (R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
             (EXISTS(
                ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
               ((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                 (R1_TOTAL_REVENUE:float ^=
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) +
         ((R1_TOTAL_REVENUE:float ^=
            (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
               LINEITEM_EXTENDEDPRICE:float) +
              (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
           EXISTS(
             ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) *
        AggSum([], 
          ((__domain_1:int ^=
             AggSum([], 
               (EXISTS(
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
            (__domain_1:int ^= 0)))) +
       (((EXISTS(
            (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
               LINEITEM_EXTENDEDPRICE:float) +
              (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
           ((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
             ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               (R1_TOTAL_REVENUE:float ^=
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int]))))) +
          (EXISTS(
             ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
            ((R1_TOTAL_REVENUE:float ^=
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
              ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                (R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) *
         AggSum([], 
           ((__domain_1:int ^=
              (((AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                 -1) +
                AggSum([], 
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                AggSum([], 
                  (EXISTS(
                     (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                        {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                AggSum([], 
                  (EXISTS(
                     (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                        {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
             (__domain_1:int ^= 0)))))) +
    ((((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
        ((((EXISTS(
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
             ((R1_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
               ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 ((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                   (R1_TOTAL_REVENUE:float ^=
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) +
            (EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              (((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                 2) +
                ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                  ((R1_TOTAL_REVENUE:float ^=
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                    (R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
                (R1_TOTAL_REVENUE:float ^=
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int]))))) *
           AggSum([], 
             ((__domain_1:int ^=
                AggSum([], 
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
               (__domain_1:int ^= 0)))) +
          ((((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              EXISTS(
                ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              2) +
             ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               (((R1_TOTAL_REVENUE:float ^=
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                  EXISTS(
                    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int]))) +
                 ((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                   EXISTS(
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) *
            AggSum([], 
              ((__domain_1:int ^=
                 (((AggSum([], 
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                    -1) +
                   AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
                (__domain_1:int ^= 0)))))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         AggSum([], 
           ((__domain_1:int ^=
              AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
             (__domain_1:int ^= 0))))) *
      -1) +
    ((R1_TOTAL_REVENUE:float ^=
       ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
      EXISTS(
        ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
      AggSum([], 
        ((__domain_1:int ^=
           (((AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 (EXISTS(
                    (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                       {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              -1) +
             AggSum([], 
               (EXISTS(
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
             AggSum([], 
               (EXISTS(
                  (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                     {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
             AggSum([], 
               (EXISTS(
                  (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                     {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                      {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
          (__domain_1:int ^= 0))))));
   COUNT_mLINEITEM1_L2_1(float)[][LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} * LINEITEM_DISCOUNT:float *
  LINEITEM_EXTENDEDPRICE:float);
   COUNT_mLINEITEM1_L2_3(float)[][LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} * LINEITEM_EXTENDEDPRICE:float);
   COUNT_mLINEITEM1_L3_1_E1_1(float)[][LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} * LINEITEM_DISCOUNT:float *
  LINEITEM_EXTENDEDPRICE:float);
   COUNT_mLINEITEM1_L3_1_E1_3(float)[][LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} * LINEITEM_EXTENDEDPRICE:float);
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, S_SUPPKEY:int] *
  ((EXISTS(
      (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
         LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
        (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
        COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
     (((R1_TOTAL_REVENUE:float ^=
         (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
        (AggSum([], 
           ((__domain_1:int ^= 0) *
             (__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))) +
          AggSum([], 
            ((__domain_1:int ^= 0) *
              (__domain_1:int ^=
                (((AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            LINEITEM_EXTENDEDPRICE:float *
                            {(-1 + LINEITEM_DISCOUNT:float)}) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                   -1) +
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          LINEITEM_EXTENDEDPRICE:float *
                          {(-1 + LINEITEM_DISCOUNT:float)}) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          LINEITEM_EXTENDEDPRICE:float *
                          {(-1 + LINEITEM_DISCOUNT:float)}) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           LINEITEM_EXTENDEDPRICE:float *
                           {(-1 + LINEITEM_DISCOUNT:float)}) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         AggSum([], 
           ((__domain_1:int ^= 0) *
             (__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))))) +
    (((EXISTS(
         (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
        ((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
          (R1_TOTAL_REVENUE:float ^=
            (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               LINEITEM_EXTENDEDPRICE:float *
               {(-1 + LINEITEM_DISCOUNT:float)}) +
              (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            AggSum([], 
              (EXISTS(
                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                (R2_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))) *
      -1)));
   COUNT_mLINEITEM1_L2_1(float)[][LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} * -1 *
  LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   COUNT_mLINEITEM1_L2_3(float)[][LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} * -1 *
  LINEITEM_EXTENDEDPRICE:float);
   COUNT_mLINEITEM1_L3_1_E1_1(float)[][LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} * -1 *
  LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   COUNT_mLINEITEM1_L3_1_E1_3(float)[][LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} * -1 *
  LINEITEM_EXTENDEDPRICE:float);
}

ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ((S_SUPPKEY:int ^= SUPPLIER_SUPPKEY:int) *
  (R1_TOTAL_REVENUE:float ^=
    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
  EXISTS(
    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([], 
         (EXISTS(
            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           (R2_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
      (__domain_1:int ^= 0))));
   COUNT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_SUPPKEY:int] += 1;
}

ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ((S_SUPPKEY:int ^= SUPPLIER_SUPPKEY:int) *
  (R1_TOTAL_REVENUE:float ^=
    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
  EXISTS(
    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([], 
         (EXISTS(
            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           (R2_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
      (__domain_1:int ^= 0))) *
  -1);
   COUNT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_SUPPKEY:int] += -1;
}

ON SYSTEM READY {
}

CORRECT COUNT_mLINEITEM1[][delta_S_NAME:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, delta_S_NAME:string, delta_S_ADDRESS:string, delta_S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ((((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
    (((((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
         ((EXISTS(
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            2) +
           EXISTS(
             (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                LINEITEM_EXTENDEDPRICE:float) +
               (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
        ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
          ((EXISTS(
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
               (R1_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
            (EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
              ((R1_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                (R1_TOTAL_REVENUE:float ^=
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))))) +
        ((R1_TOTAL_REVENUE:float ^=
           (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
              LINEITEM_EXTENDEDPRICE:float) +
             (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
          EXISTS(
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             AggSum([], 
               (EXISTS(
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))) +
      (((EXISTS(
           (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
              LINEITEM_EXTENDEDPRICE:float) +
             (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
          ((R1_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
            ((R1_TOTAL_REVENUE:float ^=
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
              (delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int)))) +
         (EXISTS(
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
           ((R1_TOTAL_REVENUE:float ^=
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             ((R1_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               (delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int))))) *
        AggSum([], 
          ((__domain_1:int ^= 0) *
            (__domain_1:int ^=
              (((AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                 -1) +
                AggSum([], 
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                AggSum([], 
                  (EXISTS(
                     (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                        {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                AggSum([], 
                  (EXISTS(
                     (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                        {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))))))) +
   ((((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
       ((((EXISTS(
             (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                LINEITEM_EXTENDEDPRICE:float) +
               (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            ((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                ((R1_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                  (R1_TOTAL_REVENUE:float ^=
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))))) +
           (EXISTS(
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             (((R1_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                2) +
               ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 ((R1_TOTAL_REVENUE:float ^=
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                   (R1_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
               (R1_TOTAL_REVENUE:float ^=
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))))) *
          AggSum([], 
            ((__domain_1:int ^= 0) *
              (__domain_1:int ^=
                AggSum([], 
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))) +
         ((((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             2) +
            ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
              (((R1_TOTAL_REVENUE:float ^=
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                 EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
                ((R1_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  EXISTS(
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))))) *
           AggSum([], 
             ((__domain_1:int ^= 0) *
               (__domain_1:int ^=
                 (((AggSum([], 
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                    -1) +
                   AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))))))) +
      ((R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        EXISTS(
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        AggSum([], 
          ((__domain_1:int ^= 0) *
            (__domain_1:int ^=
              AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))))) *
     -1) +
   ((R1_TOTAL_REVENUE:float ^=
      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
     EXISTS(
       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         (__domain_1:int ^=
           (((AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 (EXISTS(
                    (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                       {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              -1) +
             AggSum([], 
               (EXISTS(
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
             AggSum([], 
               (EXISTS(
                  (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                     {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
             AggSum([], 
               (EXISTS(
                  (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                     {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                      {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))))) *
  delta_COUNT_mLINEITEM1:int);
}

CORRECT COUNT_mLINEITEM1_L2_1[][delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L2_1:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, delta_S_SUPPKEY:int] *
  ((((((R1_TOTAL_REVENUE:float ^=
         (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_1:float) *
            -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
        ((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
          -1)) *
       EXISTS(
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
      (((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
         (R1_TOTAL_REVENUE:float ^=
           (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
               delta_COUNT_mLINEITEM1_L2_1:float) *
              -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
         ((R1_TOTAL_REVENUE:float ^=
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
           -1)) *
        (EXISTS(
           (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
               delta_COUNT_mLINEITEM1_L2_1:float) *
              -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          (EXISTS(
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)))) *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         (__domain_1:int ^=
           (((AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 (EXISTS(
                    (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                       {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              -1) +
             AggSum([], 
               (EXISTS(
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
             AggSum([], 
               (EXISTS(
                  (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                     {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
             AggSum([], 
               (EXISTS(
                  (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                     {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                      {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))))) +
    ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
      ((((((R1_TOTAL_REVENUE:float ^=
             (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                 delta_COUNT_mLINEITEM1_L2_1:float) *
                -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
            ((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
              -1)) *
           ((EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
              2) +
             EXISTS(
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
          (((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             (R1_TOTAL_REVENUE:float ^=
               (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                   delta_COUNT_mLINEITEM1_L2_1:float) *
                  -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             ((R1_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)) *
            (((EXISTS(
                 (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                     delta_COUNT_mLINEITEM1_L2_1:float) *
                    -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                (EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               2) +
              EXISTS(
                (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_1:float) *
                   -1) +
                  ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              (EXISTS(
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1))) +
          ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            (((EXISTS(
                 (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                     delta_COUNT_mLINEITEM1_L2_1:float) *
                    -1) +
                   ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                (EXISTS(
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               ((R1_TOTAL_REVENUE:float ^=
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 (R1_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
              ((EXISTS(
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 EXISTS(
                   (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L2_1:float) *
                      -1) +
                     ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 (EXISTS(
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1)) *
                ((((R1_TOTAL_REVENUE:float ^=
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                    (R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) *
                   -1) +
                  (R1_TOTAL_REVENUE:float ^=
                    (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L2_1:float) *
                       -1) +
                      ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                  (R1_TOTAL_REVENUE:float ^=
                    (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L2_1:float) *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
              ((EXISTS(
                  (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_1:float) *
                     -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1)) *
                ((R1_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                  (R1_TOTAL_REVENUE:float ^=
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
              ((EXISTS(
                  ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 EXISTS(
                   (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L2_1:float) *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1)) *
                ((((R1_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                    (R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) *
                   -1) +
                  (R1_TOTAL_REVENUE:float ^=
                    (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L2_1:float) *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                  (R1_TOTAL_REVENUE:float ^=
                    (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L2_1:float) *
                       -1) +
                      ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))))) +
          (((R1_TOTAL_REVENUE:float ^=
              (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_1:float) *
                 -1) +
                ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)) *
            EXISTS(
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
          (((R1_TOTAL_REVENUE:float ^=
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             (R1_TOTAL_REVENUE:float ^=
               (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                   delta_COUNT_mLINEITEM1_L2_1:float) *
                  -1) +
                 ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)) *
            (EXISTS(
               (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                   delta_COUNT_mLINEITEM1_L2_1:float) *
                  -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              (EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1)))) *
         AggSum([], 
           ((__domain_1:int ^= 0) *
             (__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))) +
        ((((EXISTS(
              (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_1:float) *
                 -1) +
                ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             (EXISTS(
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)) *
            ((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              ((R1_TOTAL_REVENUE:float ^=
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                (delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int)))) +
           ((EXISTS(
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              EXISTS(
                (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_1:float) *
                   -1) +
                  ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              (EXISTS(
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1)) *
             ((R1_TOTAL_REVENUE:float ^=
                (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_1:float) *
                   -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
               ((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                 -1) +
               ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 ((R1_TOTAL_REVENUE:float ^=
                    (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L2_1:float) *
                       -1) +
                      ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                   ((R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                     -1))))) +
           ((EXISTS(
               (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                   delta_COUNT_mLINEITEM1_L2_1:float) *
                  -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              (EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1)) *
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
               ((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                 (delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int)))) +
           ((EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              EXISTS(
                (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_1:float) *
                   -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              (EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1)) *
             ((R1_TOTAL_REVENUE:float ^=
                (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_1:float) *
                   -1) +
                  ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
               ((R1_TOTAL_REVENUE:float ^=
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                 -1) +
               ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 ((R1_TOTAL_REVENUE:float ^=
                    (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L2_1:float) *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                   ((R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                     -1)))))) *
          AggSum([], 
            ((__domain_1:int ^= 0) *
              (__domain_1:int ^=
                (((AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                   -1) +
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))))))) +
    (((((((R1_TOTAL_REVENUE:float ^=
            (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                delta_COUNT_mLINEITEM1_L2_1:float) *
               -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
           ((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             -1)) *
          EXISTS(
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
         (((R1_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
            (R1_TOTAL_REVENUE:float ^=
              (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_1:float) *
                 -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
            ((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
              -1)) *
           (EXISTS(
              (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_1:float) *
                 -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             (EXISTS(
                ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)))) *
        AggSum([], 
          ((__domain_1:int ^= 0) *
            (__domain_1:int ^=
              AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))) +
       ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
         (((((EXISTS(
                (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_1:float) *
                   -1) +
                  ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
               (EXISTS(
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                 -1)) *
              ((R1_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                  ((R1_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                    (R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))))) +
             ((EXISTS(
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                EXISTS(
                  (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_1:float) *
                     -1) +
                    ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                (EXISTS(
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               ((R1_TOTAL_REVENUE:float ^=
                  (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_1:float) *
                     -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 ((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1) +
                 ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                   ((((R1_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                       (R1_TOTAL_REVENUE:float ^=
                         (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L2_1(float)[]
                            [delta_S_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L2_3(float)[]
                           [delta_S_SUPPKEY:int]))) *
                      -1) +
                     (R1_TOTAL_REVENUE:float ^=
                       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L2_1:float) *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     (R1_TOTAL_REVENUE:float ^=
                       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L2_1:float) *
                          -1) +
                         ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))))) +
             ((EXISTS(
                 (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                     delta_COUNT_mLINEITEM1_L2_1:float) *
                    -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                (EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               (((R1_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  2) +
                 ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                   ((R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     (R1_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
                 (R1_TOTAL_REVENUE:float ^=
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
             ((EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                EXISTS(
                  (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_1:float) *
                     -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                (EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               ((((R1_TOTAL_REVENUE:float ^=
                    (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L2_1:float) *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                   ((R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                     -1)) *
                  2) +
                 ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                   ((((R1_TOTAL_REVENUE:float ^=
                        (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L2_1(float)[]
                           [delta_S_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                       (R1_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L2_1(float)[]
                           [delta_S_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L2_3(float)[]
                           [delta_S_SUPPKEY:int]))) *
                      -1) +
                     (R1_TOTAL_REVENUE:float ^=
                       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L2_1:float) *
                          -1) +
                         ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     (R1_TOTAL_REVENUE:float ^=
                       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L2_1:float) *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
                 (R1_TOTAL_REVENUE:float ^=
                   (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L2_1:float) *
                      -1) +
                     ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 ((R1_TOTAL_REVENUE:float ^=
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1)))) *
            AggSum([], 
              ((__domain_1:int ^= 0) *
                (__domain_1:int ^=
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))) +
           (((((((R1_TOTAL_REVENUE:float ^=
                   (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L2_1:float) *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                  ((R1_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                    -1)) *
                 EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
                (((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                   (R1_TOTAL_REVENUE:float ^=
                     (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_1:float) *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                   ((R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                     -1)) *
                  (EXISTS(
                     (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_1:float) *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                      -1)))) *
               2) +
              ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                ((((R1_TOTAL_REVENUE:float ^=
                     (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_1:float) *
                        -1) +
                       ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                    ((R1_TOTAL_REVENUE:float ^=
                       (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                      -1)) *
                   EXISTS(
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
                  (((R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     (R1_TOTAL_REVENUE:float ^=
                       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L2_1:float) *
                          -1) +
                         ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     ((R1_TOTAL_REVENUE:float ^=
                        (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L2_1(float)[]
                           [delta_S_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                       -1)) *
                    (EXISTS(
                       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L2_1:float) *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L2_1(float)[]
                           [delta_S_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L2_3(float)[]
                           [delta_S_SUPPKEY:int])) *
                        -1))) +
                  (((R1_TOTAL_REVENUE:float ^=
                      (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                          delta_COUNT_mLINEITEM1_L2_1:float) *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     ((R1_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                       -1)) *
                    EXISTS(
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
                  (((R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     (R1_TOTAL_REVENUE:float ^=
                       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L2_1:float) *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     ((R1_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                       -1)) *
                    (EXISTS(
                       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L2_1:float) *
                          -1) +
                         ({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                      (EXISTS(
                         (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L2_1(float)[]
                            [delta_S_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L2_3(float)[]
                           [delta_S_SUPPKEY:int])) *
                        -1)))))) *
             AggSum([], 
               ((__domain_1:int ^= 0) *
                 (__domain_1:int ^=
                   (((AggSum([], 
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                          (R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                          {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                      -1) +
                     AggSum([], 
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))))))) *
      -1)));
}

CORRECT COUNT_mLINEITEM1_L2_3[][delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L2_3:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, delta_S_SUPPKEY:int] *
  ((((((R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
           delta_COUNT_mLINEITEM1_L2_3:float)) +
        ((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
          -1)) *
       EXISTS(
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
      (((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
         (R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_3:float)) +
         ((R1_TOTAL_REVENUE:float ^=
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
           -1)) *
        (EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_3:float)) +
          (EXISTS(
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)))) *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         (__domain_1:int ^=
           (((AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 (EXISTS(
                    (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                       {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              -1) +
             AggSum([], 
               (EXISTS(
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
             AggSum([], 
               (EXISTS(
                  (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                     {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
             AggSum([], 
               (EXISTS(
                  (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                     {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                      {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))))) +
    ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
      ((((((R1_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
               delta_COUNT_mLINEITEM1_L2_3:float)) +
            ((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
              -1)) *
           ((EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
              2) +
             EXISTS(
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
          (((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             (R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                 delta_COUNT_mLINEITEM1_L2_3:float)) +
             ((R1_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)) *
            (((EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                   delta_COUNT_mLINEITEM1_L2_3:float)) +
                (EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               2) +
              EXISTS(
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_3:float)) +
              (EXISTS(
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1))) +
          ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            (((EXISTS(
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                   delta_COUNT_mLINEITEM1_L2_3:float)) +
                (EXISTS(
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               ((R1_TOTAL_REVENUE:float ^=
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 (R1_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
              ((EXISTS(
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 EXISTS(
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                     delta_COUNT_mLINEITEM1_L2_3:float)) +
                 (EXISTS(
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1)) *
                ((((R1_TOTAL_REVENUE:float ^=
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                    (R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) *
                   -1) +
                  (R1_TOTAL_REVENUE:float ^=
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_3:float)) +
                  (R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_3:float)))) +
              ((EXISTS(
                  ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_3:float)) +
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1)) *
                ((R1_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                  (R1_TOTAL_REVENUE:float ^=
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
              ((EXISTS(
                  ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                 EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                     delta_COUNT_mLINEITEM1_L2_3:float)) +
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1)) *
                ((((R1_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                    (R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) *
                   -1) +
                  (R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_3:float)) +
                  (R1_TOTAL_REVENUE:float ^=
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_3:float)))))) +
          (((R1_TOTAL_REVENUE:float ^=
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                delta_COUNT_mLINEITEM1_L2_3:float)) +
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)) *
            EXISTS(
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
          (((R1_TOTAL_REVENUE:float ^=
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             (R1_TOTAL_REVENUE:float ^=
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                 delta_COUNT_mLINEITEM1_L2_3:float)) +
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)) *
            (EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                 delta_COUNT_mLINEITEM1_L2_3:float)) +
              (EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1)))) *
         AggSum([], 
           ((__domain_1:int ^= 0) *
             (__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))) +
        ((((EXISTS(
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                delta_COUNT_mLINEITEM1_L2_3:float)) +
             (EXISTS(
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)) *
            ((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              ((R1_TOTAL_REVENUE:float ^=
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                (delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int)))) +
           ((EXISTS(
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              EXISTS(
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_3:float)) +
              (EXISTS(
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1)) *
             ((R1_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_3:float)) +
               ((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                 -1) +
               ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 ((R1_TOTAL_REVENUE:float ^=
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_3:float)) +
                   ((R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                     -1))))) +
           ((EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                 delta_COUNT_mLINEITEM1_L2_3:float)) +
              (EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1)) *
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
               ((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                 (delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int)))) +
           ((EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
              EXISTS(
                ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_3:float)) +
              (EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                -1)) *
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_3:float)) +
               ((R1_TOTAL_REVENUE:float ^=
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                 -1) +
               ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 ((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_3:float)) +
                   ((R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                     -1)))))) *
          AggSum([], 
            ((__domain_1:int ^= 0) *
              (__domain_1:int ^=
                (((AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                   -1) +
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))))))))) +
    (((((((R1_TOTAL_REVENUE:float ^=
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
              delta_COUNT_mLINEITEM1_L2_3:float)) +
           ((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             -1)) *
          EXISTS(
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
         (((R1_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
            (R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                delta_COUNT_mLINEITEM1_L2_3:float)) +
            ((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
              -1)) *
           (EXISTS(
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                delta_COUNT_mLINEITEM1_L2_3:float)) +
             (EXISTS(
                ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
               -1)))) *
        AggSum([], 
          ((__domain_1:int ^= 0) *
            (__domain_1:int ^=
              AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))) +
       ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
         (((((EXISTS(
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                  delta_COUNT_mLINEITEM1_L2_3:float)) +
               (EXISTS(
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                 -1)) *
              ((R1_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                  ((R1_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                    (R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))))) +
             ((EXISTS(
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                EXISTS(
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_3:float)) +
                (EXISTS(
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               ((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_3:float)) +
                 ((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1) +
                 ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                   ((((R1_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                       (R1_TOTAL_REVENUE:float ^=
                         (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L2_1(float)[]
                            [delta_S_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L2_3(float)[]
                           [delta_S_SUPPKEY:int]))) *
                      -1) +
                     (R1_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_3:float)) +
                     (R1_TOTAL_REVENUE:float ^=
                       (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_3:float)))))) +
             ((EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                   delta_COUNT_mLINEITEM1_L2_3:float)) +
                (EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               (((R1_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  2) +
                 ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                   ((R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     (R1_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
                 (R1_TOTAL_REVENUE:float ^=
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
             ((EXISTS(
                 ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                EXISTS(
                  ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                    delta_COUNT_mLINEITEM1_L2_3:float)) +
                (EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                  -1)) *
               ((((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L2_3:float)) +
                   ((R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                     -1)) *
                  2) +
                 ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                   ((((R1_TOTAL_REVENUE:float ^=
                        (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L2_1(float)[]
                           [delta_S_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                       (R1_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L2_1(float)[]
                           [delta_S_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L2_3(float)[]
                           [delta_S_SUPPKEY:int]))) *
                      -1) +
                     (R1_TOTAL_REVENUE:float ^=
                       (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_3:float)) +
                     (R1_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_3:float)))) +
                 (R1_TOTAL_REVENUE:float ^=
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                     delta_COUNT_mLINEITEM1_L2_3:float)) +
                 ((R1_TOTAL_REVENUE:float ^=
                    (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                   -1)))) *
            AggSum([], 
              ((__domain_1:int ^= 0) *
                (__domain_1:int ^=
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))) +
           (((((((R1_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                     delta_COUNT_mLINEITEM1_L2_3:float)) +
                  ((R1_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                    -1)) *
                 EXISTS(
                   ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
                (((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                   (R1_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L2_3:float)) +
                   ((R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                     -1)) *
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L2_3:float)) +
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                      -1)))) *
               2) +
              ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                ((((R1_TOTAL_REVENUE:float ^=
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L2_3:float)) +
                    ((R1_TOTAL_REVENUE:float ^=
                       (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                      -1)) *
                   EXISTS(
                     ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
                  (((R1_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     (R1_TOTAL_REVENUE:float ^=
                       (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_3:float)) +
                     ((R1_TOTAL_REVENUE:float ^=
                        (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L2_1(float)[]
                           [delta_S_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                       -1)) *
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_3:float)) +
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L2_1(float)[]
                           [delta_S_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L2_3(float)[]
                           [delta_S_SUPPKEY:int])) *
                        -1))) +
                  (((R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L2_3:float)) +
                     ((R1_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                       -1)) *
                    EXISTS(
                      (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
                  (((R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
                     (R1_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_3:float)) +
                     ((R1_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
                       -1)) *
                    (EXISTS(
                       (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L2_3:float)) +
                      (EXISTS(
                         (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L2_1(float)[]
                            [delta_S_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L2_3(float)[]
                           [delta_S_SUPPKEY:int])) *
                        -1)))))) *
             AggSum([], 
               ((__domain_1:int ^= 0) *
                 (__domain_1:int ^=
                   (((AggSum([], 
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                          (R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                          {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                      -1) +
                     AggSum([], 
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))))))))) *
      -1)));
}

CORRECT COUNT_mLINEITEM1_L3_1_E1_1[][delta_R2_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L3_1_E1_1:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, S_SUPPKEY:int] *
  (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
     (((((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
          ((EXISTS(
              ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
             2) +
            EXISTS(
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
         ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
           ((EXISTS(
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              ((R1_TOTAL_REVENUE:float ^=
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                (R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
             (EXISTS(
                ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
               ((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                 (R1_TOTAL_REVENUE:float ^=
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) +
         ((R1_TOTAL_REVENUE:float ^=
            (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
               LINEITEM_EXTENDEDPRICE:float) +
              (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
           EXISTS(
             ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) *
        AggSum([], 
          ((__domain_1:int ^= 0) *
            ((__domain_1:int ^=
               (AggSum([], 
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   ((((R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) *
                       (EXISTS(
                          (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                             -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) +
                         (EXISTS(
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           -1))) +
                      (EXISTS(
                         (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                            -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        ((R2_TOTAL_REVENUE:float ^=
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) +
                          ((R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1)))) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
              ((__domain_1:int ^=
                 AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                -1))))) +
       (((EXISTS(
            (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
               LINEITEM_EXTENDEDPRICE:float) +
              (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
           ((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
               (S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int)))) +
          (EXISTS(
             ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
            ((R1_TOTAL_REVENUE:float ^=
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
              ((R1_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                (S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int))))) *
         AggSum([], 
           ((__domain_1:int ^= 0) *
             ((__domain_1:int ^=
                (((AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      ((((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          (EXISTS(
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) +
                            (EXISTS(
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1))) +
                         (EXISTS(
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           ((R2_TOTAL_REVENUE:float ^=
                              (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                 -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) +
                             ((R2_TOTAL_REVENUE:float ^=
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1)))) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      ((((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          (EXISTS(
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                 LINEITEM_EXTENDEDPRICE:float) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) +
                            (EXISTS(
                               (({LINEITEM_SUPPKEY:int =
                                 delta_R2_SUPPKEY:int} *
                                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                  {LINEITEM_SHIPDATE:date >=
                                  DATE('1996-1-1')} *
                                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                  LINEITEM_EXTENDEDPRICE:float) +
                                 (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1))) +
                         (EXISTS(
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           ((R2_TOTAL_REVENUE:float ^=
                              (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                 -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) +
                             ((R2_TOTAL_REVENUE:float ^=
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1)))) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                   -1) +
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                             -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         ((R2_TOTAL_REVENUE:float ^=
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) +
                          (EXISTS(
                             (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                             -1) +
                            ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         ((R2_TOTAL_REVENUE:float ^=
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) +
                          (EXISTS(
                             (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                             -1) +
                            ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         ((R2_TOTAL_REVENUE:float ^=
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) +
                           ((R2_TOTAL_REVENUE:float ^=
                              (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                 LINEITEM_EXTENDEDPRICE:float) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
               ((__domain_1:int ^=
                  (((AggSum([], 
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                      AggSum([], 
                        (EXISTS(
                           (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                          (R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                          {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                     -1) +
                    AggSum([], 
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
                 -1))))))) +
    ((((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
        ((((EXISTS(
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
             ((R1_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
               ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 ((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                   (R1_TOTAL_REVENUE:float ^=
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) +
            (EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              (((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                 2) +
                ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                  ((R1_TOTAL_REVENUE:float ^=
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                    (R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
                (R1_TOTAL_REVENUE:float ^=
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int]))))) *
           AggSum([], 
             ((__domain_1:int ^= 0) *
               ((__domain_1:int ^=
                  (AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      ((((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          (EXISTS(
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) +
                            (EXISTS(
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1))) +
                         (EXISTS(
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           ((R2_TOTAL_REVENUE:float ^=
                              (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                 -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) +
                             ((R2_TOTAL_REVENUE:float ^=
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1)))) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
                 ((__domain_1:int ^=
                    AggSum([], 
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                   -1))))) +
          ((((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              EXISTS(
                ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              2) +
             ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               (((R1_TOTAL_REVENUE:float ^=
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                  EXISTS(
                    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int]))) +
                 ((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                   EXISTS(
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) *
            AggSum([], 
              ((__domain_1:int ^= 0) *
                ((__domain_1:int ^=
                   (((AggSum([], 
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                          (R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                          {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         ((((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             (EXISTS(
                                (((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] +
                                    delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                   -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) +
                               (EXISTS(
                                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                    ][delta_R2_SUPPKEY:int] * -1) +
                                    COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                    ][delta_R2_SUPPKEY:int])) *
                                 -1))) +
                            (EXISTS(
                               (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] +
                                   delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                  -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              ((R2_TOTAL_REVENUE:float ^=
                                 (((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                    ][delta_R2_SUPPKEY:int] +
                                     delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                    -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) +
                                ((R2_TOTAL_REVENUE:float ^=
                                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                     ][delta_R2_SUPPKEY:int] * -1) +
                                     COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                     ][delta_R2_SUPPKEY:int])) *
                                  -1)))) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         ((((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             (EXISTS(
                                (((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] +
                                    delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                   -1) +
                                  ({LINEITEM_SUPPKEY:int =
                                   delta_R2_SUPPKEY:int} *
                                    {LINEITEM_SHIPDATE:date <
                                    DATE('1996-4-1')} *
                                    {LINEITEM_SHIPDATE:date >=
                                    DATE('1996-1-1')} *
                                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                    LINEITEM_EXTENDEDPRICE:float) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) +
                               (EXISTS(
                                  (({LINEITEM_SUPPKEY:int =
                                    delta_R2_SUPPKEY:int} *
                                     {LINEITEM_SHIPDATE:date <
                                     DATE('1996-4-1')} *
                                     {LINEITEM_SHIPDATE:date >=
                                     DATE('1996-1-1')} *
                                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                     LINEITEM_EXTENDEDPRICE:float) +
                                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                     ][delta_R2_SUPPKEY:int] * -1) +
                                    COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                    ][delta_R2_SUPPKEY:int])) *
                                 -1))) +
                            (EXISTS(
                               (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] +
                                   delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                  -1) +
                                 ({LINEITEM_SUPPKEY:int =
                                  delta_R2_SUPPKEY:int} *
                                   {LINEITEM_SHIPDATE:date <
                                   DATE('1996-4-1')} *
                                   {LINEITEM_SHIPDATE:date >=
                                   DATE('1996-1-1')} *
                                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                   LINEITEM_EXTENDEDPRICE:float) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              ((R2_TOTAL_REVENUE:float ^=
                                 (((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                    ][delta_R2_SUPPKEY:int] +
                                     delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                    -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) +
                                ((R2_TOTAL_REVENUE:float ^=
                                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                     ][delta_R2_SUPPKEY:int] * -1) +
                                     COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                     ][delta_R2_SUPPKEY:int])) *
                                  -1)))) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                      -1) +
                     AggSum([], 
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                 -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) +
                             (EXISTS(
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            ((R2_TOTAL_REVENUE:float ^=
                               (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] +
                                   delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                  -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                 -1) +
                                ({LINEITEM_SUPPKEY:int =
                                 delta_R2_SUPPKEY:int} *
                                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                  {LINEITEM_SHIPDATE:date >=
                                  DATE('1996-1-1')} *
                                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                  LINEITEM_EXTENDEDPRICE:float) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) +
                             (EXISTS(
                                (({LINEITEM_SUPPKEY:int =
                                  delta_R2_SUPPKEY:int} *
                                   {LINEITEM_SHIPDATE:date <
                                   DATE('1996-4-1')} *
                                   {LINEITEM_SHIPDATE:date >=
                                   DATE('1996-1-1')} *
                                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                   LINEITEM_EXTENDEDPRICE:float) +
                                  (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                 LINEITEM_EXTENDEDPRICE:float) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            ((R2_TOTAL_REVENUE:float ^=
                               (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] +
                                   delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                  -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                 -1) +
                                ({LINEITEM_SUPPKEY:int =
                                 delta_R2_SUPPKEY:int} *
                                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                  {LINEITEM_SHIPDATE:date >=
                                  DATE('1996-1-1')} *
                                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                  LINEITEM_EXTENDEDPRICE:float) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) +
                             (EXISTS(
                                (({LINEITEM_SUPPKEY:int =
                                  delta_R2_SUPPKEY:int} *
                                   {LINEITEM_SHIPDATE:date <
                                   DATE('1996-4-1')} *
                                   {LINEITEM_SHIPDATE:date >=
                                   DATE('1996-1-1')} *
                                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                   LINEITEM_EXTENDEDPRICE:float) +
                                  (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                 LINEITEM_EXTENDEDPRICE:float) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            ((R2_TOTAL_REVENUE:float ^=
                               (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] +
                                   delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                  -1) +
                                 ({LINEITEM_SUPPKEY:int =
                                  delta_R2_SUPPKEY:int} *
                                   {LINEITEM_SHIPDATE:date <
                                   DATE('1996-4-1')} *
                                   {LINEITEM_SHIPDATE:date >=
                                   DATE('1996-1-1')} *
                                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                   LINEITEM_EXTENDEDPRICE:float) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 (({LINEITEM_SUPPKEY:int =
                                   delta_R2_SUPPKEY:int} *
                                    {LINEITEM_SHIPDATE:date <
                                    DATE('1996-4-1')} *
                                    {LINEITEM_SHIPDATE:date >=
                                    DATE('1996-1-1')} *
                                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                    LINEITEM_EXTENDEDPRICE:float) +
                                   (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                    ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
                  ((__domain_1:int ^=
                     (((AggSum([], 
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                            (R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [R2_SUPPKEY:int])) *
                            {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                         AggSum([], 
                           (EXISTS(
                              (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                 LINEITEM_EXTENDEDPRICE:float) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [R2_SUPPKEY:int])) *
                             (R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [R2_SUPPKEY:int])) *
                             {R2_TOTAL_REVENUE:float >
                             R1_TOTAL_REVENUE:float}))) *
                        -1) +
                       AggSum([], 
                         (EXISTS(
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
                    -1))))))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         AggSum([], 
           ((__domain_1:int ^= 0) *
             ((__domain_1:int ^=
                (AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                             -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         ((R2_TOTAL_REVENUE:float ^=
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
               ((__domain_1:int ^=
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                 -1)))))) *
      -1) +
    ((R1_TOTAL_REVENUE:float ^=
       ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
      EXISTS(
        ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          ((__domain_1:int ^=
             (((AggSum([], 
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   ((((R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) *
                       (EXISTS(
                          (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                             -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) +
                         (EXISTS(
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           -1))) +
                      (EXISTS(
                         (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                            -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        ((R2_TOTAL_REVENUE:float ^=
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) +
                          ((R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1)))) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   ((((R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) *
                       (EXISTS(
                          (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                             -1) +
                            ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) +
                         (EXISTS(
                            (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           -1))) +
                      (EXISTS(
                         (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                            -1) +
                           ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        ((R2_TOTAL_REVENUE:float ^=
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) +
                          ((R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1)))) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                -1) +
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 (EXISTS(
                    (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                       {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 (EXISTS(
                    (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                       {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                        {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 ((((R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     (EXISTS(
                        (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) +
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         -1))) +
                    (EXISTS(
                       (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int])) *
                      ((R2_TOTAL_REVENUE:float ^=
                         (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                            -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) +
                        ((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          -1)))) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 ((((R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     (EXISTS(
                        (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                           -1) +
                          ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) +
                       (EXISTS(
                          (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         -1))) +
                    (EXISTS(
                       (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                          -1) +
                         ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int])) *
                      ((R2_TOTAL_REVENUE:float ^=
                         (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                            -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) +
                        ((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          -1)))) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 ((((R2_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                         [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     (EXISTS(
                        (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                           -1) +
                          ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) +
                       (EXISTS(
                          (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         -1))) +
                    (EXISTS(
                       (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                          -1) +
                         ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int])) *
                      ((R2_TOTAL_REVENUE:float ^=
                         (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                            -1) +
                           ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) +
                        ((R2_TOTAL_REVENUE:float ^=
                           (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          -1)))) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
            ((__domain_1:int ^=
               (((AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                  -1) +
                 AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
              -1)))))));
}

CORRECT COUNT_mLINEITEM1_L3_1_E1_3[][delta_R2_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L3_1_E1_3:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, S_SUPPKEY:int] *
  (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
     (((((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
          ((EXISTS(
              ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
             2) +
            EXISTS(
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
         ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
           ((EXISTS(
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              ((R1_TOTAL_REVENUE:float ^=
                 (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                    {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                    LINEITEM_EXTENDEDPRICE:float) +
                   (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                (R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
             (EXISTS(
                ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
               ((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                 (R1_TOTAL_REVENUE:float ^=
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) +
         ((R1_TOTAL_REVENUE:float ^=
            (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
               LINEITEM_EXTENDEDPRICE:float) +
              (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
           EXISTS(
             ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) *
        AggSum([], 
          ((__domain_1:int ^= 0) *
            ((__domain_1:int ^=
               (AggSum([], 
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   ((((R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) *
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                         (EXISTS(
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           -1))) +
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                        ((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                          ((R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1)))) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
              ((__domain_1:int ^=
                 AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                -1))))) +
       (((EXISTS(
            (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
               LINEITEM_EXTENDEDPRICE:float) +
              (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
           ((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
             ((R1_TOTAL_REVENUE:float ^=
                (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                   {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                   LINEITEM_EXTENDEDPRICE:float) +
                  (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
               (S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int)))) +
          (EXISTS(
             ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
            ((R1_TOTAL_REVENUE:float ^=
               (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                  LINEITEM_EXTENDEDPRICE:float) +
                 (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
              ((R1_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                (S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int))))) *
         AggSum([], 
           ((__domain_1:int ^= 0) *
             ((__domain_1:int ^=
                (((AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      ((((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                            (EXISTS(
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1))) +
                         (EXISTS(
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                             ((R2_TOTAL_REVENUE:float ^=
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1)))) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      ((((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          (EXISTS(
                             (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                            (EXISTS(
                               (({LINEITEM_SUPPKEY:int =
                                 delta_R2_SUPPKEY:int} *
                                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                  {LINEITEM_SHIPDATE:date >=
                                  DATE('1996-1-1')} *
                                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                  LINEITEM_EXTENDEDPRICE:float) +
                                 (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1))) +
                         (EXISTS(
                            (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                             ((R2_TOTAL_REVENUE:float ^=
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1)))) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                   -1) +
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                         ((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                          (EXISTS(
                             (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                         ((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                          (EXISTS(
                             (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                         ((R2_TOTAL_REVENUE:float ^=
                            (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                           ((R2_TOTAL_REVENUE:float ^=
                              (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                 LINEITEM_EXTENDEDPRICE:float) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
               ((__domain_1:int ^=
                  (((AggSum([], 
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                      AggSum([], 
                        (EXISTS(
                           (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                          (R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                          {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                     -1) +
                    AggSum([], 
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
                 -1))))))) +
    ((((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
        ((((EXISTS(
              (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                 LINEITEM_EXTENDEDPRICE:float) +
                (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
             ((R1_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
               ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 ((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                   (R1_TOTAL_REVENUE:float ^=
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) +
            (EXISTS(
               ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              (((R1_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                 2) +
                ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                  ((R1_TOTAL_REVENUE:float ^=
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
                    (R1_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
                (R1_TOTAL_REVENUE:float ^=
                  (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                     {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                     LINEITEM_EXTENDEDPRICE:float) +
                    (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int]))))) *
           AggSum([], 
             ((__domain_1:int ^= 0) *
               ((__domain_1:int ^=
                  (AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      ((((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                            (EXISTS(
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1))) +
                         (EXISTS(
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                             ((R2_TOTAL_REVENUE:float ^=
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1)))) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
                 ((__domain_1:int ^=
                    AggSum([], 
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                   -1))))) +
          ((((R1_TOTAL_REVENUE:float ^=
               ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              EXISTS(
                ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
              2) +
             ((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               (((R1_TOTAL_REVENUE:float ^=
                   (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                      {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                      ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                      LINEITEM_EXTENDEDPRICE:float) +
                     (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                     COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                  EXISTS(
                    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int]))) +
                 ((R1_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
                   EXISTS(
                     (({LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))))) *
            AggSum([], 
              ((__domain_1:int ^= 0) *
                ((__domain_1:int ^=
                   (((AggSum([], 
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                          (R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                          {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         ((((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             (EXISTS(
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                               (EXISTS(
                                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                    ][delta_R2_SUPPKEY:int] * -1) +
                                    COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                    ][delta_R2_SUPPKEY:int])) *
                                 -1))) +
                            (EXISTS(
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int] +
                                   delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                                ((R2_TOTAL_REVENUE:float ^=
                                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                     ][delta_R2_SUPPKEY:int] * -1) +
                                     COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                     ][delta_R2_SUPPKEY:int])) *
                                  -1)))) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         ((((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             (EXISTS(
                                (({LINEITEM_SUPPKEY:int =
                                  delta_R2_SUPPKEY:int} *
                                   {LINEITEM_SHIPDATE:date <
                                   DATE('1996-4-1')} *
                                   {LINEITEM_SHIPDATE:date >=
                                   DATE('1996-1-1')} *
                                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                   LINEITEM_EXTENDEDPRICE:float) +
                                  (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                               (EXISTS(
                                  (({LINEITEM_SUPPKEY:int =
                                    delta_R2_SUPPKEY:int} *
                                     {LINEITEM_SHIPDATE:date <
                                     DATE('1996-4-1')} *
                                     {LINEITEM_SHIPDATE:date >=
                                     DATE('1996-1-1')} *
                                     ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                     LINEITEM_EXTENDEDPRICE:float) +
                                    (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                     ][delta_R2_SUPPKEY:int] * -1) +
                                    COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                    ][delta_R2_SUPPKEY:int])) *
                                 -1))) +
                            (EXISTS(
                               (({LINEITEM_SUPPKEY:int =
                                 delta_R2_SUPPKEY:int} *
                                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                  {LINEITEM_SHIPDATE:date >=
                                  DATE('1996-1-1')} *
                                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                  LINEITEM_EXTENDEDPRICE:float) +
                                 (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int] +
                                   delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                                ((R2_TOTAL_REVENUE:float ^=
                                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                     ][delta_R2_SUPPKEY:int] * -1) +
                                     COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                     ][delta_R2_SUPPKEY:int])) *
                                  -1)))) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                      -1) +
                     AggSum([], 
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                             (EXISTS(
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                            ((R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                 LINEITEM_EXTENDEDPRICE:float) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                             (EXISTS(
                                (({LINEITEM_SUPPKEY:int =
                                  delta_R2_SUPPKEY:int} *
                                   {LINEITEM_SHIPDATE:date <
                                   DATE('1996-4-1')} *
                                   {LINEITEM_SHIPDATE:date >=
                                   DATE('1996-1-1')} *
                                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                   LINEITEM_EXTENDEDPRICE:float) +
                                  (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                            ((R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                 LINEITEM_EXTENDEDPRICE:float) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                             (EXISTS(
                                (({LINEITEM_SUPPKEY:int =
                                  delta_R2_SUPPKEY:int} *
                                   {LINEITEM_SHIPDATE:date <
                                   DATE('1996-4-1')} *
                                   {LINEITEM_SHIPDATE:date >=
                                   DATE('1996-1-1')} *
                                   ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                   LINEITEM_EXTENDEDPRICE:float) +
                                  (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                            ((R2_TOTAL_REVENUE:float ^=
                               (({LINEITEM_SUPPKEY:int =
                                 delta_R2_SUPPKEY:int} *
                                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                  {LINEITEM_SHIPDATE:date >=
                                  DATE('1996-1-1')} *
                                  ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                  LINEITEM_EXTENDEDPRICE:float) +
                                 (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 (({LINEITEM_SUPPKEY:int =
                                   delta_R2_SUPPKEY:int} *
                                    {LINEITEM_SHIPDATE:date <
                                    DATE('1996-4-1')} *
                                    {LINEITEM_SHIPDATE:date >=
                                    DATE('1996-1-1')} *
                                    ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                    LINEITEM_EXTENDEDPRICE:float) +
                                   (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                    ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
                  ((__domain_1:int ^=
                     (((AggSum([], 
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                            (R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [R2_SUPPKEY:int])) *
                            {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                         AggSum([], 
                           (EXISTS(
                              (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                 LINEITEM_EXTENDEDPRICE:float) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [R2_SUPPKEY:int])) *
                             (R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [R2_SUPPKEY:int])) *
                             {R2_TOTAL_REVENUE:float >
                             R1_TOTAL_REVENUE:float}))) *
                        -1) +
                       AggSum([], 
                         (EXISTS(
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                                LINEITEM_EXTENDEDPRICE:float) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
                    -1))))))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         AggSum([], 
           ((__domain_1:int ^= 0) *
             ((__domain_1:int ^=
                (AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                         ((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
               ((__domain_1:int ^=
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                 -1)))))) *
      -1) +
    ((R1_TOTAL_REVENUE:float ^=
       ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
      EXISTS(
        ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          ((__domain_1:int ^=
             (((AggSum([], 
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   ((((R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) *
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                         (EXISTS(
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           -1))) +
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                        ((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                          ((R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1)))) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   ((((R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) *
                       (EXISTS(
                          (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                         (EXISTS(
                            (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                               LINEITEM_EXTENDEDPRICE:float) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           -1))) +
                      (EXISTS(
                         (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                        ((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                          ((R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1)))) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                -1) +
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 (EXISTS(
                    (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                       {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 (EXISTS(
                    (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                       {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                       {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                       ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                       LINEITEM_EXTENDEDPRICE:float) +
                      (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                        {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                        LINEITEM_EXTENDEDPRICE:float) +
                       (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 ((((R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int] +
                          delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         -1))) +
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                         [delta_R2_SUPPKEY:int] * -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                      ((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                        ((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          -1)))) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 ((((R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     (EXISTS(
                        (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int] +
                          delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                       (EXISTS(
                          (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         -1))) +
                    (EXISTS(
                       (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                      ((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                        ((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          -1)))) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 ((((R2_TOTAL_REVENUE:float ^=
                      (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                         [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     (EXISTS(
                        (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int] +
                          delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                       (EXISTS(
                          (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                             LINEITEM_EXTENDEDPRICE:float) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         -1))) +
                    (EXISTS(
                       (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                      ((R2_TOTAL_REVENUE:float ^=
                         (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                            LINEITEM_EXTENDEDPRICE:float) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                        ((R2_TOTAL_REVENUE:float ^=
                           (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                              LINEITEM_EXTENDEDPRICE:float) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          -1)))) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
            ((__domain_1:int ^=
               (((AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                           LINEITEM_EXTENDEDPRICE:float) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                  -1) +
                 AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                         LINEITEM_EXTENDEDPRICE:float) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          ((-1 * LINEITEM_DISCOUNT:float) + 1) *
                          LINEITEM_EXTENDEDPRICE:float) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
              -1)))))));
}

CORRECT COUNT_mLINEITEM1[][delta_S_NAME:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, delta_S_NAME:string, delta_S_ADDRESS:string, delta_S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (((EXISTS(
     (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
        {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
        LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
       (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
       COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
    (((R1_TOTAL_REVENUE:float ^=
        (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
           LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
          (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
       (AggSum([], 
          ((__domain_1:int ^=
             AggSum([], 
               (EXISTS(
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
            (__domain_1:int ^= 0))) +
         AggSum([], 
           ((__domain_1:int ^=
              (((AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          LINEITEM_EXTENDEDPRICE:float *
                          {(-1 + LINEITEM_DISCOUNT:float)}) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                 -1) +
                AggSum([], 
                  (EXISTS(
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                AggSum([], 
                  (EXISTS(
                     (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                        {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        LINEITEM_EXTENDEDPRICE:float *
                        {(-1 + LINEITEM_DISCOUNT:float)}) +
                       (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                AggSum([], 
                  (EXISTS(
                     (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                        {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                        {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                        LINEITEM_EXTENDEDPRICE:float *
                        {(-1 + LINEITEM_DISCOUNT:float)}) +
                       (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    (R2_TOTAL_REVENUE:float ^=
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         LINEITEM_EXTENDEDPRICE:float *
                         {(-1 + LINEITEM_DISCOUNT:float)}) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                    {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
             (__domain_1:int ^= 0))))) +
      ((R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        AggSum([], 
          ((__domain_1:int ^=
             AggSum([], 
               (EXISTS(
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 (R2_TOTAL_REVENUE:float ^=
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                 {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
            (__domain_1:int ^= 0)))))) +
   (((EXISTS(
        (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
           LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
          (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
         (R1_TOTAL_REVENUE:float ^=
           (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
              LINEITEM_EXTENDEDPRICE:float *
              {(-1 + LINEITEM_DISCOUNT:float)}) +
             (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
      ((R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        EXISTS(
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) *
     AggSum([], 
       ((__domain_1:int ^=
          AggSum([], 
            (EXISTS(
               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              (R2_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
         (__domain_1:int ^= 0))) *
     -1)) *
  delta_COUNT_mLINEITEM1:int);
}

CORRECT COUNT_mLINEITEM1_L2_1[][delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L2_1:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, delta_S_SUPPKEY:int] *
  (((EXISTS(
       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
           delta_COUNT_mLINEITEM1_L2_1:float) *
          -1) +
         ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
           LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
      (EXISTS(
         (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        -1)) *
     (((R1_TOTAL_REVENUE:float ^=
         (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        (AggSum([], 
           ((__domain_1:int ^=
              AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
             (__domain_1:int ^= 0))) +
          AggSum([], 
            ((__domain_1:int ^=
               (((AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           LINEITEM_EXTENDEDPRICE:float *
                           {(-1 + LINEITEM_DISCOUNT:float)}) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                  -1) +
                 AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         LINEITEM_EXTENDEDPRICE:float *
                         {(-1 + LINEITEM_DISCOUNT:float)}) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         LINEITEM_EXTENDEDPRICE:float *
                         {(-1 + LINEITEM_DISCOUNT:float)}) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          LINEITEM_EXTENDEDPRICE:float *
                          {(-1 + LINEITEM_DISCOUNT:float)}) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
              (__domain_1:int ^= 0))))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
         AggSum([], 
           ((__domain_1:int ^=
              AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
             (__domain_1:int ^= 0)))))) +
    ((EXISTS(
        (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
           LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
          (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
       EXISTS(
         (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_1:float) *
            -1) +
           ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
             LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
       (EXISTS(
          (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
             LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
            (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
         -1)) *
      ((((R1_TOTAL_REVENUE:float ^=
           (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
               delta_COUNT_mLINEITEM1_L2_1:float) *
              -1) +
             ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               LINEITEM_EXTENDEDPRICE:float *
               {(-1 + LINEITEM_DISCOUNT:float)}) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          ((R1_TOTAL_REVENUE:float ^=
             (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                LINEITEM_EXTENDEDPRICE:float *
                {(-1 + LINEITEM_DISCOUNT:float)}) +
               (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)) *
         (AggSum([], 
            ((__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              (__domain_1:int ^= 0))) +
           AggSum([], 
             ((__domain_1:int ^=
                (((AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            LINEITEM_EXTENDEDPRICE:float *
                            {(-1 + LINEITEM_DISCOUNT:float)}) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                   -1) +
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          LINEITEM_EXTENDEDPRICE:float *
                          {(-1 + LINEITEM_DISCOUNT:float)}) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          LINEITEM_EXTENDEDPRICE:float *
                          {(-1 + LINEITEM_DISCOUNT:float)}) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           LINEITEM_EXTENDEDPRICE:float *
                           {(-1 + LINEITEM_DISCOUNT:float)}) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
               (__domain_1:int ^= 0))))) +
        (((R1_TOTAL_REVENUE:float ^=
            (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                delta_COUNT_mLINEITEM1_L2_1:float) *
               -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
           ((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             -1)) *
          AggSum([], 
            ((__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              (__domain_1:int ^= 0)))))) +
    ((((EXISTS(
          (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
              delta_COUNT_mLINEITEM1_L2_1:float) *
             -1) +
            ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
              LINEITEM_EXTENDEDPRICE:float *
              {(-1 + LINEITEM_DISCOUNT:float)}) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
         (EXISTS(
            (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               LINEITEM_EXTENDEDPRICE:float *
               {(-1 + LINEITEM_DISCOUNT:float)}) +
              (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
           -1)) *
        ((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          (R1_TOTAL_REVENUE:float ^=
            (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               LINEITEM_EXTENDEDPRICE:float *
               {(-1 + LINEITEM_DISCOUNT:float)}) +
              (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
       ((EXISTS(
           (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
              LINEITEM_EXTENDEDPRICE:float *
              {(-1 + LINEITEM_DISCOUNT:float)}) +
             (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          EXISTS(
            (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                delta_COUNT_mLINEITEM1_L2_1:float) *
               -1) +
              ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                LINEITEM_EXTENDEDPRICE:float *
                {(-1 + LINEITEM_DISCOUNT:float)}) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          (EXISTS(
             (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                LINEITEM_EXTENDEDPRICE:float *
                {(-1 + LINEITEM_DISCOUNT:float)}) +
               (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)) *
         ((((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             (R1_TOTAL_REVENUE:float ^=
               (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  LINEITEM_EXTENDEDPRICE:float *
                  {(-1 + LINEITEM_DISCOUNT:float)}) +
                 (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) *
            -1) +
           (R1_TOTAL_REVENUE:float ^=
             (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                 delta_COUNT_mLINEITEM1_L2_1:float) *
                -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
           (R1_TOTAL_REVENUE:float ^=
             (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                 delta_COUNT_mLINEITEM1_L2_1:float) *
                -1) +
               ((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                 LINEITEM_EXTENDEDPRICE:float *
                 {(-1 + LINEITEM_DISCOUNT:float)}) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
       (((R1_TOTAL_REVENUE:float ^=
           (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
               delta_COUNT_mLINEITEM1_L2_1:float) *
              -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          ((R1_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)) *
         EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
       (((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          (R1_TOTAL_REVENUE:float ^=
            (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                delta_COUNT_mLINEITEM1_L2_1:float) *
               -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          ((R1_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)) *
         (EXISTS(
            (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
                delta_COUNT_mLINEITEM1_L2_1:float) *
               -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
           (EXISTS(
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             -1)))) *
      AggSum([], 
        ((__domain_1:int ^=
           AggSum([], 
             (EXISTS(
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               (R2_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
          (__domain_1:int ^= 0))) *
      -1)));
}

CORRECT COUNT_mLINEITEM1_L2_3[][delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L2_3:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, delta_S_SUPPKEY:int] *
  (((EXISTS(
       (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
          LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
         (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
         delta_COUNT_mLINEITEM1_L2_3:float)) +
      (EXISTS(
         (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        -1)) *
     (((R1_TOTAL_REVENUE:float ^=
         (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        (AggSum([], 
           ((__domain_1:int ^=
              AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
             (__domain_1:int ^= 0))) +
          AggSum([], 
            ((__domain_1:int ^=
               (((AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           LINEITEM_EXTENDEDPRICE:float *
                           {(-1 + LINEITEM_DISCOUNT:float)}) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                  -1) +
                 AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         LINEITEM_EXTENDEDPRICE:float *
                         {(-1 + LINEITEM_DISCOUNT:float)}) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                 AggSum([], 
                   (EXISTS(
                      (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                         LINEITEM_EXTENDEDPRICE:float *
                         {(-1 + LINEITEM_DISCOUNT:float)}) +
                        (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          LINEITEM_EXTENDEDPRICE:float *
                          {(-1 + LINEITEM_DISCOUNT:float)}) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
              (__domain_1:int ^= 0))))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
         AggSum([], 
           ((__domain_1:int ^=
              AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
             (__domain_1:int ^= 0)))))) +
    ((EXISTS(
        (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
           LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
          (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
       EXISTS(
         (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
           delta_COUNT_mLINEITEM1_L2_3:float)) +
       (EXISTS(
          (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
             LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
            (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
         -1)) *
      ((((R1_TOTAL_REVENUE:float ^=
           (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
              LINEITEM_EXTENDEDPRICE:float *
              {(-1 + LINEITEM_DISCOUNT:float)}) +
             (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_3:float)) +
          ((R1_TOTAL_REVENUE:float ^=
             (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                LINEITEM_EXTENDEDPRICE:float *
                {(-1 + LINEITEM_DISCOUNT:float)}) +
               (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)) *
         (AggSum([], 
            ((__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              (__domain_1:int ^= 0))) +
           AggSum([], 
             ((__domain_1:int ^=
                (((AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                    AggSum([], 
                      (EXISTS(
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            LINEITEM_EXTENDEDPRICE:float *
                            {(-1 + LINEITEM_DISCOUNT:float)}) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                   -1) +
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          LINEITEM_EXTENDEDPRICE:float *
                          {(-1 + LINEITEM_DISCOUNT:float)}) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    (EXISTS(
                       (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                          {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                          {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                          LINEITEM_EXTENDEDPRICE:float *
                          {(-1 + LINEITEM_DISCOUNT:float)}) +
                         (COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           LINEITEM_EXTENDEDPRICE:float *
                           {(-1 + LINEITEM_DISCOUNT:float)}) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
               (__domain_1:int ^= 0))))) +
        (((R1_TOTAL_REVENUE:float ^=
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
              delta_COUNT_mLINEITEM1_L2_3:float)) +
           ((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             -1)) *
          AggSum([], 
            ((__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              (__domain_1:int ^= 0)))))) +
    ((((EXISTS(
          (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
             LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
            (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
            delta_COUNT_mLINEITEM1_L2_3:float)) +
         (EXISTS(
            (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               LINEITEM_EXTENDEDPRICE:float *
               {(-1 + LINEITEM_DISCOUNT:float)}) +
              (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
           -1)) *
        ((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          (R1_TOTAL_REVENUE:float ^=
            (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               LINEITEM_EXTENDEDPRICE:float *
               {(-1 + LINEITEM_DISCOUNT:float)}) +
              (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])))) +
       ((EXISTS(
           (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
              LINEITEM_EXTENDEDPRICE:float *
              {(-1 + LINEITEM_DISCOUNT:float)}) +
             (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          EXISTS(
            (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               LINEITEM_EXTENDEDPRICE:float *
               {(-1 + LINEITEM_DISCOUNT:float)}) +
              (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
              delta_COUNT_mLINEITEM1_L2_3:float)) +
          (EXISTS(
             (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                LINEITEM_EXTENDEDPRICE:float *
                {(-1 + LINEITEM_DISCOUNT:float)}) +
               (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)) *
         ((((R1_TOTAL_REVENUE:float ^=
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
             (R1_TOTAL_REVENUE:float ^=
               (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                  {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                  LINEITEM_EXTENDEDPRICE:float *
                  {(-1 + LINEITEM_DISCOUNT:float)}) +
                 (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) *
            -1) +
           (R1_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
               delta_COUNT_mLINEITEM1_L2_3:float)) +
           (R1_TOTAL_REVENUE:float ^=
             (((delta_S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                LINEITEM_EXTENDEDPRICE:float *
                {(-1 + LINEITEM_DISCOUNT:float)}) +
               (COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
               delta_COUNT_mLINEITEM1_L2_3:float)))) +
       (((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_3:float)) +
          ((R1_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)) *
         EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
       (((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
          (R1_TOTAL_REVENUE:float ^=
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
              delta_COUNT_mLINEITEM1_L2_3:float)) +
          ((R1_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
            -1)) *
         (EXISTS(
            ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
              delta_COUNT_mLINEITEM1_L2_3:float)) +
           (EXISTS(
              ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
                COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
             -1)))) *
      AggSum([], 
        ((__domain_1:int ^=
           AggSum([], 
             (EXISTS(
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               (R2_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
          (__domain_1:int ^= 0))) *
      -1)));
}

CORRECT COUNT_mLINEITEM1_L3_1_E1_1[][delta_R2_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L3_1_E1_1:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, S_SUPPKEY:int] *
  ((EXISTS(
      (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
         LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
        (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
        COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
     (((R1_TOTAL_REVENUE:float ^=
         (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
        (AggSum([], 
           ((__domain_1:int ^= 0) *
             ((__domain_1:int ^=
                (AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                             -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         ((R2_TOTAL_REVENUE:float ^=
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
               ((__domain_1:int ^=
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                 -1)))) +
          AggSum([], 
            ((__domain_1:int ^= 0) *
              ((__domain_1:int ^=
                 (((AggSum([], 
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             LINEITEM_EXTENDEDPRICE:float *
                             {(-1 + LINEITEM_DISCOUNT:float)}) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                 -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) +
                             (EXISTS(
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            ((R2_TOTAL_REVENUE:float ^=
                               (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] +
                                   delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                  -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                  delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                 -1) +
                                ({LINEITEM_SUPPKEY:int =
                                 delta_R2_SUPPKEY:int} *
                                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                  {LINEITEM_SHIPDATE:date >=
                                  DATE('1996-1-1')} *
                                  LINEITEM_EXTENDEDPRICE:float *
                                  {(-1 + LINEITEM_DISCOUNT:float)}) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) +
                             (EXISTS(
                                (({LINEITEM_SUPPKEY:int =
                                  delta_R2_SUPPKEY:int} *
                                   {LINEITEM_SHIPDATE:date <
                                   DATE('1996-4-1')} *
                                   {LINEITEM_SHIPDATE:date >=
                                   DATE('1996-1-1')} *
                                   LINEITEM_EXTENDEDPRICE:float *
                                   {(-1 + LINEITEM_DISCOUNT:float)}) +
                                  (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 LINEITEM_EXTENDEDPRICE:float *
                                 {(-1 + LINEITEM_DISCOUNT:float)}) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            ((R2_TOTAL_REVENUE:float ^=
                               (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] +
                                   delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                  -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                    -1) +
                   AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           LINEITEM_EXTENDEDPRICE:float *
                           {(-1 + LINEITEM_DISCOUNT:float)}) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           LINEITEM_EXTENDEDPRICE:float *
                           {(-1 + LINEITEM_DISCOUNT:float)}) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            LINEITEM_EXTENDEDPRICE:float *
                            {(-1 + LINEITEM_DISCOUNT:float)}) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     ((((R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         (EXISTS(
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) +
                           (EXISTS(
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1))) +
                        (EXISTS(
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          ((R2_TOTAL_REVENUE:float ^=
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) +
                            ((R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1)))) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     ((((R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         (EXISTS(
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                LINEITEM_EXTENDEDPRICE:float *
                                {(-1 + LINEITEM_DISCOUNT:float)}) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) +
                           (EXISTS(
                              (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 LINEITEM_EXTENDEDPRICE:float *
                                 {(-1 + LINEITEM_DISCOUNT:float)}) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1))) +
                        (EXISTS(
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               LINEITEM_EXTENDEDPRICE:float *
                               {(-1 + LINEITEM_DISCOUNT:float)}) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          ((R2_TOTAL_REVENUE:float ^=
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) +
                            ((R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1)))) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     ((((R2_TOTAL_REVENUE:float ^=
                          (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             LINEITEM_EXTENDEDPRICE:float *
                             {(-1 + LINEITEM_DISCOUNT:float)}) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         (EXISTS(
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                LINEITEM_EXTENDEDPRICE:float *
                                {(-1 + LINEITEM_DISCOUNT:float)}) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) +
                           (EXISTS(
                              (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 LINEITEM_EXTENDEDPRICE:float *
                                 {(-1 + LINEITEM_DISCOUNT:float)}) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1))) +
                        (EXISTS(
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               LINEITEM_EXTENDEDPRICE:float *
                               {(-1 + LINEITEM_DISCOUNT:float)}) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          ((R2_TOTAL_REVENUE:float ^=
                             (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                                -1) +
                               ({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 LINEITEM_EXTENDEDPRICE:float *
                                 {(-1 + LINEITEM_DISCOUNT:float)}) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) +
                            ((R2_TOTAL_REVENUE:float ^=
                               (({LINEITEM_SUPPKEY:int =
                                 delta_R2_SUPPKEY:int} *
                                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                  {LINEITEM_SHIPDATE:date >=
                                  DATE('1996-1-1')} *
                                  LINEITEM_EXTENDEDPRICE:float *
                                  {(-1 + LINEITEM_DISCOUNT:float)}) +
                                 (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1)))) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
                ((__domain_1:int ^=
                   (((AggSum([], 
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                          (R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                          {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               LINEITEM_EXTENDEDPRICE:float *
                               {(-1 + LINEITEM_DISCOUNT:float)}) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                      -1) +
                     AggSum([], 
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             LINEITEM_EXTENDEDPRICE:float *
                             {(-1 + LINEITEM_DISCOUNT:float)}) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             LINEITEM_EXTENDEDPRICE:float *
                             {(-1 + LINEITEM_DISCOUNT:float)}) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              LINEITEM_EXTENDEDPRICE:float *
                              {(-1 + LINEITEM_DISCOUNT:float)}) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
                  -1)))))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         AggSum([], 
           ((__domain_1:int ^= 0) *
             ((__domain_1:int ^=
                (AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                              -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                             -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         ((R2_TOTAL_REVENUE:float ^=
                            (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                               -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
               ((__domain_1:int ^=
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                 -1))))))) +
    (((EXISTS(
         (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
        ((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
          (R1_TOTAL_REVENUE:float ^=
            (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               LINEITEM_EXTENDEDPRICE:float *
               {(-1 + LINEITEM_DISCOUNT:float)}) +
              (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          ((__domain_1:int ^=
             (AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 ((((R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     (EXISTS(
                        (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int])) +
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         -1))) +
                    (EXISTS(
                       (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int])) *
                      ((R2_TOTAL_REVENUE:float ^=
                         (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                            -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) +
                        ((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          -1)))) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
            ((__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              -1)))) *
      -1)));
}

CORRECT COUNT_mLINEITEM1_L3_1_E1_3[][delta_R2_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L3_1_E1_3:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   COUNT(int)[][S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_PHONE:string, R1_TOTAL_REVENUE:float] += 
  (COUNT_mLINEITEM1(int)[]
 [S_NAME:string, S_ADDRESS:string, S_PHONE:string, S_SUPPKEY:int] *
  ((EXISTS(
      (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
         {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
         {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
         LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
        (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
        COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
     (((R1_TOTAL_REVENUE:float ^=
         (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
        (AggSum([], 
           ((__domain_1:int ^= 0) *
             ((__domain_1:int ^=
                (AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                         ((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
               ((__domain_1:int ^=
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                 -1)))) +
          AggSum([], 
            ((__domain_1:int ^= 0) *
              ((__domain_1:int ^=
                 (((AggSum([], 
                      (EXISTS(
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                        (R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                        {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             LINEITEM_EXTENDEDPRICE:float *
                             {(-1 + LINEITEM_DISCOUNT:float)}) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                             (EXISTS(
                                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                            ((R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       ((((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int])) *
                           (EXISTS(
                              (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 LINEITEM_EXTENDEDPRICE:float *
                                 {(-1 + LINEITEM_DISCOUNT:float)}) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int] +
                                delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                             (EXISTS(
                                (({LINEITEM_SUPPKEY:int =
                                  delta_R2_SUPPKEY:int} *
                                   {LINEITEM_SHIPDATE:date <
                                   DATE('1996-4-1')} *
                                   {LINEITEM_SHIPDATE:date >=
                                   DATE('1996-1-1')} *
                                   LINEITEM_EXTENDEDPRICE:float *
                                   {(-1 + LINEITEM_DISCOUNT:float)}) +
                                  (COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                  COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                  [delta_R2_SUPPKEY:int])) *
                               -1))) +
                          (EXISTS(
                             (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                LINEITEM_EXTENDEDPRICE:float *
                                {(-1 + LINEITEM_DISCOUNT:float)}) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                            ((R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int] +
                                 delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                              ((R2_TOTAL_REVENUE:float ^=
                                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[
                                   ][delta_R2_SUPPKEY:int] * -1) +
                                   COUNT_mLINEITEM1_L3_1_E1_3(float)[
                                   ][delta_R2_SUPPKEY:int])) *
                                -1)))) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                    -1) +
                   AggSum([], 
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           LINEITEM_EXTENDEDPRICE:float *
                           {(-1 + LINEITEM_DISCOUNT:float)}) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     (EXISTS(
                        (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                           {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                           {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                           LINEITEM_EXTENDEDPRICE:float *
                           {(-1 + LINEITEM_DISCOUNT:float)}) +
                          (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                       (R2_TOTAL_REVENUE:float ^=
                         (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                            LINEITEM_EXTENDEDPRICE:float *
                            {(-1 + LINEITEM_DISCOUNT:float)}) +
                           (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [R2_SUPPKEY:int])) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     ((((R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         (EXISTS(
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                           (EXISTS(
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1))) +
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                          ((R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                            ((R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1)))) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     ((((R2_TOTAL_REVENUE:float ^=
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         (EXISTS(
                            (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               LINEITEM_EXTENDEDPRICE:float *
                               {(-1 + LINEITEM_DISCOUNT:float)}) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                           (EXISTS(
                              (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 LINEITEM_EXTENDEDPRICE:float *
                                 {(-1 + LINEITEM_DISCOUNT:float)}) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1))) +
                        (EXISTS(
                           (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              LINEITEM_EXTENDEDPRICE:float *
                              {(-1 + LINEITEM_DISCOUNT:float)}) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                          ((R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                            ((R2_TOTAL_REVENUE:float ^=
                               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1)))) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                   AggSum([], 
                     ((((R2_TOTAL_REVENUE:float ^=
                          (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             LINEITEM_EXTENDEDPRICE:float *
                             {(-1 + LINEITEM_DISCOUNT:float)}) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         (EXISTS(
                            (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               LINEITEM_EXTENDEDPRICE:float *
                               {(-1 + LINEITEM_DISCOUNT:float)}) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                           (EXISTS(
                              (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                 {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                 {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                 LINEITEM_EXTENDEDPRICE:float *
                                 {(-1 + LINEITEM_DISCOUNT:float)}) +
                                (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                 [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1))) +
                        (EXISTS(
                           (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              LINEITEM_EXTENDEDPRICE:float *
                              {(-1 + LINEITEM_DISCOUNT:float)}) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                          ((R2_TOTAL_REVENUE:float ^=
                             (({LINEITEM_SUPPKEY:int = delta_R2_SUPPKEY:int} *
                                {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                                LINEITEM_EXTENDEDPRICE:float *
                                {(-1 + LINEITEM_DISCOUNT:float)}) +
                               (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int] +
                               delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                            ((R2_TOTAL_REVENUE:float ^=
                               (({LINEITEM_SUPPKEY:int =
                                 delta_R2_SUPPKEY:int} *
                                  {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                                  {LINEITEM_SHIPDATE:date >=
                                  DATE('1996-1-1')} *
                                  LINEITEM_EXTENDEDPRICE:float *
                                  {(-1 + LINEITEM_DISCOUNT:float)}) +
                                 (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                  [delta_R2_SUPPKEY:int] * -1) +
                                 COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                 [delta_R2_SUPPKEY:int])) *
                              -1)))) *
                       {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
                ((__domain_1:int ^=
                   (((AggSum([], 
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                          (R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                          {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                       AggSum([], 
                         (EXISTS(
                            (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                               LINEITEM_EXTENDEDPRICE:float *
                               {(-1 + LINEITEM_DISCOUNT:float)}) +
                              (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [R2_SUPPKEY:int])) *
                           (R2_TOTAL_REVENUE:float ^=
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [R2_SUPPKEY:int])) *
                           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                      -1) +
                     AggSum([], 
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             LINEITEM_EXTENDEDPRICE:float *
                             {(-1 + LINEITEM_DISCOUNT:float)}) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                     AggSum([], 
                       (EXISTS(
                          (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                             {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                             {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                             LINEITEM_EXTENDEDPRICE:float *
                             {(-1 + LINEITEM_DISCOUNT:float)}) +
                            (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [R2_SUPPKEY:int])) *
                         (R2_TOTAL_REVENUE:float ^=
                           (((R2_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
                              {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
                              {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
                              LINEITEM_EXTENDEDPRICE:float *
                              {(-1 + LINEITEM_DISCOUNT:float)}) +
                             (COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [R2_SUPPKEY:int])) *
                         {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) *
                  -1)))))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         AggSum([], 
           ((__domain_1:int ^= 0) *
             ((__domain_1:int ^=
                (AggSum([], 
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                         -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     (R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                     {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
                  AggSum([], 
                    ((((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int])) *
                        (EXISTS(
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int] +
                             delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                          (EXISTS(
                             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                               [delta_R2_SUPPKEY:int] * -1) +
                               COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                               [delta_R2_SUPPKEY:int])) *
                            -1))) +
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int] +
                            delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                         ((R2_TOTAL_REVENUE:float ^=
                            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                              [delta_R2_SUPPKEY:int] * -1) +
                              COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                              [delta_R2_SUPPKEY:int] +
                              delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                           ((R2_TOTAL_REVENUE:float ^=
                              ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                                [delta_R2_SUPPKEY:int] * -1) +
                                COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                                [delta_R2_SUPPKEY:int])) *
                             -1)))) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
               ((__domain_1:int ^=
                  AggSum([], 
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                          -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      (R2_TOTAL_REVENUE:float ^=
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                           -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                      {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
                 -1))))))) +
    (((EXISTS(
         (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
            {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
            {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
            LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)}) +
           (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
        ((R1_TOTAL_REVENUE:float ^=
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) +
          (R1_TOTAL_REVENUE:float ^=
            (((S_SUPPKEY:int ^= LINEITEM_SUPPKEY:int) *
               {LINEITEM_SHIPDATE:date < DATE('1996-4-1')} *
               {LINEITEM_SHIPDATE:date >= DATE('1996-1-1')} *
               LINEITEM_EXTENDEDPRICE:float *
               {(-1 + LINEITEM_DISCOUNT:float)}) +
              (COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])) *
         EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][S_SUPPKEY:int])))) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          ((__domain_1:int ^=
             (AggSum([], 
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  (R2_TOTAL_REVENUE:float ^=
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                  {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
               AggSum([], 
                 ((((R2_TOTAL_REVENUE:float ^=
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     (EXISTS(
                        ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                          [delta_R2_SUPPKEY:int] * -1) +
                          COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                          [delta_R2_SUPPKEY:int] +
                          delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                       (EXISTS(
                          ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                            [delta_R2_SUPPKEY:int] * -1) +
                            COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                            [delta_R2_SUPPKEY:int])) *
                         -1))) +
                    (EXISTS(
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                         [delta_R2_SUPPKEY:int] * -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                      ((R2_TOTAL_REVENUE:float ^=
                         ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                           [delta_R2_SUPPKEY:int] * -1) +
                           COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                           [delta_R2_SUPPKEY:int] +
                           delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                        ((R2_TOTAL_REVENUE:float ^=
                           ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                             [delta_R2_SUPPKEY:int] * -1) +
                             COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                             [delta_R2_SUPPKEY:int])) *
                          -1)))) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
            ((__domain_1:int ^=
               AggSum([], 
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   (R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
                   {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
              -1)))) *
      -1)));
}

CORRECT COUNT_mLINEITEM1_L2_1[][delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L2_1:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ({delta_S_SUPPKEY:int = SUPPLIER_SUPPKEY:int} *
  ((((R1_TOTAL_REVENUE:float ^=
       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
           delta_COUNT_mLINEITEM1_L2_1:float) *
          -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
      ((R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        -1)) *
     EXISTS(
       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
    (((R1_TOTAL_REVENUE:float ^=
        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
       (R1_TOTAL_REVENUE:float ^=
         (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_1:float) *
            -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
         -1)) *
      (EXISTS(
         (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_1:float) *
            -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
        (EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
          -1)))) *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([], 
         (EXISTS(
            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           (R2_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
      (__domain_1:int ^= 0))));
}

CORRECT COUNT_mLINEITEM1_L2_3[][delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L2_3:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ({delta_S_SUPPKEY:int = SUPPLIER_SUPPKEY:int} *
  ((((R1_TOTAL_REVENUE:float ^=
       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
         delta_COUNT_mLINEITEM1_L2_3:float)) +
      ((R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        -1)) *
     EXISTS(
       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
    (((R1_TOTAL_REVENUE:float ^=
        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
       (R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
           delta_COUNT_mLINEITEM1_L2_3:float)) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
         -1)) *
      (EXISTS(
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
           delta_COUNT_mLINEITEM1_L2_3:float)) +
        (EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
          -1)))) *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([], 
         (EXISTS(
            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           (R2_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
      (__domain_1:int ^= 0))));
}

CORRECT COUNT_mLINEITEM1_L3_1_E1_1[][delta_R2_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L3_1_E1_1:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ((R1_TOTAL_REVENUE:float ^=
   ((COUNT_mLINEITEM1_L2_1(float)[][SUPPLIER_SUPPKEY:int] * -1) +
     COUNT_mLINEITEM1_L2_3(float)[][SUPPLIER_SUPPKEY:int])) *
  EXISTS(
    ((COUNT_mLINEITEM1_L2_1(float)[][SUPPLIER_SUPPKEY:int] * -1) +
      COUNT_mLINEITEM1_L2_3(float)[][SUPPLIER_SUPPKEY:int])) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      ((__domain_1:int ^=
         (AggSum([], 
            (EXISTS(
               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              (R2_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
           AggSum([], 
             ((((R2_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][delta_R2_SUPPKEY:int] *
                     -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][delta_R2_SUPPKEY:int])) *
                 (EXISTS(
                    (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                       [delta_R2_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                      [delta_R2_SUPPKEY:int])) +
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     -1))) +
                (EXISTS(
                   (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                      [delta_R2_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                     [delta_R2_SUPPKEY:int])) *
                  ((R2_TOTAL_REVENUE:float ^=
                     (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                       [delta_R2_SUPPKEY:int])) +
                    ((R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                         [delta_R2_SUPPKEY:int] * -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int])) *
                      -1)))) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
        ((__domain_1:int ^=
           AggSum([], 
             (EXISTS(
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               (R2_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
          -1)))));
}

CORRECT COUNT_mLINEITEM1_L3_1_E1_3[][delta_R2_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L3_1_E1_3:float FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ((R1_TOTAL_REVENUE:float ^=
   ((COUNT_mLINEITEM1_L2_1(float)[][SUPPLIER_SUPPKEY:int] * -1) +
     COUNT_mLINEITEM1_L2_3(float)[][SUPPLIER_SUPPKEY:int])) *
  EXISTS(
    ((COUNT_mLINEITEM1_L2_1(float)[][SUPPLIER_SUPPKEY:int] * -1) +
      COUNT_mLINEITEM1_L2_3(float)[][SUPPLIER_SUPPKEY:int])) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      ((__domain_1:int ^=
         (AggSum([], 
            (EXISTS(
               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              (R2_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
           AggSum([], 
             ((((R2_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][delta_R2_SUPPKEY:int] *
                     -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][delta_R2_SUPPKEY:int])) *
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                      [delta_R2_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                      [delta_R2_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     -1))) +
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                     [delta_R2_SUPPKEY:int] * -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                     [delta_R2_SUPPKEY:int] +
                     delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                  ((R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                       [delta_R2_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                       [delta_R2_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                    ((R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                         [delta_R2_SUPPKEY:int] * -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int])) *
                      -1)))) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
        ((__domain_1:int ^=
           AggSum([], 
             (EXISTS(
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               (R2_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
          -1)))));
}

CORRECT COUNT_mLINEITEM1_L2_1[][delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L2_1:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ({delta_S_SUPPKEY:int = SUPPLIER_SUPPKEY:int} *
  ((((R1_TOTAL_REVENUE:float ^=
       (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
           delta_COUNT_mLINEITEM1_L2_1:float) *
          -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
      ((R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        -1)) *
     EXISTS(
       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
    (((R1_TOTAL_REVENUE:float ^=
        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
       (R1_TOTAL_REVENUE:float ^=
         (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_1:float) *
            -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
         -1)) *
      (EXISTS(
         (((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] +
             delta_COUNT_mLINEITEM1_L2_1:float) *
            -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
        (EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
          -1)))) *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([], 
         (EXISTS(
            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           (R2_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
      (__domain_1:int ^= 0))) *
  -1);
}

CORRECT COUNT_mLINEITEM1_L2_3[][delta_S_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L2_3:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][delta_S_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ({delta_S_SUPPKEY:int = SUPPLIER_SUPPKEY:int} *
  ((((R1_TOTAL_REVENUE:float ^=
       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
         delta_COUNT_mLINEITEM1_L2_3:float)) +
      ((R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
        -1)) *
     EXISTS(
       ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
         COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int]))) +
    (((R1_TOTAL_REVENUE:float ^=
        ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
          COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) +
       (R1_TOTAL_REVENUE:float ^=
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
           delta_COUNT_mLINEITEM1_L2_3:float)) +
       ((R1_TOTAL_REVENUE:float ^=
          ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
            COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
         -1)) *
      (EXISTS(
         ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
           COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int] +
           delta_COUNT_mLINEITEM1_L2_3:float)) +
        (EXISTS(
           ((COUNT_mLINEITEM1_L2_1(float)[][delta_S_SUPPKEY:int] * -1) +
             COUNT_mLINEITEM1_L2_3(float)[][delta_S_SUPPKEY:int])) *
          -1)))) *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([], 
         (EXISTS(
            ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
              COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           (R2_TOTAL_REVENUE:float ^=
             ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
               COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
           {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
      (__domain_1:int ^= 0))) *
  -1);
}

CORRECT COUNT_mLINEITEM1_L3_1_E1_1[][delta_R2_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L3_1_E1_1:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ((R1_TOTAL_REVENUE:float ^=
   ((COUNT_mLINEITEM1_L2_1(float)[][SUPPLIER_SUPPKEY:int] * -1) +
     COUNT_mLINEITEM1_L2_3(float)[][SUPPLIER_SUPPKEY:int])) *
  EXISTS(
    ((COUNT_mLINEITEM1_L2_1(float)[][SUPPLIER_SUPPKEY:int] * -1) +
      COUNT_mLINEITEM1_L2_3(float)[][SUPPLIER_SUPPKEY:int])) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      ((__domain_1:int ^=
         (AggSum([], 
            (EXISTS(
               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              (R2_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
           AggSum([], 
             ((((R2_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][delta_R2_SUPPKEY:int] *
                     -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][delta_R2_SUPPKEY:int])) *
                 (EXISTS(
                    (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                       [delta_R2_SUPPKEY:int] +
                        delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                       -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                      [delta_R2_SUPPKEY:int])) +
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     -1))) +
                (EXISTS(
                   (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                      [delta_R2_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                      -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                     [delta_R2_SUPPKEY:int])) *
                  ((R2_TOTAL_REVENUE:float ^=
                     (((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] +
                         delta_COUNT_mLINEITEM1_L3_1_E1_1:float) *
                        -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                       [delta_R2_SUPPKEY:int])) +
                    ((R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                         [delta_R2_SUPPKEY:int] * -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int])) *
                      -1)))) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
        ((__domain_1:int ^=
           AggSum([], 
             (EXISTS(
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               (R2_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
          -1)))) *
  -1);
}

CORRECT COUNT_mLINEITEM1_L3_1_E1_3[][delta_R2_SUPPKEY:int] += delta_COUNT_mLINEITEM1_L3_1_E1_3:float FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, R1_TOTAL_REVENUE:float] += 
  ((R1_TOTAL_REVENUE:float ^=
   ((COUNT_mLINEITEM1_L2_1(float)[][SUPPLIER_SUPPKEY:int] * -1) +
     COUNT_mLINEITEM1_L2_3(float)[][SUPPLIER_SUPPKEY:int])) *
  EXISTS(
    ((COUNT_mLINEITEM1_L2_1(float)[][SUPPLIER_SUPPKEY:int] * -1) +
      COUNT_mLINEITEM1_L2_3(float)[][SUPPLIER_SUPPKEY:int])) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      ((__domain_1:int ^=
         (AggSum([], 
            (EXISTS(
               ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                 COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              (R2_TOTAL_REVENUE:float ^=
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
              {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})) +
           AggSum([], 
             ((((R2_TOTAL_REVENUE:float ^=
                  ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][delta_R2_SUPPKEY:int] *
                     -1) +
                    COUNT_mLINEITEM1_L3_1_E1_3(float)[][delta_R2_SUPPKEY:int])) *
                 (EXISTS(
                    ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                      [delta_R2_SUPPKEY:int] * -1) +
                      COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                      [delta_R2_SUPPKEY:int] +
                      delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                   (EXISTS(
                      ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                        [delta_R2_SUPPKEY:int] * -1) +
                        COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                        [delta_R2_SUPPKEY:int])) *
                     -1))) +
                (EXISTS(
                   ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                     [delta_R2_SUPPKEY:int] * -1) +
                     COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                     [delta_R2_SUPPKEY:int] +
                     delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) *
                  ((R2_TOTAL_REVENUE:float ^=
                     ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                       [delta_R2_SUPPKEY:int] * -1) +
                       COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                       [delta_R2_SUPPKEY:int] +
                       delta_COUNT_mLINEITEM1_L3_1_E1_3:float)) +
                    ((R2_TOTAL_REVENUE:float ^=
                       ((COUNT_mLINEITEM1_L3_1_E1_1(float)[]
                         [delta_R2_SUPPKEY:int] * -1) +
                         COUNT_mLINEITEM1_L3_1_E1_3(float)[]
                         [delta_R2_SUPPKEY:int])) *
                      -1)))) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float})))) +
        ((__domain_1:int ^=
           AggSum([], 
             (EXISTS(
                ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                  COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               (R2_TOTAL_REVENUE:float ^=
                 ((COUNT_mLINEITEM1_L3_1_E1_1(float)[][R2_SUPPKEY:int] * -1) +
                   COUNT_mLINEITEM1_L3_1_E1_3(float)[][R2_SUPPKEY:int])) *
               {R2_TOTAL_REVENUE:float > R1_TOTAL_REVENUE:float}))) *
          -1)))) *
  -1);
}
