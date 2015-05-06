-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem_small.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM ORDERS(ORDERS_ORDERKEY int, ORDERS_CUSTKEY int, ORDERS_ORDERSTATUS string, ORDERS_TOTALPRICE float, ORDERS_ORDERDATE date, ORDERS_ORDERPRIORITY string, ORDERS_CLERK string, ORDERS_SHIPPRIORITY int, ORDERS_COMMENT string)
  FROM FILE 'data/tpch/orders_small.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM CUSTOMER(CUSTOMER_CUSTKEY int, CUSTOMER_NAME string, CUSTOMER_ADDRESS string, CUSTOMER_NATIONKEY int, CUSTOMER_PHONE string, CUSTOMER_ACCTBAL float, CUSTOMER_MKTSEGMENT string, CUSTOMER_COMMENT string)
  FROM FILE 'data/tpch/customer.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP QUERY18(float)[]
[C_NAME:string, C_CUSTKEY:int, O_ORDERKEY:int, O_ORDERDATE:date,
   O_TOTALPRICE:float] := 
(EXISTS(
   (EXISTS(
      AggSum([O_ORDERKEY:int], 
        (LINEITEM(O_ORDERKEY:int, L2_PARTKEY:int, L2_SUPPKEY:int,
                    L2_LINENUMBER:int, L2_QUANTITY:float,
                    L2_EXTENDEDPRICE:float, L2_DISCOUNT:float, L2_TAX:float,
                    L2_RETURNFLAG:string, L2_LINESTATUS:string,
                    L2_SHIPDATE:date, L2_COMMITDATE:date,
                    L2_RECEIPTDATE:date, L2_SHIPINSTRUCT:string,
                    L2_SHIPMODE:string, L2_COMMENT:string) *
          L2_QUANTITY:float))) *
     AggSum([], 
       ((L3_QTY:float ^=
          AggSum([O_ORDERKEY:int], 
            (LINEITEM(O_ORDERKEY:int, L2_PARTKEY:int, L2_SUPPKEY:int,
                        L2_LINENUMBER:int, L2_QUANTITY:float,
                        L2_EXTENDEDPRICE:float, L2_DISCOUNT:float,
                        L2_TAX:float, L2_RETURNFLAG:string,
                        L2_LINESTATUS:string, L2_SHIPDATE:date,
                        L2_COMMITDATE:date, L2_RECEIPTDATE:date,
                        L2_SHIPINSTRUCT:string, L2_SHIPMODE:string,
                        L2_COMMENT:string) *
              L2_QUANTITY:float))) *
         {L3_QTY:float > 100})))) *
  AggSum([C_NAME:string, C_CUSTKEY:int, O_ORDERKEY:int, O_ORDERDATE:date,
            O_TOTALPRICE:float], 
    (CUSTOMER(C_CUSTKEY:int, C_NAME:string, C_ADDRESS:string,
                C_NATIONKEY:int, C_PHONE:string, C_ACCTBAL:float,
                C_MKTSEGMENT:string, C_COMMENT:string) *
      ORDERS(O_ORDERKEY:int, C_CUSTKEY:int, O_ORDERSTATUS:string,
               O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
               O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
      LINEITEM(O_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int,
                 L_LINENUMBER:int, L_QUANTITY:float, L_EXTENDEDPRICE:float,
                 L_DISCOUNT:float, L_TAX:float, L_RETURNFLAG:string,
                 L_LINESTATUS:string, L_SHIPDATE:date, L_COMMITDATE:date,
                 L_RECEIPTDATE:date, L_SHIPINSTRUCT:string,
                 L_SHIPMODE:string, L_COMMENT:string) *
      L_QUANTITY:float)));

DECLARE MAP QUERY18_mORDERS2(int)[][C_NAME:string, QUERY18_mORDERSORDERS_CUSTKEY:int] := 
AggSum([C_NAME:string, QUERY18_mORDERSORDERS_CUSTKEY:int], 
  CUSTOMER(QUERY18_mORDERSORDERS_CUSTKEY:int, C_NAME:string,
             C_ADDRESS:string, C_NATIONKEY:int, C_PHONE:string,
             C_ACCTBAL:float, C_MKTSEGMENT:string, C_COMMENT:string));

DECLARE MAP QUERY18_mCUSTOMER1(float)[]
[O_ORDERKEY:int, QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int, O_ORDERDATE:date,
   O_TOTALPRICE:float] := 
AggSum([O_ORDERKEY:int, QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,
          O_ORDERDATE:date, O_TOTALPRICE:float], 
  (ORDERS(O_ORDERKEY:int, QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,
            O_ORDERSTATUS:string, O_TOTALPRICE:float, O_ORDERDATE:date,
            O_ORDERPRIORITY:string, O_CLERK:string, O_SHIPPRIORITY:int,
            O_COMMENT:string) *
    LINEITEM(O_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
               L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
               L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
               L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
               L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    L_QUANTITY:float));

DECLARE MAP QUERY18_mCUSTOMER1_mLINEITEM1(int)[]
[QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int,
   QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int, O_ORDERDATE:date,
   O_TOTALPRICE:float] := 
AggSum([QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int,
          QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int, O_ORDERDATE:date,
          O_TOTALPRICE:float], 
  ORDERS(QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int,
           QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int, O_ORDERSTATUS:string,
           O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
           O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string));

DECLARE MAP QUERY18_mLINEITEM1(float)[]
[O_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date,
   O_TOTALPRICE:float] := 
AggSum([O_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date,
          O_TOTALPRICE:float], 
  (CUSTOMER(C_CUSTKEY:int, C_NAME:string, C_ADDRESS:string, C_NATIONKEY:int,
              C_PHONE:string, C_ACCTBAL:float, C_MKTSEGMENT:string,
              C_COMMENT:string) *
    ORDERS(O_ORDERKEY:int, C_CUSTKEY:int, O_ORDERSTATUS:string,
             O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
             O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
    LINEITEM(O_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
               L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
               L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
               L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
               L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    L_QUANTITY:float));

DECLARE MAP QUERY18_mLINEITEM1_mLINEITEM1(int)[]
[QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int, C_NAME:string,
   C_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] := 
AggSum([QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int, C_NAME:string,
          C_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float], 
  (CUSTOMER(C_CUSTKEY:int, C_NAME:string, C_ADDRESS:string, C_NATIONKEY:int,
              C_PHONE:string, C_ACCTBAL:float, C_MKTSEGMENT:string,
              C_COMMENT:string) *
    ORDERS(QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int, C_CUSTKEY:int,
             O_ORDERSTATUS:string, O_TOTALPRICE:float, O_ORDERDATE:date,
             O_ORDERPRIORITY:string, O_CLERK:string, O_SHIPPRIORITY:int,
             O_COMMENT:string)));

DECLARE MAP QUERY18_mLINEITEM1_E1_1_L1_1(float)[][O_ORDERKEY:int] := 
AggSum([O_ORDERKEY:int], 
  (LINEITEM(O_ORDERKEY:int, L2_PARTKEY:int, L2_SUPPKEY:int,
              L2_LINENUMBER:int, L2_QUANTITY:float, L2_EXTENDEDPRICE:float,
              L2_DISCOUNT:float, L2_TAX:float, L2_RETURNFLAG:string,
              L2_LINESTATUS:string, L2_SHIPDATE:date, L2_COMMITDATE:date,
              L2_RECEIPTDATE:date, L2_SHIPINSTRUCT:string,
              L2_SHIPMODE:string, L2_COMMENT:string) *
    L2_QUANTITY:float));

-------------------- QUERIES --------------------
DECLARE QUERY QUERY18 := QUERY18(float)[][C_NAME:string, C_CUSTKEY:int, O_ORDERKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18_mCUSTOMER1(float)[][LINEITEM_ORDERKEY:int, QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mCUSTOMER1_mLINEITEM1(int)[]
 [LINEITEM_ORDERKEY:int, QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,
    O_ORDERDATE:date, O_TOTALPRICE:float] *
  LINEITEM_QUANTITY:float);
   QUERY18_mLINEITEM1(float)[][LINEITEM_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mLINEITEM1_mLINEITEM1(int)[]
 [LINEITEM_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date,
    O_TOTALPRICE:float] *
  LINEITEM_QUANTITY:float);
   QUERY18_mLINEITEM1_E1_1_L1_1(float)[][LINEITEM_ORDERKEY:int] += LINEITEM_QUANTITY:float;
   QUERY18(float)[][C_NAME:string, C_CUSTKEY:int, O_ORDERKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] := 
  (QUERY18_mLINEITEM1(float)[]
 [O_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date,
    O_TOTALPRICE:float] *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][O_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][O_ORDERKEY:int]))));
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18_mCUSTOMER1(float)[][LINEITEM_ORDERKEY:int, QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mCUSTOMER1_mLINEITEM1(int)[]
 [LINEITEM_ORDERKEY:int, QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,
    O_ORDERDATE:date, O_TOTALPRICE:float] *
  -1 * LINEITEM_QUANTITY:float);
   QUERY18_mLINEITEM1(float)[][LINEITEM_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mLINEITEM1_mLINEITEM1(int)[]
 [LINEITEM_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date,
    O_TOTALPRICE:float] *
  -1 * LINEITEM_QUANTITY:float);
   QUERY18_mLINEITEM1_E1_1_L1_1(float)[][LINEITEM_ORDERKEY:int] += (-1 * LINEITEM_QUANTITY:float);
   QUERY18(float)[][C_NAME:string, C_CUSTKEY:int, O_ORDERKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] := 
  (QUERY18_mLINEITEM1(float)[]
 [O_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date,
    O_TOTALPRICE:float] *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][O_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][O_ORDERKEY:int]))));
}

ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  (EXISTS(
   (AggSum([], 
      ((L3_QTY:float ^=
         QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
        {L3_QTY:float > 100})) *
     EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]);
   QUERY18_mCUSTOMER1(float)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int];
   QUERY18_mCUSTOMER1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 1;
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  (QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]);
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int];
}

ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  (EXISTS(
   (AggSum([], 
      ((L3_QTY:float ^=
         QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
        {L3_QTY:float > 100})) *
     EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] * -1);
   QUERY18_mCUSTOMER1(float)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] * -1);
   QUERY18_mCUSTOMER1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += -1;
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  (QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] * -1);
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += (QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] * -1);
}

ON + CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18(float)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, O_ORDERKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mCUSTOMER1(float)[]
 [O_ORDERKEY:int, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][O_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][O_ORDERKEY:int]))));
   QUERY18_mORDERS2(int)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int] += 1;
   QUERY18_mLINEITEM1(float)[][O_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  QUERY18_mCUSTOMER1(float)[]
[O_ORDERKEY:int, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float];
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  QUERY18_mCUSTOMER1_mLINEITEM1(int)[]
[QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int, CUSTOMER_CUSTKEY:int,
   O_ORDERDATE:date, O_TOTALPRICE:float];
}

ON - CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18(float)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, O_ORDERKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mCUSTOMER1(float)[]
 [O_ORDERKEY:int, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][O_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][O_ORDERKEY:int]))) *
  -1);
   QUERY18_mORDERS2(int)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int] += -1;
   QUERY18_mLINEITEM1(float)[][O_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mCUSTOMER1(float)[]
 [O_ORDERKEY:int, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] *
  -1);
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mCUSTOMER1_mLINEITEM1(int)[]
 [QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int, CUSTOMER_CUSTKEY:int,
    O_ORDERDATE:date, O_TOTALPRICE:float] *
  -1);
}

ON SYSTEM READY {
}

CORRECT QUERY18_mCUSTOMER1_mLINEITEM1[][delta_QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int,delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mCUSTOMER1_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18_mCUSTOMER1(float)[][LINEITEM_ORDERKEY:int, delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({LINEITEM_ORDERKEY:int =
 delta_QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int} *
  delta_QUERY18_mCUSTOMER1_mLINEITEM1:int * LINEITEM_QUANTITY:float);
}

CORRECT QUERY18_mLINEITEM1_mLINEITEM1[][delta_QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int,delta_C_NAME:string,delta_C_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mLINEITEM1_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18_mLINEITEM1(float)[][LINEITEM_ORDERKEY:int, delta_C_NAME:string, delta_C_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({LINEITEM_ORDERKEY:int =
 delta_QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int} *
  delta_QUERY18_mLINEITEM1_mLINEITEM1:int * LINEITEM_QUANTITY:float);
}

CORRECT QUERY18_mLINEITEM1[][delta_O_ORDERKEY:int,delta_C_NAME:string,delta_C_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mLINEITEM1:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18(float)[][delta_C_NAME:string, delta_C_CUSTKEY:int, delta_O_ORDERKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] := 
  (EXISTS(
   (AggSum([], 
      ((L3_QTY:float ^=
         QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
        {L3_QTY:float > 100})) *
     EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
  delta_QUERY18_mLINEITEM1:float);
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, C_CUSTKEY:int, delta_O_ORDERKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] := 
  (QUERY18_mLINEITEM1(float)[]
 [delta_O_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date,
    O_TOTALPRICE:float] *
  (EXISTS(
     ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             {L3_QTY:float > 100})) +
          AggSum([], 
            (((L3_QTY:float ^=
                (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int] +
                  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
               ((L3_QTY:float ^=
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
                 -1)) *
              {L3_QTY:float > 100})))) +
       ((AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             {L3_QTY:float > 100})) +
          ((delta_O_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                   [delta_O_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [delta_O_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) *
         (EXISTS(
            (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int] +
              delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
           (EXISTS(
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             -1))))) +
    (EXISTS(
       (AggSum([], 
          ((L3_QTY:float ^=
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
            {L3_QTY:float > 100})) *
         EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
      -1)));
}

CORRECT QUERY18_mCUSTOMER1_mLINEITEM1[][delta_QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int,delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mCUSTOMER1_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18_mCUSTOMER1(float)[][LINEITEM_ORDERKEY:int, delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({LINEITEM_ORDERKEY:int =
 delta_QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int} * -1 *
  delta_QUERY18_mCUSTOMER1_mLINEITEM1:int * LINEITEM_QUANTITY:float);
}

CORRECT QUERY18_mLINEITEM1_mLINEITEM1[][delta_QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int,delta_C_NAME:string,delta_C_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mLINEITEM1_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18_mLINEITEM1(float)[][LINEITEM_ORDERKEY:int, delta_C_NAME:string, delta_C_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({LINEITEM_ORDERKEY:int =
 delta_QUERY18_mLINEITEM1_mLINEITEMLINEITEM_ORDERKEY:int} * -1 *
  delta_QUERY18_mLINEITEM1_mLINEITEM1:int * LINEITEM_QUANTITY:float);
}

CORRECT QUERY18_mLINEITEM1[][delta_O_ORDERKEY:int,delta_C_NAME:string,delta_C_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mLINEITEM1:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18(float)[][delta_C_NAME:string, delta_C_CUSTKEY:int, delta_O_ORDERKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] := 
  (EXISTS(
   (AggSum([], 
      ((L3_QTY:float ^=
         QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
        {L3_QTY:float > 100})) *
     EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
  delta_QUERY18_mLINEITEM1:float);
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, C_CUSTKEY:int, delta_O_ORDERKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] := 
  (QUERY18_mLINEITEM1(float)[]
 [delta_O_ORDERKEY:int, C_NAME:string, C_CUSTKEY:int, O_ORDERDATE:date,
    O_TOTALPRICE:float] *
  (EXISTS(
     ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             {L3_QTY:float > 100})) +
          AggSum([], 
            (((L3_QTY:float ^=
                (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int] +
                  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
               ((L3_QTY:float ^=
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
                 -1)) *
              {L3_QTY:float > 100})))) +
       ((AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             {L3_QTY:float > 100})) +
          ((delta_O_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                   [delta_O_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [delta_O_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) *
         (EXISTS(
            (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int] +
              delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
           (EXISTS(
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             -1))))) +
    (EXISTS(
       (AggSum([], 
          ((L3_QTY:float ^=
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
            {L3_QTY:float > 100})) *
         EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
      -1)));
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  (((EXISTS(
       ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
          (AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) +
         ((AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
              AggSum([], 
                (((L3_QTY:float ^=
                    (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int] +
                      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                   ((L3_QTY:float ^=
                      QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                      [ORDERS_ORDERKEY:int]) *
                     -1)) *
                  {L3_QTY:float > 100})))) *
           (EXISTS(
              (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
             (EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               -1))))) +
      (EXISTS(
         (AggSum([], 
            ((L3_QTY:float ^=
               QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
              {L3_QTY:float > 100})) *
           EXISTS(
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
        -1)) *
     QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) +
    ((EXISTS(
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
             {L3_QTY:float > 100})) *
          EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) +
       ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
         (EXISTS(
            ((EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               (AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 AggSum([], 
                   (((L3_QTY:float ^=
                       (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                        [ORDERS_ORDERKEY:int] +
                         delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                      ((L3_QTY:float ^=
                         QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                         [ORDERS_ORDERKEY:int]) *
                        -1)) *
                     {L3_QTY:float > 100})))) +
              ((AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
                   AggSum([], 
                     (((L3_QTY:float ^=
                         (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                          [ORDERS_ORDERKEY:int] +
                           delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                        ((L3_QTY:float ^=
                           QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                           [ORDERS_ORDERKEY:int]) *
                          -1)) *
                       {L3_QTY:float > 100})))) *
                (EXISTS(
                   (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int] +
                     delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                  (EXISTS(
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    -1))))) +
           (EXISTS(
              (AggSum([], 
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   {L3_QTY:float > 100})) *
                EXISTS(
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
             -1)))) *
      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)));
   QUERY18_mCUSTOMER1(float)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
}

CORRECT QUERY18_mORDERS2[][delta_C_NAME:string,delta_QUERY18_mORDERSORDERS_CUSTKEY:int] += delta_QUERY18_mORDERS2:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  delta_QUERY18_mORDERS2:int);
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  (((EXISTS(
       ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
          (AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) +
         ((AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
              AggSum([], 
                (((L3_QTY:float ^=
                    (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int] +
                      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                   ((L3_QTY:float ^=
                      QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                      [ORDERS_ORDERKEY:int]) *
                     -1)) *
                  {L3_QTY:float > 100})))) *
           (EXISTS(
              (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
             (EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               -1))))) +
      (EXISTS(
         (AggSum([], 
            ((L3_QTY:float ^=
               QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
              {L3_QTY:float > 100})) *
           EXISTS(
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
        -1)) *
     QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) +
    ((EXISTS(
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
             {L3_QTY:float > 100})) *
          EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) +
       ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
         (EXISTS(
            ((EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               (AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 AggSum([], 
                   (((L3_QTY:float ^=
                       (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                        [ORDERS_ORDERKEY:int] +
                         delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                      ((L3_QTY:float ^=
                         QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                         [ORDERS_ORDERKEY:int]) *
                        -1)) *
                     {L3_QTY:float > 100})))) +
              ((AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
                   AggSum([], 
                     (((L3_QTY:float ^=
                         (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                          [ORDERS_ORDERKEY:int] +
                           delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                        ((L3_QTY:float ^=
                           QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                           [ORDERS_ORDERKEY:int]) *
                          -1)) *
                       {L3_QTY:float > 100})))) *
                (EXISTS(
                   (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int] +
                     delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                  (EXISTS(
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    -1))))) +
           (EXISTS(
              (AggSum([], 
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   {L3_QTY:float > 100})) *
                EXISTS(
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
             -1)))) *
      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)));
   QUERY18_mCUSTOMER1(float)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
}

CORRECT QUERY18_mORDERS2[][delta_C_NAME:string,delta_QUERY18_mORDERSORDERS_CUSTKEY:int] += delta_QUERY18_mORDERS2:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  delta_QUERY18_mORDERS2:int);
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  (((EXISTS(
       ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
          (AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) +
         ((AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
              AggSum([], 
                (((L3_QTY:float ^=
                    (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int] +
                      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                   ((L3_QTY:float ^=
                      QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                      [ORDERS_ORDERKEY:int]) *
                     -1)) *
                  {L3_QTY:float > 100})))) *
           (EXISTS(
              (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
             (EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               -1))))) +
      (EXISTS(
         (AggSum([], 
            ((L3_QTY:float ^=
               QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
              {L3_QTY:float > 100})) *
           EXISTS(
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
        -1)) *
     QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) +
    ((EXISTS(
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
             {L3_QTY:float > 100})) *
          EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) +
       ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
         (EXISTS(
            ((EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               (AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 AggSum([], 
                   (((L3_QTY:float ^=
                       (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                        [ORDERS_ORDERKEY:int] +
                         delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                      ((L3_QTY:float ^=
                         QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                         [ORDERS_ORDERKEY:int]) *
                        -1)) *
                     {L3_QTY:float > 100})))) +
              ((AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
                   AggSum([], 
                     (((L3_QTY:float ^=
                         (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                          [ORDERS_ORDERKEY:int] +
                           delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                        ((L3_QTY:float ^=
                           QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                           [ORDERS_ORDERKEY:int]) *
                          -1)) *
                       {L3_QTY:float > 100})))) *
                (EXISTS(
                   (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int] +
                     delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                  (EXISTS(
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    -1))))) +
           (EXISTS(
              (AggSum([], 
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   {L3_QTY:float > 100})) *
                EXISTS(
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
             -1)))) *
      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)));
   QUERY18_mCUSTOMER1(float)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
}

CORRECT QUERY18_mORDERS2[][delta_C_NAME:string,delta_QUERY18_mORDERSORDERS_CUSTKEY:int] += delta_QUERY18_mORDERS2:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  delta_QUERY18_mORDERS2:int);
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  (((EXISTS(
       ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
          (AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) +
         ((AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
              AggSum([], 
                (((L3_QTY:float ^=
                    (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int] +
                      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                   ((L3_QTY:float ^=
                      QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                      [ORDERS_ORDERKEY:int]) *
                     -1)) *
                  {L3_QTY:float > 100})))) *
           (EXISTS(
              (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
             (EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               -1))))) +
      (EXISTS(
         (AggSum([], 
            ((L3_QTY:float ^=
               QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
              {L3_QTY:float > 100})) *
           EXISTS(
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
        -1)) *
     QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) +
    ((EXISTS(
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
             {L3_QTY:float > 100})) *
          EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) +
       ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
         (EXISTS(
            ((EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               (AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 AggSum([], 
                   (((L3_QTY:float ^=
                       (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                        [ORDERS_ORDERKEY:int] +
                         delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                      ((L3_QTY:float ^=
                         QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                         [ORDERS_ORDERKEY:int]) *
                        -1)) *
                     {L3_QTY:float > 100})))) +
              ((AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
                   AggSum([], 
                     (((L3_QTY:float ^=
                         (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                          [ORDERS_ORDERKEY:int] +
                           delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                        ((L3_QTY:float ^=
                           QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                           [ORDERS_ORDERKEY:int]) *
                          -1)) *
                       {L3_QTY:float > 100})))) *
                (EXISTS(
                   (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int] +
                     delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                  (EXISTS(
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    -1))))) +
           (EXISTS(
              (AggSum([], 
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   {L3_QTY:float > 100})) *
                EXISTS(
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
             -1)))) *
      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) *
  -1);
   QUERY18_mCUSTOMER1(float)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} * -1 *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] * -1 *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
}

CORRECT QUERY18_mORDERS2[][delta_C_NAME:string,delta_QUERY18_mORDERSORDERS_CUSTKEY:int] += delta_QUERY18_mORDERS2:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] * -1 *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] * -1 *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} * -1 *
  delta_QUERY18_mORDERS2:int);
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  (((EXISTS(
       ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
          (AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) +
         ((AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
              AggSum([], 
                (((L3_QTY:float ^=
                    (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int] +
                      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                   ((L3_QTY:float ^=
                      QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                      [ORDERS_ORDERKEY:int]) *
                     -1)) *
                  {L3_QTY:float > 100})))) *
           (EXISTS(
              (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
             (EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               -1))))) +
      (EXISTS(
         (AggSum([], 
            ((L3_QTY:float ^=
               QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
              {L3_QTY:float > 100})) *
           EXISTS(
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
        -1)) *
     QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) +
    ((EXISTS(
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
             {L3_QTY:float > 100})) *
          EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) +
       ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
         (EXISTS(
            ((EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               (AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 AggSum([], 
                   (((L3_QTY:float ^=
                       (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                        [ORDERS_ORDERKEY:int] +
                         delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                      ((L3_QTY:float ^=
                         QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                         [ORDERS_ORDERKEY:int]) *
                        -1)) *
                     {L3_QTY:float > 100})))) +
              ((AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
                   AggSum([], 
                     (((L3_QTY:float ^=
                         (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                          [ORDERS_ORDERKEY:int] +
                           delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                        ((L3_QTY:float ^=
                           QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                           [ORDERS_ORDERKEY:int]) *
                          -1)) *
                       {L3_QTY:float > 100})))) *
                (EXISTS(
                   (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int] +
                     delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                  (EXISTS(
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    -1))))) +
           (EXISTS(
              (AggSum([], 
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   {L3_QTY:float > 100})) *
                EXISTS(
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
             -1)))) *
      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) *
  -1);
   QUERY18_mCUSTOMER1(float)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} * -1 *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] * -1 *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
}

CORRECT QUERY18_mORDERS2[][delta_C_NAME:string,delta_QUERY18_mORDERSORDERS_CUSTKEY:int] += delta_QUERY18_mORDERS2:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] * -1 *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] * -1 *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} * -1 *
  delta_QUERY18_mORDERS2:int);
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] *
  (((EXISTS(
       ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
          (AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) +
         ((AggSum([], 
             ((L3_QTY:float ^=
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               {L3_QTY:float > 100})) +
            ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
              AggSum([], 
                (((L3_QTY:float ^=
                    (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int] +
                      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                   ((L3_QTY:float ^=
                      QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                      [ORDERS_ORDERKEY:int]) *
                     -1)) *
                  {L3_QTY:float > 100})))) *
           (EXISTS(
              (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] +
                delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
             (EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               -1))))) +
      (EXISTS(
         (AggSum([], 
            ((L3_QTY:float ^=
               QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
              {L3_QTY:float > 100})) *
           EXISTS(
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
        -1)) *
     QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) +
    ((EXISTS(
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
             {L3_QTY:float > 100})) *
          EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) +
       ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
         (EXISTS(
            ((EXISTS(
                QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
               (AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 AggSum([], 
                   (((L3_QTY:float ^=
                       (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                        [ORDERS_ORDERKEY:int] +
                         delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                      ((L3_QTY:float ^=
                         QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                         [ORDERS_ORDERKEY:int]) *
                        -1)) *
                     {L3_QTY:float > 100})))) +
              ((AggSum([], 
                  ((L3_QTY:float ^=
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    {L3_QTY:float > 100})) +
                 ((ORDERS_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
                   AggSum([], 
                     (((L3_QTY:float ^=
                         (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                          [ORDERS_ORDERKEY:int] +
                           delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                        ((L3_QTY:float ^=
                           QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                           [ORDERS_ORDERKEY:int]) *
                          -1)) *
                       {L3_QTY:float > 100})))) *
                (EXISTS(
                   (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int] +
                     delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                  (EXISTS(
                     QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                     [ORDERS_ORDERKEY:int]) *
                    -1))))) +
           (EXISTS(
              (AggSum([], 
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [ORDERS_ORDERKEY:int]) *
                   {L3_QTY:float > 100})) *
                EXISTS(
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
             -1)))) *
      delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) *
  -1);
   QUERY18_mCUSTOMER1(float)[][ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} * -1 *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_ORDERKEY:int = delta_O_ORDERKEY:int} *
  QUERY18_mORDERS2(int)[][C_NAME:string, ORDERS_CUSTKEY:int] * -1 *
  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float);
}

CORRECT QUERY18_mORDERS2[][delta_C_NAME:string,delta_QUERY18_mORDERSORDERS_CUSTKEY:int] += delta_QUERY18_mORDERS2:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   QUERY18(float)[][delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int]))) *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] * -1 *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1(float)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} *
  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][ORDERS_ORDERKEY:int] * -1 *
  delta_QUERY18_mORDERS2:int);
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][ORDERS_ORDERKEY:int, delta_C_NAME:string, ORDERS_CUSTKEY:int, ORDERS_ORDERDATE:date, ORDERS_TOTALPRICE:float] += 
  ({ORDERS_CUSTKEY:int = delta_QUERY18_mORDERSORDERS_CUSTKEY:int} * -1 *
  delta_QUERY18_mORDERS2:int);
}

CORRECT QUERY18_mCUSTOMER1[][delta_O_ORDERKEY:int,delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mCUSTOMER1:float FOR ON + CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18(float)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
  delta_QUERY18_mCUSTOMER1:float);
   QUERY18_mLINEITEM1(float)[][delta_O_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  delta_QUERY18_mCUSTOMER1:float);
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON + CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18(float)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mCUSTOMER1(float)[]
 [delta_O_ORDERKEY:int, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date,
    O_TOTALPRICE:float] *
  (EXISTS(
     ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             {L3_QTY:float > 100})) +
          AggSum([], 
            (((L3_QTY:float ^=
                (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int] +
                  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
               ((L3_QTY:float ^=
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
                 -1)) *
              {L3_QTY:float > 100})))) +
       ((AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             {L3_QTY:float > 100})) +
          ((delta_O_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                   [delta_O_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [delta_O_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) *
         (EXISTS(
            (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int] +
              delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
           (EXISTS(
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             -1))))) +
    (EXISTS(
       (AggSum([], 
          ((L3_QTY:float ^=
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
            {L3_QTY:float > 100})) *
         EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
      -1)));
}

CORRECT QUERY18_mCUSTOMER1[][delta_O_ORDERKEY:int,delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mCUSTOMER1:float FOR ON + CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18(float)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
  delta_QUERY18_mCUSTOMER1:float);
   QUERY18_mLINEITEM1(float)[][delta_O_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  delta_QUERY18_mCUSTOMER1:float);
}

CORRECT QUERY18_mCUSTOMER1_mLINEITEM1[][delta_QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int,delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mCUSTOMER1_mLINEITEM1:int FOR ON + CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][delta_QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  delta_QUERY18_mCUSTOMER1_mLINEITEM1:int);
}

CORRECT QUERY18_mCUSTOMER1[][delta_O_ORDERKEY:int,delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mCUSTOMER1:float FOR ON - CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18(float)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
  -1 * delta_QUERY18_mCUSTOMER1:float);
   QUERY18_mLINEITEM1(float)[][delta_O_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} * 
-1 * delta_QUERY18_mCUSTOMER1:float);
}

CORRECT QUERY18_mLINEITEM1_E1_1_L1_1[][delta_O_ORDERKEY:int] += delta_QUERY18_mLINEITEM1_E1_1_L1_1:float FOR ON - CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18(float)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERKEY:int, O_ORDERDATE:date, O_TOTALPRICE:float] += 
  (QUERY18_mCUSTOMER1(float)[]
 [delta_O_ORDERKEY:int, CUSTOMER_CUSTKEY:int, O_ORDERDATE:date,
    O_TOTALPRICE:float] *
  (EXISTS(
     ((EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
        (AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             {L3_QTY:float > 100})) +
          AggSum([], 
            (((L3_QTY:float ^=
                (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int] +
                  delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
               ((L3_QTY:float ^=
                  QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
                 -1)) *
              {L3_QTY:float > 100})))) +
       ((AggSum([], 
           ((L3_QTY:float ^=
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             {L3_QTY:float > 100})) +
          ((delta_O_ORDERKEY:int ^= delta_O_ORDERKEY:int) *
            AggSum([], 
              (((L3_QTY:float ^=
                  (QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                   [delta_O_ORDERKEY:int] +
                    delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
                 ((L3_QTY:float ^=
                    QUERY18_mLINEITEM1_E1_1_L1_1(float)[]
                    [delta_O_ORDERKEY:int]) *
                   -1)) *
                {L3_QTY:float > 100})))) *
         (EXISTS(
            (QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int] +
              delta_QUERY18_mLINEITEM1_E1_1_L1_1:float)) +
           (EXISTS(
              QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
             -1))))) +
    (EXISTS(
       (AggSum([], 
          ((L3_QTY:float ^=
             QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
            {L3_QTY:float > 100})) *
         EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
      -1)) *
  -1);
}

CORRECT QUERY18_mCUSTOMER1[][delta_O_ORDERKEY:int,delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mCUSTOMER1:float FOR ON - CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18(float)[][CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  EXISTS(
    (AggSum([], 
       ((L3_QTY:float ^=
          QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]) *
         {L3_QTY:float > 100})) *
      EXISTS( QUERY18_mLINEITEM1_E1_1_L1_1(float)[][delta_O_ORDERKEY:int]))) *
  -1 * delta_QUERY18_mCUSTOMER1:float);
   QUERY18_mLINEITEM1(float)[][delta_O_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} * 
-1 * delta_QUERY18_mCUSTOMER1:float);
}

CORRECT QUERY18_mCUSTOMER1_mLINEITEM1[][delta_QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int,delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int,delta_O_ORDERDATE:date,delta_O_TOTALPRICE:float] += delta_QUERY18_mCUSTOMER1_mLINEITEM1:int FOR ON - CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   QUERY18_mLINEITEM1_mLINEITEM1(int)[][delta_QUERY18_mCUSTOMER1_mLINEITEMLINEITEM_ORDERKEY:int, CUSTOMER_NAME:string, CUSTOMER_CUSTKEY:int, delta_O_ORDERDATE:date, delta_O_TOTALPRICE:float] += 
  ({CUSTOMER_CUSTKEY:int = delta_QUERY18_mCUSTOMERCUSTOMER_CUSTKEY:int} * 
-1 * delta_QUERY18_mCUSTOMER1_mLINEITEM1:int);
}
