-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM ORDERS(ORDERS_ORDERKEY int, ORDERS_CUSTKEY int, ORDERS_ORDERSTATUS string, ORDERS_TOTALPRICE float, ORDERS_ORDERDATE date, ORDERS_ORDERPRIORITY string, ORDERS_CLERK string, ORDERS_SHIPPRIORITY int, ORDERS_COMMENT string)
  FROM FILE 'data/tpch/orders.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP ORDER_COUNT(int)[][O_ORDERPRIORITY:string] := 
AggSum([O_ORDERPRIORITY:string], 
  (ORDERS(O_ORDERKEY:int, O_CUSTKEY:int, O_ORDERSTATUS:string,
            O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
            O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
    {O_ORDERDATE:date >= DATE('1993-7-1')} *
    {O_ORDERDATE:date < DATE('1993-10-1')} *
    EXISTS(
      AggSum([O_ORDERKEY:int], 
        (LINEITEM(O_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int,
                    L_LINENUMBER:int, L_QUANTITY:float,
                    L_EXTENDEDPRICE:float, L_DISCOUNT:float, L_TAX:float,
                    L_RETURNFLAG:string, L_LINESTATUS:string,
                    L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
                    L_SHIPINSTRUCT:string, L_SHIPMODE:string,
                    L_COMMENT:string) *
          {L_COMMITDATE:date < L_RECEIPTDATE:date})))));

DECLARE MAP ORDER_COUNT_mLINEITEM1(int)[][O_ORDERPRIORITY:string, O_ORDERKEY:int] := 
AggSum([O_ORDERPRIORITY:string, O_ORDERKEY:int], 
  (ORDERS(O_ORDERKEY:int, O_CUSTKEY:int, O_ORDERSTATUS:string,
            O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
            O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
    {O_ORDERDATE:date >= DATE('1993-7-1')} *
    {O_ORDERDATE:date < DATE('1993-10-1')}));

DECLARE MAP ORDER_COUNT_mORDERS3_E1_1(int)[][ORDER_COUNT_mORDERSORDERS_ORDERKEY:int] := 
AggSum([ORDER_COUNT_mORDERSORDERS_ORDERKEY:int], 
  (LINEITEM(ORDER_COUNT_mORDERSORDERS_ORDERKEY:int, L_PARTKEY:int,
              L_SUPPKEY:int, L_LINENUMBER:int, L_QUANTITY:float,
              L_EXTENDEDPRICE:float, L_DISCOUNT:float, L_TAX:float,
              L_RETURNFLAG:string, L_LINESTATUS:string, L_SHIPDATE:date,
              L_COMMITDATE:date, L_RECEIPTDATE:date, L_SHIPINSTRUCT:string,
              L_SHIPMODE:string, L_COMMENT:string) *
    {L_COMMITDATE:date < L_RECEIPTDATE:date}));

-------------------- QUERIES --------------------
DECLARE QUERY ORDER_COUNT := ORDER_COUNT(int)[][O_ORDERPRIORITY:string];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   ORDER_COUNT(int)[][O_ORDERPRIORITY:string] += 
  (ORDER_COUNT_mLINEITEM1(int)[][O_ORDERPRIORITY:string, LINEITEM_ORDERKEY:int] *
  (EXISTS(
     (ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int] +
       {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date})) +
    (EXISTS( ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int]) * -1)));
   ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int] += {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date};
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   ORDER_COUNT(int)[][O_ORDERPRIORITY:string] += 
  (AggSum([O_ORDERPRIORITY:string], 
   (ORDER_COUNT_mLINEITEM1(int)[][O_ORDERPRIORITY:string, O_ORDERKEY:int] *
     EXISTS(
       (ORDER_COUNT_mORDERS3_E1_1(int)[][O_ORDERKEY:int] +
         ((O_ORDERKEY:int ^= LINEITEM_ORDERKEY:int) *
           {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} * -1))))) +
  (AggSum([O_ORDERPRIORITY:string], 
     (ORDER_COUNT_mLINEITEM1(int)[][O_ORDERPRIORITY:string, O_ORDERKEY:int] *
       EXISTS( ORDER_COUNT_mORDERS3_E1_1(int)[][O_ORDERKEY:int]))) *
    -1));
   ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int] += ({LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} * -1);
}

ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   ORDER_COUNT(int)[][ORDERS_ORDERPRIORITY:string] += 
  ({ORDERS_ORDERDATE:date >= DATE('1993-7-1')} *
  {ORDERS_ORDERDATE:date < DATE('1993-10-1')} *
  EXISTS( ORDER_COUNT_mORDERS3_E1_1(int)[][ORDERS_ORDERKEY:int]));
   ORDER_COUNT_mLINEITEM1(int)[][ORDERS_ORDERPRIORITY:string, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERDATE:date >= DATE('1993-7-1')} *
  {ORDERS_ORDERDATE:date < DATE('1993-10-1')});
}

ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   ORDER_COUNT(int)[][ORDERS_ORDERPRIORITY:string] += 
  ({ORDERS_ORDERDATE:date >= DATE('1993-7-1')} *
  {ORDERS_ORDERDATE:date < DATE('1993-10-1')} *
  EXISTS( ORDER_COUNT_mORDERS3_E1_1(int)[][ORDERS_ORDERKEY:int]) * -1);
   ORDER_COUNT_mLINEITEM1(int)[][ORDERS_ORDERPRIORITY:string, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERDATE:date >= DATE('1993-7-1')} *
  {ORDERS_ORDERDATE:date < DATE('1993-10-1')} * -1);
}

ON SYSTEM READY {
}

CORRECT ORDER_COUNT_mLINEITEM1[][delta_O_ORDERPRIORITY:string,delta_O_ORDERKEY:int] += delta_ORDER_COUNT_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   ORDER_COUNT(int)[][delta_O_ORDERPRIORITY:string] += 
  ({LINEITEM_ORDERKEY:int = delta_O_ORDERKEY:int} *
  (EXISTS(
     (ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int] +
       {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date})) +
    (EXISTS( ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int]) * -1)) *
  delta_ORDER_COUNT_mLINEITEM1:int);
}

CORRECT ORDER_COUNT_mORDERS3_E1_1[][delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_ORDER_COUNT_mORDERS3_E1_1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   ORDER_COUNT(int)[][O_ORDERPRIORITY:string] += 
  ({LINEITEM_ORDERKEY:int = delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int} *
  ORDER_COUNT_mLINEITEM1(int)[]
  [O_ORDERPRIORITY:string, LINEITEM_ORDERKEY:int] *
  (((EXISTS(
       (ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int] +
         {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date})) +
      EXISTS(
        (ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int] +
          delta_ORDER_COUNT_mORDERS3_E1_1:int)) +
      (EXISTS( ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int]) * -1)) *
     -1) +
    EXISTS(
      (ORDER_COUNT_mORDERS3_E1_1(int)[][LINEITEM_ORDERKEY:int] +
        {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} +
        delta_ORDER_COUNT_mORDERS3_E1_1:int))));
}

CORRECT ORDER_COUNT_mLINEITEM1[][delta_O_ORDERPRIORITY:string,delta_O_ORDERKEY:int] += delta_ORDER_COUNT_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   ORDER_COUNT(int)[][delta_O_ORDERPRIORITY:string] += 
  ((EXISTS(
    (ORDER_COUNT_mORDERS3_E1_1(int)[][delta_O_ORDERKEY:int] +
      ({LINEITEM_ORDERKEY:int = delta_O_ORDERKEY:int} *
        {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} * -1))) +
   (EXISTS( ORDER_COUNT_mORDERS3_E1_1(int)[][delta_O_ORDERKEY:int]) * -1)) *
  delta_ORDER_COUNT_mLINEITEM1:int);
}

CORRECT ORDER_COUNT_mORDERS3_E1_1[][delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_ORDER_COUNT_mORDERS3_E1_1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   ORDER_COUNT(int)[][O_ORDERPRIORITY:string] += 
  (ORDER_COUNT_mLINEITEM1(int)[]
 [O_ORDERPRIORITY:string, delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int] *
  (((EXISTS(
       (ORDER_COUNT_mORDERS3_E1_1(int)[]
        [delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int] +
         ({LINEITEM_ORDERKEY:int =
          delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int} *
           {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} * -1))) +
      EXISTS(
        (ORDER_COUNT_mORDERS3_E1_1(int)[]
         [delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int] +
          delta_ORDER_COUNT_mORDERS3_E1_1:int)) +
      (EXISTS(
         ORDER_COUNT_mORDERS3_E1_1(int)[]
         [delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int]) *
        -1)) *
     -1) +
    EXISTS(
      (ORDER_COUNT_mORDERS3_E1_1(int)[]
       [delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int] +
        ({LINEITEM_ORDERKEY:int =
         delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int} *
          {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} * -1) +
        delta_ORDER_COUNT_mORDERS3_E1_1:int))));
}

CORRECT ORDER_COUNT_mORDERS3_E1_1[][delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_ORDER_COUNT_mORDERS3_E1_1:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   ORDER_COUNT(int)[][ORDERS_ORDERPRIORITY:string] += 
  ({ORDERS_ORDERKEY:int = delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {ORDERS_ORDERDATE:date >= DATE('1993-7-1')} *
  {ORDERS_ORDERDATE:date < DATE('1993-10-1')} *
  (EXISTS(
     (ORDER_COUNT_mORDERS3_E1_1(int)[][ORDERS_ORDERKEY:int] +
       delta_ORDER_COUNT_mORDERS3_E1_1:int)) +
    (EXISTS( ORDER_COUNT_mORDERS3_E1_1(int)[][ORDERS_ORDERKEY:int]) * -1)));
}

CORRECT ORDER_COUNT_mORDERS3_E1_1[][delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_ORDER_COUNT_mORDERS3_E1_1:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   ORDER_COUNT(int)[][ORDERS_ORDERPRIORITY:string] += 
  ({ORDERS_ORDERKEY:int = delta_ORDER_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {ORDERS_ORDERDATE:date >= DATE('1993-7-1')} *
  {ORDERS_ORDERDATE:date < DATE('1993-10-1')} *
  (EXISTS(
     (ORDER_COUNT_mORDERS3_E1_1(int)[][ORDERS_ORDERKEY:int] +
       delta_ORDER_COUNT_mORDERS3_E1_1:int)) +
    (EXISTS( ORDER_COUNT_mORDERS3_E1_1(int)[][ORDERS_ORDERKEY:int]) * -1)) *
  -1);
}
