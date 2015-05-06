-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM ORDERS(ORDERS_ORDERKEY int, ORDERS_CUSTKEY int, ORDERS_ORDERSTATUS string, ORDERS_TOTALPRICE float, ORDERS_ORDERDATE date, ORDERS_ORDERPRIORITY string, ORDERS_CLERK string, ORDERS_SHIPPRIORITY int, ORDERS_COMMENT string)
  FROM FILE 'data/tpch/orders.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP HIGH_LINE_COUNT(int)[][L_SHIPMODE:string] := 
AggSum([L_SHIPMODE:string], 
  (ORDERS(O_ORDERKEY:int, O_CUSTKEY:int, O_ORDERSTATUS:string,
            O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
            O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
    LINEITEM(O_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
               L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
               L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
               L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
               L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    ({L_SHIPMODE:string = 'MAIL'} + {L_SHIPMODE:string = 'SHIP'}) *
    {L_COMMITDATE:date < L_RECEIPTDATE:date} *
    {L_SHIPDATE:date < L_COMMITDATE:date} *
    {L_RECEIPTDATE:date >= DATE('1994-1-1')} *
    {L_RECEIPTDATE:date < DATE('1995-1-1')} *
    ({O_ORDERPRIORITY:string = '1-URGENT'} +
      {O_ORDERPRIORITY:string = '2-HIGH'})));

DECLARE MAP HIGH_LINE_COUNT_mLINEITEM1(int)[]
[HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int] := 
AggSum([HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int], 
  ((O_ORDERPRIORITY:string ^= '1-URGENT') *
    ORDERS(HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int, O_CUSTKEY:int,
             O_ORDERSTATUS:string, O_TOTALPRICE:float, O_ORDERDATE:date,
             O_ORDERPRIORITY:string, O_CLERK:string, O_SHIPPRIORITY:int,
             O_COMMENT:string)));

DECLARE MAP HIGH_LINE_COUNT_mLINEITEM8(int)[]
[HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int] := 
AggSum([HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int], 
  ((O_ORDERPRIORITY:string ^= '2-HIGH') *
    ORDERS(HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int, O_CUSTKEY:int,
             O_ORDERSTATUS:string, O_TOTALPRICE:float, O_ORDERDATE:date,
             O_ORDERPRIORITY:string, O_CLERK:string, O_SHIPPRIORITY:int,
             O_COMMENT:string)));

DECLARE MAP HIGH_LINE_COUNT_mORDERS1(int)[]
[L_SHIPMODE:string, HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] := 
((L_SHIPMODE:string ^= 'MAIL') *
  AggSum([L_SHIPMODE:string, HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int], 
    (LINEITEM(HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int, L_PARTKEY:int,
                L_SUPPKEY:int, L_LINENUMBER:int, L_QUANTITY:float,
                L_EXTENDEDPRICE:float, L_DISCOUNT:float, L_TAX:float,
                L_RETURNFLAG:string, L_LINESTATUS:string, L_SHIPDATE:date,
                L_COMMITDATE:date, L_RECEIPTDATE:date, L_SHIPINSTRUCT:string,
                L_SHIPMODE:string, L_COMMENT:string) *
      {L_COMMITDATE:date < L_RECEIPTDATE:date} *
      {L_SHIPDATE:date < L_COMMITDATE:date} *
      {L_RECEIPTDATE:date >= DATE('1994-1-1')} *
      {L_RECEIPTDATE:date < DATE('1995-1-1')})));

DECLARE MAP HIGH_LINE_COUNT_mORDERS4(int)[]
[L_SHIPMODE:string, HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] := 
((L_SHIPMODE:string ^= 'SHIP') *
  AggSum([L_SHIPMODE:string, HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int], 
    (LINEITEM(HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int, L_PARTKEY:int,
                L_SUPPKEY:int, L_LINENUMBER:int, L_QUANTITY:float,
                L_EXTENDEDPRICE:float, L_DISCOUNT:float, L_TAX:float,
                L_RETURNFLAG:string, L_LINESTATUS:string, L_SHIPDATE:date,
                L_COMMITDATE:date, L_RECEIPTDATE:date, L_SHIPINSTRUCT:string,
                L_SHIPMODE:string, L_COMMENT:string) *
      {L_COMMITDATE:date < L_RECEIPTDATE:date} *
      {L_SHIPDATE:date < L_COMMITDATE:date} *
      {L_RECEIPTDATE:date >= DATE('1994-1-1')} *
      {L_RECEIPTDATE:date < DATE('1995-1-1')})));

DECLARE MAP LOW_LINE_COUNT(int)[][L_SHIPMODE:string] := 
AggSum([L_SHIPMODE:string], 
  (ORDERS(O_ORDERKEY:int, O_CUSTKEY:int, O_ORDERSTATUS:string,
            O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
            O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
    LINEITEM(O_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
               L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
               L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
               L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
               L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    ({L_SHIPMODE:string = 'MAIL'} + {L_SHIPMODE:string = 'SHIP'}) *
    {L_COMMITDATE:date < L_RECEIPTDATE:date} *
    {L_SHIPDATE:date < L_COMMITDATE:date} *
    {L_RECEIPTDATE:date >= DATE('1994-1-1')} *
    {L_RECEIPTDATE:date < DATE('1995-1-1')} *
    {'1-URGENT' != O_ORDERPRIORITY:string} *
    {'2-HIGH' != O_ORDERPRIORITY:string}));

DECLARE MAP LOW_LINE_COUNT_mLINEITEM6(int)[]
[LOW_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int] := 
AggSum([LOW_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int], 
  (ORDERS(LOW_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int, O_CUSTKEY:int,
            O_ORDERSTATUS:string, O_TOTALPRICE:float, O_ORDERDATE:date,
            O_ORDERPRIORITY:string, O_CLERK:string, O_SHIPPRIORITY:int,
            O_COMMENT:string) *
    {'1-URGENT' != O_ORDERPRIORITY:string} *
    {'2-HIGH' != O_ORDERPRIORITY:string}));

-------------------- QUERIES --------------------
DECLARE QUERY HIGH_LINE_COUNT := HIGH_LINE_COUNT(int)[][L_SHIPMODE:string];

DECLARE QUERY LOW_LINE_COUNT := LOW_LINE_COUNT(int)[][L_SHIPMODE:string];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  ({LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  (HIGH_LINE_COUNT_mLINEITEM1(int)[][LINEITEM_ORDERKEY:int] +
    HIGH_LINE_COUNT_mLINEITEM8(int)[][LINEITEM_ORDERKEY:int]) *
  ({LINEITEM_SHIPMODE:string = 'SHIP'} + {LINEITEM_SHIPMODE:string = 'MAIL'}));
   HIGH_LINE_COUNT_mORDERS1(int)[][LINEITEM_SHIPMODE:string, LINEITEM_ORDERKEY:int] += 
  ((LINEITEM_SHIPMODE:string ^= 'MAIL') *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')});
   HIGH_LINE_COUNT_mORDERS4(int)[][LINEITEM_SHIPMODE:string, LINEITEM_ORDERKEY:int] += 
  ((LINEITEM_SHIPMODE:string ^= 'SHIP') *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')});
   LOW_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  (LOW_LINE_COUNT_mLINEITEM6(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  ({LINEITEM_SHIPMODE:string = 'MAIL'} + {LINEITEM_SHIPMODE:string = 'SHIP'}));
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  ({LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  (HIGH_LINE_COUNT_mLINEITEM1(int)[][LINEITEM_ORDERKEY:int] +
    HIGH_LINE_COUNT_mLINEITEM8(int)[][LINEITEM_ORDERKEY:int]) *
  ({LINEITEM_SHIPMODE:string = 'SHIP'} + {LINEITEM_SHIPMODE:string = 'MAIL'}) *
  -1);
   HIGH_LINE_COUNT_mORDERS1(int)[][LINEITEM_SHIPMODE:string, LINEITEM_ORDERKEY:int] += 
  ((LINEITEM_SHIPMODE:string ^= 'MAIL') *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} * -1);
   HIGH_LINE_COUNT_mORDERS4(int)[][LINEITEM_SHIPMODE:string, LINEITEM_ORDERKEY:int] += 
  ((LINEITEM_SHIPMODE:string ^= 'SHIP') *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} * -1);
   LOW_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  (LOW_LINE_COUNT_mLINEITEM6(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  ({LINEITEM_SHIPMODE:string = 'MAIL'} + {LINEITEM_SHIPMODE:string = 'SHIP'}) *
  -1);
}

ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][L_SHIPMODE:string] += 
  ((HIGH_LINE_COUNT_mORDERS1(int)[][L_SHIPMODE:string, ORDERS_ORDERKEY:int] +
   HIGH_LINE_COUNT_mORDERS4(int)[][L_SHIPMODE:string, ORDERS_ORDERKEY:int]) *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}));
   HIGH_LINE_COUNT_mLINEITEM1(int)[][ORDERS_ORDERKEY:int] += {ORDERS_ORDERPRIORITY:string = '1-URGENT'};
   HIGH_LINE_COUNT_mLINEITEM8(int)[][ORDERS_ORDERKEY:int] += {ORDERS_ORDERPRIORITY:string = '2-HIGH'};
   LOW_LINE_COUNT(int)[][L_SHIPMODE:string] += 
  ({'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} *
  (HIGH_LINE_COUNT_mORDERS1(int)[][L_SHIPMODE:string, ORDERS_ORDERKEY:int] +
    HIGH_LINE_COUNT_mORDERS4(int)[][L_SHIPMODE:string, ORDERS_ORDERKEY:int]));
   LOW_LINE_COUNT_mLINEITEM6(int)[][ORDERS_ORDERKEY:int] += 
  ({'1-URGENT' != ORDERS_ORDERPRIORITY:string} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string});
}

ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][L_SHIPMODE:string] += 
  ((HIGH_LINE_COUNT_mORDERS1(int)[][L_SHIPMODE:string, ORDERS_ORDERKEY:int] +
   HIGH_LINE_COUNT_mORDERS4(int)[][L_SHIPMODE:string, ORDERS_ORDERKEY:int]) *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}) *
  -1);
   HIGH_LINE_COUNT_mLINEITEM1(int)[][ORDERS_ORDERKEY:int] += ({ORDERS_ORDERPRIORITY:string = '1-URGENT'} * -1);
   HIGH_LINE_COUNT_mLINEITEM8(int)[][ORDERS_ORDERKEY:int] += ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} * -1);
   LOW_LINE_COUNT(int)[][L_SHIPMODE:string] += 
  ({'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} *
  (HIGH_LINE_COUNT_mORDERS1(int)[][L_SHIPMODE:string, ORDERS_ORDERKEY:int] +
    HIGH_LINE_COUNT_mORDERS4(int)[][L_SHIPMODE:string, ORDERS_ORDERKEY:int]) *
  -1);
   LOW_LINE_COUNT_mLINEITEM6(int)[][ORDERS_ORDERKEY:int] += 
  ({'1-URGENT' != ORDERS_ORDERPRIORITY:string} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string} * -1);
}

ON SYSTEM READY {
}

CORRECT HIGH_LINE_COUNT_mLINEITEM1[][delta_HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  ({LINEITEM_ORDERKEY:int =
 delta_HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  ({LINEITEM_SHIPMODE:string = 'SHIP'} + {LINEITEM_SHIPMODE:string = 'MAIL'}) *
  delta_HIGH_LINE_COUNT_mLINEITEM1:int);
}

CORRECT HIGH_LINE_COUNT_mLINEITEM8[][delta_HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mLINEITEM8:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  ({LINEITEM_ORDERKEY:int =
 delta_HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  ({LINEITEM_SHIPMODE:string = 'SHIP'} + {LINEITEM_SHIPMODE:string = 'MAIL'}) *
  delta_HIGH_LINE_COUNT_mLINEITEM8:int);
}

CORRECT LOW_LINE_COUNT_mLINEITEM6[][delta_LOW_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_LOW_LINE_COUNT_mLINEITEM6:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   LOW_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  ({LINEITEM_ORDERKEY:int =
 delta_LOW_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  ({LINEITEM_SHIPMODE:string = 'MAIL'} + {LINEITEM_SHIPMODE:string = 'SHIP'}) *
  delta_LOW_LINE_COUNT_mLINEITEM6:int);
}

CORRECT HIGH_LINE_COUNT_mLINEITEM1[][delta_HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  ({LINEITEM_ORDERKEY:int =
 delta_HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  ({LINEITEM_SHIPMODE:string = 'SHIP'} + {LINEITEM_SHIPMODE:string = 'MAIL'}) *
  -1 * delta_HIGH_LINE_COUNT_mLINEITEM1:int);
}

CORRECT HIGH_LINE_COUNT_mLINEITEM8[][delta_HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mLINEITEM8:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  ({LINEITEM_ORDERKEY:int =
 delta_HIGH_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  ({LINEITEM_SHIPMODE:string = 'SHIP'} + {LINEITEM_SHIPMODE:string = 'MAIL'}) *
  -1 * delta_HIGH_LINE_COUNT_mLINEITEM8:int);
}

CORRECT LOW_LINE_COUNT_mLINEITEM6[][delta_LOW_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_LOW_LINE_COUNT_mLINEITEM6:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   LOW_LINE_COUNT(int)[][LINEITEM_SHIPMODE:string] += 
  ({LINEITEM_ORDERKEY:int =
 delta_LOW_LINE_COUNT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date < DATE('1995-1-1')} *
  {LINEITEM_RECEIPTDATE:date >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE:date < LINEITEM_COMMITDATE:date} *
  {LINEITEM_COMMITDATE:date < LINEITEM_RECEIPTDATE:date} *
  ({LINEITEM_SHIPMODE:string = 'MAIL'} + {LINEITEM_SHIPMODE:string = 'SHIP'}) *
  -1 * delta_LOW_LINE_COUNT_mLINEITEM6:int);
}

CORRECT HIGH_LINE_COUNT_mORDERS1[][delta_L_SHIPMODE:string,delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mORDERS1:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}) *
  delta_HIGH_LINE_COUNT_mORDERS1:int);
   LOW_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} *
  delta_HIGH_LINE_COUNT_mORDERS1:int);
}

CORRECT HIGH_LINE_COUNT_mORDERS4[][delta_L_SHIPMODE:string,delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mORDERS4:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}) *
  delta_HIGH_LINE_COUNT_mORDERS4:int);
   LOW_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} *
  delta_HIGH_LINE_COUNT_mORDERS4:int);
}

CORRECT HIGH_LINE_COUNT_mORDERS1[][delta_L_SHIPMODE:string,delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mORDERS1:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}) *
  delta_HIGH_LINE_COUNT_mORDERS1:int);
   LOW_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} *
  delta_HIGH_LINE_COUNT_mORDERS1:int);
}

CORRECT HIGH_LINE_COUNT_mORDERS4[][delta_L_SHIPMODE:string,delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mORDERS4:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}) *
  delta_HIGH_LINE_COUNT_mORDERS4:int);
   LOW_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} *
  delta_HIGH_LINE_COUNT_mORDERS4:int);
}

CORRECT HIGH_LINE_COUNT_mORDERS1[][delta_L_SHIPMODE:string,delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mORDERS1:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}) *
  -1 * delta_HIGH_LINE_COUNT_mORDERS1:int);
   LOW_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} * -1 *
  delta_HIGH_LINE_COUNT_mORDERS1:int);
}

CORRECT HIGH_LINE_COUNT_mORDERS4[][delta_L_SHIPMODE:string,delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mORDERS4:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}) *
  -1 * delta_HIGH_LINE_COUNT_mORDERS4:int);
   LOW_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} * -1 *
  delta_HIGH_LINE_COUNT_mORDERS4:int);
}

CORRECT HIGH_LINE_COUNT_mORDERS1[][delta_L_SHIPMODE:string,delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mORDERS1:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}) *
  -1 * delta_HIGH_LINE_COUNT_mORDERS1:int);
   LOW_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} * -1 *
  delta_HIGH_LINE_COUNT_mORDERS1:int);
}

CORRECT HIGH_LINE_COUNT_mORDERS4[][delta_L_SHIPMODE:string,delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int] += delta_HIGH_LINE_COUNT_mORDERS4:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   HIGH_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  ({ORDERS_ORDERPRIORITY:string = '2-HIGH'} +
    {ORDERS_ORDERPRIORITY:string = '1-URGENT'}) *
  -1 * delta_HIGH_LINE_COUNT_mORDERS4:int);
   LOW_LINE_COUNT(int)[][delta_L_SHIPMODE:string] += 
  ({ORDERS_ORDERKEY:int = delta_HIGH_LINE_COUNT_mORDERSORDERS_ORDERKEY:int} *
  {'2-HIGH' != ORDERS_ORDERPRIORITY:string} *
  {'1-URGENT' != ORDERS_ORDERPRIORITY:string} * -1 *
  delta_HIGH_LINE_COUNT_mORDERS4:int);
}