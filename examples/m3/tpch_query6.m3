-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'examples/data/tpch/lineitem.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP REVENUE[][] := 
AggSum([], 
  (LINEITEM(L_ORDERKEY, L_PARTKEY, L_SUPPKEY, L_LINENUMBER, L_QUANTITY,
              L_EXTENDEDPRICE, L_DISCOUNT, L_TAX, L_RETURNFLAG, L_LINESTATUS,
              L_SHIPDATE, L_COMMITDATE, L_RECEIPTDATE, L_SHIPINSTRUCT,
              L_SHIPMODE, L_COMMENT) *
    {L_SHIPDATE >= DATE('1994-1-1')} * {L_SHIPDATE < DATE('1995-1-1')} *
    {L_DISCOUNT >= 0.05} * {L_DISCOUNT <= 0.07} * {L_QUANTITY < 24} *
    L_EXTENDEDPRICE * L_DISCOUNT));

-------------------- QUERIES --------------------
DECLARE QUERY REVENUE := REVENUE(float)[][];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY, LINEITEM_PARTKEY, LINEITEM_SUPPKEY, LINEITEM_LINENUMBER, LINEITEM_QUANTITY, LINEITEM_EXTENDEDPRICE, LINEITEM_DISCOUNT, LINEITEM_TAX, LINEITEM_RETURNFLAG, LINEITEM_LINESTATUS, LINEITEM_SHIPDATE, LINEITEM_COMMITDATE, LINEITEM_RECEIPTDATE, LINEITEM_SHIPINSTRUCT, LINEITEM_SHIPMODE, LINEITEM_COMMENT) {
   REVENUE(float)[][] += 
  ({LINEITEM_SHIPDATE >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE < DATE('1995-1-1')} * {LINEITEM_DISCOUNT >= 0.05} *
  {LINEITEM_DISCOUNT <= 0.07} * {LINEITEM_QUANTITY < 24} *
  LINEITEM_EXTENDEDPRICE * LINEITEM_DISCOUNT);
}

ON - LINEITEM(LINEITEM_ORDERKEY, LINEITEM_PARTKEY, LINEITEM_SUPPKEY, LINEITEM_LINENUMBER, LINEITEM_QUANTITY, LINEITEM_EXTENDEDPRICE, LINEITEM_DISCOUNT, LINEITEM_TAX, LINEITEM_RETURNFLAG, LINEITEM_LINESTATUS, LINEITEM_SHIPDATE, LINEITEM_COMMITDATE, LINEITEM_RECEIPTDATE, LINEITEM_SHIPINSTRUCT, LINEITEM_SHIPMODE, LINEITEM_COMMENT) {
   REVENUE(float)[][] += 
  ({LINEITEM_SHIPDATE >= DATE('1994-1-1')} *
  {LINEITEM_SHIPDATE < DATE('1995-1-1')} * {LINEITEM_DISCOUNT >= 0.05} *
  {LINEITEM_DISCOUNT <= 0.07} * {LINEITEM_QUANTITY < 24} * -1 *
  LINEITEM_EXTENDEDPRICE * LINEITEM_DISCOUNT);
}

ON SYSTEM READY {
}
