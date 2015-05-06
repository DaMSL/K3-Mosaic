-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM ORDERS(ORDERS_ORDERKEY int, ORDERS_CUSTKEY int, ORDERS_ORDERSTATUS string, ORDERS_TOTALPRICE float, ORDERS_ORDERDATE date, ORDERS_ORDERPRIORITY string, ORDERS_CLERK string, ORDERS_SHIPPRIORITY int, ORDERS_COMMENT string)
  FROM FILE 'data/tpch/orders.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM SUPPLIER(SUPPLIER_SUPPKEY int, SUPPLIER_NAME string, SUPPLIER_ADDRESS string, SUPPLIER_NATIONKEY int, SUPPLIER_PHONE string, SUPPLIER_ACCTBAL float, SUPPLIER_COMMENT string)
  FROM FILE 'data/tpch/supplier.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE TABLE NATION(NATION_NATIONKEY int, NATION_NAME string, NATION_REGIONKEY int, NATION_COMMENT string)
  FROM FILE 'data/tpch/nation.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP NUMWAIT(int)[][S_NAME:string] := 
AggSum([S_NAME:string], 
  ((N_NAME:string ^= 'SAUDI ARABIA') * (O_ORDERSTATUS:string ^= 'F') *
    SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
               S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string) *
    LINEITEM(L1_ORDERKEY:int, L1_PARTKEY:int, S_SUPPKEY:int,
               L1_LINENUMBER:int, L1_QUANTITY:float, L1_EXTENDEDPRICE:float,
               L1_DISCOUNT:float, L1_TAX:float, L1_RETURNFLAG:string,
               L1_LINESTATUS:string, L1_SHIPDATE:date, L1_COMMITDATE:date,
               L1_RECEIPTDATE:date, L1_SHIPINSTRUCT:string,
               L1_SHIPMODE:string, L1_COMMENT:string) *
    ORDERS(L1_ORDERKEY:int, O_CUSTKEY:int, O_ORDERSTATUS:string,
             O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
             O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string) *
    {L1_RECEIPTDATE:date > L1_COMMITDATE:date} *
    EXISTS(
      AggSum([L1_ORDERKEY:int], 
        (LINEITEM(L1_ORDERKEY:int, L2_PARTKEY:int, L2_SUPPKEY:int,
                    L2_LINENUMBER:int, L2_QUANTITY:float,
                    L2_EXTENDEDPRICE:float, L2_DISCOUNT:float, L2_TAX:float,
                    L2_RETURNFLAG:string, L2_LINESTATUS:string,
                    L2_SHIPDATE:date, L2_COMMITDATE:date,
                    L2_RECEIPTDATE:date, L2_SHIPINSTRUCT:string,
                    L2_SHIPMODE:string, L2_COMMENT:string) *
          {L2_SUPPKEY:int != S_SUPPKEY:int}))) *
    AggSum([], 
      ((__domain_1:int ^=
         AggSum([L1_ORDERKEY:int], 
           (LINEITEM(L1_ORDERKEY:int, L3_PARTKEY:int, L3_SUPPKEY:int,
                       L3_LINENUMBER:int, L3_QUANTITY:float,
                       L3_EXTENDEDPRICE:float, L3_DISCOUNT:float,
                       L3_TAX:float, L3_RETURNFLAG:string,
                       L3_LINESTATUS:string, L3_SHIPDATE:date,
                       L3_COMMITDATE:date, L3_RECEIPTDATE:date,
                       L3_SHIPINSTRUCT:string, L3_SHIPMODE:string,
                       L3_COMMENT:string) *
             {L3_SUPPKEY:int != S_SUPPKEY:int} *
             {L3_RECEIPTDATE:date > L3_COMMITDATE:date}))) *
        (__domain_1:int ^= 0)))));

DECLARE MAP NUMWAIT_pLINEITEM6(int)[]
[S_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, S_SUPPKEY:int] := 
AggSum([S_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, S_SUPPKEY:int], 
  ((N_NAME:string ^= 'SAUDI ARABIA') *
    SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
               S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string) *
    LINEITEM(NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, L1_PARTKEY:int,
               S_SUPPKEY:int, L1_LINENUMBER:int, L1_QUANTITY:float,
               L1_EXTENDEDPRICE:float, L1_DISCOUNT:float, L1_TAX:float,
               L1_RETURNFLAG:string, L1_LINESTATUS:string, L1_SHIPDATE:date,
               L1_COMMITDATE:date, L1_RECEIPTDATE:date,
               L1_SHIPINSTRUCT:string, L1_SHIPMODE:string, L1_COMMENT:string) *
    {L1_RECEIPTDATE:date > L1_COMMITDATE:date}));

DECLARE MAP NUMWAIT_mLINEITEM1(int)[]
[S_NAME:string, NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int] := 
AggSum([S_NAME:string, NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int], 
  ((N_NAME:string ^= 'SAUDI ARABIA') *
    SUPPLIER(NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int, S_NAME:string,
               S_ADDRESS:string, S_NATIONKEY:int, S_PHONE:string,
               S_ACCTBAL:float, S_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string)));

DECLARE MAP NUMWAIT_mLINEITEM2(int)[][NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int] := 
AggSum([NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int], 
  ((O_ORDERSTATUS:string ^= 'F') *
    ORDERS(NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int, O_CUSTKEY:int,
             O_ORDERSTATUS:string, O_TOTALPRICE:float, O_ORDERDATE:date,
             O_ORDERPRIORITY:string, O_CLERK:string, O_SHIPPRIORITY:int,
             O_COMMENT:string)));

DECLARE MAP NUMWAIT_mLINEITEM7(int)[][S_NAME:string, S_SUPPKEY:int, L1_ORDERKEY:int] := 
AggSum([S_NAME:string, S_SUPPKEY:int, L1_ORDERKEY:int], 
  ((N_NAME:string ^= 'SAUDI ARABIA') * (O_ORDERSTATUS:string ^= 'F') *
    SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
               S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string) *
    ORDERS(L1_ORDERKEY:int, O_CUSTKEY:int, O_ORDERSTATUS:string,
             O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
             O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
    LINEITEM(L1_ORDERKEY:int, L1_PARTKEY:int, S_SUPPKEY:int,
               L1_LINENUMBER:int, L1_QUANTITY:float, L1_EXTENDEDPRICE:float,
               L1_DISCOUNT:float, L1_TAX:float, L1_RETURNFLAG:string,
               L1_LINESTATUS:string, L1_SHIPDATE:date, L1_COMMITDATE:date,
               L1_RECEIPTDATE:date, L1_SHIPINSTRUCT:string,
               L1_SHIPMODE:string, L1_COMMENT:string) *
    {L1_RECEIPTDATE:date > L1_COMMITDATE:date}));

DECLARE MAP NUMWAIT_mSUPPLIER1(int)[]
[NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] := 
AggSum([NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int, L1_ORDERKEY:int], 
  ((O_ORDERSTATUS:string ^= 'F') *
    LINEITEM(L1_ORDERKEY:int, L1_PARTKEY:int,
               NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int, L1_LINENUMBER:int,
               L1_QUANTITY:float, L1_EXTENDEDPRICE:float, L1_DISCOUNT:float,
               L1_TAX:float, L1_RETURNFLAG:string, L1_LINESTATUS:string,
               L1_SHIPDATE:date, L1_COMMITDATE:date, L1_RECEIPTDATE:date,
               L1_SHIPINSTRUCT:string, L1_SHIPMODE:string, L1_COMMENT:string) *
    ORDERS(L1_ORDERKEY:int, O_CUSTKEY:int, O_ORDERSTATUS:string,
             O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
             O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
    {L1_RECEIPTDATE:date > L1_COMMITDATE:date}));

DECLARE MAP NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] := 
AggSum([L1_ORDERKEY:int, L3_SUPPKEY:int], 
  (LINEITEM(L1_ORDERKEY:int, L3_PARTKEY:int, L3_SUPPKEY:int,
              L3_LINENUMBER:int, L3_QUANTITY:float, L3_EXTENDEDPRICE:float,
              L3_DISCOUNT:float, L3_TAX:float, L3_RETURNFLAG:string,
              L3_LINESTATUS:string, L3_SHIPDATE:date, L3_COMMITDATE:date,
              L3_RECEIPTDATE:date, L3_SHIPINSTRUCT:string,
              L3_SHIPMODE:string, L3_COMMENT:string) *
    {L3_RECEIPTDATE:date > L3_COMMITDATE:date}));

DECLARE MAP NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] := 
AggSum([L1_ORDERKEY:int, L2_SUPPKEY:int], 
  LINEITEM(L1_ORDERKEY:int, L2_PARTKEY:int, L2_SUPPKEY:int,
             L2_LINENUMBER:int, L2_QUANTITY:float, L2_EXTENDEDPRICE:float,
             L2_DISCOUNT:float, L2_TAX:float, L2_RETURNFLAG:string,
             L2_LINESTATUS:string, L2_SHIPDATE:date, L2_COMMITDATE:date,
             L2_RECEIPTDATE:date, L2_SHIPINSTRUCT:string, L2_SHIPMODE:string,
             L2_COMMENT:string));

DECLARE MAP NUMWAIT_mSUPPLIER2(int)[][NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
AggSum([NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((N_NAME:string ^= 'SAUDI ARABIA') *
    NATION(NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int, N_NAME:string,
             N_REGIONKEY:int, N_COMMENT:string)));

-------------------- QUERIES --------------------
DECLARE QUERY NUMWAIT := NUMWAIT(int)[][S_NAME:string];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  (NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  (({LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
     EXISTS(
       AggSum([LINEITEM_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_E3_1(int)[]
          [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
           {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
     NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         (__domain_1:int ^=
           AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int})))))) +
    ((AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
        ((__domain_1:int ^= 0) *
          NUMWAIT_pLINEITEM6(int)[]
          [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
          (__domain_1:int ^=
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          EXISTS(
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int}))))) +
       AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
         ((__domain_1:int ^= 0) *
           NUMWAIT_pLINEITEM6(int)[]
           [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
           (__domain_1:int ^=
             AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
           EXISTS(
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != S_SUPPKEY:int})) +
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))) *
      -1) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int})))) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int})) +
            ({LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
              {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date}))) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))));
   NUMWAIT_pLINEITEM6(int)[][S_NAME:string, LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += 
  (NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date});
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  (NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date});
   NUMWAIT_mSUPPLIER1(int)[][LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  (NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date});
   NUMWAIT_mSUPPLIER1_L2_1(int)[][LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date};
   NUMWAIT_mSUPPLIER1_E3_1(int)[][LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += 1;
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  (((({LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
     AggSum([], 
       EXISTS(
         AggSum([LINEITEM_ORDERKEY:int], 
           (NUMWAIT_mSUPPLIER1_E3_1(int)[]
            [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
             {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})))) *
     NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
     NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
     AggSum([], 
       ((__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
         (__domain_1:int ^= 0)))) +
    AggSum([S_NAME:string], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_mLINEITEM7(int)[]
        [S_NAME:string, S_SUPPKEY:int, L1_ORDERKEY:int] *
        (__domain_1:int ^=
          AggSum([L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
        EXISTS(
          AggSum([L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
              {L2_SUPPKEY:int != S_SUPPKEY:int}))))) +
    AggSum([S_NAME:string], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_mLINEITEM7(int)[]
        [S_NAME:string, S_SUPPKEY:int, L1_ORDERKEY:int] *
        (__domain_1:int ^=
          AggSum([L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
        EXISTS(
          (AggSum([L1_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            ((L1_ORDERKEY:int ^= LINEITEM_ORDERKEY:int) *
              {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1)))))) *
   -1) +
  AggSum([S_NAME:string], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mLINEITEM7(int)[]
      [S_NAME:string, S_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
      EXISTS(
        (AggSum([L1_ORDERKEY:int], 
           (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
             {L2_SUPPKEY:int != S_SUPPKEY:int})) +
          ((L1_ORDERKEY:int ^= LINEITEM_ORDERKEY:int) *
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1))))) +
  AggSum([S_NAME:string], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mLINEITEM7(int)[]
      [S_NAME:string, S_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        (AggSum([L1_ORDERKEY:int], 
           (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
             {L3_SUPPKEY:int != S_SUPPKEY:int})) +
          ((L1_ORDERKEY:int ^= LINEITEM_ORDERKEY:int) *
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
            {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * 
          -1))) *
      EXISTS(
        (AggSum([L1_ORDERKEY:int], 
           (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
             {L2_SUPPKEY:int != S_SUPPKEY:int})) +
          ((L1_ORDERKEY:int ^= LINEITEM_ORDERKEY:int) *
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1))))));
   NUMWAIT_pLINEITEM6(int)[][S_NAME:string, LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += 
  (NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1);
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  (NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1);
   NUMWAIT_mSUPPLIER1(int)[][LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  (NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1);
   NUMWAIT_mSUPPLIER1_L2_1(int)[][LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += ({LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1);
   NUMWAIT_mSUPPLIER1_E3_1(int)[][LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += -1;
}

ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([S_NAME:string, ORDERS_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_pLINEITEM6(int)[]
      [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int] *
      (__domain_1:int ^=
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != S_SUPPKEY:int}))))));
   NUMWAIT_mLINEITEM2(int)[][ORDERS_ORDERKEY:int] += {ORDERS_ORDERSTATUS:string = 'F'};
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, S_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERSTATUS:string = 'F'} *
  NUMWAIT_pLINEITEM6(int)[]
  [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int]);
   NUMWAIT_mSUPPLIER1(int)[][NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERSTATUS:string = 'F'} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [ORDERS_ORDERKEY:int, NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int]);
}

ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([S_NAME:string, ORDERS_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_pLINEITEM6(int)[]
      [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int] *
      (__domain_1:int ^=
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != S_SUPPKEY:int}))))) *
  -1);
   NUMWAIT_mLINEITEM2(int)[][ORDERS_ORDERKEY:int] += ({ORDERS_ORDERSTATUS:string = 'F'} * -1);
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, S_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERSTATUS:string = 'F'} *
  NUMWAIT_pLINEITEM6(int)[]
  [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int] * -1);
   NUMWAIT_mSUPPLIER1(int)[][NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERSTATUS:string = 'F'} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [ORDERS_ORDERKEY:int, NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int] * -1);
}

ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  (AggSum([SUPPLIER_SUPPKEY:int], 
   ((__domain_1:int ^= 0) *
     NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
     (__domain_1:int ^=
       AggSum([L1_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
           {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
     EXISTS(
       AggSum([L1_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
           {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int]);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  (NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int]);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int];
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  (NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int]);
}

ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  (AggSum([SUPPLIER_SUPPKEY:int], 
   ((__domain_1:int ^= 0) *
     NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
     (__domain_1:int ^=
       AggSum([L1_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
           {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
     EXISTS(
       AggSum([L1_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
           {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  (NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] * -1);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += (NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  (NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] *
  NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] * 
-1);
}

ON SYSTEM READY {
   NUMWAIT_mSUPPLIER2(int)[][NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
  AggSum([NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((N_NAME:string ^= 'SAUDI ARABIA') *
    NATION(NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int, N_NAME:string,
             N_REGIONKEY:int, N_COMMENT:string)));
}

CORRECT NUMWAIT_mLINEITEM2[][delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_NUMWAIT_mLINEITEM2:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  (({LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
     EXISTS(
       AggSum([LINEITEM_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_E3_1(int)[]
          [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
           {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
     NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
         (__domain_1:int ^= 0)))) +
    ((AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
        ((__domain_1:int ^= 0) *
          NUMWAIT_pLINEITEM6(int)[]
          [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
          (__domain_1:int ^=
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          EXISTS(
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int}))))) +
       AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
         ((__domain_1:int ^= 0) *
           NUMWAIT_pLINEITEM6(int)[]
           [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
           (__domain_1:int ^=
             AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
           EXISTS(
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != S_SUPPKEY:int})) +
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))) *
      -1) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int})))) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int})) +
            ({LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
              {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date}))) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))) *
  delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mSUPPLIER1(int)[][LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM2:int);
}

CORRECT NUMWAIT_mSUPPLIER1_E3_1[][delta_L1_ORDERKEY:int,delta_L2_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_E3_1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  (({LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
     (EXISTS(
        (AggSum([LINEITEM_ORDERKEY:int], 
           (NUMWAIT_mSUPPLIER1_E3_1(int)[]
            [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
             {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})) +
          ({delta_L2_SUPPKEY:int != LINEITEM_SUPPKEY:int} *
            delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
       (EXISTS(
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_E3_1(int)[]
             [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
              {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
         -1)) *
     NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         (__domain_1:int ^=
           AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int})))))) +
    ((AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
        ((__domain_1:int ^= 0) *
          NUMWAIT_pLINEITEM6(int)[]
          [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
          (__domain_1:int ^=
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          (EXISTS(
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != S_SUPPKEY:int})) +
               ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
                 delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
            (EXISTS(
               AggSum([LINEITEM_ORDERKEY:int], 
                 (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                  [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                   {L2_SUPPKEY:int != S_SUPPKEY:int}))) *
              -1)))) +
       AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
         ((__domain_1:int ^= 0) *
           NUMWAIT_pLINEITEM6(int)[]
           [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
           (__domain_1:int ^=
             AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
           (EXISTS(
              (AggSum([LINEITEM_ORDERKEY:int], 
                 (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                  [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                   {L2_SUPPKEY:int != S_SUPPKEY:int})) +
                {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} +
                ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
                  delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
             (EXISTS(
                (AggSum([LINEITEM_ORDERKEY:int], 
                   (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                    [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                     {L2_SUPPKEY:int != S_SUPPKEY:int})) +
                  {LINEITEM_SUPPKEY:int != S_SUPPKEY:int})) *
               -1))))) *
      -1) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
        (EXISTS(
           (AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int})) +
             {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} +
             ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
               delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
          (EXISTS(
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != S_SUPPKEY:int})) +
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int})) *
            -1)))) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int})) +
            ({LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
              {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date}))) *
        (EXISTS(
           (AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int})) +
             {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} +
             ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
               delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
          (EXISTS(
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != S_SUPPKEY:int})) +
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int})) *
            -1))))));
}

CORRECT NUMWAIT_mLINEITEM1[][delta_S_NAME:string,delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int] += delta_NUMWAIT_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  EXISTS(
    AggSum([LINEITEM_ORDERKEY:int], 
      (NUMWAIT_mSUPPLIER1_E3_1(int)[][LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
        {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([LINEITEM_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_L2_1(int)[]
          [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
           {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
      (__domain_1:int ^= 0))) *
  delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_pLINEITEM6(int)[][delta_S_NAME:string, LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM1:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  (({LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
     EXISTS(
       AggSum([LINEITEM_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_E3_1(int)[]
          [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
           {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
     NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         ((__domain_1:int ^=
            (AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int})) +
              ({delta_L3_SUPPKEY:int != LINEITEM_SUPPKEY:int} *
                delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
           ((__domain_1:int ^=
              AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                 [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                  {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
             -1))))) +
    ((AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
        ((__domain_1:int ^= 0) *
          NUMWAIT_pLINEITEM6(int)[]
          [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
          ((__domain_1:int ^=
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                 [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                  {L3_SUPPKEY:int != S_SUPPKEY:int})) +
               ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
                 delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
            ((__domain_1:int ^=
               AggSum([LINEITEM_ORDERKEY:int], 
                 (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                  [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                   {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
              -1)) *
          EXISTS(
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int}))))) +
       AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
         ((__domain_1:int ^= 0) *
           NUMWAIT_pLINEITEM6(int)[]
           [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
           ((__domain_1:int ^=
              (AggSum([LINEITEM_ORDERKEY:int], 
                 (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                  [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                   {L3_SUPPKEY:int != S_SUPPKEY:int})) +
                ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
                  delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
             ((__domain_1:int ^=
                AggSum([LINEITEM_ORDERKEY:int], 
                  (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                   [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                    {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
               -1)) *
           EXISTS(
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != S_SUPPKEY:int})) +
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))) *
      -1) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        ((__domain_1:int ^=
           (AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int})) +
             ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
               delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
          ((__domain_1:int ^=
             AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
            -1)) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int})))) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        ((__domain_1:int ^=
           (AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int})) +
             ({LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
               {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date}) +
             ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
               delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
          ((__domain_1:int ^=
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                 [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                  {L3_SUPPKEY:int != S_SUPPKEY:int})) +
               ({LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
                 {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date}))) *
            -1)) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))));
}

CORRECT NUMWAIT_pLINEITEM6[][delta_S_NAME:string,delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int,delta_S_SUPPKEY:int] += delta_NUMWAIT_pLINEITEM6:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  ((EXISTS(
      (AggSum([LINEITEM_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_E3_1(int)[]
          [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
           {L2_SUPPKEY:int != delta_S_SUPPKEY:int})) +
        {LINEITEM_SUPPKEY:int != delta_S_SUPPKEY:int})) *
     (AggSum([], 
        ((__domain_1:int ^=
           AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
          (__domain_1:int ^= 0))) +
       AggSum([], 
         ((__domain_1:int ^=
            (AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != delta_S_SUPPKEY:int})) +
              ({LINEITEM_SUPPKEY:int != delta_S_SUPPKEY:int} *
                {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date}))) *
           (__domain_1:int ^= 0))))) +
    ((AggSum([], 
        ((__domain_1:int ^=
           AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
          (__domain_1:int ^= 0) *
          EXISTS(
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != delta_S_SUPPKEY:int}))))) +
       (EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != delta_S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != delta_S_SUPPKEY:int})) *
         AggSum([], 
           ((__domain_1:int ^=
              AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                 [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                  {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
             (__domain_1:int ^= 0))))) *
      -1)) *
  delta_NUMWAIT_pLINEITEM6:int);
}

CORRECT NUMWAIT_mLINEITEM1[][delta_S_NAME:string,delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int] += delta_NUMWAIT_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  EXISTS(
    AggSum([LINEITEM_ORDERKEY:int], 
      (NUMWAIT_mSUPPLIER1_E3_1(int)[][LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
        {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([LINEITEM_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_L2_1(int)[]
          [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
           {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
      (__domain_1:int ^= 0))) *
  delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_pLINEITEM6(int)[][delta_S_NAME:string, LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM1:int);
}

CORRECT NUMWAIT_mLINEITEM2[][delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_NUMWAIT_mLINEITEM2:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  (({LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
     EXISTS(
       AggSum([LINEITEM_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_E3_1(int)[]
          [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
           {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
     NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
         (__domain_1:int ^= 0)))) +
    ((AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
        ((__domain_1:int ^= 0) *
          NUMWAIT_pLINEITEM6(int)[]
          [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
          (__domain_1:int ^=
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          EXISTS(
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int}))))) +
       AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
         ((__domain_1:int ^= 0) *
           NUMWAIT_pLINEITEM6(int)[]
           [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
           (__domain_1:int ^=
             AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
           EXISTS(
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != S_SUPPKEY:int})) +
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))) *
      -1) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int})))) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int})) +
            ({LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
              {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date}))) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))) *
  delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mSUPPLIER1(int)[][LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM2:int);
}

CORRECT NUMWAIT_mLINEITEM1[][delta_S_NAME:string,delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int] += delta_NUMWAIT_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  EXISTS(
    AggSum([LINEITEM_ORDERKEY:int], 
      (NUMWAIT_mSUPPLIER1_E3_1(int)[][LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
        {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([LINEITEM_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_L2_1(int)[]
          [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
           {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
      (__domain_1:int ^= 0))) *
  delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_pLINEITEM6(int)[][delta_S_NAME:string, LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM1:int);
}

CORRECT NUMWAIT_mLINEITEM2[][delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_NUMWAIT_mLINEITEM2:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  (({LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
     EXISTS(
       AggSum([LINEITEM_ORDERKEY:int], 
         (NUMWAIT_mSUPPLIER1_E3_1(int)[]
          [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
           {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
     NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
         (__domain_1:int ^= 0)))) +
    ((AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
        ((__domain_1:int ^= 0) *
          NUMWAIT_pLINEITEM6(int)[]
          [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
          (__domain_1:int ^=
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          EXISTS(
            AggSum([LINEITEM_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int}))))) +
       AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
         ((__domain_1:int ^= 0) *
           NUMWAIT_pLINEITEM6(int)[]
           [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
           (__domain_1:int ^=
             AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
           EXISTS(
             (AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != S_SUPPKEY:int})) +
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))) *
      -1) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int})))) +
    AggSum([S_NAME:string, LINEITEM_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_pLINEITEM6(int)[]
        [S_NAME:string, LINEITEM_ORDERKEY:int, S_SUPPKEY:int] *
        (__domain_1:int ^=
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int})) +
            ({LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
              {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date}))) *
        EXISTS(
          (AggSum([LINEITEM_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int}))))) *
  delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mSUPPLIER1(int)[][LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  delta_NUMWAIT_mLINEITEM2:int);
}

CORRECT NUMWAIT_mSUPPLIER1_E3_1[][delta_L1_ORDERKEY:int,delta_L2_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_E3_1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  (((({LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
     AggSum([], 
       ((LINEITEM_ORDERKEY:int ^= delta_L1_ORDERKEY:int) *
         (EXISTS(
            (AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                 {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})) +
              ({delta_L2_SUPPKEY:int != LINEITEM_SUPPKEY:int} *
                delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
           (EXISTS(
              AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
             -1)))) *
     NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
     NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
     AggSum([], 
       ((__domain_1:int ^=
          AggSum([LINEITEM_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
         (__domain_1:int ^= 0)))) +
    AggSum([S_NAME:string, delta_L1_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_mLINEITEM7(int)[]
        [S_NAME:string, S_SUPPKEY:int, delta_L1_ORDERKEY:int] *
        (__domain_1:int ^=
          AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
        (EXISTS(
           (AggSum([delta_L1_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int})) +
             ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
               delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
          (EXISTS(
             AggSum([delta_L1_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
                 {L2_SUPPKEY:int != S_SUPPKEY:int}))) *
            -1)))) +
    AggSum([S_NAME:string, delta_L1_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_mLINEITEM7(int)[]
        [S_NAME:string, S_SUPPKEY:int, delta_L1_ORDERKEY:int] *
        (__domain_1:int ^=
          AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
        (EXISTS(
           (AggSum([delta_L1_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int})) +
             ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1) +
             ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
               delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
          (EXISTS(
             (AggSum([delta_L1_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_E3_1(int)[]
                 [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
                  {L2_SUPPKEY:int != S_SUPPKEY:int})) +
               ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
                 {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1))) *
            -1))))) *
   -1) +
  AggSum([S_NAME:string, delta_L1_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mLINEITEM7(int)[]
      [S_NAME:string, S_SUPPKEY:int, delta_L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
      (EXISTS(
         (AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_E3_1(int)[]
             [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
              {L2_SUPPKEY:int != S_SUPPKEY:int})) +
           ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
             {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1) +
           ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
        (EXISTS(
           (AggSum([delta_L1_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int})) +
             ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1))) *
          -1)))) +
  AggSum([S_NAME:string, delta_L1_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mLINEITEM7(int)[]
      [S_NAME:string, S_SUPPKEY:int, delta_L1_ORDERKEY:int] *
      (__domain_1:int ^=
        (AggSum([delta_L1_ORDERKEY:int], 
           (NUMWAIT_mSUPPLIER1_L2_1(int)[]
            [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
             {L3_SUPPKEY:int != S_SUPPKEY:int})) +
          ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
            {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * 
          -1))) *
      (EXISTS(
         (AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_E3_1(int)[]
             [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
              {L2_SUPPKEY:int != S_SUPPKEY:int})) +
           ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
             {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1) +
           ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
        (EXISTS(
           (AggSum([delta_L1_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_E3_1(int)[]
               [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
                {L2_SUPPKEY:int != S_SUPPKEY:int})) +
             ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1))) *
          -1)))));
}

CORRECT NUMWAIT_mLINEITEM1[][delta_S_NAME:string,delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int] += delta_NUMWAIT_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  AggSum([], 
    EXISTS(
      AggSum([LINEITEM_ORDERKEY:int], 
        (NUMWAIT_mSUPPLIER1_E3_1(int)[]
         [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
          {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})))) *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([LINEITEM_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_pLINEITEM6(int)[][delta_S_NAME:string, LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM1:int);
}

CORRECT NUMWAIT_mLINEITEM2[][delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_NUMWAIT_mLINEITEM2:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  AggSum([], 
    EXISTS(
      AggSum([LINEITEM_ORDERKEY:int], 
        (NUMWAIT_mSUPPLIER1_E3_1(int)[]
         [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
          {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})))) *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([LINEITEM_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mSUPPLIER1(int)[][LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM2:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  (((((LINEITEM_ORDERKEY:int ^= delta_L1_ORDERKEY:int) *
     {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
     AggSum([], 
       EXISTS(
         AggSum([LINEITEM_ORDERKEY:int], 
           (NUMWAIT_mSUPPLIER1_E3_1(int)[]
            [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
             {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})))) *
     NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
     NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         ((__domain_1:int ^=
            (AggSum([LINEITEM_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int})) +
              ({delta_L3_SUPPKEY:int != LINEITEM_SUPPKEY:int} *
                delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
           ((__domain_1:int ^=
              AggSum([LINEITEM_ORDERKEY:int], 
                (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                 [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
                  {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))) *
             -1))))) +
    AggSum([S_NAME:string, delta_L1_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_mLINEITEM7(int)[]
        [S_NAME:string, S_SUPPKEY:int, delta_L1_ORDERKEY:int] *
        ((__domain_1:int ^=
           (AggSum([delta_L1_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int})) +
             ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
               delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
          ((__domain_1:int ^=
             AggSum([delta_L1_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
            -1)) *
        EXISTS(
          AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_E3_1(int)[]
             [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
              {L2_SUPPKEY:int != S_SUPPKEY:int}))))) +
    AggSum([S_NAME:string, delta_L1_ORDERKEY:int], 
      ((__domain_1:int ^= 0) *
        NUMWAIT_mLINEITEM7(int)[]
        [S_NAME:string, S_SUPPKEY:int, delta_L1_ORDERKEY:int] *
        ((__domain_1:int ^=
           (AggSum([delta_L1_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int})) +
             ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
               delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
          ((__domain_1:int ^=
             AggSum([delta_L1_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
            -1)) *
        EXISTS(
          (AggSum([delta_L1_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int})) +
            ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
              {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1)))))) *
   -1) +
  AggSum([S_NAME:string, delta_L1_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mLINEITEM7(int)[]
      [S_NAME:string, S_SUPPKEY:int, delta_L1_ORDERKEY:int] *
      ((__domain_1:int ^=
         (AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int})) +
           ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([delta_L1_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          -1)) *
      EXISTS(
        (AggSum([delta_L1_ORDERKEY:int], 
           (NUMWAIT_mSUPPLIER1_E3_1(int)[]
            [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
             {L2_SUPPKEY:int != S_SUPPKEY:int})) +
          ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1))))) +
  AggSum([S_NAME:string, delta_L1_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mLINEITEM7(int)[]
      [S_NAME:string, S_SUPPKEY:int, delta_L1_ORDERKEY:int] *
      ((__domain_1:int ^=
         (AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int})) +
           ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
             {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
             {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * 
           -1) +
           ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           (AggSum([delta_L1_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != S_SUPPKEY:int})) +
             ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
               {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} *
               {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * 
             -1))) *
          -1)) *
      EXISTS(
        (AggSum([delta_L1_ORDERKEY:int], 
           (NUMWAIT_mSUPPLIER1_E3_1(int)[]
            [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
             {L2_SUPPKEY:int != S_SUPPKEY:int})) +
          ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
            {LINEITEM_SUPPKEY:int != S_SUPPKEY:int} * -1))))));
}

CORRECT NUMWAIT_mLINEITEM7[][delta_S_NAME:string,delta_S_SUPPKEY:int,delta_L1_ORDERKEY:int] += delta_NUMWAIT_mLINEITEM7:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  (((EXISTS(
     (AggSum([delta_L1_ORDERKEY:int], 
        (NUMWAIT_mSUPPLIER1_E3_1(int)[]
         [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
          {L2_SUPPKEY:int != delta_S_SUPPKEY:int})) +
       ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
         {LINEITEM_SUPPKEY:int != delta_S_SUPPKEY:int} * -1))) *
    (AggSum([], 
       ((__domain_1:int ^=
          AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
         (__domain_1:int ^= 0))) +
      AggSum([], 
        ((__domain_1:int ^=
           (AggSum([delta_L1_ORDERKEY:int], 
              (NUMWAIT_mSUPPLIER1_L2_1(int)[]
               [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
                {L3_SUPPKEY:int != delta_S_SUPPKEY:int})) +
             ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
               {LINEITEM_SUPPKEY:int != delta_S_SUPPKEY:int} *
               {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * 
             -1))) *
          (__domain_1:int ^= 0))))) +
   ((AggSum([], 
       ((__domain_1:int ^=
          AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
         (__domain_1:int ^= 0) *
         EXISTS(
           AggSum([delta_L1_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != delta_S_SUPPKEY:int}))))) +
      (EXISTS(
         (AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_E3_1(int)[]
             [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
              {L2_SUPPKEY:int != delta_S_SUPPKEY:int})) +
           ({LINEITEM_ORDERKEY:int = delta_L1_ORDERKEY:int} *
             {LINEITEM_SUPPKEY:int != delta_S_SUPPKEY:int} * -1))) *
        AggSum([], 
          ((__domain_1:int ^=
             AggSum([delta_L1_ORDERKEY:int], 
               (NUMWAIT_mSUPPLIER1_L2_1(int)[]
                [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
                 {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
            (__domain_1:int ^= 0))))) *
     -1)) *
  delta_NUMWAIT_mLINEITEM7:int);
}

CORRECT NUMWAIT_mLINEITEM1[][delta_S_NAME:string,delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int] += delta_NUMWAIT_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  AggSum([], 
    EXISTS(
      AggSum([LINEITEM_ORDERKEY:int], 
        (NUMWAIT_mSUPPLIER1_E3_1(int)[]
         [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
          {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})))) *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([LINEITEM_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_pLINEITEM6(int)[][delta_S_NAME:string, LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM1:int);
}

CORRECT NUMWAIT_mLINEITEM2[][delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_NUMWAIT_mLINEITEM2:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  AggSum([], 
    EXISTS(
      AggSum([LINEITEM_ORDERKEY:int], 
        (NUMWAIT_mSUPPLIER1_E3_1(int)[]
         [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
          {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})))) *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([LINEITEM_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mSUPPLIER1(int)[][LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM2:int);
}

CORRECT NUMWAIT_mLINEITEM1[][delta_S_NAME:string,delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int] += delta_NUMWAIT_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  AggSum([], 
    EXISTS(
      AggSum([LINEITEM_ORDERKEY:int], 
        (NUMWAIT_mSUPPLIER1_E3_1(int)[]
         [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
          {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})))) *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([LINEITEM_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_pLINEITEM6(int)[][delta_S_NAME:string, LINEITEM_ORDERKEY:int, LINEITEM_SUPPKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM1:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_SUPPKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_SUPPKEY:int} *
  NUMWAIT_mLINEITEM2(int)[][LINEITEM_ORDERKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM1:int);
}

CORRECT NUMWAIT_mLINEITEM2[][delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int] += delta_NUMWAIT_mLINEITEM2:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} *
  AggSum([], 
    EXISTS(
      AggSum([LINEITEM_ORDERKEY:int], 
        (NUMWAIT_mSUPPLIER1_E3_1(int)[]
         [LINEITEM_ORDERKEY:int, L2_SUPPKEY:int] *
          {L2_SUPPKEY:int != LINEITEM_SUPPKEY:int})))) *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([LINEITEM_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [LINEITEM_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != LINEITEM_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mLINEITEM7(int)[][S_NAME:string, LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  NUMWAIT_mLINEITEM1(int)[][S_NAME:string, LINEITEM_SUPPKEY:int] *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM2:int);
   NUMWAIT_mSUPPLIER1(int)[][LINEITEM_SUPPKEY:int, LINEITEM_ORDERKEY:int] += 
  ({LINEITEM_ORDERKEY:int = delta_NUMWAIT_mLINEITEMLINEITEM_ORDERKEY:int} *
  {LINEITEM_RECEIPTDATE:date > LINEITEM_COMMITDATE:date} * -1 *
  delta_NUMWAIT_mLINEITEM2:int);
}

CORRECT NUMWAIT_pLINEITEM6[][delta_S_NAME:string,delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int,delta_S_SUPPKEY:int] += delta_NUMWAIT_pLINEITEM6:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != delta_S_SUPPKEY:int}))))) *
  delta_NUMWAIT_pLINEITEM6:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, delta_S_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERKEY:int = delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} * delta_NUMWAIT_pLINEITEM6:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([S_NAME:string, ORDERS_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_pLINEITEM6(int)[]
      [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int] *
      ((__domain_1:int ^=
         (AggSum([ORDERS_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int})) +
           ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([ORDERS_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          -1)) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != S_SUPPKEY:int}))))));
   NUMWAIT_mSUPPLIER1(int)[][delta_L3_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} * delta_NUMWAIT_mSUPPLIER1_L2_1:int);
}

CORRECT NUMWAIT_mSUPPLIER1_E3_1[][delta_L1_ORDERKEY:int,delta_L2_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_E3_1:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([S_NAME:string, ORDERS_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_pLINEITEM6(int)[]
      [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int] *
      (__domain_1:int ^=
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
      (EXISTS(
         (AggSum([ORDERS_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_E3_1(int)[]
             [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
              {L2_SUPPKEY:int != S_SUPPKEY:int})) +
           ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
        (EXISTS(
           AggSum([ORDERS_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int}))) *
          -1)))));
}

CORRECT NUMWAIT_pLINEITEM6[][delta_S_NAME:string,delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int,delta_S_SUPPKEY:int] += delta_NUMWAIT_pLINEITEM6:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != delta_S_SUPPKEY:int}))))) *
  delta_NUMWAIT_pLINEITEM6:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, delta_S_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERKEY:int = delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} * delta_NUMWAIT_pLINEITEM6:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([S_NAME:string, ORDERS_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_pLINEITEM6(int)[]
      [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int] *
      ((__domain_1:int ^=
         (AggSum([ORDERS_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int})) +
           ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([ORDERS_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          -1)) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != S_SUPPKEY:int}))))));
   NUMWAIT_mSUPPLIER1(int)[][delta_L3_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} * delta_NUMWAIT_mSUPPLIER1_L2_1:int);
}

CORRECT NUMWAIT_pLINEITEM6[][delta_S_NAME:string,delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int,delta_S_SUPPKEY:int] += delta_NUMWAIT_pLINEITEM6:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != delta_S_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_pLINEITEM6:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, delta_S_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERKEY:int = delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} * -1 * delta_NUMWAIT_pLINEITEM6:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([S_NAME:string, ORDERS_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_pLINEITEM6(int)[]
      [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int] *
      ((__domain_1:int ^=
         (AggSum([ORDERS_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int})) +
           ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([ORDERS_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          -1)) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != S_SUPPKEY:int}))))) *
  -1);
   NUMWAIT_mSUPPLIER1(int)[][delta_L3_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} * -1 * delta_NUMWAIT_mSUPPLIER1_L2_1:int);
}

CORRECT NUMWAIT_mSUPPLIER1_E3_1[][delta_L1_ORDERKEY:int,delta_L2_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_E3_1:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([S_NAME:string, ORDERS_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_pLINEITEM6(int)[]
      [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int] *
      (__domain_1:int ^=
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
      (EXISTS(
         (AggSum([ORDERS_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_E3_1(int)[]
             [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
              {L2_SUPPKEY:int != S_SUPPKEY:int})) +
           ({delta_L2_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
        (EXISTS(
           AggSum([ORDERS_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_E3_1(int)[]
              [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
               {L2_SUPPKEY:int != S_SUPPKEY:int}))) *
          -1)))) *
  -1);
}

CORRECT NUMWAIT_pLINEITEM6[][delta_S_NAME:string,delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int,delta_S_SUPPKEY:int] += delta_NUMWAIT_pLINEITEM6:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][delta_S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != delta_S_SUPPKEY:int}))) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != delta_S_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_pLINEITEM6:int);
   NUMWAIT_mLINEITEM7(int)[][delta_S_NAME:string, delta_S_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERKEY:int = delta_NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} * -1 * delta_NUMWAIT_pLINEITEM6:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   NUMWAIT(int)[][S_NAME:string] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} *
  AggSum([S_NAME:string, ORDERS_ORDERKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_pLINEITEM6(int)[]
      [S_NAME:string, ORDERS_ORDERKEY:int, S_SUPPKEY:int] *
      ((__domain_1:int ^=
         (AggSum([ORDERS_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != S_SUPPKEY:int})) +
           ({delta_L3_SUPPKEY:int != S_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([ORDERS_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [ORDERS_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != S_SUPPKEY:int}))) *
          -1)) *
      EXISTS(
        AggSum([ORDERS_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [ORDERS_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != S_SUPPKEY:int}))))) *
  -1);
   NUMWAIT_mSUPPLIER1(int)[][delta_L3_SUPPKEY:int, ORDERS_ORDERKEY:int] += 
  ({ORDERS_ORDERKEY:int = delta_L1_ORDERKEY:int} *
  {ORDERS_ORDERSTATUS:string = 'F'} * -1 * delta_NUMWAIT_mSUPPLIER1_L2_1:int);
}

CORRECT NUMWAIT_mSUPPLIER1[][delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_L1_ORDERKEY:int] += delta_NUMWAIT_mSUPPLIER1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_NUMWAIT_mSUPPLIER1:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_NUMWAIT_mSUPPLIER1:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  (NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int] *
  EXISTS(
    AggSum([delta_L1_ORDERKEY:int], 
      (NUMWAIT_mSUPPLIER1_E3_1(int)[][delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
        {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      ((__domain_1:int ^=
         (AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int})) +
           ({delta_L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([delta_L1_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
          -1)))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int]);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, delta_L1_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_L3_SUPPKEY:int} *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_NUMWAIT_mSUPPLIER1_L2_1:int);
}

CORRECT NUMWAIT_mSUPPLIER1_E3_1[][delta_L1_ORDERKEY:int,delta_L2_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_E3_1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ((EXISTS(
    (AggSum([delta_L1_ORDERKEY:int], 
       (NUMWAIT_mSUPPLIER1_E3_1(int)[][delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
         {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int})) +
      ({delta_L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int} *
        delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
   (EXISTS(
      AggSum([delta_L1_ORDERKEY:int], 
        (NUMWAIT_mSUPPLIER1_E3_1(int)[]
         [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
          {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
     -1)) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int])) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int]);
}

CORRECT NUMWAIT_mSUPPLIER2[][delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_NUMWAIT_mSUPPLIER2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
  delta_NUMWAIT_mSUPPLIER2:int);
}

CORRECT NUMWAIT_mSUPPLIER2[][delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_NUMWAIT_mSUPPLIER2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
  delta_NUMWAIT_mSUPPLIER2:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  (NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int] *
  EXISTS(
    AggSum([delta_L1_ORDERKEY:int], 
      (NUMWAIT_mSUPPLIER1_E3_1(int)[][delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
        {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      ((__domain_1:int ^=
         (AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int})) +
           ({delta_L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([delta_L1_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
          -1)))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int]);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, delta_L1_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_L3_SUPPKEY:int} *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_NUMWAIT_mSUPPLIER1_L2_1:int);
}

CORRECT NUMWAIT_mSUPPLIER2[][delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_NUMWAIT_mSUPPLIER2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
  delta_NUMWAIT_mSUPPLIER2:int);
}

CORRECT NUMWAIT_mSUPPLIER1[][delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_L1_ORDERKEY:int] += delta_NUMWAIT_mSUPPLIER1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_NUMWAIT_mSUPPLIER1:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_NUMWAIT_mSUPPLIER1:int);
}

CORRECT NUMWAIT_mSUPPLIER2[][delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_NUMWAIT_mSUPPLIER2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
  delta_NUMWAIT_mSUPPLIER2:int);
}

CORRECT NUMWAIT_mSUPPLIER1[][delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_L1_ORDERKEY:int] += delta_NUMWAIT_mSUPPLIER1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER1:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER1:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  (NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int] *
  EXISTS(
    AggSum([delta_L1_ORDERKEY:int], 
      (NUMWAIT_mSUPPLIER1_E3_1(int)[][delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
        {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      ((__domain_1:int ^=
         (AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int})) +
           ({delta_L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([delta_L1_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
          -1)))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, delta_L1_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_L3_SUPPKEY:int} *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER1_L2_1:int);
}

CORRECT NUMWAIT_mSUPPLIER1_E3_1[][delta_L1_ORDERKEY:int,delta_L2_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_E3_1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ((EXISTS(
    (AggSum([delta_L1_ORDERKEY:int], 
       (NUMWAIT_mSUPPLIER1_E3_1(int)[][delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
         {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int})) +
      ({delta_L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int} *
        delta_NUMWAIT_mSUPPLIER1_E3_1:int))) +
   (EXISTS(
      AggSum([delta_L1_ORDERKEY:int], 
        (NUMWAIT_mSUPPLIER1_E3_1(int)[]
         [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
          {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
     -1)) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int])) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1);
}

CORRECT NUMWAIT_mSUPPLIER2[][delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_NUMWAIT_mSUPPLIER2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] * 
-1 * delta_NUMWAIT_mSUPPLIER2:int);
}

CORRECT NUMWAIT_mSUPPLIER2[][delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_NUMWAIT_mSUPPLIER2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] * 
-1 * delta_NUMWAIT_mSUPPLIER2:int);
}

CORRECT NUMWAIT_mSUPPLIER1_L2_1[][delta_L1_ORDERKEY:int,delta_L3_SUPPKEY:int] += delta_NUMWAIT_mSUPPLIER1_L2_1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  (NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int] *
  EXISTS(
    AggSum([delta_L1_ORDERKEY:int], 
      (NUMWAIT_mSUPPLIER1_E3_1(int)[][delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
        {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      ((__domain_1:int ^=
         (AggSum([delta_L1_ORDERKEY:int], 
            (NUMWAIT_mSUPPLIER1_L2_1(int)[]
             [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
              {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int})) +
           ({delta_L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int} *
             delta_NUMWAIT_mSUPPLIER1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([delta_L1_ORDERKEY:int], 
             (NUMWAIT_mSUPPLIER1_L2_1(int)[]
              [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
               {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
          -1)))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, delta_L1_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_L3_SUPPKEY:int} *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER1_L2_1:int);
}

CORRECT NUMWAIT_mSUPPLIER2[][delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_NUMWAIT_mSUPPLIER2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] * 
-1 * delta_NUMWAIT_mSUPPLIER2:int);
}

CORRECT NUMWAIT_mSUPPLIER2[][delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_NUMWAIT_mSUPPLIER2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] *
      (__domain_1:int ^=
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[][L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[][L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  -1 * delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_pLINEITEM6(int)[][SUPPLIER_NAME:string, NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1_L2_1(int)[]
  [NUMWAIT_pLINEITEMLINEITEM_ORDERKEY:int, SUPPLIER_SUPPKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM1(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_NUMWAIT_mSUPPLIER2:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  NUMWAIT_mSUPPLIER1(int)[][SUPPLIER_SUPPKEY:int, L1_ORDERKEY:int] * 
-1 * delta_NUMWAIT_mSUPPLIER2:int);
}

CORRECT NUMWAIT_mSUPPLIER1[][delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_L1_ORDERKEY:int] += delta_NUMWAIT_mSUPPLIER1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   NUMWAIT(int)[][SUPPLIER_NAME:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_L2_1(int)[]
           [delta_L1_ORDERKEY:int, L3_SUPPKEY:int] *
            {L3_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))) *
      EXISTS(
        AggSum([delta_L1_ORDERKEY:int], 
          (NUMWAIT_mSUPPLIER1_E3_1(int)[]
           [delta_L1_ORDERKEY:int, L2_SUPPKEY:int] *
            {L2_SUPPKEY:int != SUPPLIER_SUPPKEY:int}))))) *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER1:int);
   NUMWAIT_mLINEITEM7(int)[][SUPPLIER_NAME:string, SUPPLIER_SUPPKEY:int, delta_L1_ORDERKEY:int] += 
  ({SUPPLIER_SUPPKEY:int = delta_NUMWAIT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  NUMWAIT_mSUPPLIER2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_NUMWAIT_mSUPPLIER1:int);
}
