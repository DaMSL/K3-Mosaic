-------------------- SOURCES --------------------
CREATE STREAM ORDERS(ORDERS_ORDERKEY int, ORDERS_CUSTKEY int, ORDERS_ORDERSTATUS string, ORDERS_TOTALPRICE float, ORDERS_ORDERDATE date, ORDERS_ORDERPRIORITY string, ORDERS_CLERK string, ORDERS_SHIPPRIORITY int, ORDERS_COMMENT string)
  FROM FILE 'data/tpch/orders.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM CUSTOMER(CUSTOMER_CUSTKEY int, CUSTOMER_NAME string, CUSTOMER_ADDRESS string, CUSTOMER_NATIONKEY int, CUSTOMER_PHONE string, CUSTOMER_ACCTBAL float, CUSTOMER_MKTSEGMENT string, CUSTOMER_COMMENT string)
  FROM FILE 'data/tpch/customer.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP CUSTDIST(int)[][C_ORDERS_C_COUNT:int] := 
AggSum([C_ORDERS_C_COUNT:int], 
  (EXISTS(
     AggSum([C_ORDERS_C_CUSTKEY:int], 
       (CUSTOMER(C_CUSTKEY:int, C_NAME:string, C_ADDRESS:string,
                   C_NATIONKEY:int, C_PHONE:string, C_ACCTBAL:float,
                   C_MKTSEGMENT:string, C_COMMENT:string) *
         (C_ORDERS_C_CUSTKEY:int ^= C_CUSTKEY:int) *
         ORDERS(O_ORDERKEY:int, C_CUSTKEY:int, O_ORDERSTATUS:string,
                  O_TOTALPRICE:float, O_ORDERDATE:date,
                  O_ORDERPRIORITY:string, O_CLERK:string, O_SHIPPRIORITY:int,
                  O_COMMENT:string) *
         {0 =
         [regexp_match:int]('^.*special.*requests.*$', O_COMMENT:string)}))) *
    (C_ORDERS_C_COUNT:int ^=
      AggSum([C_ORDERS_C_CUSTKEY:int], 
        (CUSTOMER(C_CUSTKEY:int, C_NAME:string, C_ADDRESS:string,
                    C_NATIONKEY:int, C_PHONE:string, C_ACCTBAL:float,
                    C_MKTSEGMENT:string, C_COMMENT:string) *
          (C_ORDERS_C_CUSTKEY:int ^= C_CUSTKEY:int) *
          ORDERS(O_ORDERKEY:int, C_CUSTKEY:int, O_ORDERSTATUS:string,
                   O_TOTALPRICE:float, O_ORDERDATE:date,
                   O_ORDERPRIORITY:string, O_CLERK:string,
                   O_SHIPPRIORITY:int, O_COMMENT:string) *
          {0 =
          [regexp_match:int]('^.*special.*requests.*$', O_COMMENT:string)})))));

DECLARE MAP CUSTDIST_mORDERS1_E1_4(int)[][CUSTDIST_mORDERSORDERS_CUSTKEY:int] := 
AggSum([CUSTDIST_mORDERSORDERS_CUSTKEY:int], 
  CUSTOMER(CUSTDIST_mORDERSORDERS_CUSTKEY:int, C_NAME:string,
             C_ADDRESS:string, C_NATIONKEY:int, C_PHONE:string,
             C_ACCTBAL:float, C_MKTSEGMENT:string, C_COMMENT:string));

DECLARE MAP CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] := 
AggSum([C_ORDERS_C_CUSTKEY:int], 
  (CUSTOMER(C_CUSTKEY:int, C_NAME:string, C_ADDRESS:string, C_NATIONKEY:int,
              C_PHONE:string, C_ACCTBAL:float, C_MKTSEGMENT:string,
              C_COMMENT:string) *
    (C_ORDERS_C_CUSTKEY:int ^= C_CUSTKEY:int) *
    ORDERS(O_ORDERKEY:int, C_CUSTKEY:int, O_ORDERSTATUS:string,
             O_TOTALPRICE:float, O_ORDERDATE:date, O_ORDERPRIORITY:string,
             O_CLERK:string, O_SHIPPRIORITY:int, O_COMMENT:string) *
    {0 = [regexp_match:int]('^.*special.*requests.*$', O_COMMENT:string)}));

DECLARE MAP CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int] := 
AggSum([CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int], 
  (ORDERS(O_ORDERKEY:int, CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int,
            O_ORDERSTATUS:string, O_TOTALPRICE:float, O_ORDERDATE:date,
            O_ORDERPRIORITY:string, O_CLERK:string, O_SHIPPRIORITY:int,
            O_COMMENT:string) *
    {0 = [regexp_match:int]('^.*special.*requests.*$', O_COMMENT:string)}));

-------------------- QUERIES --------------------
DECLARE QUERY CUSTDIST := CUSTDIST(int)[][C_ORDERS_C_COUNT:int];

------------------- TRIGGERS --------------------
ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  (((AggSum([C_ORDERS_C_COUNT:int], 
     (EXISTS( CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) *
       (C_ORDERS_C_COUNT:int ^=
         CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))) +
    AggSum([C_ORDERS_C_COUNT:int], 
      (EXISTS(
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
           ((C_ORDERS_C_CUSTKEY:int ^= ORDERS_CUSTKEY:int) *
             {0 =
             [regexp_match:int]('^.*special.*requests.*$',
                                  ORDERS_COMMENT:string)} *
             CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
        (C_ORDERS_C_COUNT:int ^=
          CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int])))) *
   -1) +
  AggSum([C_ORDERS_C_COUNT:int], 
    (EXISTS(
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
         ((C_ORDERS_C_CUSTKEY:int ^= ORDERS_CUSTKEY:int) *
           {0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
           CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
      (C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))) +
  AggSum([C_ORDERS_C_COUNT:int], 
    (EXISTS(
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
         ((C_ORDERS_C_CUSTKEY:int ^= ORDERS_CUSTKEY:int) *
           {0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
           CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
      (C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
          ((C_ORDERS_C_CUSTKEY:int ^= ORDERS_CUSTKEY:int) *
            {0 =
            [regexp_match:int]('^.*special.*requests.*$',
                                 ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] += 
  ({0 = [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
  CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]);
   CUSTDIST_mCUSTOMER1_E1_3(int)[][ORDERS_CUSTKEY:int] += {0 = [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)};
}

ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  (((AggSum([C_ORDERS_C_COUNT:int], 
     (EXISTS( CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) *
       (C_ORDERS_C_COUNT:int ^=
         CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))) +
    AggSum([C_ORDERS_C_COUNT:int], 
      (EXISTS(
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
           ((C_ORDERS_C_CUSTKEY:int ^= ORDERS_CUSTKEY:int) *
             {0 =
             [regexp_match:int]('^.*special.*requests.*$',
                                  ORDERS_COMMENT:string)} *
             CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
        (C_ORDERS_C_COUNT:int ^=
          CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int])))) *
   -1) +
  AggSum([C_ORDERS_C_COUNT:int], 
    (EXISTS(
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
         ((C_ORDERS_C_CUSTKEY:int ^= ORDERS_CUSTKEY:int) *
           {0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
           CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
      (C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))) +
  AggSum([C_ORDERS_C_COUNT:int], 
    (EXISTS(
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
         ((C_ORDERS_C_CUSTKEY:int ^= ORDERS_CUSTKEY:int) *
           {0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
           CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
      (C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
          ((C_ORDERS_C_CUSTKEY:int ^= ORDERS_CUSTKEY:int) *
            {0 =
            [regexp_match:int]('^.*special.*requests.*$',
                                 ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] += 
  ({0 = [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
  CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1);
   CUSTDIST_mCUSTOMER1_E1_3(int)[][ORDERS_CUSTKEY:int] += 
  ({0 = [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
  -1);
}

ON + CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  (((AggSum([C_ORDERS_C_COUNT:int], 
     (EXISTS( CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) *
       (C_ORDERS_C_COUNT:int ^=
         CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))) +
    AggSum([C_ORDERS_C_COUNT:int], 
      (EXISTS(
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
           ((C_ORDERS_C_CUSTKEY:int ^= CUSTOMER_CUSTKEY:int) *
             CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]))) *
        (C_ORDERS_C_COUNT:int ^=
          CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int])))) *
   -1) +
  AggSum([C_ORDERS_C_COUNT:int], 
    (EXISTS(
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
         ((C_ORDERS_C_CUSTKEY:int ^= CUSTOMER_CUSTKEY:int) *
           CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]))) *
      (C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))) +
  AggSum([C_ORDERS_C_COUNT:int], 
    (EXISTS(
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
         ((C_ORDERS_C_CUSTKEY:int ^= CUSTOMER_CUSTKEY:int) *
           CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]))) *
      (C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
          ((C_ORDERS_C_CUSTKEY:int ^= CUSTOMER_CUSTKEY:int) *
            CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]))))));
   CUSTDIST_mORDERS1_E1_4(int)[][CUSTOMER_CUSTKEY:int] += 1;
   CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] += CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int];
}

ON - CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  (((AggSum([C_ORDERS_C_COUNT:int], 
     (EXISTS( CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) *
       (C_ORDERS_C_COUNT:int ^=
         CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))) +
    AggSum([C_ORDERS_C_COUNT:int], 
      (EXISTS(
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
           ((C_ORDERS_C_CUSTKEY:int ^= CUSTOMER_CUSTKEY:int) *
             CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * 
           -1))) *
        (C_ORDERS_C_COUNT:int ^=
          CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int])))) *
   -1) +
  AggSum([C_ORDERS_C_COUNT:int], 
    (EXISTS(
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
         ((C_ORDERS_C_CUSTKEY:int ^= CUSTOMER_CUSTKEY:int) *
           CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
      (C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))) +
  AggSum([C_ORDERS_C_COUNT:int], 
    (EXISTS(
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
         ((C_ORDERS_C_CUSTKEY:int ^= CUSTOMER_CUSTKEY:int) *
           CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
      (C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int] +
          ((C_ORDERS_C_CUSTKEY:int ^= CUSTOMER_CUSTKEY:int) *
            CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))))));
   CUSTDIST_mORDERS1_E1_4(int)[][CUSTOMER_CUSTKEY:int] += -1;
   CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] += (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1);
}

ON SYSTEM READY {
}

CORRECT CUSTDIST_mCUSTOMER1_E1_1[][delta_C_ORDERS_C_CUSTKEY:int] += delta_CUSTDIST_mCUSTOMER1_E1_1:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ((EXISTS(
    (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
      ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
        {0 =
        [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
        CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]) +
      delta_CUSTDIST_mCUSTOMER1_E1_1:int)) *
   ((((C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
       (C_ORDERS_C_COUNT:int ^=
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
           ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
             {0 =
             [regexp_match:int]('^.*special.*requests.*$',
                                  ORDERS_COMMENT:string)} *
             CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int])))) *
      -1) +
     (C_ORDERS_C_COUNT:int ^=
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
         delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
     (C_ORDERS_C_COUNT:int ^=
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
         ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
           {0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
           CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]) +
         delta_CUSTDIST_mCUSTOMER1_E1_1:int)))) +
  ((EXISTS(
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
        ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
          {0 =
          [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
          CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]) +
        delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
     (EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
            {0 =
            [regexp_match:int]('^.*special.*requests.*$',
                                 ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
       -1)) *
    ((C_ORDERS_C_COUNT:int ^=
       CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
      (C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
            {0 =
            [regexp_match:int]('^.*special.*requests.*$',
                                 ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))))) +
  (((((C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
       ((C_ORDERS_C_COUNT:int ^=
          CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) *
         -1)) *
      (EXISTS(
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
           delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
        EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
            ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
              {0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
              CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]) +
            delta_CUSTDIST_mCUSTOMER1_E1_1:int)))) +
     ((C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) *
       (((EXISTS(
            CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
           EXISTS(
             (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
               ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
                 {0 =
                 [regexp_match:int]('^.*special.*requests.*$',
                                      ORDERS_COMMENT:string)} *
                 CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int])))) *
          -1) +
         EXISTS(
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
             delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
         EXISTS(
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
             ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
               {0 =
               [regexp_match:int]('^.*special.*requests.*$',
                                    ORDERS_COMMENT:string)} *
               CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]) +
             delta_CUSTDIST_mCUSTOMER1_E1_1:int))))) *
    -1));
}

CORRECT CUSTDIST_mORDERS1_E1_4[][delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int] += delta_CUSTDIST_mORDERS1_E1_4:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ({ORDERS_CUSTKEY:int = delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int} *
  (((C_ORDERS_C_COUNT:int ^=
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
        ({0 =
         [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
          CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
     (EXISTS(
        (({0 =
          [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
           (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
             delta_CUSTDIST_mORDERS1_E1_4:int)) +
          CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
       (EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
            ({0 =
             [regexp_match:int]('^.*special.*requests.*$',
                                  ORDERS_COMMENT:string)} *
              CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
         -1))) +
    ((EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
          ({0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) +
       ((ORDERS_CUSTKEY:int ^= delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int) *
         (EXISTS(
            (({0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
               (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
                 delta_CUSTDIST_mORDERS1_E1_4:int)) +
              CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
           (EXISTS(
              (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
                ({0 =
                 [regexp_match:int]('^.*special.*requests.*$',
                                      ORDERS_COMMENT:string)} *
                  CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
             -1)))) *
      ((C_ORDERS_C_COUNT:int ^=
         (({0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
            (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
              delta_CUSTDIST_mORDERS1_E1_4:int)) +
           CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
        ((C_ORDERS_C_COUNT:int ^=
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
             ({0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
               CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
          -1)))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] += 
  ({ORDERS_CUSTKEY:int = delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int} *
  {0 = [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
  delta_CUSTDIST_mORDERS1_E1_4:int);
}

CORRECT CUSTDIST_mORDERS1_E1_4[][delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int] += delta_CUSTDIST_mORDERS1_E1_4:int FOR ON + ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ({ORDERS_CUSTKEY:int = delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int} *
  (((C_ORDERS_C_COUNT:int ^=
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
        ({0 =
         [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
          CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
     (EXISTS(
        (({0 =
          [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
           (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
             delta_CUSTDIST_mORDERS1_E1_4:int)) +
          CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
       (EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
            ({0 =
             [regexp_match:int]('^.*special.*requests.*$',
                                  ORDERS_COMMENT:string)} *
              CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
         -1))) +
    ((EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
          ({0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) +
       ((ORDERS_CUSTKEY:int ^= delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int) *
         (EXISTS(
            (({0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
               (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
                 delta_CUSTDIST_mORDERS1_E1_4:int)) +
              CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
           (EXISTS(
              (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
                ({0 =
                 [regexp_match:int]('^.*special.*requests.*$',
                                      ORDERS_COMMENT:string)} *
                  CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
             -1)))) *
      ((C_ORDERS_C_COUNT:int ^=
         (({0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
            (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
              delta_CUSTDIST_mORDERS1_E1_4:int)) +
           CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
        ((C_ORDERS_C_COUNT:int ^=
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
             ({0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
               CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int]))) *
          -1)))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] += 
  ({ORDERS_CUSTKEY:int = delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int} *
  {0 = [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
  delta_CUSTDIST_mORDERS1_E1_4:int);
}

CORRECT CUSTDIST_mCUSTOMER1_E1_1[][delta_C_ORDERS_C_CUSTKEY:int] += delta_CUSTDIST_mCUSTOMER1_E1_1:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ((EXISTS(
    (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
      ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
        {0 =
        [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
        CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1) +
      delta_CUSTDIST_mCUSTOMER1_E1_1:int)) *
   ((((C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
       (C_ORDERS_C_COUNT:int ^=
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
           ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
             {0 =
             [regexp_match:int]('^.*special.*requests.*$',
                                  ORDERS_COMMENT:string)} *
             CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1)))) *
      -1) +
     (C_ORDERS_C_COUNT:int ^=
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
         delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
     (C_ORDERS_C_COUNT:int ^=
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
         ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
           {0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
           CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1) +
         delta_CUSTDIST_mCUSTOMER1_E1_1:int)))) +
  ((EXISTS(
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
        ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
          {0 =
          [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
          CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1) +
        delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
     (EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
            {0 =
            [regexp_match:int]('^.*special.*requests.*$',
                                 ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
       -1)) *
    ((C_ORDERS_C_COUNT:int ^=
       CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
      (C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
            {0 =
            [regexp_match:int]('^.*special.*requests.*$',
                                 ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))))) +
  (((((C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
       ((C_ORDERS_C_COUNT:int ^=
          CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) *
         -1)) *
      (EXISTS(
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
           delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
        EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
            ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
              {0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
              CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1) +
            delta_CUSTDIST_mCUSTOMER1_E1_1:int)))) +
     ((C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) *
       (((EXISTS(
            CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
           EXISTS(
             (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
               ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
                 {0 =
                 [regexp_match:int]('^.*special.*requests.*$',
                                      ORDERS_COMMENT:string)} *
                 CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * 
               -1)))) *
          -1) +
         EXISTS(
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
             delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
         EXISTS(
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
             ({ORDERS_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
               {0 =
               [regexp_match:int]('^.*special.*requests.*$',
                                    ORDERS_COMMENT:string)} *
               CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1) +
             delta_CUSTDIST_mCUSTOMER1_E1_1:int))))) *
    -1));
}

CORRECT CUSTDIST_mORDERS1_E1_4[][delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int] += delta_CUSTDIST_mORDERS1_E1_4:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ({ORDERS_CUSTKEY:int = delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int} *
  (((C_ORDERS_C_COUNT:int ^=
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
        ({0 =
         [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
          CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
     (EXISTS(
        (({0 =
          [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
           (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
             delta_CUSTDIST_mORDERS1_E1_4:int) *
           -1) +
          CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
       (EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
            ({0 =
             [regexp_match:int]('^.*special.*requests.*$',
                                  ORDERS_COMMENT:string)} *
              CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
         -1))) +
    ((EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
          ({0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) +
       ((ORDERS_CUSTKEY:int ^= delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int) *
         (EXISTS(
            (({0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
               (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
                 delta_CUSTDIST_mORDERS1_E1_4:int) *
               -1) +
              CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
           (EXISTS(
              (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
                ({0 =
                 [regexp_match:int]('^.*special.*requests.*$',
                                      ORDERS_COMMENT:string)} *
                  CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * 
                -1))) *
             -1)))) *
      ((C_ORDERS_C_COUNT:int ^=
         (({0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
            (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
              delta_CUSTDIST_mORDERS1_E1_4:int) *
            -1) +
           CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
        ((C_ORDERS_C_COUNT:int ^=
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
             ({0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
               CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
          -1)))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] += 
  ({ORDERS_CUSTKEY:int = delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int} *
  {0 = [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
  -1 * delta_CUSTDIST_mORDERS1_E1_4:int);
}

CORRECT CUSTDIST_mORDERS1_E1_4[][delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int] += delta_CUSTDIST_mORDERS1_E1_4:int FOR ON - ORDERS(ORDERS_ORDERKEY:int, ORDERS_CUSTKEY:int, ORDERS_ORDERSTATUS:string, ORDERS_TOTALPRICE:float, ORDERS_ORDERDATE:date, ORDERS_ORDERPRIORITY:string, ORDERS_CLERK:string, ORDERS_SHIPPRIORITY:int, ORDERS_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ({ORDERS_CUSTKEY:int = delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int} *
  (((C_ORDERS_C_COUNT:int ^=
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
        ({0 =
         [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
          CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
     (EXISTS(
        (({0 =
          [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
           (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
             delta_CUSTDIST_mORDERS1_E1_4:int) *
           -1) +
          CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
       (EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
            ({0 =
             [regexp_match:int]('^.*special.*requests.*$',
                                  ORDERS_COMMENT:string)} *
              CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
         -1))) +
    ((EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
          ({0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
            CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) +
       ((ORDERS_CUSTKEY:int ^= delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int) *
         (EXISTS(
            (({0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
               (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
                 delta_CUSTDIST_mORDERS1_E1_4:int) *
               -1) +
              CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
           (EXISTS(
              (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
                ({0 =
                 [regexp_match:int]('^.*special.*requests.*$',
                                      ORDERS_COMMENT:string)} *
                  CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * 
                -1))) *
             -1)))) *
      ((C_ORDERS_C_COUNT:int ^=
         (({0 =
           [regexp_match:int]('^.*special.*requests.*$',
                                ORDERS_COMMENT:string)} *
            (CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] +
              delta_CUSTDIST_mORDERS1_E1_4:int) *
            -1) +
           CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int])) +
        ((C_ORDERS_C_COUNT:int ^=
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] +
             ({0 =
              [regexp_match:int]('^.*special.*requests.*$',
                                   ORDERS_COMMENT:string)} *
               CUSTDIST_mORDERS1_E1_4(int)[][ORDERS_CUSTKEY:int] * -1))) *
          -1)))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][ORDERS_CUSTKEY:int] += 
  ({ORDERS_CUSTKEY:int = delta_CUSTDIST_mORDERSORDERS_CUSTKEY:int} *
  {0 = [regexp_match:int]('^.*special.*requests.*$', ORDERS_COMMENT:string)} *
  -1 * delta_CUSTDIST_mORDERS1_E1_4:int);
}

CORRECT CUSTDIST_mCUSTOMER1_E1_1[][delta_C_ORDERS_C_CUSTKEY:int] += delta_CUSTDIST_mCUSTOMER1_E1_1:int FOR ON + CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ((EXISTS(
    (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
      ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
        CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]) +
      delta_CUSTDIST_mCUSTOMER1_E1_1:int)) *
   ((((C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
       (C_ORDERS_C_COUNT:int ^=
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
           ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
             CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])))) *
      -1) +
     (C_ORDERS_C_COUNT:int ^=
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
         delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
     (C_ORDERS_C_COUNT:int ^=
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
         ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
           CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]) +
         delta_CUSTDIST_mCUSTOMER1_E1_1:int)))) +
  ((EXISTS(
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
        ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
          CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]) +
        delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
     (EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
            CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]))) *
       -1)) *
    ((C_ORDERS_C_COUNT:int ^=
       CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
      (C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
            CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]))))) +
  (((((C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
       ((C_ORDERS_C_COUNT:int ^=
          CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) *
         -1)) *
      (EXISTS(
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
           delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
        EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
            ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
              CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]) +
            delta_CUSTDIST_mCUSTOMER1_E1_1:int)))) +
     ((C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) *
       (((EXISTS(
            CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
           EXISTS(
             (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
               ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
                 CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])))) *
          -1) +
         EXISTS(
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
             delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
         EXISTS(
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
             ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
               CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int]) +
             delta_CUSTDIST_mCUSTOMER1_E1_1:int))))) *
    -1));
}

CORRECT CUSTDIST_mCUSTOMER1_E1_3[][delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int] += delta_CUSTDIST_mCUSTOMER1_E1_3:int FOR ON + CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ({CUSTOMER_CUSTKEY:int = delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  (((C_ORDERS_C_COUNT:int ^=
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
        CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) *
     (EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
          CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
          delta_CUSTDIST_mCUSTOMER1_E1_3:int)) +
       (EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
            CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) *
         -1))) +
    ((EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
          CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) +
       ((CUSTOMER_CUSTKEY:int ^=
          delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int) *
         (EXISTS(
            (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
              CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
              delta_CUSTDIST_mCUSTOMER1_E1_3:int)) +
           (EXISTS(
              (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
                CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) *
             -1)))) *
      ((C_ORDERS_C_COUNT:int ^=
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
           CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
           delta_CUSTDIST_mCUSTOMER1_E1_3:int)) +
        ((C_ORDERS_C_COUNT:int ^=
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
             CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) *
          -1)))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] += 
  ({CUSTOMER_CUSTKEY:int = delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  delta_CUSTDIST_mCUSTOMER1_E1_3:int);
}

CORRECT CUSTDIST_mCUSTOMER1_E1_3[][delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int] += delta_CUSTDIST_mCUSTOMER1_E1_3:int FOR ON + CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ({CUSTOMER_CUSTKEY:int = delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  (((C_ORDERS_C_COUNT:int ^=
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
        CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) *
     (EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
          CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
          delta_CUSTDIST_mCUSTOMER1_E1_3:int)) +
       (EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
            CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) *
         -1))) +
    ((EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
          CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) +
       ((CUSTOMER_CUSTKEY:int ^=
          delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int) *
         (EXISTS(
            (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
              CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
              delta_CUSTDIST_mCUSTOMER1_E1_3:int)) +
           (EXISTS(
              (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
                CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) *
             -1)))) *
      ((C_ORDERS_C_COUNT:int ^=
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
           CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
           delta_CUSTDIST_mCUSTOMER1_E1_3:int)) +
        ((C_ORDERS_C_COUNT:int ^=
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
             CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int])) *
          -1)))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] += 
  ({CUSTOMER_CUSTKEY:int = delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  delta_CUSTDIST_mCUSTOMER1_E1_3:int);
}

CORRECT CUSTDIST_mCUSTOMER1_E1_1[][delta_C_ORDERS_C_CUSTKEY:int] += delta_CUSTDIST_mCUSTOMER1_E1_1:int FOR ON - CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ((EXISTS(
    (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
      ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
        CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1) +
      delta_CUSTDIST_mCUSTOMER1_E1_1:int)) *
   ((((C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
       (C_ORDERS_C_COUNT:int ^=
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
           ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
             CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * 
           -1)))) *
      -1) +
     (C_ORDERS_C_COUNT:int ^=
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
         delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
     (C_ORDERS_C_COUNT:int ^=
       (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
         ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
           CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1) +
         delta_CUSTDIST_mCUSTOMER1_E1_1:int)))) +
  ((EXISTS(
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
        ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
          CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1) +
        delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
     (EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
            CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
       -1)) *
    ((C_ORDERS_C_COUNT:int ^=
       CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
      (C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
            CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))))) +
  (((((C_ORDERS_C_COUNT:int ^=
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
          delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
       ((C_ORDERS_C_COUNT:int ^=
          CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) *
         -1)) *
      (EXISTS(
         (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
           delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
        EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
            ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
              CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * 
            -1) + delta_CUSTDIST_mCUSTOMER1_E1_1:int)))) +
     ((C_ORDERS_C_COUNT:int ^=
        CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) *
       (((EXISTS(
            CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int]) +
           EXISTS(
             (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
               ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
                 CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * 
               -1)))) *
          -1) +
         EXISTS(
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
             delta_CUSTDIST_mCUSTOMER1_E1_1:int)) +
         EXISTS(
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][delta_C_ORDERS_C_CUSTKEY:int] +
             ({CUSTOMER_CUSTKEY:int = delta_C_ORDERS_C_CUSTKEY:int} *
               CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * 
             -1) + delta_CUSTDIST_mCUSTOMER1_E1_1:int))))) *
    -1));
}

CORRECT CUSTDIST_mCUSTOMER1_E1_3[][delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int] += delta_CUSTDIST_mCUSTOMER1_E1_3:int FOR ON - CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ({CUSTOMER_CUSTKEY:int = delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  (((C_ORDERS_C_COUNT:int ^=
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
        (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
     (EXISTS(
        (((CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
            delta_CUSTDIST_mCUSTOMER1_E1_3:int) *
           -1) +
          CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int])) +
       (EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
            (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
         -1))) +
    ((EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
          (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) +
       ((CUSTOMER_CUSTKEY:int ^=
          delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int) *
         (EXISTS(
            (((CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
                delta_CUSTDIST_mCUSTOMER1_E1_3:int) *
               -1) +
              CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int])) +
           (EXISTS(
              (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
                (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
             -1)))) *
      ((C_ORDERS_C_COUNT:int ^=
         (((CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
             delta_CUSTDIST_mCUSTOMER1_E1_3:int) *
            -1) +
           CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int])) +
        ((C_ORDERS_C_COUNT:int ^=
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
             (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
          -1)))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] += 
  ({CUSTOMER_CUSTKEY:int = delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int} * 
-1 * delta_CUSTDIST_mCUSTOMER1_E1_3:int);
}

CORRECT CUSTDIST_mCUSTOMER1_E1_3[][delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int] += delta_CUSTDIST_mCUSTOMER1_E1_3:int FOR ON - CUSTOMER(CUSTOMER_CUSTKEY:int, CUSTOMER_NAME:string, CUSTOMER_ADDRESS:string, CUSTOMER_NATIONKEY:int, CUSTOMER_PHONE:string, CUSTOMER_ACCTBAL:float, CUSTOMER_MKTSEGMENT:string, CUSTOMER_COMMENT:string) {
   CUSTDIST(int)[][C_ORDERS_C_COUNT:int]:(AggSum([C_ORDERS_C_COUNT:int],((Exists(CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]) * (C_ORDERS_C_COUNT:int ^= CUSTDIST_mCUSTOMER1_E1_1(int)[][C_ORDERS_C_CUSTKEY:int]))))) += 
  ({CUSTOMER_CUSTKEY:int = delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int} *
  (((C_ORDERS_C_COUNT:int ^=
      (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
        (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
     (EXISTS(
        (((CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
            delta_CUSTDIST_mCUSTOMER1_E1_3:int) *
           -1) +
          CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int])) +
       (EXISTS(
          (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
            (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
         -1))) +
    ((EXISTS(
        (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
          (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) +
       ((CUSTOMER_CUSTKEY:int ^=
          delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int) *
         (EXISTS(
            (((CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
                delta_CUSTDIST_mCUSTOMER1_E1_3:int) *
               -1) +
              CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int])) +
           (EXISTS(
              (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
                (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
             -1)))) *
      ((C_ORDERS_C_COUNT:int ^=
         (((CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] +
             delta_CUSTDIST_mCUSTOMER1_E1_3:int) *
            -1) +
           CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int])) +
        ((C_ORDERS_C_COUNT:int ^=
           (CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] +
             (CUSTDIST_mCUSTOMER1_E1_3(int)[][CUSTOMER_CUSTKEY:int] * -1))) *
          -1)))));
   CUSTDIST_mCUSTOMER1_E1_1(int)[][CUSTOMER_CUSTKEY:int] += 
  ({CUSTOMER_CUSTKEY:int = delta_CUSTDIST_mCUSTOMERCUSTOMER_CUSTKEY:int} * 
-1 * delta_CUSTDIST_mCUSTOMER1_E1_3:int);
}
