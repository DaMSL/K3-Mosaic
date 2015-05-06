-------------------- SOURCES --------------------
CREATE STREAM PART(PART_PARTKEY int, PART_NAME string, PART_MFGR string, PART_BRAND string, PART_TYPE string, PART_SIZE int, PART_CONTAINER string, PART_RETAILPRICE float, PART_COMMENT string)
  FROM FILE 'data/tpch/part.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM SUPPLIER(SUPPLIER_SUPPKEY int, SUPPLIER_NAME string, SUPPLIER_ADDRESS string, SUPPLIER_NATIONKEY int, SUPPLIER_PHONE string, SUPPLIER_ACCTBAL float, SUPPLIER_COMMENT string)
  FROM FILE 'data/tpch/supplier.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM PARTSUPP(PARTSUPP_PARTKEY int, PARTSUPP_SUPPKEY int, PARTSUPP_AVAILQTY int, PARTSUPP_SUPPLYCOST float, PARTSUPP_COMMENT string)
  FROM FILE 'data/tpch/partsupp.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int], 
  EXISTS(
    AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int], 
      (PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                  PS_SUPPLYCOST:float, PS_COMMENT:string) *
        PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
               P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
               P_RETAILPRICE:float, P_COMMENT:string) *
        {P_BRAND:string != 'Brand#45'} *
        {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)} *
        ({P_SIZE:int = 49} + {P_SIZE:int = 14} + {P_SIZE:int = 23} +
          {P_SIZE:int = 45} + {P_SIZE:int = 19} + {P_SIZE:int = 3} +
          {P_SIZE:int = 36} + {P_SIZE:int = 9}) *
        AggSum([], 
          ((__domain_1:int ^=
             AggSum([PS_SUPPKEY:int], 
               (SUPPLIER(PS_SUPPKEY:int, S_NAME:string, S_ADDRESS:string,
                           S_NATIONKEY:int, S_PHONE:string, S_ACCTBAL:float,
                           S_COMMENT:string) *
                 {0 !=
                 [regexp_match:int]('^.*Customer.*Complaints.*$',
                                      S_COMMENT:string)}))) *
            (__domain_1:int ^= 0)))))));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
[P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] := 
((P_SIZE:int ^= 49) *
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int], 
    (PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                PS_SUPPLYCOST:float, PS_COMMENT:string) *
      PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
             P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
             P_RETAILPRICE:float, P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
[PS_SUPPKEY:int, SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] := 
AggSum([PS_SUPPKEY:int, SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int], 
  PARTSUPP(SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int,
             PS_SUPPKEY:int, PS_AVAILQTY:int, PS_SUPPLYCOST:float,
             PS_COMMENT:string));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string,
   SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPPPARTSUPP_PARTKEY:int] := 
((P_SIZE:int ^= 49) *
  AggSum([P_SIZE:int, P_BRAND:string, P_TYPE:string,
            SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPPPARTSUPP_PARTKEY:int], 
    (PART(SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPPPARTSUPP_PARTKEY:int,
            P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
            P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
            P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][PS_SUPPKEY:int] := 
AggSum([PS_SUPPKEY:int], 
  (SUPPLIER(PS_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
              S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string) *
    {0 != [regexp_match:int]('^.*Customer.*Complaints.*$', S_COMMENT:string)}));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
[P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] := 
((P_SIZE:int ^= 14) *
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int], 
    (PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                PS_SUPPLYCOST:float, PS_COMMENT:string) *
      PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
             P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
             P_RETAILPRICE:float, P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string,
   SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPPPARTSUPP_PARTKEY:int] := 
((P_SIZE:int ^= 14) *
  AggSum([P_SIZE:int, P_BRAND:string, P_TYPE:string,
            SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPPPARTSUPP_PARTKEY:int], 
    (PART(SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPPPARTSUPP_PARTKEY:int,
            P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
            P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
            P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
[P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] := 
((P_SIZE:int ^= 23) *
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int], 
    (PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                PS_SUPPLYCOST:float, PS_COMMENT:string) *
      PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
             P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
             P_RETAILPRICE:float, P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string,
   SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPPPARTSUPP_PARTKEY:int] := 
((P_SIZE:int ^= 23) *
  AggSum([P_SIZE:int, P_BRAND:string, P_TYPE:string,
            SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPPPARTSUPP_PARTKEY:int], 
    (PART(SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPPPARTSUPP_PARTKEY:int,
            P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
            P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
            P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
[P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] := 
((P_SIZE:int ^= 45) *
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int], 
    (PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                PS_SUPPLYCOST:float, PS_COMMENT:string) *
      PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
             P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
             P_RETAILPRICE:float, P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string,
   SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPPPARTSUPP_PARTKEY:int] := 
((P_SIZE:int ^= 45) *
  AggSum([P_SIZE:int, P_BRAND:string, P_TYPE:string,
            SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPPPARTSUPP_PARTKEY:int], 
    (PART(SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPPPARTSUPP_PARTKEY:int,
            P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
            P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
            P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
[P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] := 
((P_SIZE:int ^= 19) *
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int], 
    (PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                PS_SUPPLYCOST:float, PS_COMMENT:string) *
      PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
             P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
             P_RETAILPRICE:float, P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string,
   SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPPPARTSUPP_PARTKEY:int] := 
((P_SIZE:int ^= 19) *
  AggSum([P_SIZE:int, P_BRAND:string, P_TYPE:string,
            SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPPPARTSUPP_PARTKEY:int], 
    (PART(SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPPPARTSUPP_PARTKEY:int,
            P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
            P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
            P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
[P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] := 
((P_SIZE:int ^= 3) *
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int], 
    (PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                PS_SUPPLYCOST:float, PS_COMMENT:string) *
      PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
             P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
             P_RETAILPRICE:float, P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string,
   SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPPPARTSUPP_PARTKEY:int] := 
((P_SIZE:int ^= 3) *
  AggSum([P_SIZE:int, P_BRAND:string, P_TYPE:string,
            SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPPPARTSUPP_PARTKEY:int], 
    (PART(SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPPPARTSUPP_PARTKEY:int,
            P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
            P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
            P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
[P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] := 
((P_SIZE:int ^= 36) *
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int], 
    (PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                PS_SUPPLYCOST:float, PS_COMMENT:string) *
      PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
             P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
             P_RETAILPRICE:float, P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string,
   SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPPPARTSUPP_PARTKEY:int] := 
((P_SIZE:int ^= 36) *
  AggSum([P_SIZE:int, P_BRAND:string, P_TYPE:string,
            SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPPPARTSUPP_PARTKEY:int], 
    (PART(SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPPPARTSUPP_PARTKEY:int,
            P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
            P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
            P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
[P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] := 
((P_SIZE:int ^= 9) *
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int], 
    (PARTSUPP(PS_PARTKEY:int, PS_SUPPKEY:int, PS_AVAILQTY:int,
                PS_SUPPLYCOST:float, PS_COMMENT:string) *
      PART(PS_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
             P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
             P_RETAILPRICE:float, P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

DECLARE MAP SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string,
   SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPPPARTSUPP_PARTKEY:int] := 
((P_SIZE:int ^= 9) *
  AggSum([P_SIZE:int, P_BRAND:string, P_TYPE:string,
            SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPPPARTSUPP_PARTKEY:int], 
    (PART(SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPPPARTSUPP_PARTKEY:int,
            P_NAME:string, P_MFGR:string, P_BRAND:string, P_TYPE:string,
            P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
            P_COMMENT:string) *
      {P_BRAND:string != 'Brand#45'} *
      {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', P_TYPE:string)})));

-------------------- QUERIES --------------------
DECLARE QUERY SUPPLIER_CNT := SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int];

------------------- TRIGGERS --------------------
ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int]);
   SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)});
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int]);
   SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)});
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int]);
   SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)});
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int]);
   SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)});
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int]);
   SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)});
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int]);
   SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)});
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int]);
   SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)});
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int]);
   SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)});
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int], 
  EXISTS(
    ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][PS_SUPPKEY:int]))))));
}

ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int] * -1);
   SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int] * -1);
   SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int] * -1);
   SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int] * -1);
   SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int] * -1);
   SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int] * -1);
   SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int] * -1);
   SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PS_SUPPKEY:int] += 
  ((PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[]
  [PS_SUPPKEY:int, PART_PARTKEY:int] * -1);
   SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1(int)[][PART_SIZE:int, PART_BRAND:string, PART_TYPE:string, PART_PARTKEY:int] += 
  ((PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1);
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int], 
  EXISTS(
    ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][PS_SUPPKEY:int]))))));
}

ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][SUPPLIER_SUPPKEY:int] += 
  {0 !=
[regexp_match:int]('^.*Customer.*Complaints.*$', SUPPLIER_COMMENT:string)};
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int], 
  EXISTS(
    ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][PS_SUPPKEY:int]))))));
}

ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][SUPPLIER_SUPPKEY:int] += 
  ({0 !=
 [regexp_match:int]('^.*Customer.*Complaints.*$', SUPPLIER_COMMENT:string)} *
  -1);
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int], 
  EXISTS(
    ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][PS_SUPPKEY:int]))))));
}

ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int];
   SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[][PARTSUPP_SUPPKEY:int, PARTSUPP_PARTKEY:int] += 1;
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int];
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int];
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int];
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int];
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int];
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int];
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1(int)[]
[P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int];
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int], 
  EXISTS(
    ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][PS_SUPPKEY:int]))))));
}

ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  (SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1(int)[]
 [P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int] * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4(int)[][PARTSUPP_SUPPKEY:int, PARTSUPP_PARTKEY:int] += -1;
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  (SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1(int)[]
 [P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int] * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  (SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1(int)[]
 [P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int] * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  (SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1(int)[]
 [P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int] * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  (SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1(int)[]
 [P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int] * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  (SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1(int)[]
 [P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int] * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  (SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1(int)[]
 [P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int] * 
-1);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  (SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1(int)[]
 [P_SIZE:int, P_BRAND:string, P_TYPE:string, PARTSUPP_PARTKEY:int] * 
-1);
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  AggSum([P_BRAND:string, P_TYPE:string, P_SIZE:int], 
  EXISTS(
    ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][PS_SUPPKEY:int]))))));
}

ON SYSTEM READY {
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_2[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_3[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_4[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_5[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_6[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_7[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_8[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1[][delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  (EXISTS(
   ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
     [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
     (AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))) +
       AggSum([], 
         ((__domain_1:int ^= 0) *
           ((__domain_1:int ^=
              (SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int] +
                delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int)) +
             ((__domain_1:int ^=
                SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[]
                [delta_PS_SUPPKEY:int]) *
               -1))))))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4[][delta_PS_SUPPKEY:int,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 49) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 14) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 23) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 45) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 19) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 3) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 36) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, delta_PS_SUPPKEY:int] += 
  ({PART_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTPART_PARTKEY:int} *
  (PART_SIZE:int ^= 9) * {PART_BRAND:string != 'Brand#45'} *
  {0 = [regexp_match:int]('^MEDIUM POLISHED.*$', PART_TYPE:string)} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPART4:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_2[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_3[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_4[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_5[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_6[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_7[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_8[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1[][delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  (EXISTS(
   ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
     [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
     (AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))) +
       AggSum([], 
         ((__domain_1:int ^= 0) *
           ((__domain_1:int ^=
              (SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int] +
                delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int)) +
             ((__domain_1:int ^=
                SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[]
                [delta_PS_SUPPKEY:int]) *
               -1))))))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_2[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_3[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_4[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_5[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_6[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_7[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_8[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1[][delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  (EXISTS(
   ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
     [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
     (AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))) +
       AggSum([], 
         ((__domain_1:int ^= 0) *
           ((__domain_1:int ^=
              (SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int] +
                delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int)) +
             ((__domain_1:int ^=
                SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[]
                [delta_PS_SUPPKEY:int]) *
               -1))))))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_2[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_3[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_4[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_5[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_6[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_7[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_8[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1[][delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  (EXISTS(
   ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
     [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
     (AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))) +
       AggSum([], 
         ((__domain_1:int ^= 0) *
           ((__domain_1:int ^=
              (SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int] +
                delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int)) +
             ((__domain_1:int ^=
                SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[]
                [delta_PS_SUPPKEY:int]) *
               -1))))))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPPPARTSUPP_PARTKEY:int} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPPPARTSUPP_PARTKEY:int} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPPPARTSUPP_PARTKEY:int} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPPPARTSUPP_PARTKEY:int} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPPPARTSUPP_PARTKEY:int} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPPPARTSUPP_PARTKEY:int} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPPPARTSUPP_PARTKEY:int} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPPPARTSUPP_PARTKEY:int} *
  delta_SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_2[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_3[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_4[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_5[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_6[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_7[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_8[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1[][delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  (EXISTS(
   ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
     [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
     (AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))) +
       AggSum([], 
         ((__domain_1:int ^= 0) *
           ((__domain_1:int ^=
              (SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int] +
                delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int)) +
             ((__domain_1:int ^=
                SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[]
                [delta_PS_SUPPKEY:int]) *
               -1))))))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPPPARTSUPP_PARTKEY:int} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPPPARTSUPP_PARTKEY:int} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_2_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPPPARTSUPP_PARTKEY:int} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_3_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPPPARTSUPP_PARTKEY:int} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_4_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPPPARTSUPP_PARTKEY:int} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_5_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPPPARTSUPP_PARTKEY:int} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_6_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPPPARTSUPP_PARTKEY:int} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_7_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1[][delta_P_SIZE:int,delta_P_BRAND:string,delta_P_TYPE:string,delta_SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPPPARTSUPP_PARTKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int, PARTSUPP_SUPPKEY:int] += 
  ({PARTSUPP_PARTKEY:int =
 delta_SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPPPARTSUPP_PARTKEY:int} * 
-1 * delta_SUPPLIER_CNT_mPARTSUPP1_E1_8_mPARTSUPP1:int);
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_1:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_2[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_2:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_3[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_3:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_4[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_4:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_5[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_5:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_6[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_6:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_7[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_7:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_8[][delta_P_BRAND:string,delta_P_TYPE:string,delta_P_SIZE:int,delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int] := 
  (EXISTS(
   (((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
      [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
         delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
       SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int]) *
      AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int])))) +
     (AggSum([], 
        ((__domain_1:int ^=
           SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]) *
          (__domain_1:int ^= 0))) *
       delta_SUPPLIER_CNT_mPARTSUPP1_E1_8:int))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
          delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [delta_P_BRAND:string, delta_P_TYPE:string, delta_P_SIZE:int,
           delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}

CORRECT SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1[][delta_PS_SUPPKEY:int] += delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   SUPPLIER_CNT(int)[][P_BRAND:string, P_TYPE:string, P_SIZE:int] := 
  (EXISTS(
   ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
     [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
      SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
      [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
     (AggSum([], 
        ((__domain_1:int ^= 0) *
          (__domain_1:int ^=
            SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))) +
       AggSum([], 
         ((__domain_1:int ^= 0) *
           ((__domain_1:int ^=
              (SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int] +
                delta_SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1:int)) +
             ((__domain_1:int ^=
                SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[]
                [delta_PS_SUPPKEY:int]) *
               -1))))))) +
  (EXISTS(
     ((SUPPLIER_CNT_mPARTSUPP1_E1_1(int)[]
       [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_2(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_3(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_4(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_5(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_6(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_7(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int] +
        SUPPLIER_CNT_mPARTSUPP1_E1_8(int)[]
        [P_BRAND:string, P_TYPE:string, P_SIZE:int, delta_PS_SUPPKEY:int]) *
       AggSum([], 
         ((__domain_1:int ^= 0) *
           (__domain_1:int ^=
             SUPPLIER_CNT_mPARTSUPP1_E1_1_L2_1(int)[][delta_PS_SUPPKEY:int]))))) *
    -1));
}
