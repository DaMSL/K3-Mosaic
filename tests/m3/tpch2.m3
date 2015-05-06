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

CREATE TABLE NATION(NATION_NATIONKEY int, NATION_NAME string, NATION_REGIONKEY int, NATION_COMMENT string)
  FROM FILE 'data/tpch/nation.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE TABLE REGION(REGION_REGIONKEY int, REGION_NAME string, REGION_COMMENT string)
  FROM FILE 'data/tpch/region.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP COUNT(int)[]
[S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string,
   S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] := 
AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
          P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
  ((R_NAME:string ^= 'EUROPE') * (P_SIZE:int ^= 15) *
    PART(P_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
               S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string) *
    PARTSUPP(P_PARTKEY:int, S_SUPPKEY:int, PS_AVAILQTY:int,
               PS_SUPPLYCOST:float, PS_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string) *
    REGION(N_REGIONKEY:int, R_NAME:string, R_COMMENT:string) *
    {0 != [regexp_match:int]('^.*BRASS$', P_TYPE:string)} *
    AggSum([], 
      ((__domain_1:int ^=
         AggSum([P_PARTKEY:int], 
           ((R2_NAME:string ^= 'EUROPE') *
             PARTSUPP(P_PARTKEY:int, PS2_SUPPKEY:int, PS2_AVAILQTY:int,
                        PS2_SUPPLYCOST:float, PS2_COMMENT:string) *
             SUPPLIER(PS2_SUPPKEY:int, S2_NAME:string, S2_ADDRESS:string,
                        S2_NATIONKEY:int, S2_PHONE:string, S2_ACCTBAL:float,
                        S2_COMMENT:string) *
             NATION(S2_NATIONKEY:int, N2_NAME:string, N2_REGIONKEY:int,
                      N2_COMMENT:string) *
             REGION(N2_REGIONKEY:int, R2_NAME:string, R2_COMMENT:string) *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
        (__domain_1:int ^= 0)))));

DECLARE MAP COUNT_mPARTSUPP1(int)[]
[S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
   S_PHONE:string, S_COMMENT:string, COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] := 
AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
          S_PHONE:string, S_COMMENT:string,
          COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int], 
  ((R_NAME:string ^= 'EUROPE') *
    SUPPLIER(COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int, S_NAME:string,
               S_ADDRESS:string, S_NATIONKEY:int, S_PHONE:string,
               S_ACCTBAL:float, S_COMMENT:string) *
    REGION(N_REGIONKEY:int, R_NAME:string, R_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string)));

DECLARE MAP COUNT_mPARTSUPP2(int)[][P_PARTKEY:int, P_MFGR:string] := 
AggSum([P_PARTKEY:int, P_MFGR:string], 
  ((P_SIZE:int ^= 15) *
    PART(P_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    {0 != [regexp_match:int]('^.*BRASS$', P_TYPE:string)}));

DECLARE MAP COUNT_mPARTSUPP4_L2_2(int)[][COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] := 
AggSum([COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int], 
  ((R2_NAME:string ^= 'EUROPE') *
    SUPPLIER(COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int, S2_NAME:string,
               S2_ADDRESS:string, S2_NATIONKEY:int, S2_PHONE:string,
               S2_ACCTBAL:float, S2_COMMENT:string) *
    NATION(S2_NATIONKEY:int, N2_NAME:string, N2_REGIONKEY:int,
             N2_COMMENT:string) *
    REGION(N2_REGIONKEY:int, R2_NAME:string, R2_COMMENT:string)));

DECLARE MAP COUNT_mSUPPLIER1(int)[][N_NAME:string, COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
AggSum([N_NAME:string, COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((R_NAME:string ^= 'EUROPE') *
    REGION(N_REGIONKEY:int, R_NAME:string, R_COMMENT:string) *
    NATION(COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int, N_NAME:string,
             N_REGIONKEY:int, N_COMMENT:string)));

DECLARE MAP COUNT_mSUPPLIER2(int)[]
[P_PARTKEY:int, P_MFGR:string, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
   PS_SUPPLYCOST:float] := 
AggSum([P_PARTKEY:int, P_MFGR:string, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
          PS_SUPPLYCOST:float], 
  ((P_SIZE:int ^= 15) *
    PART(P_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    PARTSUPP(P_PARTKEY:int, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
               PS_AVAILQTY:int, PS_SUPPLYCOST:float, PS_COMMENT:string) *
    {0 != [regexp_match:int]('^.*BRASS$', P_TYPE:string)}));

DECLARE MAP COUNT_mSUPPLIER9(int)[]
[S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string,
   S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] := 
AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
          P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
          PS_SUPPLYCOST:float], 
  ((R_NAME:string ^= 'EUROPE') * (P_SIZE:int ^= 15) *
    PART(P_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    REGION(N_REGIONKEY:int, R_NAME:string, R_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string) *
    PARTSUPP(P_PARTKEY:int, S_SUPPKEY:int, PS_AVAILQTY:int,
               PS_SUPPLYCOST:float, PS_COMMENT:string) *
    SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
               S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string) *
    {0 != [regexp_match:int]('^.*BRASS$', P_TYPE:string)}));

DECLARE MAP COUNT_mSUPPLIER9_L2_2(int)[][COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
AggSum([COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((R2_NAME:string ^= 'EUROPE') *
    NATION(COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int, N2_NAME:string,
             N2_REGIONKEY:int, N2_COMMENT:string) *
    REGION(N2_REGIONKEY:int, R2_NAME:string, R2_COMMENT:string)));

DECLARE MAP COUNT_mSUPPLIER9_L2_3(int)[]
[P_PARTKEY:int, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] := 
AggSum([P_PARTKEY:int, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
          PS2_SUPPLYCOST:float], 
  PARTSUPP(P_PARTKEY:int, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,
             PS2_AVAILQTY:int, PS2_SUPPLYCOST:float, PS2_COMMENT:string));

DECLARE MAP COUNT_mPART1(int)[]
[S_ACCTBAL:float, S_NAME:string, N_NAME:string, COUNT_mPARTPART_PARTKEY:int,
   S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] := 
AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
          COUNT_mPARTPART_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
          S_COMMENT:string, PS_SUPPLYCOST:float], 
  ((R_NAME:string ^= 'EUROPE') *
    SUPPLIER(S_SUPPKEY:int, S_NAME:string, S_ADDRESS:string, S_NATIONKEY:int,
               S_PHONE:string, S_ACCTBAL:float, S_COMMENT:string) *
    PARTSUPP(COUNT_mPARTPART_PARTKEY:int, S_SUPPKEY:int, PS_AVAILQTY:int,
               PS_SUPPLYCOST:float, PS_COMMENT:string) *
    NATION(S_NATIONKEY:int, N_NAME:string, N_REGIONKEY:int, N_COMMENT:string) *
    REGION(N_REGIONKEY:int, R_NAME:string, R_COMMENT:string)));

DECLARE MAP COUNT_mPART1_L2_1(int)[][COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] := 
AggSum([COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float], 
  ((R2_NAME:string ^= 'EUROPE') *
    PARTSUPP(COUNT_mPARTPART_PARTKEY:int, PS2_SUPPKEY:int, PS2_AVAILQTY:int,
               PS2_SUPPLYCOST:float, PS2_COMMENT:string) *
    SUPPLIER(PS2_SUPPKEY:int, S2_NAME:string, S2_ADDRESS:string,
               S2_NATIONKEY:int, S2_PHONE:string, S2_ACCTBAL:float,
               S2_COMMENT:string) *
    NATION(S2_NATIONKEY:int, N2_NAME:string, N2_REGIONKEY:int,
             N2_COMMENT:string) *
    REGION(N2_REGIONKEY:int, R2_NAME:string, R2_COMMENT:string)));

-------------------- QUERIES --------------------
DECLARE QUERY COUNT := COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string];

------------------- TRIGGERS --------------------
ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PART_SIZE:int = 15} *
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
            S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mPART1(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
         S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        AggSum([PART_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[][PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)});
   COUNT_mPARTSUPP2(int)[][PART_PARTKEY:int, PART_MFGR:string] += 
  ({PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)});
   COUNT_mSUPPLIER2(int)[][PART_PARTKEY:int, PART_MFGR:string, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] += 
  ({PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [PART_PARTKEY:int, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float]);
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({PART_SIZE:int = 15} *
  COUNT_mPART1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
     S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)});
}

ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PART_SIZE:int = 15} *
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
            S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mPART1(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
         S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        AggSum([PART_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[][PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} * -1);
   COUNT_mPARTSUPP2(int)[][PART_PARTKEY:int, PART_MFGR:string] += 
  ({PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} * -1);
   COUNT_mSUPPLIER2(int)[][PART_PARTKEY:int, PART_MFGR:string, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] += 
  ({PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [PART_PARTKEY:int, COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1);
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({PART_SIZE:int = 15} *
  COUNT_mPART1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
     S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} * -1);
}

ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
   (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
   (S_PHONE:string ^= SUPPLIER_PHONE:string) *
   (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
   (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
   COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
   AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
     ((__domain_1:int ^= 0) *
       COUNT_mSUPPLIER2(int)[]
       [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
          PS_SUPPLYCOST:float] *
       (__domain_1:int ^=
         (AggSum([P_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
           (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
             AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
               (COUNT_mSUPPLIER9_L2_3(int)[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))))))) +
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
            P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER9(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
         P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))))) +
  (AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
             P_MFGR:string, S_ADDRESS:string, S_PHONE:string,
             S_COMMENT:string], 
     ((__domain_1:int ^= 0) *
       COUNT_mSUPPLIER9(int)[]
       [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
          P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
          PS_SUPPLYCOST:float] *
       (__domain_1:int ^=
         AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
    -1));
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int];
   COUNT_mPARTSUPP4_L2_2(int)[][SUPPLIER_SUPPKEY:int] += COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int];
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  (COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float]);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  (COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float]);
   COUNT_mPART1_L2_1(int)[][COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] += 
  (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float]);
}

ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  (((((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         (__domain_1:int ^=
           (AggSum([P_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
               AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                 (COUNT_mSUPPLIER9_L2_3(int)[]
                  [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
               -1)))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
              P_MFGR:string, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        (__domain_1:int ^=
          AggSum([P_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))))) *
   -1) +
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
            P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER9(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
         P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
            -1))))));
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += (COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] * -1);
   COUNT_mPARTSUPP4_L2_2(int)[][SUPPLIER_SUPPKEY:int] += (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] * -1);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  (COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  (COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1);
   COUNT_mPART1_L2_1(int)[][COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] += 
  (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
  -1);
}

ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  (COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  ((COUNT_mPARTSUPP1(int)[]
    [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
       S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^=
          AggSum([PARTSUPP_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[]
             [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))) *
         (__domain_1:int ^= 0)))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mPART1(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int,
           S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        (__domain_1:int ^=
          (AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
            (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
              {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) +
    (AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
               PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
               S_COMMENT:string], 
       ((__domain_1:int ^= 0) *
         COUNT_mPART1(int)[]
         [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
            S_COMMENT:string, PS_SUPPLYCOST:float] *
         (__domain_1:int ^=
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
      -1)));
   COUNT_mSUPPLIER2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string];
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  (COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int]);
   COUNT_mSUPPLIER9_L2_3(int)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += 1;
   COUNT_mPART1(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  COUNT_mPARTSUPP1(int)[]
[S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
   S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int];
   COUNT_mPART1_L2_1(int)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPLYCOST:float] += COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int];
}

ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  (((((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
     COUNT_mPARTSUPP2(int)[][P_PARTKEY:int, P_MFGR:string] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         (__domain_1:int ^=
           AggSum([P_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))))) *
     COUNT_mPARTSUPP1(int)[]
     [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
        S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int]) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
              P_MFGR:string, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        (__domain_1:int ^=
          AggSum([P_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))))) *
   -1) +
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
            P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER9(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
         P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          ((P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
            COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
            {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float} * -1))))));
   COUNT_mSUPPLIER2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += (COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] * -1);
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
    S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] * -1);
   COUNT_mSUPPLIER9_L2_3(int)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += -1;
   COUNT_mPART1(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  (COUNT_mPARTSUPP1(int)[]
 [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
    S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  -1);
   COUNT_mPART1_L2_1(int)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPLYCOST:float] += (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] * -1);
}

ON SYSTEM READY {
   COUNT_mSUPPLIER1(int)[][N_NAME:string, COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
  AggSum([N_NAME:string, COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((R_NAME:string ^= 'EUROPE') *
    REGION(N_REGIONKEY:int, R_NAME:string, R_COMMENT:string) *
    NATION(COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int, N_NAME:string,
             N_REGIONKEY:int, N_COMMENT:string)));
   COUNT_mSUPPLIER9_L2_2(int)[][COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] := 
  AggSum([COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int], 
  ((R2_NAME:string ^= 'EUROPE') *
    NATION(COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int, N2_NAME:string,
             N2_REGIONKEY:int, N2_COMMENT:string) *
    REGION(N2_REGIONKEY:int, R2_NAME:string, R2_COMMENT:string)));
}

CORRECT COUNT_mPART1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_COUNT_mPARTPART_PARTKEY:int,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_PS_SUPPLYCOST:float] += delta_COUNT_mPART1:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([PART_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[][PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))))) *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} *
  delta_COUNT_mPART1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, delta_PS_SUPPLYCOST:float] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} *
  delta_COUNT_mPART1:int);
}

CORRECT COUNT_mPART1_L2_1[][delta_COUNT_mPARTPART_PARTKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mPART1_L2_1:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
            S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mPART1(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
         S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      ((__domain_1:int ^=
         (AggSum([PART_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[][PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
           ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
             delta_COUNT_mPART1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([PART_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
          -1)))) *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)});
}

CORRECT COUNT_mSUPPLIER9_L2_3[][delta_P_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9_L2_3:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT_mSUPPLIER2(int)[][PART_PARTKEY:int, PART_MFGR:string, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, delta_PS2_SUPPLYCOST:float] += 
  ({PART_PARTKEY:int = delta_P_PARTKEY:int} * {PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} *
  delta_COUNT_mSUPPLIER9_L2_3:int);
}

CORRECT COUNT_mPART1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_COUNT_mPARTPART_PARTKEY:int,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_PS_SUPPLYCOST:float] += delta_COUNT_mPART1:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([PART_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[][PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))))) *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} *
  delta_COUNT_mPART1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, delta_PS_SUPPLYCOST:float] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} *
  delta_COUNT_mPART1:int);
}

CORRECT COUNT_mPART1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_COUNT_mPARTPART_PARTKEY:int,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_PS_SUPPLYCOST:float] += delta_COUNT_mPART1:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([PART_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[][PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))))) *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} * -1 *
  delta_COUNT_mPART1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, delta_PS_SUPPLYCOST:float] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} * -1 *
  delta_COUNT_mPART1:int);
}

CORRECT COUNT_mPART1_L2_1[][delta_COUNT_mPARTPART_PARTKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mPART1_L2_1:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
            S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mPART1(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PART_PARTKEY:int,
         S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      ((__domain_1:int ^=
         (AggSum([PART_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[][PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
           ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
             delta_COUNT_mPART1_L2_1:int))) +
        ((__domain_1:int ^=
           AggSum([PART_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
          -1)))) *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} * -1);
}

CORRECT COUNT_mSUPPLIER9_L2_3[][delta_P_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9_L2_3:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT_mSUPPLIER2(int)[][PART_PARTKEY:int, PART_MFGR:string, delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int, delta_PS2_SUPPLYCOST:float] += 
  ({PART_PARTKEY:int = delta_P_PARTKEY:int} * {PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} * -1 *
  delta_COUNT_mSUPPLIER9_L2_3:int);
}

CORRECT COUNT_mPART1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_COUNT_mPARTPART_PARTKEY:int,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_PS_SUPPLYCOST:float] += delta_COUNT_mPART1:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([PART_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[][PART_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))))) *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} * -1 *
  delta_COUNT_mPART1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PART_PARTKEY:int, PART_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, delta_PS_SUPPLYCOST:float] += 
  ({PART_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  {PART_SIZE:int = 15} *
  {0 != [regexp_match:int]('^.*BRASS$', PART_TYPE:string)} * -1 *
  delta_COUNT_mPART1:int);
}

CORRECT COUNT_mSUPPLIER1[][delta_N_NAME:string,delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER2(int)[]
      [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))))) *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER2[][delta_P_PARTKEY:int,delta_P_MFGR:string,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        (AggSum([delta_P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[]
            [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                  PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))))))) *
  delta_COUNT_mSUPPLIER2:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  delta_COUNT_mSUPPLIER2:int);
}

CORRECT COUNT_mPART1_L2_1[][delta_COUNT_mPARTPART_PARTKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mPART1_L2_1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
   (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
   (S_PHONE:string ^= SUPPLIER_PHONE:string) *
   (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
   (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
   COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
   AggSum([delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
             SUPPLIER_SUPPKEY:int], 
     ((__domain_1:int ^= 0) *
       COUNT_mSUPPLIER2(int)[]
       [delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
          SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
       ((__domain_1:int ^=
          (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
            (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
              AggSum([delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                (COUNT_mSUPPLIER9_L2_3(int)[]
                 [delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                    PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) +
            ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
              delta_COUNT_mPART1_L2_1:int))) +
         ((__domain_1:int ^=
            (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
              (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                AggSum([delta_COUNT_mPARTPART_PARTKEY:int,
                          SUPPLIER_SUPPKEY:int], 
                  (COUNT_mSUPPLIER9_L2_3(int)[]
                   [delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                      PS2_SUPPLYCOST:float] *
                    {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
           -1))))) +
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
            S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER9(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
         delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
         S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] *
      ((__domain_1:int ^=
         (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[]
             [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
           (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
             AggSum([delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
               (COUNT_mSUPPLIER9_L2_3(int)[]
                [delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                   PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) +
           ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
             delta_COUNT_mPART1_L2_1:int))) +
        ((__domain_1:int ^=
           (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
               AggSum([delta_COUNT_mPARTPART_PARTKEY:int,
                         SUPPLIER_SUPPKEY:int], 
                 (COUNT_mSUPPLIER9_L2_3(int)[]
                  [delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                     PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
          -1)))) +
  (AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
             delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
             S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
     ((__domain_1:int ^= 0) *
       COUNT_mSUPPLIER9(int)[]
       [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
          delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
          S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] *
       ((__domain_1:int ^=
          (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
            ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
              delta_COUNT_mPART1_L2_1:int))) +
         ((__domain_1:int ^=
            AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
           -1)))) *
    -1));
}

CORRECT COUNT_mSUPPLIER9_L2_2[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER9_L2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                (COUNT_mSUPPLIER9_L2_3(int)[]
                 [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_COUNT_mSUPPLIER9_L2_2:int)) +
              AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
             -1))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
              P_MFGR:string, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
               (COUNT_mSUPPLIER9_L2_3(int)[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
              (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                delta_COUNT_mSUPPLIER9_L2_2:int)) +
             AggSum([P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
            -1))))));
   COUNT_mPARTSUPP4_L2_2(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_COUNT_mSUPPLIER9_L2_2:int);
   COUNT_mPART1_L2_1(int)[][COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER9_L2_2:int);
}

CORRECT COUNT_mSUPPLIER9_L2_3[][delta_P_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9_L2_3:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                  (COUNT_mSUPPLIER9_L2_3(int)[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                      PS2_SUPPLYCOST:float] *
                    {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                 ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                   delta_COUNT_mSUPPLIER9_L2_3:int))) +
              AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([delta_P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
             -1))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
              S_PHONE:string, S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
              (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                 (COUNT_mSUPPLIER9_L2_3(int)[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                     PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                  delta_COUNT_mSUPPLIER9_L2_3:int))) +
             AggSum([delta_P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
            -1))))));
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  delta_COUNT_mSUPPLIER9_L2_3:int);
   COUNT_mPART1_L2_1(int)[][delta_P_PARTKEY:int, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_COUNT_mSUPPLIER9_L2_3:int);
}

CORRECT COUNT_mSUPPLIER9[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_P_PARTKEY:int,delta_P_MFGR:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_PS_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ((AggSum([], 
    ((__domain_1:int ^=
       (AggSum([delta_P_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[]
           [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) +
         (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
           AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
             (COUNT_mSUPPLIER9_L2_3(int)[]
              [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                 PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))))) *
      (__domain_1:int ^= 0))) +
   (AggSum([], 
      ((__domain_1:int ^=
         AggSum([delta_P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[]
            [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))) *
        (__domain_1:int ^= 0))) *
     -1)) *
  delta_COUNT_mSUPPLIER9:int);
}

CORRECT COUNT_mSUPPLIER1[][delta_N_NAME:string,delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER2(int)[]
      [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))))) *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER9_L2_2[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER9_L2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                (COUNT_mSUPPLIER9_L2_3(int)[]
                 [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_COUNT_mSUPPLIER9_L2_2:int)) +
              AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
             -1))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
              P_MFGR:string, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
               (COUNT_mSUPPLIER9_L2_3(int)[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
              (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                delta_COUNT_mSUPPLIER9_L2_2:int)) +
             AggSum([P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
            -1))))));
   COUNT_mPARTSUPP4_L2_2(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_COUNT_mSUPPLIER9_L2_2:int);
   COUNT_mPART1_L2_1(int)[][COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER9_L2_2:int);
}

CORRECT COUNT_mSUPPLIER1[][delta_N_NAME:string,delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER2(int)[]
      [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))))) *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER2[][delta_P_PARTKEY:int,delta_P_MFGR:string,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        (AggSum([delta_P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[]
            [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                  PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))))))) *
  delta_COUNT_mSUPPLIER2:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  delta_COUNT_mSUPPLIER2:int);
}

CORRECT COUNT_mSUPPLIER1[][delta_N_NAME:string,delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER2(int)[]
      [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))))) *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER1:int);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER9_L2_3[][delta_P_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9_L2_3:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                  (COUNT_mSUPPLIER9_L2_3(int)[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                      PS2_SUPPLYCOST:float] *
                    {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                 ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                   delta_COUNT_mSUPPLIER9_L2_3:int))) +
              AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([delta_P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
             -1))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
              S_PHONE:string, S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
              (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                 (COUNT_mSUPPLIER9_L2_3(int)[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                     PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                  delta_COUNT_mSUPPLIER9_L2_3:int))) +
             AggSum([delta_P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
            -1))))));
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  delta_COUNT_mSUPPLIER9_L2_3:int);
   COUNT_mPART1_L2_1(int)[][delta_P_PARTKEY:int, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_COUNT_mSUPPLIER9_L2_3:int);
}

CORRECT COUNT_mSUPPLIER9_L2_2[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER9_L2_2:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                (COUNT_mSUPPLIER9_L2_3(int)[]
                 [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_COUNT_mSUPPLIER9_L2_2:int)) +
              AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
             -1))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
              P_MFGR:string, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
               (COUNT_mSUPPLIER9_L2_3(int)[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
              (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                delta_COUNT_mSUPPLIER9_L2_2:int)) +
             AggSum([P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
            -1))))));
   COUNT_mPARTSUPP4_L2_2(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  delta_COUNT_mSUPPLIER9_L2_2:int);
   COUNT_mPART1_L2_1(int)[][COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
  delta_COUNT_mSUPPLIER9_L2_2:int);
}

CORRECT COUNT_mSUPPLIER9_L2_3[][delta_P_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9_L2_3:int FOR ON + SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                  (COUNT_mSUPPLIER9_L2_3(int)[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                      PS2_SUPPLYCOST:float] *
                    {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                 ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                   delta_COUNT_mSUPPLIER9_L2_3:int))) +
              AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([delta_P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
             -1))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
              S_PHONE:string, S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
              (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                 (COUNT_mSUPPLIER9_L2_3(int)[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                     PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                  delta_COUNT_mSUPPLIER9_L2_3:int))) +
             AggSum([delta_P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
            -1))))));
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  delta_COUNT_mSUPPLIER9_L2_3:int);
   COUNT_mPART1_L2_1(int)[][delta_P_PARTKEY:int, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
  delta_COUNT_mSUPPLIER9_L2_3:int);
}

CORRECT COUNT_mSUPPLIER1[][delta_N_NAME:string,delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER2(int)[]
      [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
            -1))))) *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER2[][delta_P_PARTKEY:int,delta_P_MFGR:string,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        (AggSum([delta_P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[]
            [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                  PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) *
            -1))))) *
  -1 * delta_COUNT_mSUPPLIER2:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] * -1 *
  delta_COUNT_mSUPPLIER2:int);
}

CORRECT COUNT_mPART1_L2_1[][delta_COUNT_mPARTPART_PARTKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mPART1_L2_1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  (((((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
               SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
            SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
              (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                AggSum([delta_COUNT_mPARTPART_PARTKEY:int,
                          SUPPLIER_SUPPKEY:int], 
                  (COUNT_mSUPPLIER9_L2_3(int)[]
                   [delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                      PS2_SUPPLYCOST:float] *
                    {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                -1) +
              ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                delta_COUNT_mPART1_L2_1:int))) +
           ((__domain_1:int ^=
              (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([delta_COUNT_mPARTPART_PARTKEY:int,
                            SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [delta_COUNT_mPARTPART_PARTKEY:int,
                        SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                  -1))) *
             -1))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
              S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
           delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
           S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
               delta_COUNT_mPART1_L2_1:int))) +
          ((__domain_1:int ^=
             AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
            -1))))) *
   -1) +
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
            S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER9(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
         delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
         S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] *
      ((__domain_1:int ^=
         (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[]
             [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
           (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
             AggSum([delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
               (COUNT_mSUPPLIER9_L2_3(int)[]
                [delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                   PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
             -1) +
           ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
             delta_COUNT_mPART1_L2_1:int))) +
        ((__domain_1:int ^=
           (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
               AggSum([delta_COUNT_mPARTPART_PARTKEY:int,
                         SUPPLIER_SUPPKEY:int], 
                 (COUNT_mSUPPLIER9_L2_3(int)[]
                  [delta_COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                     PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
               -1))) *
          -1)))));
}

CORRECT COUNT_mSUPPLIER9_L2_2[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER9_L2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                (COUNT_mSUPPLIER9_L2_3(int)[]
                 [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_COUNT_mSUPPLIER9_L2_2:int) *
               -1) +
              AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                  -1))) *
             -1)))) *
     -1) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
              P_MFGR:string, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
               (COUNT_mSUPPLIER9_L2_3(int)[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
              (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                delta_COUNT_mSUPPLIER9_L2_2:int) *
              -1) +
             AggSum([P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                 -1))) *
            -1))))));
   COUNT_mPARTSUPP4_L2_2(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_COUNT_mSUPPLIER9_L2_2:int);
   COUNT_mPART1_L2_1(int)[][COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER9_L2_2:int);
}

CORRECT COUNT_mSUPPLIER9_L2_3[][delta_P_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9_L2_3:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                  (COUNT_mSUPPLIER9_L2_3(int)[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                      PS2_SUPPLYCOST:float] *
                    {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                 ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                   delta_COUNT_mSUPPLIER9_L2_3:int)) *
               -1) +
              AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([delta_P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                  -1))) *
             -1)))) *
     -1) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
              S_PHONE:string, S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
              (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                 (COUNT_mSUPPLIER9_L2_3(int)[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                     PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                  delta_COUNT_mSUPPLIER9_L2_3:int)) *
              -1) +
             AggSum([delta_P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                 -1))) *
            -1))))));
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] * -1 *
  delta_COUNT_mSUPPLIER9_L2_3:int);
   COUNT_mPART1_L2_1(int)[][delta_P_PARTKEY:int, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_COUNT_mSUPPLIER9_L2_3:int);
}

CORRECT COUNT_mSUPPLIER9[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_P_PARTKEY:int,delta_P_MFGR:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_PS_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  (((AggSum([], 
     ((__domain_1:int ^=
        AggSum([delta_P_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[]
           [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))) *
       (__domain_1:int ^= 0))) *
    -1) +
   AggSum([], 
     ((__domain_1:int ^=
        (AggSum([delta_P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[]
            [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                  PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) *
            -1))) *
       (__domain_1:int ^= 0)))) *
  delta_COUNT_mSUPPLIER9:int);
}

CORRECT COUNT_mSUPPLIER1[][delta_N_NAME:string,delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER2(int)[]
      [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
            -1))))) *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER9_L2_2[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER9_L2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                (COUNT_mSUPPLIER9_L2_3(int)[]
                 [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_COUNT_mSUPPLIER9_L2_2:int) *
               -1) +
              AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                  -1))) *
             -1)))) *
     -1) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
              P_MFGR:string, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
               (COUNT_mSUPPLIER9_L2_3(int)[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
              (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                delta_COUNT_mSUPPLIER9_L2_2:int) *
              -1) +
             AggSum([P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                 -1))) *
            -1))))));
   COUNT_mPARTSUPP4_L2_2(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_COUNT_mSUPPLIER9_L2_2:int);
   COUNT_mPART1_L2_1(int)[][COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER9_L2_2:int);
}

CORRECT COUNT_mSUPPLIER1[][delta_N_NAME:string,delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER2(int)[]
      [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
            -1))))) *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER2[][delta_P_PARTKEY:int,delta_P_MFGR:string,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        (AggSum([delta_P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[]
            [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                  PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) *
            -1))))) *
  -1 * delta_COUNT_mSUPPLIER2:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] * -1 *
  delta_COUNT_mSUPPLIER2:int);
}

CORRECT COUNT_mSUPPLIER1[][delta_N_NAME:string,delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER1:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER2(int)[]
      [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
         PS_SUPPLYCOST:float] *
      (__domain_1:int ^=
        (AggSum([P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
          (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
            AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
              (COUNT_mSUPPLIER9_L2_3(int)[]
               [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
            -1))))) *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPARTSUPP1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mSUPPLIER9(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, P_PARTKEY:int, P_MFGR:string, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER2(int)[]
  [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER1:int);
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, delta_N_NAME:string, COUNT_mPARTPART_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, PS_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER1:int);
}

CORRECT COUNT_mSUPPLIER9_L2_3[][delta_P_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9_L2_3:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                  (COUNT_mSUPPLIER9_L2_3(int)[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                      PS2_SUPPLYCOST:float] *
                    {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                 ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                   delta_COUNT_mSUPPLIER9_L2_3:int)) *
               -1) +
              AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([delta_P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                  -1))) *
             -1)))) *
     -1) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
              S_PHONE:string, S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
              (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                 (COUNT_mSUPPLIER9_L2_3(int)[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                     PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                  delta_COUNT_mSUPPLIER9_L2_3:int)) *
              -1) +
             AggSum([delta_P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                 -1))) *
            -1))))));
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] * -1 *
  delta_COUNT_mSUPPLIER9_L2_3:int);
   COUNT_mPART1_L2_1(int)[][delta_P_PARTKEY:int, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_COUNT_mSUPPLIER9_L2_3:int);
}

CORRECT COUNT_mSUPPLIER9_L2_2[][delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int] += delta_COUNT_mSUPPLIER9_L2_2:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                (COUNT_mSUPPLIER9_L2_3(int)[]
                 [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                 delta_COUNT_mSUPPLIER9_L2_2:int) *
               -1) +
              AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                  -1))) *
             -1)))) *
     -1) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
              P_MFGR:string, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
               (COUNT_mSUPPLIER9_L2_3(int)[]
                [P_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
              (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] +
                delta_COUNT_mSUPPLIER9_L2_2:int) *
              -1) +
             AggSum([P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[][P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                 -1))) *
            -1))))));
   COUNT_mPARTSUPP4_L2_2(int)[][SUPPLIER_SUPPKEY:int] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  -1 * delta_COUNT_mSUPPLIER9_L2_2:int);
   COUNT_mPART1_L2_1(int)[][COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_NATIONKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_NATIONKEY:int} *
  COUNT_mSUPPLIER9_L2_3(int)[]
  [COUNT_mPARTPART_PARTKEY:int, SUPPLIER_SUPPKEY:int, PS2_SUPPLYCOST:float] *
  -1 * delta_COUNT_mSUPPLIER9_L2_2:int);
}

CORRECT COUNT_mSUPPLIER9_L2_3[][delta_P_PARTKEY:int,delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9_L2_3:int FOR ON - SUPPLIER(SUPPLIER_SUPPKEY:int, SUPPLIER_NAME:string, SUPPLIER_ADDRESS:string, SUPPLIER_NATIONKEY:int, SUPPLIER_PHONE:string, SUPPLIER_ACCTBAL:float, SUPPLIER_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  (((S_NAME:string ^= SUPPLIER_NAME:string) *
     (S_ADDRESS:string ^= SUPPLIER_ADDRESS:string) *
     (S_PHONE:string ^= SUPPLIER_PHONE:string) *
     (S_ACCTBAL:float ^= SUPPLIER_ACCTBAL:float) *
     (S_COMMENT:string ^= SUPPLIER_COMMENT:string) *
     COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] *
     AggSum([delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int], 
       ((__domain_1:int ^= 0) *
         COUNT_mSUPPLIER2(int)[]
         [delta_P_PARTKEY:int, P_MFGR:string, SUPPLIER_SUPPKEY:int,
            PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
               (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                  (COUNT_mSUPPLIER9_L2_3(int)[]
                   [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                      PS2_SUPPLYCOST:float] *
                    {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                 ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                   delta_COUNT_mSUPPLIER9_L2_3:int)) *
               -1) +
              AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
           ((__domain_1:int ^=
              (AggSum([delta_P_PARTKEY:int], 
                 (COUNT_mPART1_L2_1(int)[]
                  [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                  AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                    (COUNT_mSUPPLIER9_L2_3(int)[]
                     [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                        PS2_SUPPLYCOST:float] *
                      {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                  -1))) *
             -1)))) *
     -1) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              delta_P_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
              S_PHONE:string, S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_P_PARTKEY:int,
           P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           ((COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
              (AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                 (COUNT_mSUPPLIER9_L2_3(int)[]
                  [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                     PS2_SUPPLYCOST:float] *
                   {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
                ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                  delta_COUNT_mSUPPLIER9_L2_3:int)) *
              -1) +
             AggSum([delta_P_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
          ((__domain_1:int ^=
             (AggSum([delta_P_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] *
                 AggSum([delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int], 
                   (COUNT_mSUPPLIER9_L2_3(int)[]
                    [delta_P_PARTKEY:int, SUPPLIER_SUPPKEY:int,
                       PS2_SUPPLYCOST:float] *
                     {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) *
                 -1))) *
            -1))))));
   COUNT_mPART1(int)[][SUPPLIER_ACCTBAL:float, SUPPLIER_NAME:string, N_NAME:string, delta_P_PARTKEY:int, SUPPLIER_ADDRESS:string, SUPPLIER_PHONE:string, SUPPLIER_COMMENT:string, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER1(int)[][N_NAME:string, SUPPLIER_NATIONKEY:int] * -1 *
  delta_COUNT_mSUPPLIER9_L2_3:int);
   COUNT_mPART1_L2_1(int)[][delta_P_PARTKEY:int, delta_PS2_SUPPLYCOST:float] += 
  ({SUPPLIER_SUPPKEY:int = delta_COUNT_mSUPPLIERSUPPLIER_SUPPKEY:int} *
  COUNT_mSUPPLIER9_L2_2(int)[][SUPPLIER_NATIONKEY:int] * -1 *
  delta_COUNT_mSUPPLIER9_L2_3:int);
}

CORRECT COUNT_mPARTSUPP2[][delta_P_PARTKEY:int,delta_P_MFGR:string] += delta_COUNT_mPARTSUPP2:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  ((COUNT_mPARTSUPP1(int)[]
    [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
       S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         (__domain_1:int ^=
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float})))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mPART1(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int,
           S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        (__domain_1:int ^=
          (AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
            (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
              {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) +
    (AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
               PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
               S_COMMENT:string], 
       ((__domain_1:int ^= 0) *
         COUNT_mPART1(int)[]
         [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
            S_COMMENT:string, PS_SUPPLYCOST:float] *
         (__domain_1:int ^=
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
      -1)) *
  delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER2(int)[][PARTSUPP_PARTKEY:int, delta_P_MFGR:string, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} * delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  delta_COUNT_mPARTSUPP2:int);
}

CORRECT COUNT_mPARTSUPP1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([PARTSUPP_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[]
           [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))))) *
  delta_COUNT_mPARTSUPP1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  delta_COUNT_mPARTSUPP1:int);
   COUNT_mPART1(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mPART1_L2_1[][delta_COUNT_mPARTPART_PARTKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mPART1_L2_1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  ((COUNT_mPARTSUPP1(int)[]
    [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
       S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         ((__domain_1:int ^=
            (AggSum([PARTSUPP_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float})) +
              ({delta_PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float} *
                delta_COUNT_mPART1_L2_1:int))) +
           ((__domain_1:int ^=
              AggSum([PARTSUPP_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))) *
             -1))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mPART1(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int,
           S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           (AggSum([PARTSUPP_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
               {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float}) +
             ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
               delta_COUNT_mPART1_L2_1:int))) +
          ((__domain_1:int ^=
             (AggSum([PARTSUPP_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
               (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
                 {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
            -1)))) +
    (AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
               PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
               S_COMMENT:string], 
       ((__domain_1:int ^= 0) *
         COUNT_mPART1(int)[]
         [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
            S_COMMENT:string, PS_SUPPLYCOST:float] *
         ((__domain_1:int ^=
            (AggSum([PARTSUPP_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
              ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
                delta_COUNT_mPART1_L2_1:int))) +
           ((__domain_1:int ^=
              AggSum([PARTSUPP_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
             -1)))) *
      -1)));
}

CORRECT COUNT_mPART1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_COUNT_mPARTPART_PARTKEY:int,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_PS_SUPPLYCOST:float] += delta_COUNT_mPART1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PARTSUPP_PARTKEY:int = delta_COUNT_mPARTPART_PARTKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  (AggSum([], 
     ((__domain_1:int ^=
        (AggSum([PARTSUPP_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[]
            [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) +
          (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
            {PARTSUPP_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))) *
       (__domain_1:int ^= 0))) +
    (AggSum([], 
       ((__domain_1:int ^=
          AggSum([PARTSUPP_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[]
             [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))) *
         (__domain_1:int ^= 0))) *
      -1)) *
  delta_COUNT_mPART1:int);
}

CORRECT COUNT_mPARTSUPP4_L2_2[][delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP4_L2_2:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
            S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mPART1(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int,
         S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      ((__domain_1:int ^=
         (({PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
            (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] +
              delta_COUNT_mPARTSUPP4_L2_2:int)) +
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
        ((__domain_1:int ^=
           (AggSum([PARTSUPP_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
               {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
          -1)))));
   COUNT_mPART1_L2_1(int)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_COUNT_mPARTSUPP4_L2_2:int);
}

CORRECT COUNT_mPARTSUPP2[][delta_P_PARTKEY:int,delta_P_MFGR:string] += delta_COUNT_mPARTSUPP2:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  ((COUNT_mPARTSUPP1(int)[]
    [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
       S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         (__domain_1:int ^=
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float})))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mPART1(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int,
           S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        (__domain_1:int ^=
          (AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
            (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
              {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) +
    (AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
               PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
               S_COMMENT:string], 
       ((__domain_1:int ^= 0) *
         COUNT_mPART1(int)[]
         [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
            S_COMMENT:string, PS_SUPPLYCOST:float] *
         (__domain_1:int ^=
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
      -1)) *
  delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER2(int)[][PARTSUPP_PARTKEY:int, delta_P_MFGR:string, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} * delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  delta_COUNT_mPARTSUPP2:int);
}

CORRECT COUNT_mPARTSUPP2[][delta_P_PARTKEY:int,delta_P_MFGR:string] += delta_COUNT_mPARTSUPP2:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  ((COUNT_mPARTSUPP1(int)[]
    [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
       S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         (__domain_1:int ^=
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float})))))) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
              S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mPART1(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int,
           S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        (__domain_1:int ^=
          (AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
            (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
              {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) +
    (AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
               PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
               S_COMMENT:string], 
       ((__domain_1:int ^= 0) *
         COUNT_mPART1(int)[]
         [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
            S_COMMENT:string, PS_SUPPLYCOST:float] *
         (__domain_1:int ^=
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))))) *
      -1)) *
  delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER2(int)[][PARTSUPP_PARTKEY:int, delta_P_MFGR:string, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} * delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  delta_COUNT_mPARTSUPP2:int);
}

CORRECT COUNT_mPARTSUPP1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([PARTSUPP_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[]
           [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))))) *
  delta_COUNT_mPARTSUPP1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  delta_COUNT_mPARTSUPP1:int);
   COUNT_mPART1(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mPARTSUPP1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  AggSum([], 
    ((__domain_1:int ^= 0) *
      (__domain_1:int ^=
        AggSum([PARTSUPP_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[]
           [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))))) *
  delta_COUNT_mPARTSUPP1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  delta_COUNT_mPARTSUPP1:int);
   COUNT_mPART1(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mPARTSUPP4_L2_2[][delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP4_L2_2:int FOR ON + PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            PARTSUPP_PARTKEY:int, S_ADDRESS:string, S_PHONE:string,
            S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mPART1(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int,
         S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      ((__domain_1:int ^=
         (({PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
            (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] +
              delta_COUNT_mPARTSUPP4_L2_2:int)) +
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
        ((__domain_1:int ^=
           (AggSum([PARTSUPP_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
               {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
          -1)))));
   COUNT_mPART1_L2_1(int)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  delta_COUNT_mPARTSUPP4_L2_2:int);
}

CORRECT COUNT_mPARTSUPP2[][delta_P_PARTKEY:int,delta_P_MFGR:string] += delta_COUNT_mPARTSUPP2:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([PARTSUPP_PARTKEY:int], 
         (COUNT_mPART1_L2_1(int)[]
          [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
           {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))) *
      (__domain_1:int ^= 0))) *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  -1 * delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER2(int)[][PARTSUPP_PARTKEY:int, delta_P_MFGR:string, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} * -1 *
  delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  -1 * delta_COUNT_mPARTSUPP2:int);
}

CORRECT COUNT_mPART1_L2_1[][delta_COUNT_mPARTPART_PARTKEY:int,delta_PS2_SUPPLYCOST:float] += delta_COUNT_mPART1_L2_1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  (((((delta_COUNT_mPARTPART_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
     COUNT_mPARTSUPP2(int)[]
     [delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string] *
     AggSum([], 
       ((__domain_1:int ^= 0) *
         ((__domain_1:int ^=
            (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float})) +
              ({delta_PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float} *
                delta_COUNT_mPART1_L2_1:int))) +
           ((__domain_1:int ^=
              AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
                (COUNT_mPART1_L2_1(int)[]
                 [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                  {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))) *
             -1)))) *
     COUNT_mPARTSUPP1(int)[]
     [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
        S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int]) +
    AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
              delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
              S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
      ((__domain_1:int ^= 0) *
        COUNT_mSUPPLIER9(int)[]
        [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
           delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
           S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
           PS_SUPPLYCOST:float] *
        ((__domain_1:int ^=
           (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
               delta_COUNT_mPART1_L2_1:int))) +
          ((__domain_1:int ^=
             AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
               (COUNT_mPART1_L2_1(int)[]
                [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                 {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float}))) *
            -1))))) *
   -1) +
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string,
            S_ADDRESS:string, S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER9(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string,
         delta_COUNT_mPARTPART_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
         S_PHONE:string, S_COMMENT:string, PS_SUPPLYCOST:float] *
      ((__domain_1:int ^=
         (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
            (COUNT_mPART1_L2_1(int)[]
             [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
              {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
           ((delta_COUNT_mPARTPART_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
             COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
             {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float} * -1) +
           ({delta_PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
             delta_COUNT_mPART1_L2_1:int))) +
        ((__domain_1:int ^=
           (AggSum([delta_COUNT_mPARTPART_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [delta_COUNT_mPARTPART_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             ((delta_COUNT_mPARTPART_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
               COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
               {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float} * -1))) *
          -1)))));
}

CORRECT COUNT_mPARTSUPP1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([PARTSUPP_PARTKEY:int], 
         (COUNT_mPART1_L2_1(int)[]
          [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
           {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))) *
      (__domain_1:int ^= 0))) *
  -1 * delta_COUNT_mPARTSUPP1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] * -1 *
  delta_COUNT_mPARTSUPP1:int);
   COUNT_mPART1(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mSUPPLIER9[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_P_PARTKEY:int,delta_P_MFGR:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_PS_SUPPLYCOST:float] += delta_COUNT_mSUPPLIER9:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, delta_P_PARTKEY:int, delta_P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  (((AggSum([], 
     ((__domain_1:int ^=
        AggSum([delta_P_PARTKEY:int], 
          (COUNT_mPART1_L2_1(int)[]
           [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
            {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float}))) *
       (__domain_1:int ^= 0))) *
    -1) +
   AggSum([], 
     ((__domain_1:int ^=
        (AggSum([delta_P_PARTKEY:int], 
           (COUNT_mPART1_L2_1(int)[]
            [delta_P_PARTKEY:int, PS2_SUPPLYCOST:float] *
             {PS2_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float})) +
          ((delta_P_PARTKEY:int ^= PARTSUPP_PARTKEY:int) *
            COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
            {PARTSUPP_SUPPLYCOST:float < delta_PS_SUPPLYCOST:float} * 
          -1))) *
       (__domain_1:int ^= 0)))) *
  delta_COUNT_mSUPPLIER9:int);
}

CORRECT COUNT_mPARTSUPP4_L2_2[][delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP4_L2_2:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
            S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER9(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int,
         P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      ((__domain_1:int ^=
         (({PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
            (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] +
              delta_COUNT_mPARTSUPP4_L2_2:int) *
            -1) +
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
        ((__domain_1:int ^=
           (AggSum([PARTSUPP_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
               {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float} * -1))) *
          -1)))));
   COUNT_mPART1_L2_1(int)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_COUNT_mPARTSUPP4_L2_2:int);
}

CORRECT COUNT_mPARTSUPP2[][delta_P_PARTKEY:int,delta_P_MFGR:string] += delta_COUNT_mPARTSUPP2:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([PARTSUPP_PARTKEY:int], 
         (COUNT_mPART1_L2_1(int)[]
          [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
           {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))) *
      (__domain_1:int ^= 0))) *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  -1 * delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER2(int)[][PARTSUPP_PARTKEY:int, delta_P_MFGR:string, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} * -1 *
  delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  -1 * delta_COUNT_mPARTSUPP2:int);
}

CORRECT COUNT_mPARTSUPP1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([PARTSUPP_PARTKEY:int], 
         (COUNT_mPART1_L2_1(int)[]
          [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
           {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))) *
      (__domain_1:int ^= 0))) *
  -1 * delta_COUNT_mPARTSUPP1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] * -1 *
  delta_COUNT_mPARTSUPP1:int);
   COUNT_mPART1(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mPARTSUPP2[][delta_P_PARTKEY:int,delta_P_MFGR:string] += delta_COUNT_mPARTSUPP2:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([PARTSUPP_PARTKEY:int], 
         (COUNT_mPART1_L2_1(int)[]
          [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
           {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))) *
      (__domain_1:int ^= 0))) *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  -1 * delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER2(int)[][PARTSUPP_PARTKEY:int, delta_P_MFGR:string, PARTSUPP_SUPPKEY:int, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} * -1 *
  delta_COUNT_mPARTSUPP2:int);
   COUNT_mSUPPLIER9(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, delta_P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_PARTKEY:int = delta_P_PARTKEY:int} *
  COUNT_mPARTSUPP1(int)[]
  [S_ACCTBAL:float, S_NAME:string, N_NAME:string, S_ADDRESS:string,
     S_PHONE:string, S_COMMENT:string, PARTSUPP_SUPPKEY:int] *
  -1 * delta_COUNT_mPARTSUPP2:int);
}

CORRECT COUNT_mPARTSUPP1[][delta_S_ACCTBAL:float,delta_S_NAME:string,delta_N_NAME:string,delta_S_ADDRESS:string,delta_S_PHONE:string,delta_S_COMMENT:string,delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP1:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] *
  AggSum([], 
    ((__domain_1:int ^=
       AggSum([PARTSUPP_PARTKEY:int], 
         (COUNT_mPART1_L2_1(int)[]
          [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
           {PS2_SUPPLYCOST:float < PARTSUPP_SUPPLYCOST:float}))) *
      (__domain_1:int ^= 0))) *
  -1 * delta_COUNT_mPARTSUPP1:int);
   COUNT_mSUPPLIER9(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  COUNT_mPARTSUPP2(int)[][PARTSUPP_PARTKEY:int, P_MFGR:string] * -1 *
  delta_COUNT_mPARTSUPP1:int);
   COUNT_mPART1(int)[][delta_S_ACCTBAL:float, delta_S_NAME:string, delta_N_NAME:string, PARTSUPP_PARTKEY:int, delta_S_ADDRESS:string, delta_S_PHONE:string, delta_S_COMMENT:string, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_COUNT_mPARTSUPP1:int);
}

CORRECT COUNT_mPARTSUPP4_L2_2[][delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int] += delta_COUNT_mPARTSUPP4_L2_2:int FOR ON - PARTSUPP(PARTSUPP_PARTKEY:int, PARTSUPP_SUPPKEY:int, PARTSUPP_AVAILQTY:int, PARTSUPP_SUPPLYCOST:float, PARTSUPP_COMMENT:string) {
   COUNT(int)[][S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} *
  AggSum([S_ACCTBAL:float, S_NAME:string, N_NAME:string,
            PARTSUPP_PARTKEY:int, P_MFGR:string, S_ADDRESS:string,
            S_PHONE:string, S_COMMENT:string], 
    ((__domain_1:int ^= 0) *
      COUNT_mSUPPLIER9(int)[]
      [S_ACCTBAL:float, S_NAME:string, N_NAME:string, PARTSUPP_PARTKEY:int,
         P_MFGR:string, S_ADDRESS:string, S_PHONE:string, S_COMMENT:string,
         PS_SUPPLYCOST:float] *
      ((__domain_1:int ^=
         (({PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float} *
            (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] +
              delta_COUNT_mPARTSUPP4_L2_2:int) *
            -1) +
           AggSum([PARTSUPP_PARTKEY:int], 
             (COUNT_mPART1_L2_1(int)[]
              [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
               {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})))) +
        ((__domain_1:int ^=
           (AggSum([PARTSUPP_PARTKEY:int], 
              (COUNT_mPART1_L2_1(int)[]
               [PARTSUPP_PARTKEY:int, PS2_SUPPLYCOST:float] *
                {PS2_SUPPLYCOST:float < PS_SUPPLYCOST:float})) +
             (COUNT_mPARTSUPP4_L2_2(int)[][PARTSUPP_SUPPKEY:int] *
               {PARTSUPP_SUPPLYCOST:float < PS_SUPPLYCOST:float} * -1))) *
          -1)))));
   COUNT_mPART1_L2_1(int)[][PARTSUPP_PARTKEY:int, PARTSUPP_SUPPLYCOST:float] += 
  ({PARTSUPP_SUPPKEY:int = delta_COUNT_mPARTSUPPPARTSUPP_SUPPKEY:int} * 
-1 * delta_COUNT_mPARTSUPP4_L2_2:int);
}
