-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM PART(PART_PARTKEY int, PART_NAME string, PART_MFGR string, PART_BRAND string, PART_TYPE string, PART_SIZE int, PART_CONTAINER string, PART_RETAILPRICE float, PART_COMMENT string)
  FROM FILE 'data/tpch/part.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP AVG_YEARLY(float)[][] := 
(AggSum([], 
   ((P_BRAND:string ^= 'Brand#23') * (P_CONTAINER:string ^= 'MED BOX') *
     LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
                L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
                L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
                L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
                L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
     PART(L_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
            P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
            P_RETAILPRICE:float, P_COMMENT:string) *
     AggSum([], 
       ((__sql_inline_agg_2:float ^=
          (AggSum([L_PARTKEY:int], 
             (LINEITEM(L2_ORDERKEY:int, L_PARTKEY:int, L2_SUPPKEY:int,
                         L2_LINENUMBER:int, L2_QUANTITY:float,
                         L2_EXTENDEDPRICE:float, L2_DISCOUNT:float,
                         L2_TAX:float, L2_RETURNFLAG:string,
                         L2_LINESTATUS:string, L2_SHIPDATE:date,
                         L2_COMMITDATE:date, L2_RECEIPTDATE:date,
                         L2_SHIPINSTRUCT:string, L2_SHIPMODE:string,
                         L2_COMMENT:string) *
               L2_QUANTITY:float)) *
            AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 AggSum([L_PARTKEY:int], 
                   LINEITEM(L2_ORDERKEY:int, L_PARTKEY:int, L2_SUPPKEY:int,
                              L2_LINENUMBER:int, L2_QUANTITY:float,
                              L2_EXTENDEDPRICE:float, L2_DISCOUNT:float,
                              L2_TAX:float, L2_RETURNFLAG:string,
                              L2_LINESTATUS:string, L2_SHIPDATE:date,
                              L2_COMMITDATE:date, L2_RECEIPTDATE:date,
                              L2_SHIPINSTRUCT:string, L2_SHIPMODE:string,
                              L2_COMMENT:string))) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            0.2)) *
         {L_QUANTITY:float < __sql_inline_agg_2:float})) *
     L_EXTENDEDPRICE:float)) *
  0.142857142857);

DECLARE MAP AVG_YEARLY_pLINEITEM5(float)[]
[AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int, L_QUANTITY:float] := 
AggSum([AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int, L_QUANTITY:float], 
  (LINEITEM(L_ORDERKEY:int, AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int,
              L_SUPPKEY:int, L_LINENUMBER:int, L_QUANTITY:float,
              L_EXTENDEDPRICE:float, L_DISCOUNT:float, L_TAX:float,
              L_RETURNFLAG:string, L_LINESTATUS:string, L_SHIPDATE:date,
              L_COMMITDATE:date, L_RECEIPTDATE:date, L_SHIPINSTRUCT:string,
              L_SHIPMODE:string, L_COMMENT:string) *
    L_EXTENDEDPRICE:float));

DECLARE MAP AVG_YEARLY_mLINEITEM1(int)[][AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] := 
AggSum([AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int], 
  ((P_BRAND:string ^= 'Brand#23') * (P_CONTAINER:string ^= 'MED BOX') *
    PART(AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int, P_NAME:string,
           P_MFGR:string, P_BRAND:string, P_TYPE:string, P_SIZE:int,
           P_CONTAINER:string, P_RETAILPRICE:float, P_COMMENT:string)));

DECLARE MAP AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
[AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] := 
AggSum([AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int], 
  LINEITEM(L2_ORDERKEY:int, AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int,
             L2_SUPPKEY:int, L2_LINENUMBER:int, L2_QUANTITY:float,
             L2_EXTENDEDPRICE:float, L2_DISCOUNT:float, L2_TAX:float,
             L2_RETURNFLAG:string, L2_LINESTATUS:string, L2_SHIPDATE:date,
             L2_COMMITDATE:date, L2_RECEIPTDATE:date, L2_SHIPINSTRUCT:string,
             L2_SHIPMODE:string, L2_COMMENT:string));

DECLARE MAP AVG_YEARLY_mLINEITEM2_L1_2(float)[][AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] := 
AggSum([AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int], 
  (LINEITEM(L2_ORDERKEY:int, AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int,
              L2_SUPPKEY:int, L2_LINENUMBER:int, L2_QUANTITY:float,
              L2_EXTENDEDPRICE:float, L2_DISCOUNT:float, L2_TAX:float,
              L2_RETURNFLAG:string, L2_LINESTATUS:string, L2_SHIPDATE:date,
              L2_COMMITDATE:date, L2_RECEIPTDATE:date,
              L2_SHIPINSTRUCT:string, L2_SHIPMODE:string, L2_COMMENT:string) *
    L2_QUANTITY:float));

DECLARE MAP AVG_YEARLY_mLINEITEM5(float)[][L_PARTKEY:int, L_QUANTITY:float] := 
AggSum([L_PARTKEY:int, L_QUANTITY:float], 
  ((P_CONTAINER:string ^= 'MED BOX') * (P_BRAND:string ^= 'Brand#23') *
    PART(L_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
               L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
               L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
               L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
               L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    L_EXTENDEDPRICE:float));

-------------------- QUERIES --------------------
DECLARE QUERY AVG_YEARLY := AVG_YEARLY(float)[][];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  (AVG_YEARLY_mLINEITEM1(int)[][LINEITEM_PARTKEY:int] *
  ((((AggSum([], 
        ((__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                  [LINEITEM_PARTKEY:int] + 1)) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
               LINEITEM_QUANTITY:float) *
             0.2)) *
          {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
       LINEITEM_EXTENDEDPRICE:float) +
      AggSum([], 
        ((__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                  [LINEITEM_PARTKEY:int] + 1)) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
               LINEITEM_QUANTITY:float) *
             0.2)) *
          AVG_YEARLY_pLINEITEM5(float)[]
          [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
          {L_QUANTITY:float < __sql_inline_agg_2:float}))) *
     0.142857142857) +
    (AggSum([], 
       ((__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] * 
          0.2)) *
         AVG_YEARLY_pLINEITEM5(float)[]
         [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
         {L_QUANTITY:float < __sql_inline_agg_2:float})) *
      -0.142857142857)));
   AVG_YEARLY_pLINEITEM5(float)[][LINEITEM_PARTKEY:int, LINEITEM_QUANTITY:float] += LINEITEM_EXTENDEDPRICE:float;
   AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int] += 1;
   AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] += LINEITEM_QUANTITY:float;
   AVG_YEARLY_mLINEITEM5(float)[][LINEITEM_PARTKEY:int, LINEITEM_QUANTITY:float] += 
  (AVG_YEARLY_mLINEITEM1(int)[][LINEITEM_PARTKEY:int] *
  LINEITEM_EXTENDEDPRICE:float);
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ((((AVG_YEARLY_mLINEITEM1(int)[][LINEITEM_PARTKEY:int] *
     AggSum([], 
       ((__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int] +
                  -1)) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            ((AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] *
               0.2) +
              (-0.2 * LINEITEM_QUANTITY:float)))) *
         {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
     LINEITEM_EXTENDEDPRICE:float) +
    AggSum([], 
      (AVG_YEARLY_mLINEITEM5(float)[][L_PARTKEY:int, L_QUANTITY:float] *
        (__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][L_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            AVG_YEARLY_mLINEITEM2_L1_2(float)[][L_PARTKEY:int] * 0.2)) *
        {L_QUANTITY:float < __sql_inline_agg_2:float}))) *
   -0.142857142857) +
  (AggSum([], 
     (AVG_YEARLY_mLINEITEM5(float)[][L_PARTKEY:int, L_QUANTITY:float] *
       (__sql_inline_agg_2:float ^=
         (AggSum([], 
            ((__sql_inline_average_count_1:int ^=
               (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][L_PARTKEY:int] +
                 ((L_PARTKEY:int ^= LINEITEM_PARTKEY:int) * -1))) *
              {0 != __sql_inline_average_count_1:int} *
              {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
           ((AVG_YEARLY_mLINEITEM2_L1_2(float)[][L_PARTKEY:int] * 0.2) +
             ((L_PARTKEY:int ^= LINEITEM_PARTKEY:int) * -0.2 *
               LINEITEM_QUANTITY:float)))) *
       {L_QUANTITY:float < __sql_inline_agg_2:float})) *
    0.142857142857));
   AVG_YEARLY_pLINEITEM5(float)[][LINEITEM_PARTKEY:int, LINEITEM_QUANTITY:float] += (-1 * LINEITEM_EXTENDEDPRICE:float);
   AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int] += -1;
   AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] += (-1 * LINEITEM_QUANTITY:float);
   AVG_YEARLY_mLINEITEM5(float)[][LINEITEM_PARTKEY:int, LINEITEM_QUANTITY:float] += 
  (AVG_YEARLY_mLINEITEM1(int)[][LINEITEM_PARTKEY:int] * -1 *
  LINEITEM_EXTENDEDPRICE:float);
}

ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    ((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_average_count_1:int ^=
             AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
            {0 != __sql_inline_average_count_1:int} *
            {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
         AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
      AVG_YEARLY_pLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float] *
      {L_QUANTITY:float < __sql_inline_agg_2:float})) *
  0.142857142857);
   AVG_YEARLY_mLINEITEM1(int)[][PART_PARTKEY:int] += ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'});
   AVG_YEARLY_mLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float] += 
  ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AVG_YEARLY_pLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float]);
}

ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    ((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_average_count_1:int ^=
             AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
            {0 != __sql_inline_average_count_1:int} *
            {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
         AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
      AVG_YEARLY_pLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float] *
      {L_QUANTITY:float < __sql_inline_agg_2:float})) *
  -0.142857142857);
   AVG_YEARLY_mLINEITEM1(int)[][PART_PARTKEY:int] += ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} * -1);
   AVG_YEARLY_mLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float] += 
  ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AVG_YEARLY_pLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float] * 
-1);
}

ON SYSTEM READY {
   AVG_YEARLY(float)[][] := 0.;
}

CORRECT AVG_YEARLY_mLINEITEM1[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  ((((AggSum([], 
        ((__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                  [LINEITEM_PARTKEY:int] + 1)) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
               LINEITEM_QUANTITY:float) *
             0.2)) *
          {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
       LINEITEM_EXTENDEDPRICE:float) +
      AggSum([], 
        ((__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                  [LINEITEM_PARTKEY:int] + 1)) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
               LINEITEM_QUANTITY:float) *
             0.2)) *
          AVG_YEARLY_pLINEITEM5(float)[]
          [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
          {L_QUANTITY:float < __sql_inline_agg_2:float}))) *
     0.142857142857) +
    (AggSum([], 
       ((__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] * 
          0.2)) *
         AVG_YEARLY_pLINEITEM5(float)[]
         [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
         {L_QUANTITY:float < __sql_inline_agg_2:float})) *
      -0.142857142857)) *
  delta_AVG_YEARLY_mLINEITEM1:int);
   AVG_YEARLY_mLINEITEM5(float)[][LINEITEM_PARTKEY:int, LINEITEM_QUANTITY:float] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  delta_AVG_YEARLY_mLINEITEM1:int * LINEITEM_EXTENDEDPRICE:float);
}

CORRECT AVG_YEARLY_mLINEITEM2_L1_1_L1_1[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  AVG_YEARLY_mLINEITEM1(int)[][LINEITEM_PARTKEY:int] *
  ((((AggSum([], 
        (((__sql_inline_agg_2:float ^=
            ((AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
               LINEITEM_QUANTITY:float) *
              (AggSum([], 
                 ((__sql_inline_average_count_1:int ^=
                    (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                     [LINEITEM_PARTKEY:int] + 1)) *
                   {0 != __sql_inline_average_count_1:int} *
                   {[/:float]([listmax:int](1,
                                              __sql_inline_average_count_1:int))})) +
                AggSum([], 
                  (((__sql_inline_average_count_1:int ^=
                      (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                       [LINEITEM_PARTKEY:int] +
                        {(1 + delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int)})) +
                     ((__sql_inline_average_count_1:int ^=
                        (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                         [LINEITEM_PARTKEY:int] + 1)) *
                       -1)) *
                    {0 != __sql_inline_average_count_1:int} *
                    {[/:float]([listmax:int](1,
                                               __sql_inline_average_count_1:int))}))) *
              0.2)) +
           ((__sql_inline_agg_2:float ^=
              (AggSum([], 
                 ((__sql_inline_average_count_1:int ^=
                    (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                     [LINEITEM_PARTKEY:int] + 1)) *
                   {0 != __sql_inline_average_count_1:int} *
                   {[/:float]([listmax:int](1,
                                              __sql_inline_average_count_1:int))})) *
                (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
                  LINEITEM_QUANTITY:float) *
                0.2)) *
             -1)) *
          {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
       LINEITEM_EXTENDEDPRICE:float) +
      AggSum([], 
        (((__sql_inline_agg_2:float ^=
            ((AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
               LINEITEM_QUANTITY:float) *
              (AggSum([], 
                 ((__sql_inline_average_count_1:int ^=
                    (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                     [LINEITEM_PARTKEY:int] + 1)) *
                   {0 != __sql_inline_average_count_1:int} *
                   {[/:float]([listmax:int](1,
                                              __sql_inline_average_count_1:int))})) +
                AggSum([], 
                  (((__sql_inline_average_count_1:int ^=
                      (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                       [LINEITEM_PARTKEY:int] +
                        {(1 + delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int)})) +
                     ((__sql_inline_average_count_1:int ^=
                        (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                         [LINEITEM_PARTKEY:int] + 1)) *
                       -1)) *
                    {0 != __sql_inline_average_count_1:int} *
                    {[/:float]([listmax:int](1,
                                               __sql_inline_average_count_1:int))}))) *
              0.2)) +
           ((__sql_inline_agg_2:float ^=
              (AggSum([], 
                 ((__sql_inline_average_count_1:int ^=
                    (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                     [LINEITEM_PARTKEY:int] + 1)) *
                   {0 != __sql_inline_average_count_1:int} *
                   {[/:float]([listmax:int](1,
                                              __sql_inline_average_count_1:int))})) *
                (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
                  LINEITEM_QUANTITY:float) *
                0.2)) *
             -1)) *
          AVG_YEARLY_pLINEITEM5(float)[]
          [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
          {L_QUANTITY:float < __sql_inline_agg_2:float}))) *
     0.142857142857) +
    (AggSum([], 
       (((__sql_inline_agg_2:float ^=
           (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] *
             (AggSum([], 
                ((__sql_inline_average_count_1:int ^=
                   AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [LINEITEM_PARTKEY:int]) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))})) +
               AggSum([], 
                 (((__sql_inline_average_count_1:int ^=
                     (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                      [LINEITEM_PARTKEY:int] +
                       delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int)) +
                    ((__sql_inline_average_count_1:int ^=
                       AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                       [LINEITEM_PARTKEY:int]) *
                      -1)) *
                   {0 != __sql_inline_average_count_1:int} *
                   {[/:float]([listmax:int](1,
                                              __sql_inline_average_count_1:int))}))) *
             0.2)) +
          ((__sql_inline_agg_2:float ^=
             (AggSum([], 
                ((__sql_inline_average_count_1:int ^=
                   AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [LINEITEM_PARTKEY:int]) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))})) *
               AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] *
               0.2)) *
            -1)) *
         AVG_YEARLY_pLINEITEM5(float)[]
         [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
         {L_QUANTITY:float < __sql_inline_agg_2:float})) *
      -0.142857142857)));
}

CORRECT AVG_YEARLY_mLINEITEM2_L1_2[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM2_L1_2:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  AVG_YEARLY_mLINEITEM1(int)[][LINEITEM_PARTKEY:int] *
  ((((AggSum([], 
        (((__sql_inline_agg_2:float ^=
            (AggSum([], 
               ((__sql_inline_average_count_1:int ^=
                  (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [LINEITEM_PARTKEY:int] + 1)) *
                 {0 != __sql_inline_average_count_1:int} *
                 {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
              (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
                LINEITEM_QUANTITY:float +
                delta_AVG_YEARLY_mLINEITEM2_L1_2:float) *
              0.2)) +
           ((__sql_inline_agg_2:float ^=
              (AggSum([], 
                 ((__sql_inline_average_count_1:int ^=
                    (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                     [LINEITEM_PARTKEY:int] + 1)) *
                   {0 != __sql_inline_average_count_1:int} *
                   {[/:float]([listmax:int](1,
                                              __sql_inline_average_count_1:int))})) *
                (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
                  LINEITEM_QUANTITY:float) *
                0.2)) *
             -1)) *
          {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
       LINEITEM_EXTENDEDPRICE:float) +
      AggSum([], 
        (((__sql_inline_agg_2:float ^=
            (AggSum([], 
               ((__sql_inline_average_count_1:int ^=
                  (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [LINEITEM_PARTKEY:int] + 1)) *
                 {0 != __sql_inline_average_count_1:int} *
                 {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
              (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
                LINEITEM_QUANTITY:float +
                delta_AVG_YEARLY_mLINEITEM2_L1_2:float) *
              0.2)) +
           ((__sql_inline_agg_2:float ^=
              (AggSum([], 
                 ((__sql_inline_average_count_1:int ^=
                    (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                     [LINEITEM_PARTKEY:int] + 1)) *
                   {0 != __sql_inline_average_count_1:int} *
                   {[/:float]([listmax:int](1,
                                              __sql_inline_average_count_1:int))})) *
                (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
                  LINEITEM_QUANTITY:float) *
                0.2)) *
             -1)) *
          AVG_YEARLY_pLINEITEM5(float)[]
          [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
          {L_QUANTITY:float < __sql_inline_agg_2:float}))) *
     0.142857142857) +
    (AggSum([], 
       (((__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int]) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
               delta_AVG_YEARLY_mLINEITEM2_L1_2:float) *
             0.2)) +
          ((__sql_inline_agg_2:float ^=
             (AggSum([], 
                ((__sql_inline_average_count_1:int ^=
                   AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [LINEITEM_PARTKEY:int]) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))})) *
               AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] *
               0.2)) *
            -1)) *
         AVG_YEARLY_pLINEITEM5(float)[]
         [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
         {L_QUANTITY:float < __sql_inline_agg_2:float})) *
      -0.142857142857)));
}

CORRECT AVG_YEARLY_pLINEITEM5[][delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int,delta_L_QUANTITY:float] += delta_AVG_YEARLY_pLINEITEM5:float FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  (AVG_YEARLY_mLINEITEM1(int)[][LINEITEM_PARTKEY:int] *
  ((AggSum([], 
      ((LINEITEM_PARTKEY:int ^=
         delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int) *
        (__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int] +
                  1)) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
              LINEITEM_QUANTITY:float) *
            0.2)) *
        {delta_L_QUANTITY:float < __sql_inline_agg_2:float})) *
     0.142857142857) +
    (AggSum([], 
       ((LINEITEM_PARTKEY:int ^=
          delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int) *
         (__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int]) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] * 
           0.2)) *
         {delta_L_QUANTITY:float < __sql_inline_agg_2:float})) *
      -0.142857142857)) *
  delta_AVG_YEARLY_pLINEITEM5:float);
}

CORRECT AVG_YEARLY_mLINEITEM1[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  ((((AggSum([], 
        ((__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                  [LINEITEM_PARTKEY:int] + 1)) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
               LINEITEM_QUANTITY:float) *
             0.2)) *
          {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
       LINEITEM_EXTENDEDPRICE:float) +
      AggSum([], 
        ((__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                  [LINEITEM_PARTKEY:int] + 1)) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             (AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
               LINEITEM_QUANTITY:float) *
             0.2)) *
          AVG_YEARLY_pLINEITEM5(float)[]
          [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
          {L_QUANTITY:float < __sql_inline_agg_2:float}))) *
     0.142857142857) +
    (AggSum([], 
       ((__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] * 
          0.2)) *
         AVG_YEARLY_pLINEITEM5(float)[]
         [LINEITEM_PARTKEY:int, L_QUANTITY:float] *
         {L_QUANTITY:float < __sql_inline_agg_2:float})) *
      -0.142857142857)) *
  delta_AVG_YEARLY_mLINEITEM1:int);
   AVG_YEARLY_mLINEITEM5(float)[][LINEITEM_PARTKEY:int, LINEITEM_QUANTITY:float] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  delta_AVG_YEARLY_mLINEITEM1:int * LINEITEM_EXTENDEDPRICE:float);
}

CORRECT AVG_YEARLY_mLINEITEM1[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  AggSum([], 
    ((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_average_count_1:int ^=
             (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int] +
               -1)) *
            {0 != __sql_inline_average_count_1:int} *
            {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
         ((AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] * 0.2) +
           (-0.2 * LINEITEM_QUANTITY:float)))) *
      {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
  -0.142857142857 * delta_AVG_YEARLY_mLINEITEM1:int *
  LINEITEM_EXTENDEDPRICE:float);
   AVG_YEARLY_mLINEITEM5(float)[][LINEITEM_PARTKEY:int, LINEITEM_QUANTITY:float] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  -1 * delta_AVG_YEARLY_mLINEITEM1:int * LINEITEM_EXTENDEDPRICE:float);
}

CORRECT AVG_YEARLY_mLINEITEM2_L1_1_L1_1[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  (((((LINEITEM_PARTKEY:int ^= delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int) *
     AVG_YEARLY_mLINEITEM1(int)[][LINEITEM_PARTKEY:int] *
     AggSum([], 
       (((__sql_inline_agg_2:float ^=
           (((AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] *
               0.2) +
              (-0.2 * LINEITEM_QUANTITY:float)) *
             (AggSum([], 
                ((__sql_inline_average_count_1:int ^=
                   (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                    [LINEITEM_PARTKEY:int] + -1)) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))})) +
               AggSum([], 
                 (((__sql_inline_average_count_1:int ^=
                     (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                      [LINEITEM_PARTKEY:int] +
                       {(-1 + delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int)})) +
                    ((__sql_inline_average_count_1:int ^=
                       (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                        [LINEITEM_PARTKEY:int] + -1)) *
                      -1)) *
                   {0 != __sql_inline_average_count_1:int} *
                   {[/:float]([listmax:int](1,
                                              __sql_inline_average_count_1:int))}))))) +
          ((__sql_inline_agg_2:float ^=
             (AggSum([], 
                ((__sql_inline_average_count_1:int ^=
                   (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                    [LINEITEM_PARTKEY:int] + -1)) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))})) *
               ((AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] *
                  0.2) +
                 (-0.2 * LINEITEM_QUANTITY:float)))) *
            -1)) *
         {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
     LINEITEM_EXTENDEDPRICE:float) +
    AggSum([delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int], 
      (AVG_YEARLY_mLINEITEM5(float)[]
       [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int, L_QUANTITY:float] *
        ((__sql_inline_agg_2:float ^=
           (AVG_YEARLY_mLINEITEM2_L1_2(float)[]
            [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] *
             (AggSum([], 
                ((__sql_inline_average_count_1:int ^=
                   AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int]) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))})) +
               AggSum([], 
                 (((__sql_inline_average_count_1:int ^=
                     (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                      [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] +
                       delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int)) +
                    ((__sql_inline_average_count_1:int ^=
                       AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                       [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int]) *
                      -1)) *
                   {0 != __sql_inline_average_count_1:int} *
                   {[/:float]([listmax:int](1,
                                              __sql_inline_average_count_1:int))}))) *
             0.2)) +
          ((__sql_inline_agg_2:float ^=
             (AggSum([], 
                ((__sql_inline_average_count_1:int ^=
                   AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int]) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))})) *
               AVG_YEARLY_mLINEITEM2_L1_2(float)[]
               [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] * 0.2)) *
            -1)) *
        {L_QUANTITY:float < __sql_inline_agg_2:float}))) *
   -0.142857142857) +
  (AggSum([delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int], 
     (AVG_YEARLY_mLINEITEM5(float)[]
      [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int, L_QUANTITY:float] *
       ((__sql_inline_agg_2:float ^=
          (((AVG_YEARLY_mLINEITEM2_L1_2(float)[]
             [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] * 0.2) +
             ({LINEITEM_PARTKEY:int =
              delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} * -0.2 *
               LINEITEM_QUANTITY:float)) *
            (AggSum([], 
               ((__sql_inline_average_count_1:int ^=
                  (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] +
                    ({LINEITEM_PARTKEY:int =
                     delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} * 
                    -1))) *
                 {0 != __sql_inline_average_count_1:int} *
                 {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) +
              AggSum([], 
                (((__sql_inline_average_count_1:int ^=
                    (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                     [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] +
                      ({LINEITEM_PARTKEY:int =
                       delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} * 
                      -1) + delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int)) +
                   ((__sql_inline_average_count_1:int ^=
                      (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                       [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] +
                        ({LINEITEM_PARTKEY:int =
                         delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
                          -1))) *
                     -1)) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))}))))) +
         ((__sql_inline_agg_2:float ^=
            (AggSum([], 
               ((__sql_inline_average_count_1:int ^=
                  (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] +
                    ({LINEITEM_PARTKEY:int =
                     delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} * 
                    -1))) *
                 {0 != __sql_inline_average_count_1:int} *
                 {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
              ((AVG_YEARLY_mLINEITEM2_L1_2(float)[]
                [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] * 0.2) +
                ({LINEITEM_PARTKEY:int =
                 delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} * -0.2 *
                  LINEITEM_QUANTITY:float)))) *
           -1)) *
       {L_QUANTITY:float < __sql_inline_agg_2:float})) *
    0.142857142857));
}

CORRECT AVG_YEARLY_mLINEITEM2_L1_2[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM2_L1_2:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  (((((LINEITEM_PARTKEY:int ^= delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int) *
     AVG_YEARLY_mLINEITEM1(int)[][LINEITEM_PARTKEY:int] *
     AggSum([], 
       (((__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                  [LINEITEM_PARTKEY:int] + -1)) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             (((AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] +
                 delta_AVG_YEARLY_mLINEITEM2_L1_2:float) *
                0.2) +
               (-0.2 * LINEITEM_QUANTITY:float)))) +
          ((__sql_inline_agg_2:float ^=
             (AggSum([], 
                ((__sql_inline_average_count_1:int ^=
                   (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                    [LINEITEM_PARTKEY:int] + -1)) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))})) *
               ((AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] *
                  0.2) +
                 (-0.2 * LINEITEM_QUANTITY:float)))) *
            -1)) *
         {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
     LINEITEM_EXTENDEDPRICE:float) +
    AggSum([delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int], 
      (AVG_YEARLY_mLINEITEM5(float)[]
       [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int, L_QUANTITY:float] *
        ((__sql_inline_agg_2:float ^=
           (AggSum([], 
              ((__sql_inline_average_count_1:int ^=
                 AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                 [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int]) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
             (AVG_YEARLY_mLINEITEM2_L1_2(float)[]
              [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] +
               delta_AVG_YEARLY_mLINEITEM2_L1_2:float) *
             0.2)) +
          ((__sql_inline_agg_2:float ^=
             (AggSum([], 
                ((__sql_inline_average_count_1:int ^=
                   AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int]) *
                  {0 != __sql_inline_average_count_1:int} *
                  {[/:float]([listmax:int](1,
                                             __sql_inline_average_count_1:int))})) *
               AVG_YEARLY_mLINEITEM2_L1_2(float)[]
               [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] * 0.2)) *
            -1)) *
        {L_QUANTITY:float < __sql_inline_agg_2:float}))) *
   -0.142857142857) +
  (AggSum([delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int], 
     (AVG_YEARLY_mLINEITEM5(float)[]
      [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int, L_QUANTITY:float] *
       ((__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                 [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] +
                  ({LINEITEM_PARTKEY:int =
                   delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} * 
                  -1))) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            (((AVG_YEARLY_mLINEITEM2_L1_2(float)[]
               [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] +
                delta_AVG_YEARLY_mLINEITEM2_L1_2:float) *
               0.2) +
              ({LINEITEM_PARTKEY:int =
               delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} * -0.2 *
                LINEITEM_QUANTITY:float)))) +
         ((__sql_inline_agg_2:float ^=
            (AggSum([], 
               ((__sql_inline_average_count_1:int ^=
                  (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[]
                   [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] +
                    ({LINEITEM_PARTKEY:int =
                     delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} * 
                    -1))) *
                 {0 != __sql_inline_average_count_1:int} *
                 {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
              ((AVG_YEARLY_mLINEITEM2_L1_2(float)[]
                [delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] * 0.2) +
                ({LINEITEM_PARTKEY:int =
                 delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} * -0.2 *
                  LINEITEM_QUANTITY:float)))) *
           -1)) *
       {L_QUANTITY:float < __sql_inline_agg_2:float})) *
    0.142857142857));
}

CORRECT AVG_YEARLY_mLINEITEM5[][delta_L_PARTKEY:int,delta_L_QUANTITY:float] += delta_AVG_YEARLY_mLINEITEM5:float FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  (((AggSum([], 
     ((__sql_inline_agg_2:float ^=
        (AggSum([], 
           ((__sql_inline_average_count_1:int ^=
              AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][delta_L_PARTKEY:int]) *
             {0 != __sql_inline_average_count_1:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
          AVG_YEARLY_mLINEITEM2_L1_2(float)[][delta_L_PARTKEY:int] * 
        0.2)) *
       {delta_L_QUANTITY:float < __sql_inline_agg_2:float})) *
    -0.142857142857) +
   (AggSum([], 
      ((__sql_inline_agg_2:float ^=
         (AggSum([], 
            ((__sql_inline_average_count_1:int ^=
               (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][delta_L_PARTKEY:int] +
                 ({LINEITEM_PARTKEY:int = delta_L_PARTKEY:int} * -1))) *
              {0 != __sql_inline_average_count_1:int} *
              {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
           ((AVG_YEARLY_mLINEITEM2_L1_2(float)[][delta_L_PARTKEY:int] * 0.2) +
             ({LINEITEM_PARTKEY:int = delta_L_PARTKEY:int} * -0.2 *
               LINEITEM_QUANTITY:float)))) *
        {delta_L_QUANTITY:float < __sql_inline_agg_2:float})) *
     0.142857142857)) *
  delta_AVG_YEARLY_mLINEITEM5:float);
}

CORRECT AVG_YEARLY_mLINEITEM1[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  AggSum([], 
    ((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_average_count_1:int ^=
             (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][LINEITEM_PARTKEY:int] +
               -1)) *
            {0 != __sql_inline_average_count_1:int} *
            {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
         ((AVG_YEARLY_mLINEITEM2_L1_2(float)[][LINEITEM_PARTKEY:int] * 0.2) +
           (-0.2 * LINEITEM_QUANTITY:float)))) *
      {LINEITEM_QUANTITY:float < __sql_inline_agg_2:float})) *
  -0.142857142857 * delta_AVG_YEARLY_mLINEITEM1:int *
  LINEITEM_EXTENDEDPRICE:float);
   AVG_YEARLY_mLINEITEM5(float)[][LINEITEM_PARTKEY:int, LINEITEM_QUANTITY:float] += 
  ({LINEITEM_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  -1 * delta_AVG_YEARLY_mLINEITEM1:int * LINEITEM_EXTENDEDPRICE:float);
}

CORRECT AVG_YEARLY_mLINEITEM2_L1_1_L1_1[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  {PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    (((__sql_inline_agg_2:float ^=
        (AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] *
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) +
            AggSum([], 
              (((__sql_inline_average_count_1:int ^=
                  (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int] +
                    delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int)) +
                 ((__sql_inline_average_count_1:int ^=
                    AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
                   -1)) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))}))) *
          0.2)) +
       ((__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
         -1)) *
      AVG_YEARLY_pLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float] *
      {L_QUANTITY:float < __sql_inline_agg_2:float})) *
  0.142857142857);
}

CORRECT AVG_YEARLY_mLINEITEM2_L1_2[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM2_L1_2:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  {PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    (((__sql_inline_agg_2:float ^=
        (AggSum([], 
           ((__sql_inline_average_count_1:int ^=
              AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
             {0 != __sql_inline_average_count_1:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
          (AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] +
            delta_AVG_YEARLY_mLINEITEM2_L1_2:float) *
          0.2)) +
       ((__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
         -1)) *
      AVG_YEARLY_pLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float] *
      {L_QUANTITY:float < __sql_inline_agg_2:float})) *
  0.142857142857);
}

CORRECT AVG_YEARLY_pLINEITEM5[][delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int,delta_L_QUANTITY:float] += delta_AVG_YEARLY_pLINEITEM5:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    ((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_average_count_1:int ^=
             AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
            {0 != __sql_inline_average_count_1:int} *
            {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
         AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
      (PART_PARTKEY:int ^= delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int) *
      {delta_L_QUANTITY:float < __sql_inline_agg_2:float})) *
  0.142857142857 * delta_AVG_YEARLY_pLINEITEM5:float);
   AVG_YEARLY_mLINEITEM5(float)[][PART_PARTKEY:int, delta_L_QUANTITY:float] += 
  ({PART_PARTKEY:int = delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int} *
  {PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  delta_AVG_YEARLY_pLINEITEM5:float);
}

CORRECT AVG_YEARLY_pLINEITEM5[][delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int,delta_L_QUANTITY:float] += delta_AVG_YEARLY_pLINEITEM5:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    ((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_average_count_1:int ^=
             AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
            {0 != __sql_inline_average_count_1:int} *
            {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
         AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
      (PART_PARTKEY:int ^= delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int) *
      {delta_L_QUANTITY:float < __sql_inline_agg_2:float})) *
  0.142857142857 * delta_AVG_YEARLY_pLINEITEM5:float);
   AVG_YEARLY_mLINEITEM5(float)[][PART_PARTKEY:int, delta_L_QUANTITY:float] += 
  ({PART_PARTKEY:int = delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int} *
  {PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  delta_AVG_YEARLY_pLINEITEM5:float);
}

CORRECT AVG_YEARLY_mLINEITEM2_L1_1_L1_1[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  {PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    (((__sql_inline_agg_2:float ^=
        (AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] *
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) +
            AggSum([], 
              (((__sql_inline_average_count_1:int ^=
                  (AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int] +
                    delta_AVG_YEARLY_mLINEITEM2_L1_1_L1_1:int)) +
                 ((__sql_inline_average_count_1:int ^=
                    AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
                   -1)) *
                {0 != __sql_inline_average_count_1:int} *
                {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))}))) *
          0.2)) +
       ((__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
         -1)) *
      AVG_YEARLY_pLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float] *
      {L_QUANTITY:float < __sql_inline_agg_2:float})) *
  -0.142857142857);
}

CORRECT AVG_YEARLY_mLINEITEM2_L1_2[][delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int] += delta_AVG_YEARLY_mLINEITEM2_L1_2:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_PARTKEY:int = delta_AVG_YEARLY_mLINEITEMLINEITEM_PARTKEY:int} *
  {PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    (((__sql_inline_agg_2:float ^=
        (AggSum([], 
           ((__sql_inline_average_count_1:int ^=
              AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
             {0 != __sql_inline_average_count_1:int} *
             {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
          (AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] +
            delta_AVG_YEARLY_mLINEITEM2_L1_2:float) *
          0.2)) +
       ((__sql_inline_agg_2:float ^=
          (AggSum([], 
             ((__sql_inline_average_count_1:int ^=
                AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
               {0 != __sql_inline_average_count_1:int} *
               {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
            AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
         -1)) *
      AVG_YEARLY_pLINEITEM5(float)[][PART_PARTKEY:int, L_QUANTITY:float] *
      {L_QUANTITY:float < __sql_inline_agg_2:float})) *
  -0.142857142857);
}

CORRECT AVG_YEARLY_pLINEITEM5[][delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int,delta_L_QUANTITY:float] += delta_AVG_YEARLY_pLINEITEM5:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    ((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_average_count_1:int ^=
             AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
            {0 != __sql_inline_average_count_1:int} *
            {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
         AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
      (PART_PARTKEY:int ^= delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int) *
      {delta_L_QUANTITY:float < __sql_inline_agg_2:float})) *
  -0.142857142857 * delta_AVG_YEARLY_pLINEITEM5:float);
   AVG_YEARLY_mLINEITEM5(float)[][PART_PARTKEY:int, delta_L_QUANTITY:float] += 
  ({PART_PARTKEY:int = delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int} *
  {PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  -1 * delta_AVG_YEARLY_pLINEITEM5:float);
}

CORRECT AVG_YEARLY_pLINEITEM5[][delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int,delta_L_QUANTITY:float] += delta_AVG_YEARLY_pLINEITEM5:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   AVG_YEARLY(float)[][] += 
  ({PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  AggSum([], 
    ((__sql_inline_agg_2:float ^=
       (AggSum([], 
          ((__sql_inline_average_count_1:int ^=
             AVG_YEARLY_mLINEITEM2_L1_1_L1_1(int)[][PART_PARTKEY:int]) *
            {0 != __sql_inline_average_count_1:int} *
            {[/:float]([listmax:int](1, __sql_inline_average_count_1:int))})) *
         AVG_YEARLY_mLINEITEM2_L1_2(float)[][PART_PARTKEY:int] * 0.2)) *
      (PART_PARTKEY:int ^= delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int) *
      {delta_L_QUANTITY:float < __sql_inline_agg_2:float})) *
  -0.142857142857 * delta_AVG_YEARLY_pLINEITEM5:float);
   AVG_YEARLY_mLINEITEM5(float)[][PART_PARTKEY:int, delta_L_QUANTITY:float] += 
  ({PART_PARTKEY:int = delta_AVG_YEARLY_pLINEITEMLINEITEM_PARTKEY:int} *
  {PART_BRAND:string = 'Brand#23'} * {PART_CONTAINER:string = 'MED BOX'} *
  -1 * delta_AVG_YEARLY_pLINEITEM5:float);
}
