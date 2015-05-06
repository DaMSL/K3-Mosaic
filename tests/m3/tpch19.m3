-------------------- SOURCES --------------------
CREATE STREAM LINEITEM(LINEITEM_ORDERKEY int, LINEITEM_PARTKEY int, LINEITEM_SUPPKEY int, LINEITEM_LINENUMBER int, LINEITEM_QUANTITY float, LINEITEM_EXTENDEDPRICE float, LINEITEM_DISCOUNT float, LINEITEM_TAX float, LINEITEM_RETURNFLAG string, LINEITEM_LINESTATUS string, LINEITEM_SHIPDATE date, LINEITEM_COMMITDATE date, LINEITEM_RECEIPTDATE date, LINEITEM_SHIPINSTRUCT string, LINEITEM_SHIPMODE string, LINEITEM_COMMENT string)
  FROM FILE 'data/tpch/lineitem.csv' LINE DELIMITED
  CSV(delimiter := '|');

CREATE STREAM PART(PART_PARTKEY int, PART_NAME string, PART_MFGR string, PART_BRAND string, PART_TYPE string, PART_SIZE int, PART_CONTAINER string, PART_RETAILPRICE float, PART_COMMENT string)
  FROM FILE 'data/tpch/part.csv' LINE DELIMITED
  CSV(delimiter := '|');

--------------------- MAPS ----------------------
DECLARE MAP REVENUE(float)[][] := 
AggSum([], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    PART(P_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
           P_TYPE:string, P_SIZE:int, P_CONTAINER:string,
           P_RETAILPRICE:float, P_COMMENT:string) *
    AggSum([], 
      ((__sql_inline_or_7:int ^=
         ({L_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
           {P_PARTKEY:int = L_PARTKEY:int} *
           ({L_SHIPMODE:string = 'AIR'} + {L_SHIPMODE:string = 'AIR REG'}) *
           {P_SIZE:int >= 1} *
           (({P_BRAND:string = 'Brand#12'} * {P_SIZE:int <= 5} *
              {L_QUANTITY:float <= 11} * {L_QUANTITY:float >= 1} *
              ({P_CONTAINER:string = 'SM CASE'} +
                {P_CONTAINER:string = 'SM BOX'} +
                {P_CONTAINER:string = 'SM PACK'} +
                {P_CONTAINER:string = 'SM PKG'}) *
              2) +
             ({P_BRAND:string = 'Brand#34'} *
               ({P_CONTAINER:string = 'LG CASE'} +
                 {P_CONTAINER:string = 'LG BOX'} +
                 {P_CONTAINER:string = 'LG PACK'} +
                 {P_CONTAINER:string = 'LG PKG'}) *
               {L_QUANTITY:float >= 20} * {L_QUANTITY:float <= 30} *
               {P_SIZE:int <= 15})))) *
        {__sql_inline_or_7:int > 0})) *
    ((-1 * L_DISCOUNT:float) + 1) * L_EXTENDEDPRICE:float));

DECLARE MAP REVENUE_mPART1(float)[]
[L_QUANTITY:float, L_SHIPMODE:string, L_PARTKEY:int, L_SHIPINSTRUCT:string] := 
AggSum([L_QUANTITY:float, L_SHIPMODE:string, L_PARTKEY:int,
          L_SHIPINSTRUCT:string], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    L_DISCOUNT:float * L_EXTENDEDPRICE:float));

DECLARE MAP REVENUE_mPART2(float)[]
[L_QUANTITY:float, L_SHIPMODE:string, L_PARTKEY:int, L_SHIPINSTRUCT:string] := 
AggSum([L_QUANTITY:float, L_SHIPMODE:string, L_PARTKEY:int,
          L_SHIPINSTRUCT:string], 
  (LINEITEM(L_ORDERKEY:int, L_PARTKEY:int, L_SUPPKEY:int, L_LINENUMBER:int,
              L_QUANTITY:float, L_EXTENDEDPRICE:float, L_DISCOUNT:float,
              L_TAX:float, L_RETURNFLAG:string, L_LINESTATUS:string,
              L_SHIPDATE:date, L_COMMITDATE:date, L_RECEIPTDATE:date,
              L_SHIPINSTRUCT:string, L_SHIPMODE:string, L_COMMENT:string) *
    L_EXTENDEDPRICE:float));

DECLARE MAP REVENUE_mLINEITEM1(int)[]
[P_CONTAINER:string, P_SIZE:int, P_BRAND:string, P_PARTKEY:int] := 
AggSum([P_CONTAINER:string, P_SIZE:int, P_BRAND:string, P_PARTKEY:int], 
  PART(P_PARTKEY:int, P_NAME:string, P_MFGR:string, P_BRAND:string,
         P_TYPE:string, P_SIZE:int, P_CONTAINER:string, P_RETAILPRICE:float,
         P_COMMENT:string));

-------------------- QUERIES --------------------
DECLARE QUERY REVENUE := REVENUE(float)[][];

------------------- TRIGGERS --------------------
ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   REVENUE(float)[][] += 
  (AggSum([], 
   (REVENUE_mLINEITEM1(int)[]
    [P_CONTAINER:string, P_SIZE:int, P_BRAND:string, P_PARTKEY:int] *
     (__sql_inline_or_7:int ^=
       ({LINEITEM_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
         {P_PARTKEY:int = LINEITEM_PARTKEY:int} *
         ({LINEITEM_SHIPMODE:string = 'AIR'} +
           {LINEITEM_SHIPMODE:string = 'AIR REG'}) *
         {P_SIZE:int >= 1} *
         (({P_BRAND:string = 'Brand#12'} * {P_SIZE:int <= 5} *
            {LINEITEM_QUANTITY:float <= 11} *
            {LINEITEM_QUANTITY:float >= 1} *
            ({P_CONTAINER:string = 'SM CASE'} +
              {P_CONTAINER:string = 'SM BOX'} +
              {P_CONTAINER:string = 'SM PACK'} +
              {P_CONTAINER:string = 'SM PKG'}) *
            2) +
           ({P_BRAND:string = 'Brand#34'} *
             ({P_CONTAINER:string = 'LG CASE'} +
               {P_CONTAINER:string = 'LG BOX'} +
               {P_CONTAINER:string = 'LG PACK'} +
               {P_CONTAINER:string = 'LG PKG'}) *
             {LINEITEM_QUANTITY:float >= 20} *
             {LINEITEM_QUANTITY:float <= 30} * {P_SIZE:int <= 15})))) *
     {__sql_inline_or_7:int > 0})) *
  ((-1 * LINEITEM_DISCOUNT:float) + 1) * LINEITEM_EXTENDEDPRICE:float);
   REVENUE_mPART1(float)[][LINEITEM_QUANTITY:float, LINEITEM_SHIPMODE:string, LINEITEM_PARTKEY:int, LINEITEM_SHIPINSTRUCT:string] += (LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   REVENUE_mPART2(float)[][LINEITEM_QUANTITY:float, LINEITEM_SHIPMODE:string, LINEITEM_PARTKEY:int, LINEITEM_SHIPINSTRUCT:string] += LINEITEM_EXTENDEDPRICE:float;
}

ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   REVENUE(float)[][] += 
  (AggSum([], 
   (REVENUE_mLINEITEM1(int)[]
    [P_CONTAINER:string, P_SIZE:int, P_BRAND:string, P_PARTKEY:int] *
     (__sql_inline_or_7:int ^=
       ({LINEITEM_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
         {P_PARTKEY:int = LINEITEM_PARTKEY:int} *
         ({LINEITEM_SHIPMODE:string = 'AIR'} +
           {LINEITEM_SHIPMODE:string = 'AIR REG'}) *
         {P_SIZE:int >= 1} *
         (({P_BRAND:string = 'Brand#12'} * {P_SIZE:int <= 5} *
            {LINEITEM_QUANTITY:float <= 11} *
            {LINEITEM_QUANTITY:float >= 1} *
            ({P_CONTAINER:string = 'SM CASE'} +
              {P_CONTAINER:string = 'SM BOX'} +
              {P_CONTAINER:string = 'SM PACK'} +
              {P_CONTAINER:string = 'SM PKG'}) *
            2) +
           ({P_BRAND:string = 'Brand#34'} *
             ({P_CONTAINER:string = 'LG CASE'} +
               {P_CONTAINER:string = 'LG BOX'} +
               {P_CONTAINER:string = 'LG PACK'} +
               {P_CONTAINER:string = 'LG PKG'}) *
             {LINEITEM_QUANTITY:float >= 20} *
             {LINEITEM_QUANTITY:float <= 30} * {P_SIZE:int <= 15})))) *
     {__sql_inline_or_7:int > 0})) *
  LINEITEM_EXTENDEDPRICE:float * {(-1 + LINEITEM_DISCOUNT:float)});
   REVENUE_mPART1(float)[][LINEITEM_QUANTITY:float, LINEITEM_SHIPMODE:string, LINEITEM_PARTKEY:int, LINEITEM_SHIPINSTRUCT:string] += (-1 * LINEITEM_DISCOUNT:float * LINEITEM_EXTENDEDPRICE:float);
   REVENUE_mPART2(float)[][LINEITEM_QUANTITY:float, LINEITEM_SHIPMODE:string, LINEITEM_PARTKEY:int, LINEITEM_SHIPINSTRUCT:string] += (-1 * LINEITEM_EXTENDEDPRICE:float);
}

ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   REVENUE(float)[][] += 
  ((AggSum([], 
    (REVENUE_mPART1(float)[]
     [L_QUANTITY:float, L_SHIPMODE:string, L_PARTKEY:int,
        L_SHIPINSTRUCT:string] *
      (__sql_inline_or_7:int ^=
        ({L_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
          {PART_PARTKEY:int = L_PARTKEY:int} *
          ({L_SHIPMODE:string = 'AIR'} + {L_SHIPMODE:string = 'AIR REG'}) *
          {PART_SIZE:int >= 1} *
          (({PART_BRAND:string = 'Brand#12'} * {PART_SIZE:int <= 5} *
             {L_QUANTITY:float <= 11} * {L_QUANTITY:float >= 1} *
             ({PART_CONTAINER:string = 'SM CASE'} +
               {PART_CONTAINER:string = 'SM BOX'} +
               {PART_CONTAINER:string = 'SM PACK'} +
               {PART_CONTAINER:string = 'SM PKG'}) *
             2) +
            ({PART_BRAND:string = 'Brand#34'} *
              ({PART_CONTAINER:string = 'LG CASE'} +
                {PART_CONTAINER:string = 'LG BOX'} +
                {PART_CONTAINER:string = 'LG PACK'} +
                {PART_CONTAINER:string = 'LG PKG'}) *
              {L_QUANTITY:float >= 20} * {L_QUANTITY:float <= 30} *
              {PART_SIZE:int <= 15})))) *
      {__sql_inline_or_7:int > 0})) *
   -1) +
  AggSum([], 
    (REVENUE_mPART2(float)[]
     [L_QUANTITY:float, L_SHIPMODE:string, L_PARTKEY:int,
        L_SHIPINSTRUCT:string] *
      (__sql_inline_or_7:int ^=
        ({L_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
          {PART_PARTKEY:int = L_PARTKEY:int} *
          ({L_SHIPMODE:string = 'AIR'} + {L_SHIPMODE:string = 'AIR REG'}) *
          {PART_SIZE:int >= 1} *
          (({PART_BRAND:string = 'Brand#12'} * {PART_SIZE:int <= 5} *
             {L_QUANTITY:float <= 11} * {L_QUANTITY:float >= 1} *
             ({PART_CONTAINER:string = 'SM CASE'} +
               {PART_CONTAINER:string = 'SM BOX'} +
               {PART_CONTAINER:string = 'SM PACK'} +
               {PART_CONTAINER:string = 'SM PKG'}) *
             2) +
            ({PART_BRAND:string = 'Brand#34'} *
              ({PART_CONTAINER:string = 'LG CASE'} +
                {PART_CONTAINER:string = 'LG BOX'} +
                {PART_CONTAINER:string = 'LG PACK'} +
                {PART_CONTAINER:string = 'LG PKG'}) *
              {L_QUANTITY:float >= 20} * {L_QUANTITY:float <= 30} *
              {PART_SIZE:int <= 15})))) *
      {__sql_inline_or_7:int > 0})));
   REVENUE_mLINEITEM1(int)[][PART_CONTAINER:string, PART_SIZE:int, PART_BRAND:string, PART_PARTKEY:int] += 1;
}

ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   REVENUE(float)[][] += 
  (AggSum([], 
   (REVENUE_mPART1(float)[]
    [L_QUANTITY:float, L_SHIPMODE:string, L_PARTKEY:int,
       L_SHIPINSTRUCT:string] *
     (__sql_inline_or_7:int ^=
       ({L_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
         {PART_PARTKEY:int = L_PARTKEY:int} *
         ({L_SHIPMODE:string = 'AIR'} + {L_SHIPMODE:string = 'AIR REG'}) *
         {PART_SIZE:int >= 1} *
         (({PART_BRAND:string = 'Brand#12'} * {PART_SIZE:int <= 5} *
            {L_QUANTITY:float <= 11} * {L_QUANTITY:float >= 1} *
            ({PART_CONTAINER:string = 'SM CASE'} +
              {PART_CONTAINER:string = 'SM BOX'} +
              {PART_CONTAINER:string = 'SM PACK'} +
              {PART_CONTAINER:string = 'SM PKG'}) *
            2) +
           ({PART_BRAND:string = 'Brand#34'} *
             ({PART_CONTAINER:string = 'LG CASE'} +
               {PART_CONTAINER:string = 'LG BOX'} +
               {PART_CONTAINER:string = 'LG PACK'} +
               {PART_CONTAINER:string = 'LG PKG'}) *
             {L_QUANTITY:float >= 20} * {L_QUANTITY:float <= 30} *
             {PART_SIZE:int <= 15})))) *
     {__sql_inline_or_7:int > 0})) +
  (AggSum([], 
     (REVENUE_mPART2(float)[]
      [L_QUANTITY:float, L_SHIPMODE:string, L_PARTKEY:int,
         L_SHIPINSTRUCT:string] *
       (__sql_inline_or_7:int ^=
         ({L_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
           {PART_PARTKEY:int = L_PARTKEY:int} *
           ({L_SHIPMODE:string = 'AIR'} + {L_SHIPMODE:string = 'AIR REG'}) *
           {PART_SIZE:int >= 1} *
           (({PART_BRAND:string = 'Brand#12'} * {PART_SIZE:int <= 5} *
              {L_QUANTITY:float <= 11} * {L_QUANTITY:float >= 1} *
              ({PART_CONTAINER:string = 'SM CASE'} +
                {PART_CONTAINER:string = 'SM BOX'} +
                {PART_CONTAINER:string = 'SM PACK'} +
                {PART_CONTAINER:string = 'SM PKG'}) *
              2) +
             ({PART_BRAND:string = 'Brand#34'} *
               ({PART_CONTAINER:string = 'LG CASE'} +
                 {PART_CONTAINER:string = 'LG BOX'} +
                 {PART_CONTAINER:string = 'LG PACK'} +
                 {PART_CONTAINER:string = 'LG PKG'}) *
               {L_QUANTITY:float >= 20} * {L_QUANTITY:float <= 30} *
               {PART_SIZE:int <= 15})))) *
       {__sql_inline_or_7:int > 0})) *
    -1));
   REVENUE_mLINEITEM1(int)[][PART_CONTAINER:string, PART_SIZE:int, PART_BRAND:string, PART_PARTKEY:int] += -1;
}

ON SYSTEM READY {
   REVENUE(float)[][] := 0.;
}

CORRECT REVENUE_mLINEITEM1[][delta_P_CONTAINER:string,delta_P_SIZE:int,delta_P_BRAND:string,delta_P_PARTKEY:int] += delta_REVENUE_mLINEITEM1:int FOR ON + LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   REVENUE(float)[][] += 
  (AggSum([], 
   ((__sql_inline_or_7:int ^=
      ({LINEITEM_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
        {delta_P_PARTKEY:int = LINEITEM_PARTKEY:int} *
        ({LINEITEM_SHIPMODE:string = 'AIR'} +
          {LINEITEM_SHIPMODE:string = 'AIR REG'}) *
        {delta_P_SIZE:int >= 1} *
        (({delta_P_BRAND:string = 'Brand#12'} * {delta_P_SIZE:int <= 5} *
           {LINEITEM_QUANTITY:float <= 11} * {LINEITEM_QUANTITY:float >= 1} *
           ({delta_P_CONTAINER:string = 'SM CASE'} +
             {delta_P_CONTAINER:string = 'SM BOX'} +
             {delta_P_CONTAINER:string = 'SM PACK'} +
             {delta_P_CONTAINER:string = 'SM PKG'}) *
           2) +
          ({delta_P_BRAND:string = 'Brand#34'} *
            ({delta_P_CONTAINER:string = 'LG CASE'} +
              {delta_P_CONTAINER:string = 'LG BOX'} +
              {delta_P_CONTAINER:string = 'LG PACK'} +
              {delta_P_CONTAINER:string = 'LG PKG'}) *
            {LINEITEM_QUANTITY:float >= 20} *
            {LINEITEM_QUANTITY:float <= 30} * {delta_P_SIZE:int <= 15})))) *
     {__sql_inline_or_7:int > 0})) *
  ((-1 * LINEITEM_DISCOUNT:float) + 1) * delta_REVENUE_mLINEITEM1:int *
  LINEITEM_EXTENDEDPRICE:float);
}

CORRECT REVENUE_mLINEITEM1[][delta_P_CONTAINER:string,delta_P_SIZE:int,delta_P_BRAND:string,delta_P_PARTKEY:int] += delta_REVENUE_mLINEITEM1:int FOR ON - LINEITEM(LINEITEM_ORDERKEY:int, LINEITEM_PARTKEY:int, LINEITEM_SUPPKEY:int, LINEITEM_LINENUMBER:int, LINEITEM_QUANTITY:float, LINEITEM_EXTENDEDPRICE:float, LINEITEM_DISCOUNT:float, LINEITEM_TAX:float, LINEITEM_RETURNFLAG:string, LINEITEM_LINESTATUS:string, LINEITEM_SHIPDATE:date, LINEITEM_COMMITDATE:date, LINEITEM_RECEIPTDATE:date, LINEITEM_SHIPINSTRUCT:string, LINEITEM_SHIPMODE:string, LINEITEM_COMMENT:string) {
   REVENUE(float)[][] += 
  (AggSum([], 
   ((__sql_inline_or_7:int ^=
      ({LINEITEM_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
        {delta_P_PARTKEY:int = LINEITEM_PARTKEY:int} *
        ({LINEITEM_SHIPMODE:string = 'AIR'} +
          {LINEITEM_SHIPMODE:string = 'AIR REG'}) *
        {delta_P_SIZE:int >= 1} *
        (({delta_P_BRAND:string = 'Brand#12'} * {delta_P_SIZE:int <= 5} *
           {LINEITEM_QUANTITY:float <= 11} * {LINEITEM_QUANTITY:float >= 1} *
           ({delta_P_CONTAINER:string = 'SM CASE'} +
             {delta_P_CONTAINER:string = 'SM BOX'} +
             {delta_P_CONTAINER:string = 'SM PACK'} +
             {delta_P_CONTAINER:string = 'SM PKG'}) *
           2) +
          ({delta_P_BRAND:string = 'Brand#34'} *
            ({delta_P_CONTAINER:string = 'LG CASE'} +
              {delta_P_CONTAINER:string = 'LG BOX'} +
              {delta_P_CONTAINER:string = 'LG PACK'} +
              {delta_P_CONTAINER:string = 'LG PKG'}) *
            {LINEITEM_QUANTITY:float >= 20} *
            {LINEITEM_QUANTITY:float <= 30} * {delta_P_SIZE:int <= 15})))) *
     {__sql_inline_or_7:int > 0})) *
  delta_REVENUE_mLINEITEM1:int * LINEITEM_EXTENDEDPRICE:float *
  {(-1 + LINEITEM_DISCOUNT:float)});
}

CORRECT REVENUE_mPART1[][delta_L_QUANTITY:float,delta_L_SHIPMODE:string,delta_L_PARTKEY:int,delta_L_SHIPINSTRUCT:string] += delta_REVENUE_mPART1:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   REVENUE(float)[][] += 
  (AggSum([], 
   ((__sql_inline_or_7:int ^=
      ({delta_L_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
        {PART_PARTKEY:int = delta_L_PARTKEY:int} *
        ({delta_L_SHIPMODE:string = 'AIR'} +
          {delta_L_SHIPMODE:string = 'AIR REG'}) *
        {PART_SIZE:int >= 1} *
        (({PART_BRAND:string = 'Brand#12'} * {PART_SIZE:int <= 5} *
           {delta_L_QUANTITY:float <= 11} * {delta_L_QUANTITY:float >= 1} *
           ({PART_CONTAINER:string = 'SM CASE'} +
             {PART_CONTAINER:string = 'SM BOX'} +
             {PART_CONTAINER:string = 'SM PACK'} +
             {PART_CONTAINER:string = 'SM PKG'}) *
           2) +
          ({PART_BRAND:string = 'Brand#34'} *
            ({PART_CONTAINER:string = 'LG CASE'} +
              {PART_CONTAINER:string = 'LG BOX'} +
              {PART_CONTAINER:string = 'LG PACK'} +
              {PART_CONTAINER:string = 'LG PKG'}) *
            {delta_L_QUANTITY:float >= 20} * {delta_L_QUANTITY:float <= 30} *
            {PART_SIZE:int <= 15})))) *
     {__sql_inline_or_7:int > 0})) *
  -1 * delta_REVENUE_mPART1:float);
}

CORRECT REVENUE_mPART2[][delta_L_QUANTITY:float,delta_L_SHIPMODE:string,delta_L_PARTKEY:int,delta_L_SHIPINSTRUCT:string] += delta_REVENUE_mPART2:float FOR ON + PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   REVENUE(float)[][] += 
  (AggSum([], 
   ((__sql_inline_or_7:int ^=
      ({delta_L_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
        {PART_PARTKEY:int = delta_L_PARTKEY:int} *
        ({delta_L_SHIPMODE:string = 'AIR'} +
          {delta_L_SHIPMODE:string = 'AIR REG'}) *
        {PART_SIZE:int >= 1} *
        (({PART_BRAND:string = 'Brand#12'} * {PART_SIZE:int <= 5} *
           {delta_L_QUANTITY:float <= 11} * {delta_L_QUANTITY:float >= 1} *
           ({PART_CONTAINER:string = 'SM CASE'} +
             {PART_CONTAINER:string = 'SM BOX'} +
             {PART_CONTAINER:string = 'SM PACK'} +
             {PART_CONTAINER:string = 'SM PKG'}) *
           2) +
          ({PART_BRAND:string = 'Brand#34'} *
            ({PART_CONTAINER:string = 'LG CASE'} +
              {PART_CONTAINER:string = 'LG BOX'} +
              {PART_CONTAINER:string = 'LG PACK'} +
              {PART_CONTAINER:string = 'LG PKG'}) *
            {delta_L_QUANTITY:float >= 20} * {delta_L_QUANTITY:float <= 30} *
            {PART_SIZE:int <= 15})))) *
     {__sql_inline_or_7:int > 0})) *
  delta_REVENUE_mPART2:float);
}

CORRECT REVENUE_mPART1[][delta_L_QUANTITY:float,delta_L_SHIPMODE:string,delta_L_PARTKEY:int,delta_L_SHIPINSTRUCT:string] += delta_REVENUE_mPART1:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   REVENUE(float)[][] += 
  (AggSum([], 
   ((__sql_inline_or_7:int ^=
      ({delta_L_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
        {PART_PARTKEY:int = delta_L_PARTKEY:int} *
        ({delta_L_SHIPMODE:string = 'AIR'} +
          {delta_L_SHIPMODE:string = 'AIR REG'}) *
        {PART_SIZE:int >= 1} *
        (({PART_BRAND:string = 'Brand#12'} * {PART_SIZE:int <= 5} *
           {delta_L_QUANTITY:float <= 11} * {delta_L_QUANTITY:float >= 1} *
           ({PART_CONTAINER:string = 'SM CASE'} +
             {PART_CONTAINER:string = 'SM BOX'} +
             {PART_CONTAINER:string = 'SM PACK'} +
             {PART_CONTAINER:string = 'SM PKG'}) *
           2) +
          ({PART_BRAND:string = 'Brand#34'} *
            ({PART_CONTAINER:string = 'LG CASE'} +
              {PART_CONTAINER:string = 'LG BOX'} +
              {PART_CONTAINER:string = 'LG PACK'} +
              {PART_CONTAINER:string = 'LG PKG'}) *
            {delta_L_QUANTITY:float >= 20} * {delta_L_QUANTITY:float <= 30} *
            {PART_SIZE:int <= 15})))) *
     {__sql_inline_or_7:int > 0})) *
  delta_REVENUE_mPART1:float);
}

CORRECT REVENUE_mPART2[][delta_L_QUANTITY:float,delta_L_SHIPMODE:string,delta_L_PARTKEY:int,delta_L_SHIPINSTRUCT:string] += delta_REVENUE_mPART2:float FOR ON - PART(PART_PARTKEY:int, PART_NAME:string, PART_MFGR:string, PART_BRAND:string, PART_TYPE:string, PART_SIZE:int, PART_CONTAINER:string, PART_RETAILPRICE:float, PART_COMMENT:string) {
   REVENUE(float)[][] += 
  (AggSum([], 
   ((__sql_inline_or_7:int ^=
      ({delta_L_SHIPINSTRUCT:string = 'DELIVER IN PERSON'} *
        {PART_PARTKEY:int = delta_L_PARTKEY:int} *
        ({delta_L_SHIPMODE:string = 'AIR'} +
          {delta_L_SHIPMODE:string = 'AIR REG'}) *
        {PART_SIZE:int >= 1} *
        (({PART_BRAND:string = 'Brand#12'} * {PART_SIZE:int <= 5} *
           {delta_L_QUANTITY:float <= 11} * {delta_L_QUANTITY:float >= 1} *
           ({PART_CONTAINER:string = 'SM CASE'} +
             {PART_CONTAINER:string = 'SM BOX'} +
             {PART_CONTAINER:string = 'SM PACK'} +
             {PART_CONTAINER:string = 'SM PKG'}) *
           2) +
          ({PART_BRAND:string = 'Brand#34'} *
            ({PART_CONTAINER:string = 'LG CASE'} +
              {PART_CONTAINER:string = 'LG BOX'} +
              {PART_CONTAINER:string = 'LG PACK'} +
              {PART_CONTAINER:string = 'LG PKG'}) *
            {delta_L_QUANTITY:float >= 20} * {delta_L_QUANTITY:float <= 30} *
            {PART_SIZE:int <= 15})))) *
     {__sql_inline_or_7:int > 0})) *
  -1 * delta_REVENUE_mPART2:float);
}
