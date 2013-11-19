SET search_path = 'TPCH_@@DATASET@@';

SELECT l_returnflag, l_linestatus, 
  SUM(l_quantity) AS sum_qty,
  SUM(l_extendedprice) AS sum_base_price,
  SUM(l_extendedprice * (1-l_discount)) AS sum_disc_price,
  SUM(l_extendedprice * (1-l_discount) * (1+l_tax)) AS sum_charge,
  AVG(l_quantity) AS avg_qty,
  AVG(l_extendedprice) AS avg_price,
  AVG(l_discount) AS avg_disc,
  COUNT(*) AS count_order
FROM LINEITEM
WHERE l_shipdate <= DATE('1997-09-01')
GROUP BY l_returnflag, l_linestatus;