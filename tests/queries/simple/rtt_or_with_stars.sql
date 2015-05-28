INCLUDE 'queries/simple/schemas.sql';

SELECT 
  t1.*, 
  t2.*
FROM 
  S, 
  T t1, 
  T t2
WHERE 
  t1.D = t2.C 
    OR 
  2 = (S.B*t1.D)
;
