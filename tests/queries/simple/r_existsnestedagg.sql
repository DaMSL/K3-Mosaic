INCLUDE 'queries/simple/schemas.sql';

SELECT r1.A, SUM(1)
FROM R r1
WHERE EXISTS 
  ( SELECT 1
    FROM (SELECT SUM(r2.B) as B FROM R r2 WHERE r2.A = r1.B) n1
    WHERE r1.A *3 < n1.B )
GROUP BY r1.A
