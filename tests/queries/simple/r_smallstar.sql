INCLUDE 'queries/simple/schemas.sql';

SELECT r1.B, SUM(r1.A * r2.A)
FROM R r1, R r2 
WHERE r1.B = r2.B
GROUP BY r1.B
