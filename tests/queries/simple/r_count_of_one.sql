INCLUDE 'queries/simple/schemas.sql';

SELECT C
FROM (SELECT R.A, COUNT(*) as C FROM R GROUP BY A) s
WHERE s.A = 3
