INCLUDE 'queries/simple/schemas.sql';

SELECT *
FROM (SELECT R.A, COUNT(*) as C FROM R GROUP BY R.A) s
WHERE s.A = 3
