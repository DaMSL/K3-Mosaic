INCLUDE 'queries/simple/schemas.sql';

SELECT A, SUM(B)
FROM R
GROUP BY A;

