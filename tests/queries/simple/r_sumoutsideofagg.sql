INCLUDE 'queries/simple/schemas.sql';

SELECT A, A+SUM(B) FROM R GROUP BY A;
