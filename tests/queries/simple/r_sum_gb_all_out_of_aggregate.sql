INCLUDE 'queries/simple/schemas.sql';

SELECT A+SUM(1) FROM R GROUP BY A,B
