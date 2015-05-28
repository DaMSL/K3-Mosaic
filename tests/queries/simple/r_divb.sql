INCLUDE 'queries/simple/schemas.sql';

SELECT (100000*SUM(1))/B FROM R GROUP BY B;

