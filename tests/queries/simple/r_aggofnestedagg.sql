INCLUDE 'queries/simple/schemas.sql';

SELECT SUM(1) FROM (SELECT SUM(1) FROM R) r;
