INCLUDE 'queries/simple/schemas.sql';

SELECT SUM((SELECT SUM(1) FROM R r1)) FROM R r2;
