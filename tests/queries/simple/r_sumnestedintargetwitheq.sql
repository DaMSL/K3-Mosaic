INCLUDE 'queries/simple/schemas.sql';

SELECT SUM((SELECT SUM(1) FROM R r2 WHERE r1.A = r2.A)) FROM R r1;
