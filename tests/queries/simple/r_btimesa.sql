INCLUDE 'queries/simple/schemas.sql';

SELECT A, SUM(B * (SELECT SUM(r2.A) FROM R r2)) FROM R r1 GROUP BY A
