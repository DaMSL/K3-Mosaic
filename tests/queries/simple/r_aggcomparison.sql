INCLUDE 'queries/simple/schemas.sql';

SELECT *
FROM R r1, (SELECT SUM(r3.B) AS C FROM R r3) S
WHERE r1.A > (SELECT SUM(C) FROM R r2);
