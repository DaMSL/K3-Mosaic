INCLUDE 'queries/simple/schemas.sql';

SELECT r3.C FROM (
  SELECT r2.C, COUNT(*) FROM (
    SELECT r1.A, COUNT(*) AS C FROM R r1 GROUP BY r1.A
  ) r2 GROUP BY C
) r3;
