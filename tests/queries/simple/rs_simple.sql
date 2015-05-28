INCLUDE 'queries/simple/schemas.sql';

SELECT r.A, SUM(s.C)
FROM R r, S s
WHERE r.B = S.B
GROUP BY r.A;
