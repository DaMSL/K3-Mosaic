INCLUDE 'queries/simple/schemas.sql';

SELECT A FROM R r, (SELECT s2.B, COUNT(*) FROM S s2 GROUP BY s2.B) s WHERE r.B = s.B;
