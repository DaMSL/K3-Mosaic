INCLUDE 'queries/simple/schemas.sql';

SELECT A FROM R r, (SELECT S.B, COUNT(*) FROM S GROUP BY S.B) s2 WHERE r.B < s2.B;
