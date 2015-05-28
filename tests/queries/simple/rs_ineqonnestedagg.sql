INCLUDE 'queries/simple/schemas.sql';

SELECT A FROM R r, (SELECT s2.B, COUNT(*) AS CNT FROM S s2 GROUP BY s2.B) s WHERE r.B = s.B AND r.A < CNT;
