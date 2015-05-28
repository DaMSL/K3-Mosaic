INCLUDE 'queries/simple/schemas.sql';

SELECT r1.B FROM S s, R r1, R r2 WHERE s.B=r1.A AND s.C = r2.A;
