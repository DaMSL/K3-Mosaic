INCLUDE 'queries/simple/schemas.sql';

SELECT r1.A, r1.B FROM S s, R r1, R r2 WHERE s.B=r2.A AND s.C = r2.A;
