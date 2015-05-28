INCLUDE 'queries/simple/schemas.sql';

SELECT r1.B FROM S s, R r1, R r2 WHERE s.C=r1.A AND s.D = r2.A;
