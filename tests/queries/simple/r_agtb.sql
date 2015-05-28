INCLUDE 'queries/simple/schemas.sql';

SELECT r1.A as BOB, r2.B as JOE
FROM R r1, R r2
WHERE r1.B > r2.A
