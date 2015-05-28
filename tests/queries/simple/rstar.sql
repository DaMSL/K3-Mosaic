INCLUDE 'queries/simple/schemas.sql';

SELECT sum(1) 
FROM  R ra, R rb, R rc 
WHERE ra.A = rb.A AND
      rb.A = rc.A;

