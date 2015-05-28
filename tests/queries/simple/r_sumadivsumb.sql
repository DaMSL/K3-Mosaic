INCLUDE 'queries/simple/schemas.sql';

SELECT SUM(A)/(1+SUM(B)) FROM R
