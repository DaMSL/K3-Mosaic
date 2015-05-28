INCLUDE 'queries/simple/schemas.sql';

SELECT SUM(A) FROM R WHERE A = B AND A <= B
