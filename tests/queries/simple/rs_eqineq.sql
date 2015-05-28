INCLUDE 'queries/simple/schemas.sql';

SELECT * FROM R,S WHERE R.B = S.B AND R.A < S.C
