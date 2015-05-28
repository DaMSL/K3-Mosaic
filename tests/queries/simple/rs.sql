INCLUDE 'queries/simple/schemas.sql';

SELECT sum(A*C), sum(A+C) FROM R,S WHERE R.B=S.B;
