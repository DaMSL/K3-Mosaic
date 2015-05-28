INCLUDE 'queries/simple/schemas.sql';

SELECT * FROM R NATURAL JOIN S WHERE R.A < S.C
