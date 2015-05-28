INCLUDE 'queries/simple/schemas.sql';

SELECT SUM(r.A*s.C) as RESULT FROM R r, S s WHERE r.B = s.B;
