INCLUDE 'queries/simple/schemas.sql';

SELECT * FROM R r2 WHERE r2.B IN (SELECT r1.A FROM R r1);
