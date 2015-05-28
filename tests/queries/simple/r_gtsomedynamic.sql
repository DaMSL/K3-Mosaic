INCLUDE 'queries/simple/schemas.sql';

SELECT * FROM R r2 WHERE r2.B > SOME (SELECT r1.A FROM R r1);
