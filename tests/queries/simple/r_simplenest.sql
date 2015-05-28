INCLUDE 'queries/simple/schemas.sql';

SELECT A FROM R r1 WHERE EXISTS (SELECT R2.A FROM R r2);
