INCLUDE 'queries/simple/schemas.sql';

SELECT A, SUM(B)/A FROM R GROUP BY A;
