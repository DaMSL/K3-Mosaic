INCLUDE 'queries/simple/schemas.sql';

SELECT * FROM R;
SELECT COUNT(*) FROM R;
SELECT COUNT(DISTINCT) FROM R;
SELECT COUNT(DISTINCT B) FROM R;
SELECT COUNT(DISTINCT B) FROM R GROUP BY A;
