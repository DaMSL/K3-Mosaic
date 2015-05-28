INCLUDE 'queries/simple/schemas.sql';

SELECT * FROM R WHERE B IN (SELECT 1);
