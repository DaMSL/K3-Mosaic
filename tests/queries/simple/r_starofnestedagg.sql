INCLUDE 'queries/simple/schemas.sql';

SELECT * FROM (SELECT COUNT(*) FROM R) n;
