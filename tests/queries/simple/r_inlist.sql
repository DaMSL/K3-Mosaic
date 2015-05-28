INCLUDE 'queries/simple/schemas.sql';

SELECT * FROM R WHERE A IN LIST (1, 2, 3);
SELECT * FROM R WHERE NOT A IN LIST (1, 2, 3);
