INCLUDE 'queries/simple/schemas.sql';

SELECT foo 
FROM (SELECT R.A AS foo, COUNT(*) FROM R GROUP BY foo) q;

SELECT foo 
FROM (SELECT R.A AS foo, COUNT(*) FROM R GROUP BY R.A) q;
