CREATE STREAM R(A float, B float) 
  FROM FILE 'data/simple/r.dat' LINE DELIMITED csv;

SELECT A, SUM(B * (SELECT SUM(r2.A) FROM R r2 WHERE r1.A = r2.A)) 
FROM R r1 GROUP BY A
