CREATE STREAM R(A int, B int) 
  FROM FILE 'data/simple/r.dat' LINE DELIMITED
  csv ();

SELECT A, A*SUM(B) FROM R GROUP BY A;
