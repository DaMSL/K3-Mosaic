CREATE STREAM R(A int, B int) 
  FROM FILE 'data/simple/r.dat' LINE DELIMITED
  CSV ();

SELECT A, SUM(B)/A FROM R GROUP BY A;