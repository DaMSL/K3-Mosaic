CREATE STREAM R(A int, B int) 
  FROM FILE 'data/simple/r.dat' LINE DELIMITED
  CSV ();

SELECT SUM(1) FROM R;
