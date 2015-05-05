CREATE STREAM R(A int, B int) 
  FROM FILE 'data/simple/r.dat' LINE DELIMITED
  CSV ();

SELECT * FROM (SELECT * FROM R) n;
