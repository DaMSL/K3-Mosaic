CREATE STREAM R(A float, B float) 
  FROM FILE 'data/simple/r.dat' LINE DELIMITED
  CSV ();

SELECT SUM(A)/(1+SUM(B)) FROM R
