CREATE STREAM R(A int, B int)
FROM FILE 'data/simple/r.dat' LINE DELIMITED
CSV ();

SELECT A+SUM(1) FROM R GROUP BY A,B
