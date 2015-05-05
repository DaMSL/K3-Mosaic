CREATE STREAM R(A int, B int)
FROM FILE 'data/simple/r.dat' LINE DELIMITED
CSV ();

SELECT * FROM R WHERE R.A <= R.B AND R.A >= R.B;
