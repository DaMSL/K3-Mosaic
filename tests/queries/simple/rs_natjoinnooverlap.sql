CREATE STREAM R(A int, B int)
FROM FILE 'data/simple/r.dat' LINE DELIMITED
CSV ();

CREATE STREAM S(C int, D int)
FROM FILE 'data/simple/r.dat' LINE DELIMITED
CSV ();

SELECT R.* FROM R NATURAL JOIN S;
