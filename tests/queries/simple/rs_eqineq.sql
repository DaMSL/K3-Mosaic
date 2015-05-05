CREATE STREAM R(A int, B int)
FROM FILE 'data/simple/r.dat' LINE DELIMITED
CSV ();

CREATE STREAM S(B int, C int)
FROM FILE 'data/simple/s.dat' LINE DELIMITED
CSV ();

SELECT * FROM R,S WHERE R.B = S.B AND R.A < S.C
