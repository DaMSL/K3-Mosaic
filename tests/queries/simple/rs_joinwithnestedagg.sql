CREATE STREAM R(A int, B int) 
  FROM FILE 'data/simple/r.dat' LINE DELIMITED
  CSV ();

CREATE STREAM S(B int, C int) 
  FROM FILE 'data/simple/s.dat' LINE DELIMITED
  CSV ();

SELECT A FROM R r, (SELECT s2.B, COUNT(*) FROM S s2 GROUP BY s2.B) s WHERE r.B = s.B;
