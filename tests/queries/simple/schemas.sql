CREATE STREAM R(A int, B int) 
  FROM FILE 'data/simple/r.dat' LINE DELIMITED csv;
  
CREATE STREAM S(B int, C int) 
  FROM FILE 'data/simple/s.dat' LINE DELIMITED csv;

CREATE STREAM T(C int, D int) 
  FROM FILE 'data/simple/t.dat' LINE DELIMITED csv;

CREATE STREAM AGENDA (
    mux   VARCHAR,
    event INT,
    A     INT,
    B     INT,
    C     INT,
    D     INT
  ) 
  FROM FILE 'agenda.csv'
  LINE DELIMITED AGENDA (delimiter := '|', mapping :=
    'R:2,3;
     S:3,4;
     T:4,5');
  

