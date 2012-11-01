-------------------- SOURCES --------------------
CREATE STREAM R(R_A int, R_B int)
  FROM FILE '../../experiments/data/simple/standard/r.dat' LINE DELIMITED
  CSV();

--------------------- MAPS ----------------------
DECLARE MAP A(int)[][] := 
AggSum([], (R(R_A:int, R_B:int) * R_A:int));

-------------------- QUERIES --------------------
DECLARE QUERY A := A(int)[][];

------------------- TRIGGERS --------------------
ON + R(R_A:int, R_B:int) {
   A(int)[][] += R_A:int;
}

ON - R(R_A:int, R_B:int) {
   A(int)[][] += (-1 * R_A:int);
}

ON SYSTEM READY {
}
