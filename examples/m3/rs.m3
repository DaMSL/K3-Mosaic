-------------------- SOURCES --------------------
CREATE STREAM R(R_A int, R_B int)
  FROM FILE '../../experiments/data/simple/standard/r.dat' LINE DELIMITED
  CSV();

CREATE STREAM S(S_B int, S_C int)
  FROM FILE '../../experiments/data/simple/standard/s.dat' LINE DELIMITED
  CSV();

--------------------- MAPS ----------------------
DECLARE MAP ATIMESC(int)[][] := 
AggSum([], (R(R_A:int, R_B:int) * S(R_B:int, S_C:int) * R_A:int * S_C:int));

DECLARE MAP ATIMESC_mS1(int)[][ATIMESC_mSS_B:int] := 
AggSum([ATIMESC_mSS_B:int], (R(R_A:int, ATIMESC_mSS_B:int) * R_A:int));

DECLARE MAP ATIMESC_mR1(int)[][ATIMESC_mRR_B:int] := 
AggSum([ATIMESC_mRR_B:int], (S(ATIMESC_mRR_B:int, S_C:int) * S_C:int));

-------------------- QUERIES --------------------
DECLARE QUERY ATIMESC := ATIMESC(int)[][];

------------------- TRIGGERS --------------------
ON + R(R_A:int, R_B:int) {
   ATIMESC(int)[][] += (ATIMESC_mR1(int)[][R_B:int] * R_A:int);
   ATIMESC_mS1(int)[][R_B:int] += R_A:int;
}

ON - R(R_A:int, R_B:int) {
   ATIMESC(int)[][] += (ATIMESC_mR1(int)[][R_B:int] * -1 * R_A:int);
   ATIMESC_mS1(int)[][R_B:int] += (-1 * R_A:int);
}

ON + S(S_B:int, S_C:int) {
   ATIMESC(int)[][] += (ATIMESC_mS1(int)[][S_B:int] * S_C:int);
   ATIMESC_mR1(int)[][S_B:int] += S_C:int;
}

ON - S(S_B:int, S_C:int) {
   ATIMESC(int)[][] += (ATIMESC_mS1(int)[][S_B:int] * -1 * S_C:int);
   ATIMESC_mR1(int)[][S_B:int] += (-1 * S_C:int);
}

ON SYSTEM READY {
}

CORRECT ATIMESC_mR1[][delta_ATIMESC_mRR_B:int] += delta_ATIMESC_mR1:int FOR ON + R(R_A:int, R_B:int) {
   ATIMESC(int)[][] += ({R_B:int = delta_ATIMESC_mRR_B:int} * delta_ATIMESC_mR1:int * R_A:int);
}

CORRECT ATIMESC_mR1[][delta_ATIMESC_mRR_B:int] += delta_ATIMESC_mR1:int FOR ON - R(R_A:int, R_B:int) {
   ATIMESC(int)[][] += ({R_B:int = delta_ATIMESC_mRR_B:int} * -1 * delta_ATIMESC_mR1:int * R_A:int);
}

CORRECT ATIMESC_mS1[][delta_ATIMESC_mSS_B:int] += delta_ATIMESC_mS1:int FOR ON + S(S_B:int, S_C:int) {
   ATIMESC(int)[][] += ({S_B:int = delta_ATIMESC_mSS_B:int} * delta_ATIMESC_mS1:int * S_C:int);
}

CORRECT ATIMESC_mS1[][delta_ATIMESC_mSS_B:int] += delta_ATIMESC_mS1:int FOR ON - S(S_B:int, S_C:int) {
   ATIMESC(int)[][] += ({S_B:int = delta_ATIMESC_mSS_B:int} * -1 * delta_ATIMESC_mS1:int * S_C:int);
}
