-------------------- SOURCES --------------------
CREATE STREAM R(R_A int, R_B int)
  FROM FILE '../../experiments/data/simple/standard/r.dat' LINE DELIMITED
  CSV();

CREATE STREAM S(S_B int, S_C int)
  FROM FILE '../../experiments/data/simple/standard/s.dat' LINE DELIMITED
  CSV();

CREATE STREAM T(T_C int, T_D int)
  FROM FILE '../../experiments/data/simple/standard/t.dat' LINE DELIMITED
  CSV();

--------------------- MAPS ----------------------
DECLARE MAP ATIMESD(int)[][] := 
AggSum([], 
  (R(R_A:int, R_B:int) * S(R_B:int, S_C:int) * T(S_C:int, T_D:int) *
    R_A:int * T_D:int));

DECLARE MAP ATIMESD_mT1(int)[][ATIMESD_mTT_C:int] := 
AggSum([ATIMESD_mTT_C:int], 
  (R(R_A:int, R_B:int) * S(R_B:int, ATIMESD_mTT_C:int) * R_A:int));

DECLARE MAP ATIMESD_mT1_mR1(int)[][ATIMESD_mT1_mRR_B:int, ATIMESD_mTT_C:int] := 
S(ATIMESD_mT1_mRR_B:int, ATIMESD_mTT_C:int);

DECLARE MAP ATIMESD_mS1(int)[][ATIMESD_mSS_B:int] := 
AggSum([ATIMESD_mSS_B:int], (R(R_A:int, ATIMESD_mSS_B:int) * R_A:int));

DECLARE MAP ATIMESD_mS2(int)[][ATIMESD_mSS_C:int] := 
AggSum([ATIMESD_mSS_C:int], (T(ATIMESD_mSS_C:int, T_D:int) * T_D:int));

DECLARE MAP ATIMESD_mR1(int)[][ATIMESD_mRR_B:int] := 
AggSum([ATIMESD_mRR_B:int], 
  (S(ATIMESD_mRR_B:int, S_C:int) * T(S_C:int, T_D:int) * T_D:int));

-------------------- QUERIES --------------------
DECLARE QUERY ATIMESD := ATIMESD(int)[][];

------------------- TRIGGERS --------------------
ON + R(R_A:int, R_B:int) {
   ATIMESD(int)[][] += (ATIMESD_mR1(int)[][R_B:int] * R_A:int);
   ATIMESD_mT1(int)[][ATIMESD_mTT_C:int] += (ATIMESD_mT1_mR1(int)[][R_B:int, ATIMESD_mTT_C:int] * R_A:int);
   ATIMESD_mS1(int)[][R_B:int] += R_A:int;
}

ON - R(R_A:int, R_B:int) {
   ATIMESD(int)[][] += (ATIMESD_mR1(int)[][R_B:int] * -1 * R_A:int);
   ATIMESD_mT1(int)[][ATIMESD_mTT_C:int] += (ATIMESD_mT1_mR1(int)[][R_B:int, ATIMESD_mTT_C:int] * -1 * R_A:int);
   ATIMESD_mS1(int)[][R_B:int] += (-1 * R_A:int);
}

ON + S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += (ATIMESD_mS1(int)[][S_B:int] * ATIMESD_mS2(int)[][S_C:int]);
   ATIMESD_mT1(int)[][S_C:int] += ATIMESD_mS1(int)[][S_B:int];
   ATIMESD_mT1_mR1(int)[][S_B:int, S_C:int] += 1;
   ATIMESD_mR1(int)[][S_B:int] += ATIMESD_mS2(int)[][S_C:int];
}

ON - S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += (ATIMESD_mS1(int)[][S_B:int] * ATIMESD_mS2(int)[][S_C:int] * -1);
   ATIMESD_mT1(int)[][S_C:int] += (ATIMESD_mS1(int)[][S_B:int] * -1);
   ATIMESD_mT1_mR1(int)[][S_B:int, S_C:int] += -1;
   ATIMESD_mR1(int)[][S_B:int] += (ATIMESD_mS2(int)[][S_C:int] * -1);
}

ON + T(T_C:int, T_D:int) {
   ATIMESD(int)[][] += (ATIMESD_mT1(int)[][T_C:int] * T_D:int);
   ATIMESD_mS2(int)[][T_C:int] += T_D:int;
   ATIMESD_mR1(int)[][ATIMESD_mRR_B:int] += (ATIMESD_mT1_mR1(int)[][ATIMESD_mRR_B:int, T_C:int] * T_D:int);
}

ON - T(T_C:int, T_D:int) {
   ATIMESD(int)[][] += (ATIMESD_mT1(int)[][T_C:int] * -1 * T_D:int);
   ATIMESD_mS2(int)[][T_C:int] += (-1 * T_D:int);
   ATIMESD_mR1(int)[][ATIMESD_mRR_B:int] += (ATIMESD_mT1_mR1(int)[][ATIMESD_mRR_B:int, T_C:int] * -1 * T_D:int);
}

ON SYSTEM READY {
}

CORRECT ATIMESD_mR1[][delta_ATIMESD_mRR_B:int] += delta_ATIMESD_mR1:int FOR ON + R(R_A:int, R_B:int) {
   ATIMESD(int)[][] += ({R_B:int = delta_ATIMESD_mRR_B:int} * delta_ATIMESD_mR1:int * R_A:int);
}

CORRECT ATIMESD_mT1_mR1[][delta_ATIMESD_mT1_mRR_B:int,delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1_mR1:int FOR ON + R(R_A:int, R_B:int) {
   ATIMESD_mT1(int)[][delta_ATIMESD_mTT_C:int] += 
  ({R_B:int = delta_ATIMESD_mT1_mRR_B:int} * delta_ATIMESD_mT1_mR1:int *
  R_A:int);
}

CORRECT ATIMESD_mR1[][delta_ATIMESD_mRR_B:int] += delta_ATIMESD_mR1:int FOR ON - R(R_A:int, R_B:int) {
   ATIMESD(int)[][] += ({R_B:int = delta_ATIMESD_mRR_B:int} * -1 * delta_ATIMESD_mR1:int * R_A:int);
}

CORRECT ATIMESD_mT1_mR1[][delta_ATIMESD_mT1_mRR_B:int,delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1_mR1:int FOR ON - R(R_A:int, R_B:int) {
   ATIMESD_mT1(int)[][delta_ATIMESD_mTT_C:int] += 
  ({R_B:int = delta_ATIMESD_mT1_mRR_B:int} * -1 * delta_ATIMESD_mT1_mR1:int *
  R_A:int);
}

CORRECT ATIMESD_mS1[][delta_ATIMESD_mSS_B:int] += delta_ATIMESD_mS1:int FOR ON + S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += 
  ({S_B:int = delta_ATIMESD_mSS_B:int} * ATIMESD_mS2(int)[][S_C:int] *
  delta_ATIMESD_mS1:int);
   ATIMESD_mT1(int)[][S_C:int] += ({S_B:int = delta_ATIMESD_mSS_B:int} * delta_ATIMESD_mS1:int);
}

CORRECT ATIMESD_mS2[][delta_ATIMESD_mSS_C:int] += delta_ATIMESD_mS2:int FOR ON + S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += 
  ({S_C:int = delta_ATIMESD_mSS_C:int} * ATIMESD_mS1(int)[][S_B:int] *
  delta_ATIMESD_mS2:int);
   ATIMESD_mR1(int)[][S_B:int] += ({S_C:int = delta_ATIMESD_mSS_C:int} * delta_ATIMESD_mS2:int);
}

CORRECT ATIMESD_mS1[][delta_ATIMESD_mSS_B:int] += delta_ATIMESD_mS1:int FOR ON + S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += 
  ({S_B:int = delta_ATIMESD_mSS_B:int} * ATIMESD_mS2(int)[][S_C:int] *
  delta_ATIMESD_mS1:int);
   ATIMESD_mT1(int)[][S_C:int] += ({S_B:int = delta_ATIMESD_mSS_B:int} * delta_ATIMESD_mS1:int);
}

CORRECT ATIMESD_mS2[][delta_ATIMESD_mSS_C:int] += delta_ATIMESD_mS2:int FOR ON + S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += 
  ({S_C:int = delta_ATIMESD_mSS_C:int} * ATIMESD_mS1(int)[][S_B:int] *
  delta_ATIMESD_mS2:int);
   ATIMESD_mR1(int)[][S_B:int] += ({S_C:int = delta_ATIMESD_mSS_C:int} * delta_ATIMESD_mS2:int);
}

CORRECT ATIMESD_mS1[][delta_ATIMESD_mSS_B:int] += delta_ATIMESD_mS1:int FOR ON - S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += 
  ({S_B:int = delta_ATIMESD_mSS_B:int} * ATIMESD_mS2(int)[][S_C:int] * 
-1 * delta_ATIMESD_mS1:int);
   ATIMESD_mT1(int)[][S_C:int] += ({S_B:int = delta_ATIMESD_mSS_B:int} * -1 * delta_ATIMESD_mS1:int);
}

CORRECT ATIMESD_mS2[][delta_ATIMESD_mSS_C:int] += delta_ATIMESD_mS2:int FOR ON - S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += 
  ({S_C:int = delta_ATIMESD_mSS_C:int} * ATIMESD_mS1(int)[][S_B:int] * 
-1 * delta_ATIMESD_mS2:int);
   ATIMESD_mR1(int)[][S_B:int] += ({S_C:int = delta_ATIMESD_mSS_C:int} * -1 * delta_ATIMESD_mS2:int);
}

CORRECT ATIMESD_mS1[][delta_ATIMESD_mSS_B:int] += delta_ATIMESD_mS1:int FOR ON - S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += 
  ({S_B:int = delta_ATIMESD_mSS_B:int} * ATIMESD_mS2(int)[][S_C:int] * 
-1 * delta_ATIMESD_mS1:int);
   ATIMESD_mT1(int)[][S_C:int] += ({S_B:int = delta_ATIMESD_mSS_B:int} * -1 * delta_ATIMESD_mS1:int);
}

CORRECT ATIMESD_mS2[][delta_ATIMESD_mSS_C:int] += delta_ATIMESD_mS2:int FOR ON - S(S_B:int, S_C:int) {
   ATIMESD(int)[][] += 
  ({S_C:int = delta_ATIMESD_mSS_C:int} * ATIMESD_mS1(int)[][S_B:int] * 
-1 * delta_ATIMESD_mS2:int);
   ATIMESD_mR1(int)[][S_B:int] += ({S_C:int = delta_ATIMESD_mSS_C:int} * -1 * delta_ATIMESD_mS2:int);
}

CORRECT ATIMESD_mT1[][delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1:int FOR ON + T(T_C:int, T_D:int) {
   ATIMESD(int)[][] += ({T_C:int = delta_ATIMESD_mTT_C:int} * delta_ATIMESD_mT1:int * T_D:int);
}

CORRECT ATIMESD_mT1_mR1[][delta_ATIMESD_mT1_mRR_B:int,delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1_mR1:int FOR ON + T(T_C:int, T_D:int) {
   ATIMESD_mR1(int)[][delta_ATIMESD_mT1_mRR_B:int] += ({T_C:int = delta_ATIMESD_mTT_C:int} * delta_ATIMESD_mT1_mR1:int * T_D:int);
}

CORRECT ATIMESD_mT1[][delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1:int FOR ON - T(T_C:int, T_D:int) {
   ATIMESD(int)[][] += ({T_C:int = delta_ATIMESD_mTT_C:int} * -1 * delta_ATIMESD_mT1:int * T_D:int);
}

CORRECT ATIMESD_mT1_mR1[][delta_ATIMESD_mT1_mRR_B:int,delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1_mR1:int FOR ON - T(T_C:int, T_D:int) {
   ATIMESD_mR1(int)[][delta_ATIMESD_mT1_mRR_B:int] += 
  ({T_C:int = delta_ATIMESD_mTT_C:int} * -1 * delta_ATIMESD_mT1_mR1:int *
  T_D:int);
}
