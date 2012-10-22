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
AggSum([], (R(R_A, R_B) * S(R_B, S_C) * T(S_C, T_D) * R_A * T_D));

DECLARE MAP ATIMESD_mT1(int)[][ATIMESD_mTT_C:int] := 
AggSum([ATIMESD_mTT_C], (R(R_A, R_B) * S(R_B, ATIMESD_mTT_C) * R_A));

DECLARE MAP ATIMESD_mT1_mR1(int)[][ATIMESD_mT1_mRR_B:int, ATIMESD_mTT_C:int] := 
S(ATIMESD_mT1_mRR_B, ATIMESD_mTT_C);

DECLARE MAP ATIMESD_mS1(int)[][ATIMESD_mSS_B:int] := 
AggSum([ATIMESD_mSS_B], (R(R_A, ATIMESD_mSS_B) * R_A));

DECLARE MAP ATIMESD_mS2(int)[][ATIMESD_mSS_C:int] := 
AggSum([ATIMESD_mSS_C], (T(ATIMESD_mSS_C, T_D) * T_D));

DECLARE MAP ATIMESD_mR1(int)[][ATIMESD_mRR_B:int] := 
AggSum([ATIMESD_mRR_B], (S(ATIMESD_mRR_B, S_C) * T(S_C, T_D) * T_D));

-------------------- QUERIES --------------------
DECLARE QUERY ATIMESD := ATIMESD(int)[][];

------------------- TRIGGERS --------------------
ON + R(R_A, R_B) {
   ATIMESD(int)[][] += (ATIMESD_mR1(int)[][R_B] * R_A);
   ATIMESD_mT1(int)[][ATIMESD_mTT_C] += (ATIMESD_mT1_mR1(int)[][R_B, ATIMESD_mTT_C] * R_A);
   ATIMESD_mS1(int)[][R_B] += R_A;
}

ON - R(R_A, R_B) {
   ATIMESD(int)[][] += (ATIMESD_mR1(int)[][R_B] * -1 * R_A);
   ATIMESD_mT1(int)[][ATIMESD_mTT_C] += (ATIMESD_mT1_mR1(int)[][R_B, ATIMESD_mTT_C] * -1 * R_A);
   ATIMESD_mS1(int)[][R_B] += (-1 * R_A);
}

ON + S(S_B, S_C) {
   ATIMESD(int)[][] += (ATIMESD_mS1(int)[][S_B] * ATIMESD_mS2(int)[][S_C]);
   ATIMESD_mT1(int)[][S_C] += ATIMESD_mS1(int)[][S_B];
   ATIMESD_mT1_mR1(int)[][S_B, S_C] += 1;
   ATIMESD_mR1(int)[][S_B] += ATIMESD_mS2(int)[][S_C];
}

ON - S(S_B, S_C) {
   ATIMESD(int)[][] += (ATIMESD_mS1(int)[][S_B] * ATIMESD_mS2(int)[][S_C] * -1);
   ATIMESD_mT1(int)[][S_C] += (ATIMESD_mS1(int)[][S_B] * -1);
   ATIMESD_mT1_mR1(int)[][S_B, S_C] += -1;
   ATIMESD_mR1(int)[][S_B] += (ATIMESD_mS2(int)[][S_C] * -1);
}

ON + T(T_C, T_D) {
   ATIMESD(int)[][] += (ATIMESD_mT1(int)[][T_C] * T_D);
   ATIMESD_mS2(int)[][T_C] += T_D;
   ATIMESD_mR1(int)[][ATIMESD_mRR_B] += (ATIMESD_mT1_mR1(int)[][ATIMESD_mRR_B, T_C] * T_D);
}

ON - T(T_C, T_D) {
   ATIMESD(int)[][] += (ATIMESD_mT1(int)[][T_C] * -1 * T_D);
   ATIMESD_mS2(int)[][T_C] += (-1 * T_D);
   ATIMESD_mR1(int)[][ATIMESD_mRR_B] += (ATIMESD_mT1_mR1(int)[][ATIMESD_mRR_B, T_C] * -1 * T_D);
}

ON SYSTEM READY {
}

CORRECT ATIMESD_mR1[][delta_ATIMESD_mRR_B:int] += delta_ATIMESD_mR1:int FOR ON + R(R_A, R_B) {
   ATIMESD(int)[][] += ({R_B = delta_ATIMESD_mRR_B} * delta_ATIMESD_mR1 * R_A);
}

CORRECT ATIMESD_mT1_mR1[][delta_ATIMESD_mT1_mRR_B:int,delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1_mR1:int FOR ON + R(R_A, R_B) {
   ATIMESD_mT1(int)[][delta_ATIMESD_mTT_C] += ({R_B = delta_ATIMESD_mT1_mRR_B} * delta_ATIMESD_mT1_mR1 * R_A);
}

CORRECT ATIMESD_mR1[][delta_ATIMESD_mRR_B:int] += delta_ATIMESD_mR1:int FOR ON - R(R_A, R_B) {
   ATIMESD(int)[][] += ({R_B = delta_ATIMESD_mRR_B} * -1 * delta_ATIMESD_mR1 * R_A);
}

CORRECT ATIMESD_mT1_mR1[][delta_ATIMESD_mT1_mRR_B:int,delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1_mR1:int FOR ON - R(R_A, R_B) {
   ATIMESD_mT1(int)[][delta_ATIMESD_mTT_C] += ({R_B = delta_ATIMESD_mT1_mRR_B} * -1 * delta_ATIMESD_mT1_mR1 * R_A);
}

CORRECT ATIMESD_mS1[][delta_ATIMESD_mSS_B:int] += delta_ATIMESD_mS1:int FOR ON + S(S_B, S_C) {
   ATIMESD(int)[][] += ({S_B = delta_ATIMESD_mSS_B} * ATIMESD_mS2(int)[][S_C] * delta_ATIMESD_mS1);
   ATIMESD_mT1(int)[][S_C] += ({S_B = delta_ATIMESD_mSS_B} * delta_ATIMESD_mS1);
}

CORRECT ATIMESD_mS2[][delta_ATIMESD_mSS_C:int] += delta_ATIMESD_mS2:int FOR ON + S(S_B, S_C) {
   ATIMESD(int)[][] += ({S_C = delta_ATIMESD_mSS_C} * ATIMESD_mS1(int)[][S_B] * delta_ATIMESD_mS2);
   ATIMESD_mR1(int)[][S_B] += ({S_C = delta_ATIMESD_mSS_C} * delta_ATIMESD_mS2);
}

CORRECT ATIMESD_mS1[][delta_ATIMESD_mSS_B:int] += delta_ATIMESD_mS1:int FOR ON + S(S_B, S_C) {
   ATIMESD(int)[][] += ({S_B = delta_ATIMESD_mSS_B} * ATIMESD_mS2(int)[][S_C] * delta_ATIMESD_mS1);
   ATIMESD_mT1(int)[][S_C] += ({S_B = delta_ATIMESD_mSS_B} * delta_ATIMESD_mS1);
}

CORRECT ATIMESD_mS2[][delta_ATIMESD_mSS_C:int] += delta_ATIMESD_mS2:int FOR ON + S(S_B, S_C) {
   ATIMESD(int)[][] += ({S_C = delta_ATIMESD_mSS_C} * ATIMESD_mS1(int)[][S_B] * delta_ATIMESD_mS2);
   ATIMESD_mR1(int)[][S_B] += ({S_C = delta_ATIMESD_mSS_C} * delta_ATIMESD_mS2);
}

CORRECT ATIMESD_mS1[][delta_ATIMESD_mSS_B:int] += delta_ATIMESD_mS1:int FOR ON - S(S_B, S_C) {
   ATIMESD(int)[][] += 
  ({S_B = delta_ATIMESD_mSS_B} * ATIMESD_mS2(int)[][S_C] * -1 *
  delta_ATIMESD_mS1);
   ATIMESD_mT1(int)[][S_C] += ({S_B = delta_ATIMESD_mSS_B} * -1 * delta_ATIMESD_mS1);
}

CORRECT ATIMESD_mS2[][delta_ATIMESD_mSS_C:int] += delta_ATIMESD_mS2:int FOR ON - S(S_B, S_C) {
   ATIMESD(int)[][] += 
  ({S_C = delta_ATIMESD_mSS_C} * ATIMESD_mS1(int)[][S_B] * -1 *
  delta_ATIMESD_mS2);
   ATIMESD_mR1(int)[][S_B] += ({S_C = delta_ATIMESD_mSS_C} * -1 * delta_ATIMESD_mS2);
}

CORRECT ATIMESD_mS1[][delta_ATIMESD_mSS_B:int] += delta_ATIMESD_mS1:int FOR ON - S(S_B, S_C) {
   ATIMESD(int)[][] += 
  ({S_B = delta_ATIMESD_mSS_B} * ATIMESD_mS2(int)[][S_C] * -1 *
  delta_ATIMESD_mS1);
   ATIMESD_mT1(int)[][S_C] += ({S_B = delta_ATIMESD_mSS_B} * -1 * delta_ATIMESD_mS1);
}

CORRECT ATIMESD_mS2[][delta_ATIMESD_mSS_C:int] += delta_ATIMESD_mS2:int FOR ON - S(S_B, S_C) {
   ATIMESD(int)[][] += 
  ({S_C = delta_ATIMESD_mSS_C} * ATIMESD_mS1(int)[][S_B] * -1 *
  delta_ATIMESD_mS2);
   ATIMESD_mR1(int)[][S_B] += ({S_C = delta_ATIMESD_mSS_C} * -1 * delta_ATIMESD_mS2);
}

CORRECT ATIMESD_mT1[][delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1:int FOR ON + T(T_C, T_D) {
   ATIMESD(int)[][] += ({T_C = delta_ATIMESD_mTT_C} * delta_ATIMESD_mT1 * T_D);
}

CORRECT ATIMESD_mT1_mR1[][delta_ATIMESD_mT1_mRR_B:int,delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1_mR1:int FOR ON + T(T_C, T_D) {
   ATIMESD_mR1(int)[][delta_ATIMESD_mT1_mRR_B] += ({T_C = delta_ATIMESD_mTT_C} * delta_ATIMESD_mT1_mR1 * T_D);
}

CORRECT ATIMESD_mT1[][delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1:int FOR ON - T(T_C, T_D) {
   ATIMESD(int)[][] += ({T_C = delta_ATIMESD_mTT_C} * -1 * delta_ATIMESD_mT1 * T_D);
}

CORRECT ATIMESD_mT1_mR1[][delta_ATIMESD_mT1_mRR_B:int,delta_ATIMESD_mTT_C:int] += delta_ATIMESD_mT1_mR1:int FOR ON - T(T_C, T_D) {
   ATIMESD_mR1(int)[][delta_ATIMESD_mT1_mRR_B] += ({T_C = delta_ATIMESD_mTT_C} * -1 * delta_ATIMESD_mT1_mR1 * T_D);
}
