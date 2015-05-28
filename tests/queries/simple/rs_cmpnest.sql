INCLUDE 'queries/simple/schemas.sql';

SELECT R.A,R.B FROM R WHERE  R.A < ( SELECT SUM(S.C) FROM S WHERE R.B = S.B );

/*
SELECT sum(A+B) FROM R;

SELECT sum(R.A) FROM R WHERE R.B = (SELECT sum(S.C) FROM S);
*/
