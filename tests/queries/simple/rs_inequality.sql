INCLUDE 'queries/simple/schemas.sql';

SELECT sum(A*D) FROM R,S WHERE R.B<S.C;
