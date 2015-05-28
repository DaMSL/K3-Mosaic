INCLUDE 'queries/simple/schemas.sql';

SELECT sum(A*D) AS AtimesD FROM R,S,T WHERE R.B=S.B AND S.C=T.C;
