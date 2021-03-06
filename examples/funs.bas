10  REM TEST BASIC STANDARD FUNCTIONS AND USER DEFINED FUNCTIONS.
20  DEF FNI(X) = X + 1
30  DEF FNS(Y) = SQR(Y ^ 2 - Y) + FNI(LOG(Y - 100))
40  LET A = 10
50  LET B = 4
60  LET P = 3.141592654
70  PRINT 1 + SIN(A - B * 2)
80  PRINT 2 + COS(-1 * P)
90  PRINT 3 + TAN(A + B)
100 PRINT 4 + ATN(P * 2)
110 LET E = EXP(A - (2 * B + 1))
120 PRINT LOG(E) + 5
130 PRINT ABS(B - A) + 6
140 PRINT FNI(B - FNI(A - 7)) * 2
150 PRINT P - 10 * FNS(SQR(B) + 1)
160 END
