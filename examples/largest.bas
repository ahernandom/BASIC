10  REM FIND THE LARGEST OF SEVERAL NUMBERS USING READ/DATA.
20  READ L 
30  PRINT "LARGEST NUMBER FOUND SO FAR... " L
40  READ N
50  IF N <= L THEN 40
60  LET L = N
70  GOTO 30
80  DATA 83, 40, 21, 69, 62, 28, 91,  9, 22, 90
90  DATA 39, 89, 19, 47, 19, 77, 69, 50,  0, 20
100 DATA 19, 16, 19, 25, 78, 85, 87, 83, 45, 15
110 DATA 61, 29,  3, 80, 18, 34, 17, 21, 89, 13
120 DATA 77, 24, 79, 77, 10, 67,  2, 59, 95, 17
130 DATA 45,  0, 36, 52, 66, 19, 36,  0,  2,  7
140 DATA 95, 46,  3, 61, 79, 26, 40, 24, 83, 36
150 DATA 63, 57, 80, 43, 74, 20, 83, 72,  1, 56
160 DATA 39, 46, 37, 56, 97, 34,  7,  1, 77, 50
170 DATA 67, 41, 55, 75, 11, 65, 12, 15, 75, 16
180 DATA 54, 91, 90, 18, 38, 76, 44, 55, 82, 90
190 DATA 43, 82, 41, 54, 68, 61, 23, 77, 43, 93
200 DATA 75, 71, 31, 98, 40, 69, 70, 96, 50, 27
210 DATA 12,  1, 30, 65, 48, 12, 59, 38, 83, 39
220 DATA 52, 68, 50, 73, 42, 35,  6, 35, 74, 62
230 DATA 29, 35, 98, 44, 50, 13, 98,  0, 39,  2
240 DATA  2, 66, 66, 30, 73, 70, 85, 64, 11,  8
250 DATA  6,  2, 44, 40, 62, 17, 61, 99, 78, 86
260 DATA 32, 66, 64, 36, 83, 29, 63, 89, 23, 31
270 DATA 15, 52,  2,  4, 46, 75, 10, 30, 66, 64
999 END
