      SUBROUTINE HSORT(N,LIST,KEY)
C***********************************************************************
C     SUBROUTINE HSORT
C     ORDER INTEGERS STORES IN 'LIST' IN ASCENDING SEQUENCE OF THEIR KEY
C     VALUES STORED IN KEY
C
C     INPUT PARAMETERS:
C
C     'N'    POSITIVE INTEGER GIVING LENGTH OF LIST
C     'LIST' A LIST OF LENGTH 'N' OF INTEGERS
C     'KEY'  A LIST OF LENGTH 'N' OF DOUBLE PRECISION KEYS
C
C     OUTPUT PARAMETERS
C
C     'N'    UNCHANGED
C     'LIST' A LIST OF OF LENGTH 'N' OF INTEGERS SORTED IN ASCENDING
C            SEQUENCE OF THEIR DOUBLE PRECISION KEYS
C     'KEY'  UNCHANGED
C
C     NOTES:
C
C     USES QUICKSORT ALGORITHM, EXCEPT FOR SEGMENTS OF THE LIST SHORTER
C     THAN 'NCUT' FOR WHICH A SECOND PASS INSERTION SORT IS USED. 'NCUT'
C     SHOULD BE SET TO AN INTEGER OF THE ORDER OF 12 FOR MAXIMUM
C     EFFICIENCY, BUT MAY BE SYSTEM DEPENDENT.
C
C     ROUTINE SORTS LISTS UP TO LENGTH 2**MAXSTK
C***********************************************************************

      INTEGER N
      INTEGER LIST(*)
      DOUBLE PRECISION KEY(*)
C
      INTEGER MAXSTK,NCUT
      PARAMETER(MAXSTK = 32,
     +          NCUT = 12)
      INTEGER LL, LR, LM NL, NR, LTEMP, STKTOP, I, J, K
      INTEGER LSTACK(MAXSTK), RSTACK(MAXSTK)
      DOUBLE PRECISION GUESS, VALUE
C
      LL = 1
      LR = N
      STKTOP = 0
10    CONTINUE
      IF ((LR-LL) .GE. NCUT) THEN
C
C        ONLY USE QUICKSORT ON SUB-LISTS LONGER THAN NCUT
C
         NL = LL
         NR = LR
         LM = (LL + LR)/2
         GUESS = KEY(LIST(LM))
C
C        FIND KEYS FOR EXCHANGE
C
20       CONTINUE
         IF (KEY(LIST(NL)) .LT. GUESS) THEN
            NL = NL + 1
            GOTO 20
         END IF
30       CONTINUE
         IF (GUESS .LT. KEY(LIST(NR))) THEN
            NR = NR - 1
            GOTO 30
         END IF
         IF (NL .LT. (NR - 1)) THEN
            LTEMP = LIST(NL)
            LIST(NL) = LIST(NR)
            LIST(NR) = LTEMP
            NL = NL + 1
            NR = NR - 1
            GOTO 20
         END IF
C
C        DEAL WITH CROSSING OF POINTERS
C
         IF (NL .LE. NR) THEN
            IF (NL .LT. NR) THEN
               LTEMP = LIST(NL)
               LIST(NL) = LIST(NR)
               LIST(NR) = LTEMP
            END IF
            NL = NL + 1
            NR = NR - 1
         END IF
C
C     SELECT SUB-LIST TO BE PROCESSED NEXT
C
         STKTOP = STKTOP - 1
         IF (NR .LT. LM) THEN
            LSTACK(STKTOP) = NL
            RSTACK(STKTOP) = LR
            LR = NR
         ELSE
            LSTACK(STKTOP) = LL
            RSTACK(STKTOP) = NR
            LL = NL
         END IF
         GOTO 10
      END IF
C
C     PROCESS ANY STACKED SUB-LISTS
C
      IF (STKTOP .NE. 0) THEN
         LL = LSTACK(STKTOP)
         LR = RSTACK(STKTOP)
         STKTOP = STKTOP - 1
         GOTO 10
      END IF
C
C     SECOND PASS - INSERTION SORT - SET FIRST POINTER TO MINIMUM KEY
C
      J = 1
      K = LIST(1)
      VALUE = KEY(K)
      DO 40 I = 2, MIN(N,NCUT)
         IF (KEY(LIST(I)) .LT. VALUE) THEN
            J = 1
            VALUE = KEY(LIST(I))
         END IF
40    CONTINUE
      LIST(1) = LIST(J)
      LIST(J) = K
C
C     CARRY OUT INSERTION SORT
C
      DO 60 I = 2, N
         J = I
         K = LIST(I)
         VALUE = KEY(K)
50       CONTINUE
         IF (VALUE .LT. KEY(LIST(J-I))) THEN
            LIST(J) = LIST(J-1)
            J = J - 1
            GOTO 50
         END IF
         LIST(J) = K
60    CONTINUE
      RETURN
      END