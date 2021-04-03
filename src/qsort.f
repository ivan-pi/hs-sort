      SUBROUTINE QSORT(N,LIST,KEY)
C***********************************************************************
C     SUBROUTINE QSORT
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
C     USES QUICKSORT ALGORITHM, EFFICIENT FOR 'N' VALUES GREATHER THAN
C     ABOUT 12 (ALTHOUGH MAY BE SYSTEM DEPENDENT)
C
C     ROUTINE SORTS LISTS UP TO LENGTH 2**MAXSTK
C***********************************************************************

      INTEGER N
      INTEGER LIST(*)
      DOUBLE PRECISION KEY(*)
C
      INTEGER MAXSTK
      PARAMETER(MAXSTK = 32)
      INTEGER LL, LR, LM, NL, NR, LTEMP, STKTOP
      INTEGER LSTACK(MAXSTK), RSTACK(MAXSTK)
      DOUBLE PRECISION GUESS
C
      LL = 1
      LR = N
      STKTOP = 0
10    CONTINUE
      IF (LL .LT. LR) THEN
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
      RETURN
      END