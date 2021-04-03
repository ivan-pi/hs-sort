      SUBROUTINE ISORT(N,LIST,KEY)
C***********************************************************************
C     SUBROUTINE ISORT
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
C     USES INSERTION SORT - EFFICIENT ONLY FOR 'N' VALUES LESS THAN
C     ABOUT 12 (ALTHOUGH MAY BE SYSTEM DEPENDENT)
C***********************************************************************
      INTEGER N
      INTEGER LIST(*)
      DOUBLE PRECISION KEY(*)
C
      INTEGER I, J, K
      DOUBLE PRECISION VALUE
C
C     SET FIRST POINTER TO MINIMUM KEY
C
      J = 1
      K = LIST(1)
      VALUE = KEY(K)
      DO 10 I = 2, N
         IF (KEY(LIST(I)) .LT. VALUE) THEN
            J = I
            VALUE = KEY(LIST(I))
         END IF
10    CONTINUE
      LIST(1) = LIST(J)
      LIST(J) = K
C
C     CARRY OUT INSERTION SORT
C
      DO 30 I = 2, N
         J = I
         K = LIST(I)
         VALUE = KEY(K)
20       CONTINUE
         IF (VALUE .LT. KEY(LIST(J-1))) THEN
            LIST(J) = LIST(J-1)
            J = J - 1
            GOTO 20
         END IF
         LIST(J) = K
30    CONTINUE
      RETURN
      END