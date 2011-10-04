      SUBROUTINE TRANX2(SECANT,M,AL,GP,GM,GPI,CBTH,TRNRC,TRNLC,TRNLIN)
C**********************************************************************
C
C   NAME- TRANX2      LANGUAGE-  FORTRAN 77      TYPE- SUBROUTINE
C
C   VERSION- 2.0   DATE- 2/23/93      PROGRAMMER- P.ROSENKRANZ
C            2.1    Apr. 25, 97  PWR- internal double precision
C
C   FUNCTION- COMPUTE TRANSMITTANCE MATRIX ELEMENTS IN ZEEMAN EFFECT
C              FOR OBSERVER ABOVE LAYER 1
C 
C   CALLING SEQUENCE PARAMETERS-
C
C   NAME   TYPE  I/O  UNITS   DESCRIPTION
C
C  SECANT  REAL   I           SECANT OF (ANGLE FROM VERTICAL)
C  M       INT                NO. OF ELEMENTS IN AL,GP,GM,GPI,
C                             CBTH,TRNRC,TRNLC,TRNLIN VECTORS
C  AL      REAL   I           POWER ABSORPTION (OPACITY) FROM OTHER LINES
C  GP      REAL   I           COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=+1)
C  GM      REAL   I           COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=-1)
C  GPI     REAL   I           COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=0)
C                              NOTE: AL,GP,GM,GPI FOR VERTICAL PROPAGATION
C  CBTH    REAL   I           COS(ANGLES BETWEEN MAGNETIC FIELD 
C                              AND PROPAGATION DIRECTION)
C  TRNRC   REAL   O           TRANSMITTANCE ALONG SECANT FROM TOP OF ATMOSPHERE 
C                             TO BOTTOM OF LAYER I, IN RIGHT-CIRCULAR POL.
C  TRNLC   REAL   O           TRANSMITTANCE IN LEFT-CIRCULAR POL.
C  TRNLIN  COMPLX O           OFF-DIAGONAL ELEMENT OF TRANSMITTANCE
C                             COHERENCY MATRIX; IN TERMS OF STOKES 
C                             PARAMETERS, Q=2*REAL(TRNLIN), U=2*AIMAG(TRNLIN)
C
C***********************************************************************
C
       IMPLICIT NONE
       INTEGER I,M
       REAL AL(M),CBTH(M),TRNRC(M),TRNLC(M),SECANT,AB,SIN2,OPC,OMC
       REAL*8 SQMAG
       COMPLEX GP(M),GM(M),GPI(M),TRNLIN(M),C11,C12,C22,CP,CM,CPI,DELTA,
     &  C2,EL1,EL2,D,C212
       COMPLEX*16 DUM,E11,E22,E12,P11,P22,P12,P21,P11N,P22N
c
C  square magnitude definition:
       SQMAG(DUM) = DREAL(DUM)**2 + DIMAG(DUM)**2
C
       P11 = (1.D0,0.D0)
       P22 = P11
       P12 = (0.D0,0.D0)
       P21 = P12
       DO 50 I=1,M
       AB = AL(I)*.5*SECANT
       CP = GP(I)*SECANT
       CM = GM(I)*SECANT
       CPI = GPI(I)*SECANT
       SIN2 = .5*(1.-CBTH(I)**2)
       OPC = .5*(1.+CBTH(I))**2
       OMC = .5*(1.-CBTH(I))**2
       C11 = OPC*CP + OMC*CM + SIN2*CPI +CMPLX(AB,0.)
       C22 = OMC*CP + OPC*CM + SIN2*CPI + CMPLX(AB,0.)
       C12 = SIN2*(CP+CM-CPI)
       IF(ABS(REAL(C12))+ABS(AIMAG(C12)) .LE. 0.) THEN
C        MATRIX IS DIAGONAL
         E11 = CEXP(-C11)
         E22 = CEXP(-C22)
         E12 = (0.D0,0.D0)
       ELSE
         C2 = C22 - C11
         DELTA = CSQRT(C2*C2 + 4.*C12*C12)
         IF(REAL(C2)*REAL(DELTA).LT.0.) DELTA = -DELTA
         C2 = .5*(C2+DELTA)
         EL2 = CEXP(-C2-C11)
         EL1 = EL2*CEXP(DELTA)
         C212 = C2*C12
         C2 = C2*C2
         C12 = C12*C12
         D = C2 + C12
         E11 = (C2*EL1+C12*EL2)/D
         E22 = (C12*EL1+C2*EL2)/D
         E12 = C212*(EL2-EL1)/D
       ENDIF
C      NOTE THAT E21=E12 IN CIRCULAR BASIS
       P11N = P11*E11 + P12*E12
       P22N = P21*E12 + P22*E22
       P12 = P11*E12 + P12*E22
       P21 = P21*E11 + P22*E12
       P11 = P11N
       P22 = P22N
       TRNRC(I) = SQMAG(P11) + SQMAG(P12)
       TRNLC(I) = SQMAG(P21) + SQMAG(P22)
       TRNLIN(I) = P11*DCONJG(P21) + P12*DCONJG(P22)
50     CONTINUE
       RETURN
       END
