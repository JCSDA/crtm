MODULE Tran_matrix_linearBase

   IMPLICIT NONE

   PRIVATE

   PUBLIC tran_matrix

   REAL, PARAMETER :: ZERO = 0.0, ONE = 1.0

CONTAINS

      SUBROUTINE tran_matrix(SECANT,AL,GP,GM,GPI,CBTH,TRN_V,TRN_H,TRN_VH)
!**********************************************************************
!
!   NAME- TRANX2      LANGUAGE-  FORTRAN 90      TYPE- SUBROUTINE
!
!   VERSION- 2.0   DATE- 2/23/93      PROGRAMMER- P.ROSENKRANZ
!            2.1    Apr. 25, 97  PWR- internal double precision
!
!   FUNCTION- COMPUTE TRANSMITTANCE MATRIX ELEMENTS IN ZEEMAN EFFECT
!              FOR OBSERVER ABOVE LAYER 1
! 
!   CALLING SEQUENCE PARAMETERS-
!
!   NAME   TYPE  I/O  UNITS   DESCRIPTION
!
!  SECANT  REAL   I           SECANT OF (ANGLE FROM VERTICAL)
!  AL      REAL   I           POWER ABSORPTION (OPACITY) FROM OTHER LINES
!  GP      REAL   I           COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=+1)
!  GM      REAL   I           COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=-1)
!  GPI     REAL   I           COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=0)
!                              NOTE: AL,GP,GM,GPI FOR VERTICAL PROPAGATION
!  CBTH    REAL   I           COS(ANGLES BETWEEN MAGNETIC FIELD 
!                              AND PROPAGATION DIRECTION)
!  TRN_V   REAL   O           TRANSMITTANCE ALONG SECANT FROM TOP OF ATMOSPHERE 
!                             TO BOTTOM OF LAYER I, vertical POL.
!  TRN_H   REAL   O           TRANSMITTANCE IN horizontal POL.
!  TRN_VH  COMPLX O           OFF-DIAGONAL ELEMENT OF TRANSMITTANCE
!                             COHERENCY MATRIX; 
!
!***********************************************************************
!
       REAL, INTENT(IN)     :: SECANT
       REAL, INTENT(IN)     :: AL(:),CBTH(:)
       COMPLEX, INTENT(IN)  :: GP(:),GM(:),GPI(:)
       REAL, INTENT(OUT)    :: TRN_V(:),TRN_H(:)
       COMPLEX, INTENT(OUT) :: TRN_VH(:)

       ! local
       INTEGER I,M
       REAL AB,OPC,OMC, SIN2, COS2
       REAL*8 SQMAG
       COMPLEX G11,G12,G21,G22,CP,CM,CPI,DELTA,C2,EL1,EL2,D,C212,C221, C12
       COMPLEX*16 DUM,E11,E22,E12,E21,P11,P22,P12,P21,P11N,P22N
       
!
!  square magnitude definition:
       SQMAG(DUM) = REAL(DUM)**2 + AIMAG(DUM)**2
!
       M = SIZE(GP)

       P11 = (1.D0,0.D0)
       P22 = P11
       P12 = (0.D0,0.D0)
       P21 = P12

       DO 50 I=1,M
       AB = AL(I)*.5*SECANT
       CP = GP(I)*SECANT
       CM = GM(I)*SECANT
       CPI = GPI(I)*SECANT

       COS2 = CBTH(I)*CBTH(I)
       SIN2 = ONE - COS2

       G11 = CP + CM + CMPLX(AB,0.)
       G22 = COS2*CP + COS2*CM + SIN2*CPI + CMPLX(AB,0.)
!       G12 = CMPLX(ZERO,CBTH(I))*(-CP + CM)
!       G21 = CMPLX(ZERO,CBTH(I))*(CP - CM)
       G12 = CMPLX(ZERO,CBTH(I))*(CP - CM)
       G21 = CMPLX(ZERO,CBTH(I))*(-CP + CM)

       IF(ABS(REAL(G12))+ABS(AIMAG(G12))+ABS(REAL(G21))+ABS(AIMAG(G21)) .LE. 0.) THEN
!        MATRIX IS DIAGONAL
         E11 = CEXP(-G11)
         E22 = CEXP(-G22)
         E12 = (0.D0,0.D0)
         E21 = (0.D0,0.D0)
       ELSE
         C2 = G22 - G11
         DELTA = CSQRT(C2*C2 + 4.*G12*G21)
         IF(REAL(C2)*REAL(DELTA).LT.0.) DELTA = -DELTA
         C2 = .5*(C2+DELTA)
         EL2 = CEXP(-C2-G11)  ! second eigenvalue
         EL1 = EL2*CEXP(DELTA) ! first eigenvalue
         C212 = C2*G12
         C221 = C2*G21
         C2 = C2*C2
         C12 = G12*G21
         D = C2 + C12
         E11 = (C2*EL1+C12*EL2)/D
         E22 = (C12*EL1+C2*EL2)/D
         E12 = C212*(EL2-EL1)/D
         E21 = C221*(EL2-EL1)/D
       ENDIF
!      
       P11N = P11*E11 + P12*E21
       P22N = P21*E12 + P22*E22
       P12 = P11*E12 + P12*E22
       P21 = P21*E11 + P22*E21
       P11 = P11N
       P22 = P22N
       TRN_V(I) = SQMAG(P11) + SQMAG(P12)
       TRN_H(I) = SQMAG(P21) + SQMAG(P22)
       TRN_VH(I) = P11*CONJG(P21) + P12*CONJG(P22)
50     CONTINUE

       RETURN
  END SUBROUTINE tran_matrix

END MODULE Tran_matrix_linearBase
