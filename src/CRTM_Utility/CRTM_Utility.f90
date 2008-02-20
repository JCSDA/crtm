!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Utility 
!
! PURPOSE:
!       Module containing the utility routines.
!
! CATEGORY:
!       CRTM : Utility 
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Utility 
!
! MODULES:
!       Type_Kinds:                 Module containing data type kind definitions.
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         matinv:                   Function to inverse a general matrix. 
!
!         DOUBLE_GAUSS_QUADRATURE:  Function to compute double Gaussian weights
!                                   cosine of angles. 
!
!         Legendre:                 Function to compute Legendre function. 
!
!       PRIVATE subprograms
!       -------------------
!
!         *** USERS ADD INFO HERE FOR ANY PRIVATE SUBPROGRAMS ***
!
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    QSS at JCSDA;    Quanhua.Liu@noaa.gov
!                       Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-Jun-2004
!
!  Copyright (C) 2004 Yong Han, Quanhua Liu, Paul van Delst
!M-
!--------------------------------------------------------------------------------

MODULE CRTM_UTILITY 

   ! ----------------------------------------------------------------- !
   ! This utility is specified for RT solution.                        !
   !   Deleveloper:                                                    !
   !   Quanhua Liu      Quanhua.Liu@noaa.gov                           !
   !   Adapted from package of MOM 1991.                               ! 
   ! ----------------------------------------------------------------- !
    USE Type_Kinds 
    USE CRTM_Parameters
    USE Message_Handler

   IMPLICIT NONE
    private
   public :: matinv
   public :: DOUBLE_GAUSS_QUADRATURE
   public :: Legendre

  INTERFACE Legendre 
    MODULE PROCEDURE Legendre_scalar
    MODULE PROCEDURE Legendre_rank1
  END INTERFACE 
! 

CONTAINS
!
!
      SUBROUTINE DOUBLE_GAUSS_QUADRATURE(NUM, ABSCISSAS, WEIGHTS)
!        Generates the abscissas and weights for an even 2*NUM point
!      Gauss-Legendre quadrature.  Only the NUM positive points are returned.
      INTEGER, INTENT(IN) ::  NUM
      REAL( fp_kind), DIMENSION(:) ::   ABSCISSAS, WEIGHTS
      INTEGER  N, K, I, J, L
      REAL( fp_kind ) ::   X, XP, PL, PL1, PL2, DPL
!      REAL( fp_kind ), PARAMETER :: TINY1=3.0E-14_fp_kind
      REAL( fp_kind ) :: TINY1
      PARAMETER(TINY1=3.0D-14)
      N = NUM
      K = (N+1)/2
      DO J = 1, K
        X = COS(PI*(J-POINT_25)/(N+POINT_5))
        I = 0
 100  CONTINUE
          PL1 = 1
          PL = X
          DO L = 2, N
            PL2 = PL1
            PL1 = PL
            PL = ( (2*L-1)*X*PL1 - (L-1)*PL2 )/L
          ENDDO
          DPL = N*(X*PL-PL1)/(X*X-1)
          XP = X
          X = XP - PL/DPL
          I = I+1
        IF (ABS(X-XP).GT.TINY1 .AND. I.LT.10) GO TO 100
        ABSCISSAS(J) = (ONE-X)/TWO
        ABSCISSAS(NUM+1-J) = (ONE+X)/TWO
        WEIGHTS(NUM+1-J) = ONE/((ONE-X*X)*DPL*DPL)
        WEIGHTS(J)       = ONE/((ONE-X*X)*DPL*DPL)
      ENDDO
      RETURN
      END SUBROUTINE DOUBLE_GAUSS_QUADRATURE
!
!
      FUNCTION matinv(A, Error_Status)
! Invert matrix by Gauss method
! --------------------------------------------------------------------
      IMPLICIT NONE
      REAL( fp_kind ), intent(in),dimension(:,:) :: a
      INTEGER, INTENT( OUT ) :: Error_Status

      INTEGER:: n
      REAL( fp_kind ), dimension(size(a,1),size(a,2)) :: b
      REAL(fp_kind ), dimension(size(a,1),size(a,2)) :: matinv 
      REAL( fp_kind ), dimension(size(a,1)) :: temp 
! - - - Local Variables - - -
      REAL( fp_kind ) :: c, d
      INTEGER :: i, j, k, m, imax(1), ipvt(size(a,1))
! - - - - - - - - - - - - - -
      Error_Status = SUCCESS     
      b = a
      n=size(a,1)
      matinv=a
      ipvt = (/ (i, i = 1, n) /)
! Go into loop- b, the inverse, is initialized equal to a above
      DO k = 1,n
! Find the largest value and position of that value
         imax = MAXLOC(ABS(b(k:n,k)))
         m = k-1+imax(1)
!   sigular matrix check
        if(ABS(b(m,k)).LE.(1.E-40_fp_kind)) then
        print *,'  sigular matrix '
        Error_Status = FAILURE
        RETURN
        ENDIF    
! get the row beneath the current row if the current row will
! not compute
         IF (m .ne. k) THEN
            ipvt( (/m,k/) ) = ipvt( (/k,m/) )
            b((/m,k/),:) = b((/k,m/),:)
         END IF
! d is a coefficient - brings the pivot value to one and then is applied
! to the rest of the row
         d = 1/b(k,k)
         temp = b(:,k)
         DO j = 1, n
            c = b(k,j)*d
            b(:,j) = b(:,j)-temp*c
            b(k,j) = c
         END DO 
         b(:,k) = temp*(-d)
         b(k,k) = d
      END DO
      matinv(:,ipvt) = b
      END FUNCTION matinv
!
!
      subroutine Legendre_Scalar(MOA,ANG,PL)
!  calculating Legendre polynomial using recurrence relationship
      implicit none
      integer, intent(in) :: MOA
      REAL( fp_kind ), intent(in) :: ANG
      REAL( fp_kind ), intent(out), dimension(0:MOA):: PL
      integer :: j
       PL(0)=ONE
       PL(1)=ANG
       IF(MOA.GE.2) THEN
       DO 103 J=1,MOA-1
       PL(J+1)=REAL(2*J+1,fp_kind)/REAL(J+1,fp_kind)*ANG*PL(J) &
      -REAL(J,fp_kind)/REAL(J+1,fp_kind)*PL(J-1)
 103   CONTINUE
       endif
       RETURN
       END subroutine Legendre_Scalar
!
!
      subroutine Legendre_Rank1(MOA,NU,ANG,PL)
!  calculating Legendre polynomial using recurrence relationship
      implicit none
      integer, intent(in):: MOA,NU
      REAL( fp_kind ),intent(in), dimension(NU):: ANG
      REAL( fp_kind ),intent(out),dimension(0:MOA,NU):: PL
      REAL( fp_kind ) :: TE
      integer:: i,j
       do 101 i=1,NU
       TE=ANG(i)
       PL(0,i)=ONE
       PL(1,i)=TE
       IF(MOA.GE.2) THEN
       DO 103 J=1,MOA-1
       PL(J+1,i)=REAL(2*J+1, fp_kind)/REAL(J+1, fp_kind)*TE*PL(J,i) &
      -REAL(J, fp_kind)/REAL(J+1, fp_kind)*PL(J-1,i)
 103   CONTINUE
       endif
 101   CONTINUE
       RETURN
       END subroutine Legendre_Rank1
 END MODULE CRTM_UTILITY 
!
