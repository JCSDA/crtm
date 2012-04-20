!------------------------------------------------------
! Module containing routines to perform fitting
!            Written by Y. Han, July 18, 2008
!------------------------------------------------------ 
MODULE Regression

  USE type_kinds         , ONLY : fp_kind
  USE Message_Handler    , ONLY : SUCCESS, FAILURE, Display_Message     
!  USE Common_parameters

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE

  PRIVATE

  PUBLIC Compute_AutoCross_Matrix
  PUBLIC Compute_RegressionCoeff
  PUBLIC SMOOTH_Coeff
  PUBLIC SMOOTH_Coeff_vertical
  PUBLIC Normalization
  PUBLIC Generation_H_matrix

  REAL(fp_kind), PARAMETER  :: ZERO = 0.0_fp_kind
  REAL(fp_kind), PARAMETER  :: ONE  = 1.0_fp_kind
  REAL(fp_kind), PARAMETER  :: TWO  = 2.0_fp_kind
  REAL(fp_kind), PARAMETER  :: TOLERANCE = EPSILON( ONE )
  
CONTAINS

  !-------------------------------------------------------------------
  ! Compute Auto and cross correlation matrixes:
  !      A = Transpose(X)#X   - Auto correlation
  !      C = TRanspose(X)#Y   - Cross correlation
  !  Inputs:
  !     X - an n x m array (n - sample size; m - number of variables)
  !     Y - an array of size n
  !  Outputs:
  !     A - an m x m array containing elements of auto correlation
  !     C - an array size of m containing cross correlation
  !-------------------------------------------------------------------
  SUBROUTINE Compute_AutoCross_Matrix(X, Y, A, C)
    REAL(fp_kind), INTENT( IN )  :: X(:,:)    ! n x m
    REAL(fp_kind), INTENT( IN )  :: Y(:)      ! n
    REAL(fp_kind), INTENT( OUT ) :: A(:, :)   ! m x m
    REAL(fp_kind), INTENT( OUT ) :: C(:)      ! m
    
    ! Local
    INTEGER :: m, n, i, j

    n = SIZE(X, DIM = 1)
    m = SIZE(X, DIM = 2) 

    A = ZERO
    C = ZERO

    DO i = 1, m
      C(i) = SUM(X(:, i)*Y)
      DO j = 1, i
        A(i,j)  = SUM(X(:, i)*X(:,j))
        A(j, i) = A(i, j)
      END DO
    END DO

  END SUBROUTINE Compute_AutoCross_Matrix

  !--------------------------------------------------------------------------
  ! Compute least square fitting solution:
  !     Coeff = INVERSE(A)#C  
  !   for the linear equation Y = X#Coeff, where
  !   A = Transpose(X)#X and C = TRanspose(X)#Y
  !      X an n x m array (n - sample size; m - number of variables )
  !      Y - an array of size n
  ! Inputs:
  !   A - an m x m array containing elements of auto correlation 
  !       Transpose(X)#X. m is the number of variables
  !   C - an array size of m containing cross correlation
  !     A and C may be computed using the routine Compute_AutoCross_Matrix
  ! Outputs:
  !   Coeff - an array of size m containing the solution (coefficients)
  !-------------------------------------------------------------------------- 
  FUNCTION Compute_RegressionCoeff(A, C, Coeff)RESULT(Error_Status)
    REAL(fp_kind), INTENT( IN )  :: A(:, :)     ! m x m  
    REAL(fp_kind), INTENT( IN )  :: C(:)        ! m      
    REAL(fp_kind), INTENT( OUT ) :: Coeff(:)    ! m      

    INTEGER :: Error_Status 

    ! Local
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_RegressionCoeff'
    INTEGER :: m
    REAL(fp_kind) ::  Awork(SIZE(C), SIZE(C)), Cwork(SIZE(C), 1)

    m = SIZE(C)

    Awork       = A
    Cwork(:, 1) = C                                        

    CALL DPOTRF( 'U', m, Awork, m, error_status)                                    
    IF(Error_Status /= 0)THEN                                                                    
      CALL Display_Message( ROUTINE_NAME, &                                                           
                            "Error calling LAPACK DPOTRF to perform Cholesky factorization", &                                                        
                            FAILURE )                                                                 
      RETURN                                                                                            
    END IF                                                                                            
    CALL DPOTRS( 'U', m, 1, Awork, m, Cwork, m, error_status)                  
    IF(Error_Status /= 0)THEN                                                                    
      CALL Display_Message( ROUTINE_NAME, &                                                           
                            "Error calling LAPACK DPOTRS to solve linear equation", &                                                 
                            FAILURE )                                                                 
      RETURN
    END IF                                                                                            
    Coeff = Cwork(:,1)                                                                              

  END FUNCTION Compute_RegressionCoeff

  !---------------------------------------------------
  ! Derive coefficients using constrained regression:
  !   x = (transpose(A)A + gamma*H)^(-1)transpose(A)Y
  !     = AB^(-1)*AY
  !     where AB = transpose(A)A + gamma*H
  !           AY = transpose(A)Y
  ! note that the AA and H mtrixes are both symmetric
  ! band matrix
  !   with KD = Nvar, where KD is the superdiagnals of AA
  !              and Nvar is the number of predictors + 1
  ! The up triangle of AB is stored in AB_band:
  !   AB_band(KD+1+i-j,j) = AB(i,j) for max(1,j-KD)<=i<=j
  !
  ! For example, the following 6x6 triagle symmetric band
  ! matrix with KD = 2:
  !  a11 a12 a13 
  !  a12 a22 a23 a24
  !  a13 a23 a33 a34 a35
  !      a24 a34 a44 a45 a46
  !          a35 a45 a55 a56
  !              a46 a56 a66
  !
  !  It is stored in a band matrix with dimension (2+1)x6 
  !    *    *   a13  a24  a35  a46     
  !    *   a12  a23  a34  a45  a56     
  !   a11  a22  a33  a44  a55  a66     
  !
  !---------------------------------------------------
  SUBROUTINE SMOOTH_Coeff(PdSub, kp, Gamma, Coeff, ierr)

     !inputs				     

    REAL(fp_kind), intent(in) :: Gamma
    REAL(fp_kind), intent(in), DIMENSION(:,0:,:,:) ::  PdSub
                                    !Npd,0:wNlay,1:Nangle,1:Natm    
    REAL(fp_kind), intent(in), DIMENSION(0:,:,:) ::  Kp
                                    !(0:wNlay,1:Nangle,1:Natm)
 
     REAL(fp_kind), intent(out) :: COEFF(:,:)  
     integer, intent(out) :: Ierr

    ! local						     

    real(fp_kind) :: ONE = 1.0_fp_kind, TWO = 2.0_fp_kind, ZERO = 0.0_fp_kind
    real(fp_kind), allocatable :: H_band(:, :), AA_band(:,:), AB_band(:,:), &
                                  AY(:,:), AA_layer(:,:)
 
    INTEGER :: Nsample, Nw, Nvar, Natm, Nangle 

    INTEGER :: I, J, K, M, N, iatm, iang, offset, KD

    NVAR = SIZE(PdSub, DIM=1) + 1 
    NW = SIZE(PdSub, DIM=2)
    Nangle = SIZE(PdSub, DIM=3)
    Natm = SIZE(PdSub, DIM=4)

    NSAMPLE = Nangle * Natm

    N = Nw * Nvar 
    M = Nsample*Nw
      
    allocate(H_band(Nvar + 1, N), & 
             AA_band(Nvar + 1, N), &
             AB_band(Nvar + 1, N), &
             AY(N, 1), AA_layer(Nvar, Nvar)) 
  
    !--------------------------------------------------------	
    ! H matrix used to constrain vertical coeffienct profile
    ! minimize the difference between adjacent coefficient  
    !--------------------------------------------------------
   H_band(:,:) = ZERO                         
   H_band(1, Nvar+1:N) = -ONE                 
   H_band(Nvar+1, 1:Nvar) = ONE               
   H_band(Nvar+1, Nvar+1:N-Nvar) = TWO        
   H_band(Nvar+1, N-Nvar+1:N) = ONE           

    !-------------------------------------------------------
    ! prepare AA band matrix
    !   Note here AA_layer is a layer Nvar x Nvar matrix
    !       | AA_layer1                       |
    !       |          AA_Layer2              |
    !  AA = |                   .....         |
    !       |                       AA_LayerN |
    ! prepare AY matric
    !-------------------------------------------------------

    AA_band(:,:) = ZERO
    AY(:, 1) = ZERO
    KD = Nvar    

    DO K = 1, Nw

      ! put data in up triangle    
      
      offset = (k-1)*Nvar
      AA_layer(:,:) = ZERO

      do iang = 1, Nangle
      do iatm = 1, Natm

        if(ABS(kp(k-1, iang, iatm)) > TOLERANCE) then

          AA_layer(1,1) = AA_layer(1,1) + ONE
          AY(offset+1, 1) = AY(offset+1, 1) + kp(k-1, iang, iatm)
          do i = 2, Nvar

            AY(offset+i, 1) = AY(offset+i, 1) + &
                   PdSub(i-1, k-1, iang, iatm)*kp(k-1, iang, iatm)
            AA_layer(1, i) = AA_layer(1, i) + PdSub(i-1,k-1,iang, iatm)

            do j = i, Nvar
              AA_layer(i, j) = AA_layer(i, j) + &
              PdSub(i-1,k-1,iang, iatm) * PdSub(j-1,k-1,iang,iatm) 
            enddo
          enddo   

        endif
      enddo
      enddo

      do j = 1, Nvar    
       do i = max(1,j-KD), j     
         AA_band(KD+1+i-j, j+offset) = AA_layer(i,j)     
       enddo   
      enddo    

    ENDDO      

    !----------------------------------------------
    ! prepare AB
    !----------------------------------------------

    AB_band = AA_band + Gamma * H_band

    !-----------------------------------------------------------
    ! Lapack routine to computes the solution
    ! to a real system of linear equations
    !        AB_band * X = AY
    !-----------------------------------------------------------
!!!    call DPBSV( 'U', N, KD, 1, AB_band, Nvar+1, AY, N, ierr )
    ierr = 0
    if(ierr /= 0)then
      print *, 'Error flag returned from regression DPBSV'
    endif

    !--- get coefficients ---
    do k = 1, Nw 
      do i = 1, Nvar
        COEFF(i,k) = AY((k-1)*Nvar+i, 1)
      enddo
    enddo    

    deallocate(H_band,AA_band,AB_band,AY,AA_layer)    
  
  END Subroutine SMOOTH_coeff



       SUBROUTINE SMOOTH_Coeff_vertical(H, PdSub, kp, Gamma, Coeff, ierr)

 
        IMPLICIT NONE

        !inputs                                                                                         

       REAL(fp_kind), intent(in) :: Gamma                                                               
       REAL(fp_kind), intent(in), DIMENSION(:,0:,:,:) ::  PdSub    !Npd, 0:wNlay,1:Nangle,1:Natm,Igas   
       REAL(fp_kind), intent(in), DIMENSION(0:,:,:) ::  Kp  !(0:wNlay,1:Nangle,1:Natm)                  
       REAL(fp_kind)  ::  H(:,:)                                                                        

       ! I/O                                                                                            
        REAL(fp_kind), intent(inout) :: COEFF(:,:)                                                      
        integer, intent(inout) :: Ierr                                                                  

        ! local 

 real(fp_kind), allocatable :: A(:,:), AA(:,:), AA1(:,:)
 real(fp_kind), allocatable ::  YA(:), AAinv(:,:), Y(:), B(:) 

 INTEGER :: Nsample, Nw, Nvar, Natm, Nangle

 real(fp_kind)::SUMM

 INTEGER :: I, J, K, N, M, N1, N2, N3, N4, iatm, iang

 NVAR = SIZE(PdSub, DIM=1) + 1   
 NW = SIZE(PdSub, DIM=2)         
 Nangle = SIZE(PdSub, DIM=3)     
 Natm = SIZE(PdSub, DIM=4)       

 NSAMPLE = Nangle * Natm         

 N = Nw * Nvar                   
 M = Nsample*Nw                  

print*, Nw, Nvar, Nangle, Natm, Nsample, N, M

allocate(A(M,N), AA(N,N))

!--- A

      A(:,:) = 0.0

      DO K = 1, Nw
        j = 0
        DO iatm = 1, Natm
        Do iang = 1, Nangle
        J = j + 1 

        A( (K-1)*NSAMPLE+J, (K-1)*Nvar+1)  = 1.       

        A( (K-1)*NSAMPLE+J, (K-1)*Nvar+2:K*Nvar) &
           = PdSub(1:Nvar-1,K-1,iang, iatm)    

        ENDDO
        ENDDO
      ENDDO

      allocate( YA(N), Y(M))          


       Y(:) = 0.0

      DO J = 1, Nw

        i =0
        DO iatm = 1, Natm
        Do iang = 1, Nangle
  i = i +1

   Y((J-1)*NSAMPLE  +I) = KP(J-1,Iang, Iatm)  

   ENDDO
   ENDDO
 ENDDO

summ = sum(Y)/dble(Natm*Nangle*Nw)

print*, "avg and std of Y", summ, &
      sqrt( sum((Y -summ)**2)/dble(Natm*Nangle*Nw ))

! YA=A(T)*Y

print*, " computation of YA = A(T)*Y, ", N, M

YA(:) = 0.0

DO I = 1, N

  DO K = (I-1)/Nvar*Nsample+1, (I-1)/Nvar*Nsample+Nsample 
      YA(I) = YA(I) + A(K,I) * Y(K)
  ENDDO

ENDDO

print*, " computation of A(T)A"
 

! AA

AA(:,:) = 0.0


DO I = 1, N

  DO J = (I-1)/Nvar*Nvar+1, (I-1)/Nvar*Nvar + Nvar

     DO K = (J-1)/Nvar*Nsample+1, (J-1)/Nvar*Nsample+Nsample  !ignore the zero.
       !----------------------------------------------------------------------!
       ! if Y(I) = 0 and gas/=correction term, this sample can not be counted !
       !----------------------------------------------------------------------!
       if( abs(Y(K)) > TOLERANCE ) then
       AA(I,J) = AA(I,J) + A(K,I) * A(K,J)
       endif  

     ENDDO

  ENDDO

ENDDO
 
! --- to reduce the size of array if the sample number for layer is less than 5
N1 = N
DO I = N+1-Nvar, 1, -Nvar
if(AA(I,I) >= Nvar-1) exit
N1 = I-1
ENDDO

! --- to reduce the size of array if the sample number for top-layer is less than 5
N2 = 0
DO I = 1, N+1-Nvar, Nvar
N2 = I-1
if(AA(I,I) >= Nvar-1) exit
ENDDO

!--- if the sample number >1, the coefficients in the top can not be zero
if(N2 > Nvar) then
N3 = N2
DO I = N2+1, 1, -Nvar
if(AA(I,I) == 0) exit
N3 = I
ENDDO
endif

!--- if the sample number >1, the coefficients in the bottom can not be zero
N4 = N1
DO I = N1+1, N+1-Nvar, Nvar
N4 = I-1
if(AA(I,I) == 0) exit
ENDDO

! array: 1 00 n3 n2 n1 00 n4
 
! --------- since H is not adjust based on the H, the results are ugly sometimes.
! --------- change to H is necessary
 
      deallocate(Y,A)
 
      allocate(B(N)) 
 
 
! -- to change H in the top
do i = N2+1, N2+Nvar 
H(i,i) = 1.0
enddo
! -- to change H in the bottom
do i = N1-Nvar+1, N1
H(i,i) = 1.0
enddo

print*, "gamma = ", gamma  

DO I = 1, N
DO J = 1, N

AA(I,J) = AA(I,J) + GAMMA * H(I,J)

ENDDO
ENDDO


if(N1 <= N2) then
print*, "error in the array size!"
stop
endif

allocate( AAinv(N1-N2,N1-N2) , AA1(N1-N2, N1-N2))  

print*, " computation of AAinV", N1, N2, N3, N4, nvar

AA1(1:N1-N2, 1:N1-N2) = AA(N2+1:N1, N2+1:N1)

!!!	call rinvrs(AA1,AAinv,ierr)

deallocate(AA1)


B(:)=0.

     if(ierr == 0) then 

do i = 1, N1-N2
  summ = 0.
  do j = 1, N1-N2
  summ = summ + YA(N2+j)*AAinv(j,i)
  end do
B(N2+i) = summ
end do 

do i = N1+1, N4
B(i) = B(i-Nvar)
enddo

if(N2 > Nvar) then 
do i = N3, N2
B(i) = B(N2-N3+1+i)
enddo
endif


     else if(ierr == -1) then 

      print*, "WARNING ... singular array in the computation of AAinv", N1 

     endif


COEFF(:,:) = 0.0

DO J = 1, Nw
DO I = 1,Nvar 
COEFF(I,J) = B((J-1)*Nvar  +I)
ENDDO
ENDDO


deallocate(AA,YA,AAinv,B)    

RETURN
END Subroutine SMOOTH_Coeff_vertical




Subroutine Normalization (X, factor)

! --- INPUT
REAL(fp_kind), intent(inout), DIMENSION(0:,:, :) :: X

REAL(fp_kind), intent(out) :: factor


!-- LOCAL 

REAL(fp_kind) :: XAVG, summ 

INTEGER  i,j, k
integer Natm, Nang, Nw


Nw = SIZE(X, DIM=1)-1
Nang = SIZE(X, DIM=2)
Natm = SIZE(X, DIM=3) 

XAVG = sum(X)/dble(Nang*Natm*(Nw+1))


summ = ZERO
do i = 0, Nw
do j = 1, Nang
do k = 1, Natm
summ = summ + (x(i,j,k) - xavg)**2
enddo
enddo
enddo

factor = sqrt(summ/dble(Nang*Natm*(Nw+1)))

X(0:Nw, 1:Nang, 1:Natm) = X(0:Nw, 1:Nang, 1:Natm) / factor 


return 

END Subroutine Normalization



Subroutine Generation_H_matrix( H, Nvar)

!-- input
Integer:: Nvar

! -- output
REAL(fp_kind), intent(out), DIMENSION(:, :) ::  H

! --local 	
Integer :: N, I, J, K
REAL(fp_kind), allocatable :: KK(:,:), KT(:,:)


N = Size(H, DIM=1)

allocate(KK(N,N), KT(N,N))
 

KK(:,:) = 0.

DO I = 2, N-Nvar+1
 
   KK(I,I-1) = 1.
   KK(I, I+Nvar-1) = -1.

ENDDO
 
! H = K*K

H(:,:) = 0.

KT = transpose(KK) 

print*, "multification of array, H=K(T)K"
DO I = 1, N
   DO J = 1, N

    DO K = 1, N
    H(I,J) = H(I,J) + KT(I,K)*KK(K,J)
    ENDDO

  ENDDO
ENDDO 

deallocate(kk, KT)

return

End Subroutine Generation_H_matrix

END MODULE Regression


