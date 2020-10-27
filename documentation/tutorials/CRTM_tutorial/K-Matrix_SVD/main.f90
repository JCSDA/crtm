PROGRAM main
  !
  ! Description:
  !  Program to compute the Singular
  !  Value Decomposition of a pre-
  !  viously computed K-Matrix from
  !  the CRTM using the LAPACK subroutine
  !  DGESVD.
  !
  !  The SVD has the following form:
  !
  !  K = U * S * V'
  !
  ! Author: P. Stegmann
  ! Date: 2019-12-02
  !
  IMPLICIT NONE

  ! Data Dictionary:
  INTEGER(KIND=8), PARAMETER :: M = 92 ! Atmospheric pressure levels
  INTEGER(KIND=8), PARAMETER :: N = 5 ! AMSU-A channels
  REAL(KIND=8), DIMENSION(M,N) :: K    ! CRTM K-Matrix
  REAL(KIND=8), DIMENSION(M,N) :: s
  REAL(KIND=8), DIMENSION(M,M) :: u
  REAL(KIND=8), DIMENSION(N,N) :: v
  REAL(KIND=8), DIMENSION(MIN(M,N)) :: sdiag
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: work
  INTEGER(KIND=8) :: info
  INTEGER(KIND=8) :: lda
  INTEGER(KIND=8) :: ldu
  INTEGER(KIND=8) :: ldv
  CHARACTER :: jobu
  CHARACTER :: jobv
  INTEGER(KIND=4) :: lwork
  INTEGER(KIND=4) :: ii
  
  ! Instructions:
  
  ! Read in the K-Matrix for all AMSU-A channels:
  OPEN(66,FILE='output_K.txt',STATUS='OLD')
  READ(66,*) K
  CLOSE(66)
 
  ! Prepare DGESVD working arrays:
  lwork = MAX(3*MIN(M,N) + MAX(M,N), 5*MIN(M,N))
  ALLOCATE(work(1:lwork))
  jobu = 'A'
  jobv = 'A'
  lda = M
  ldu = M
  ldv = N
 !WRITE(*,*) K 

  ! Compute SVD with LAPACK:
  CALL DGESVD(jobu,jobv,M,N,K,lda,sdiag,u,ldu,v,ldv, work,lwork,info)
  IF( info /= 0 ) THEN
    WRITE(*,*) 'Singular value decomposition failed!'
  END IF
  WRITE(*,*) 'INFO = ', info
  WRITE(*,*) 'The singular values are: '
  WRITE(*,*) sdiag
  DEALLOCATE(work)

  ! Result output:
  ! Singular values:
  OPEN(666,FILE='SVD_result.txt',STATUS='NEW')
  WRITE(666,*) sdiag
  CLOSE(666)

  !Singular vectors:
  OPEN(444,FILE='Singular_vectors.txt',STATUS='NEW')
  DO ii = 1, N
    WRITE(444,*) u(ii,:)
  END DO
  CLOSE(444)
END PROGRAM main
