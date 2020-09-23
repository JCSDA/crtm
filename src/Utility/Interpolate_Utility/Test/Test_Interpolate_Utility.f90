!
! Program to test the Interpolate_Utility module procedures
!
PROGRAM Test_Interpolate_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds,          ONLY: fp=>fp_kind
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, Display_Message
  USE File_Utility,        ONLY: Get_Lun
  USE Search_Utility,      ONLY: Value_Locate
  USE Interpolate_Utility, ONLY: Linear_Interpolate, &
                                 Polynomial_Interpolate, &
                                 Spline_Initialize, Spline_Interpolate
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_Interpolate_Utility'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  INTEGER,      PARAMETER :: N  = 25
!  INTEGER,      PARAMETER :: N  = 12
  INTEGER,      PARAMETER :: NI = 78
  INTEGER,      PARAMETER :: NORDERS = 6
  INTEGER,      PARAMETER, DIMENSION(NORDERS) :: ORDER=(/1,3,5,7,9,11/)
  REAL(fp),     PARAMETER :: ZERO = 0.0_fp
  CHARACTER(*), PARAMETER, DIMENSION(2) :: FTYPE=(/'scalar','rank-1'/)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: message
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: FileID
  INTEGER :: i, j, k
  REAL(fp), DIMENSION(N) :: x, y
  REAL(fp), DIMENSION(N) :: y2
  REAL(fp), DIMENSION(NI) :: xInt, yTrue
  INTEGER,  DIMENSION(NI) :: x_idx
  REAL(fp), DIMENSION(NI,2)         :: LinInt
  REAL(fp), DIMENSION(NI,NORDERS,2) :: PolyInt
  REAL(fp), DIMENSION(NI,2)         :: SplineInt
  REAL(fp), DIMENSION(2)         :: LinError
  REAL(fp), DIMENSION(NORDERS,2) :: PolyError
  REAL(fp), DIMENSION(2)         :: SplineError


  ! ------------------------------
  ! Create the data to interpolate
  ! ------------------------------
  ! Define the x-data
  x = (/ 0.0_fp,  0.01_fp, 0.05_fp, 0.12_fp, 0.14_fp, &
         0.22_fp, 0.30_fp, 0.32_fp, 0.33_fp, 0.335_fp,&
         0.36_fp, 0.39_fp, 0.40_fp, 0.405_fp,0.44_fp, &
         0.5_fp,  0.54_fp, 0.62_fp, 0.64_fp, 0.65_fp, &
         0.66_fp, 0.70_fp, 0.75_fp, 0.79_fp, 0.80_fp /)
!  x = (/ 0.0_fp,  0.12_fp,  0.22_fp, 0.32_fp, &
!         0.36_fp, 0.44_fp,  0.45_fp, 0.54_fp, &
!         0.64_fp, 0.70_fp,  0.72_fp, 0.80_fp /)
  ! Calculate the y-data
  y = Func(x)


  ! -----------------------------------------------------
  ! Create regular grid at which interpolates are desired
  ! -----------------------------------------------------
  xInt = (/ ( REAL(i-1), i=1,NI ) /) / REAL(NI-1)
  xInt = xInt * ( MAXVAL(x) - MINVAL(x) ) + MINVAL(x)
  ! Calculate the true curve
  yTrue = Func( xInt )


  ! --------------------
  ! Interpolate the data
  ! --------------------
  ! Initialise error sums
  PolyError   = ZERO
  SplineError = ZERO
  WRITE( *, '( /5x, "Interpolating...." )' )
  ! Linear scalar
  x_idx = Value_Locate(x,xint)
  DO i = 1, NI
    Error_Status = Linear_Interpolate( x, y, xInt(i), LinInt(i,1) )
  END DO
  LinError(1) = SUM(ABS(yTrue-LinInt(:,1)))

  ! Linear rank-1
  Error_Status = Linear_Interpolate( x, y, xInt, LinInt(:,2) )
  LinError(2) = SUM(ABS(yTrue-LinInt(:,2)))

  ! Polynomial scalar
  x_idx = Value_Locate(x,xint)
  DO j = 1, NORDERS
    DO i = 1, NI
      Error_Status = Polynomial_Interpolate( x, y, &
                                             xInt(i), PolyInt(i,j,1), &
                                             x_idx=x_idx(i), &
                                             Order=ORDER(j) )
    END DO
    PolyError(j,1) = SUM(ABS(yTrue-PolyInt(:,j,1)))
  END DO

  ! Polynomial rank-1
  DO j = 1, NORDERS
    Error_Status = Polynomial_Interpolate( x, y, &
                                           xInt, PolyInt(:,j,2), &
                                           Order=ORDER(j) )
    PolyError(j,2) = SUM(ABS(yTrue-PolyInt(:,j,2)))
  END DO

  ! Spline scalar
  Error_Status = Spline_Initialize( x, y, y2 )
  DO i = 1, NI
    Error_Status = Spline_Interpolate( x, y, xInt(i), SplineInt(i,1), y2=y2 )
  END DO
  SplineError(1) = SUM(ABS(yTrue-SplineInt(:,1)))

  ! Spline rank-1
  Error_Status = Spline_Interpolate( x, y, xInt, SplineInt(:,2) )
  SplineError(2) = SUM(ABS(yTrue-SplineInt(:,2)))


  ! ----------------------
  ! Output the error summs
  ! ----------------------
  WRITE( *, '( /5x, "Error sums, SUM(ABS(yTrue-yInt)):" )' )
  DO k = 1, 2
    WRITE( *, '( 5x, "LinInt ", a, ": ", es13.6 )' ) FTYPE(k), LinError(k)
  END DO
  DO k = 1, 2
    WRITE( *, '( 5x, "PolyInt ", a )' ) FTYPE(k)
    DO j = 1, NORDERS
      WRITE( *, '( 7x, "Order ", i2, ": ", es13.6 )' ) ORDER(j),PolyError(j,k) 
    END DO
  END DO
  DO k = 1, 2
    WRITE( *, '( 5x, "SplineInt ", a, ": ", es13.6 )' ) FTYPE(k), SplineError(k)
  END DO


  ! -----------------------
  ! Write the original data
  ! -----------------------
  FileID = Open_File('orig_xy.dat') 
  DO i = 1, N
    WRITE( FileID, '( 2( 1x, es20.11e3 ) )' ) x(i), y(i)
  END DO
  CLOSE( FileID )


  ! ----------------------------------
  ! Write the linear interpolated data
  ! ----------------------------------
  DO k = 1, 2
    IF( k==1 ) THEN
      FileID=Open_File('linint-scalar_xy.dat')
    ELSE 
      FileID=Open_File('linint-rank1_xy.dat') 
    END IF
    DO i = 1, NI
      WRITE( FileID, '( 12(1x,es20.11e3,:) )' ) xInt(i), yTrue(i), LinInt(i,k)
    END DO
    CLOSE( FileID )
  END DO


  ! --------------------------------------
  ! Write the polynomial interpolated data
  ! --------------------------------------
  DO k = 1, 2
    IF( k==1 ) THEN
      FileID=Open_File('polyint-scalar_xy.dat')
    ELSE 
      FileID=Open_File('polyint-rank1_xy.dat') 
    END IF
    DO i = 1, NI
      WRITE( FileID, '( 12(1x,es20.11e3,:) )' ) xInt(i), yTrue(i), (PolyInt(i,j,k),j=1,NORDERS)
    END DO
    CLOSE( FileID )
  END DO


  ! Write the spline interpolated data
  ! ----------------------------------
  DO k = 1, 2
    IF( k==1 ) THEN
      FileID=Open_File('splineint-scalar_xy.dat')
    ELSE 
      FileID=Open_File('splineint-rank1_xy.dat') 
    END IF
    DO i = 1, NI
      WRITE( FileID, '( 12(1x,es20.11e3,:))' ) xInt(i), yTrue(i), SplineInt(i,k)
    END DO
    CLOSE( FileID )
  END DO


CONTAINS


  FUNCTION Func( x ) RESULT( y )
    REAL(fp), DIMENSION(:), INTENT(IN) :: x
    REAL(fp), DIMENSION(SIZE(x)) :: y
    INTEGER,  PARAMETER :: N_COEFF = 5
    REAL(fp), PARAMETER, DIMENSION( 0:N_COEFF ) :: COEFF = &
      (/   0.2_fp, -25.0_fp,  200.0_fp, &
        -675.0_fp, 900.0_fp, -400.0_fp  /)
    INTEGER :: i
    y = COEFF( N_COEFF )
    DO i = N_COEFF - 1, 0, -1
      y = ( x * y ) + COEFF( i )
    END DO
  END FUNCTION Func

  FUNCTION Open_File( Filename ) RESULT( FileID )
    CHARACTER(*), INTENT(IN) :: Filename
    INTEGER :: FileID
    FileID = Get_Lun()
    OPEN( FileID, FILE   = Filename, &
                  FORM   = 'FORMATTED', &
                  ACCESS = 'SEQUENTIAL', &
                  STATUS = 'REPLACE' )
  END FUNCTION Open_File

END PROGRAM Test_Interpolate_Utility
