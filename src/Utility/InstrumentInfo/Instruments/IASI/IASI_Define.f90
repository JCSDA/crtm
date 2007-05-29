!
! IASI_Define
!
! Module containing IASI instrument definitions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Dec-2006
!                       paul.vandelst@ssec.wisc.edu
!

MODULE IASI_Define

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI
  USE Search_Utility       , ONLY: Value_Locate
  USE Linear_Interpolation , ONLY: Linear_Interpolate
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Public parameters
  PUBLIC :: IASI_D_FREQUENCY
  PUBLIC :: IASI_MAX_NCHANNELS
  ! Public module procedures
  PUBLIC :: IASI_MaxX
  PUBLIC :: IASI_X
  PUBLIC :: IASI_GFT
  PUBLIC :: IASI_F
  PUBLIC :: IASI_Resample_Idx
  PUBLIC :: IASI_Resample
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: $'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: POINT5    = 0.5_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: ONEPOINT5 = 1.5_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PARAMETER :: HUNDRED   = 100.0_fp
  REAL(fp), PARAMETER :: M2CM      = HUNDRED
  REAL(fp), PARAMETER :: LN2 = 0.693147180559945309417232_fp
  
  ! Gaussian function FWHM (cm^-1)
  REAL(fp), PARAMETER :: GFT_FWHM = POINT5
  REAL(fp), PARAMETER :: GFT_HWHM = GFT_FWHM/TWO

  ! Laser wavelength (m)
  REAL(fp), PARAMETER :: LASER_WAVELENGTH       = 1.537656349e-06_fp
  REAL(fp), PARAMETER :: LASER_WAVELENGTH_IN_CM = LASER_WAVELENGTH*M2CM

  ! Laser frequency (m^-1)
  REAL(fp), PARAMETER :: LASER_FREQUENCY   = ONE/LASER_WAVELENGTH
  REAL(fp), PARAMETER :: NYQUIST_FREQUENCY = LASER_FREQUENCY/TWO

  ! Field angle (rad)
  REAL(fp), PARAMETER :: FIELD_ANGLE = 0.01605073_fp

  ! Number of double-sided FFT points
  INTEGER,  PARAMETER :: NFFT = 51200

  ! Nominal maximum optical path delay for NFFT (m)
  REAL(fp), PARAMETER :: NOMINAL_MAXX       = 1.9679466e-02_fp
  REAL(fp), PARAMETER :: NOMINAL_MAXX_IN_CM = NOMINAL_MAXX*M2CM

  ! Parameters for the resampled frequency grid
  REAL(fp), PARAMETER :: MIN_FREQUENCY      = 640.0_fp
  REAL(fp), PARAMETER :: IASI_D_FREQUENCY   = 0.25_fp

  INTEGER , PARAMETER :: IASI_MAX_NCHANNELS = 8461

CONTAINS


  ! Function to compute the maximum optical path delay in cm.
  FUNCTION IASI_MaxX(n) RESULT(maxX)
    INTEGER, INTENT(IN) :: n
    REAL(fp) :: maxX
    maxX = REAL((n/2),fp)*(LASER_WAVELENGTH_IN_CM/TWO)*COS(FIELD_ANGLE)
  END FUNCTION IASI_MaxX


  ! Function to compute the optical path delay grid in cm.
  FUNCTION IASI_X(n) RESULT(X)
    ! Arguments
    INTEGER, INTENT(IN) :: n
    ! Function result
    REAL(fp) :: X(n)
    ! Local variables
    REAL(fp) :: maxX
    INTEGER :: i
    
    maxX = IASI_MaxX(n)
    X = (/(REAL(i,fp),i=0,n-1)/)/REAL(n-1,fp)
    X = (X * TWO*maxX) - maxX
  END FUNCTION IASI_X


  ! Function to compute the IASI apodisation function
  FUNCTION IASI_GFT(x) RESULT(gft)
    ! Arguments
    REAL(fp), INTENT(IN) :: x(:)
    ! Function result
    REAL(fp) :: gft(SIZE(x))
    ! Local variables
    INTEGER  :: n
    REAL(fp) :: maxX
    REAL(fp) :: sigma

    ! Compute the IASI GFT
    sigma = LN2/(PI*GFT_HWHM)
    WHERE ( ABS(x) <= NOMINAL_MAXX_IN_CM )
      gft = EXP(-LN2*(x/sigma)**2)
    ELSEWHERE
      gft = ZERO
    END WHERE
  END FUNCTION IASI_GFT
  

  ! Function to compute the resampled
  ! frequency grid
  ! f = 645 + 0.25*(n-1), n=1,8461
  FUNCTION IASI_F() RESULT(f)
    ! Function result
    REAL(fp) :: f(IASI_MAX_NCHANNELS)
    ! Local variables
    INTEGER :: i, n
    DO i = 1, IASI_MAX_NCHANNELS
      f(i) = MIN_FREQUENCY + (IASI_D_FREQUENCY*REAL(i-1,fp))
    END DO
  END FUNCTION IASI_F
  
  
  ! Function to compute the resampling
  ! interpolation indices
  FUNCTION IASI_Resample_Idx( f, f_int ) RESULT( idx )
    REAL(fp), INTENT(IN) :: f(:), f_int(:)
    INTEGER :: idx(SIZE(f_int))
    idx = Value_Locate( f, f_int )
  END FUNCTION IASI_Resample_Idx
  
  
  ! Function to resample a spectrum to the
  ! IASI required frequencies,
  ! f = 645 + 0.25*(n-1), n=1,8461
  FUNCTION IASI_Resample( f_in       , &  ! Input
                          spc_in     , &  ! Input
                          f_out      , &  ! Input
                          spc_out    , &  ! Output
                          f_idx      , &  ! Optional Input
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    REAL(fp)    ,           INTENT(IN)  :: f_in(:) , spc_in(:), f_out(:)
    REAL(fp)    ,           INTENT(OUT) :: spc_out(:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: f_idx(:)
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IASI_Resample'
    ! Local variables
    INTEGER :: idx(SIZE(f_out))
    
    Error_Status = SUCCESS
    
    IF ( PRESENT(f_idx) ) THEN
      idx = f_idx
    ELSE
      idx = Value_Locate( f_in, f_out )
    END IF
    
    Error_Status = Linear_Interpolate( f_in     , &  ! Input
                                       spc_in   , &  ! Input
                                       f_out    , &  ! Input
                                       spc_out  , &  ! Output
                                       x_idx=idx  )  ! Optional input
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error interpolating input IASI spectrum', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
                            
  END FUNCTION IASI_Resample

END MODULE IASI_Define
