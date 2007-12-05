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
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Public parameters
  PUBLIC :: N_IASI_FFT
  PUBLIC :: IASI_MIN_FREQUENCY
  PUBLIC :: IASI_MAX_FREQUENCY
  PUBLIC :: IASI_D_FREQUENCY
  PUBLIC :: IASI_RESAMPLE_MAXX
  PUBLIC :: N_IASI_BANDS
  PUBLIC :: N_IASI_CHANNELS
  PUBLIC :: IASI_BAND_F1
  PUBLIC :: IASI_BAND_F2
  PUBLIC :: IASI_BAND
  PUBLIC :: IASI_BAND_BEGIN_CHANNEL
  PUBLIC :: IASI_BAND_END_CHANNEL
  PUBLIC :: N_IASI_CHANNELS_PER_BAND
  PUBLIC :: MAX_N_IASI_BAND_CHANNELS
  ! Public module procedures
  PUBLIC :: IASI_MaxX
  PUBLIC :: IASI_X
  PUBLIC :: IASI_F
  PUBLIC :: IASI_GFT
  PUBLIC :: IASI_nPts
  PUBLIC :: IASI_Channels
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: $'
  ! Keyword set value
  INTEGER,  PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: POINT5    = 0.5_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: ONEPOINT5 = 1.5_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PARAMETER :: HUNDRED   = 100.0_fp
  REAL(fp), PARAMETER :: M2CM      = HUNDRED
  REAL(fp), PARAMETER :: LN2 = 0.693147180559945309417232_fp
  
  ! Instrument parameters
  ! ---------------------
  ! Gaussian function FWHM (cm^-1)
  REAL(fp), PARAMETER :: GFT_FWHM = POINT5
  REAL(fp), PARAMETER :: GFT_HWHM = GFT_FWHM/TWO

  ! Laser wavelength (m)
  REAL(fp), PARAMETER :: LASER_WAVELENGTH_IN_M  = 1.537656349e-06_fp
  REAL(fp), PARAMETER :: LASER_WAVELENGTH       = LASER_WAVELENGTH_IN_M*M2CM

  ! Laser frequency (m^-1)
  REAL(fp), PARAMETER :: LASER_FREQUENCY   = ONE/LASER_WAVELENGTH_IN_M
  REAL(fp), PARAMETER :: NYQUIST_FREQUENCY = LASER_FREQUENCY/TWO

  ! Field angle (rad)
  REAL(fp), PARAMETER :: FIELD_ANGLE = 0.01605073_fp

  ! Number of double-sided FFT points
  INTEGER,  PARAMETER :: N_IASI_FFT = 51200

  ! Nominal maximum optical path delay for N_IASI_FFT (m)
  REAL(fp), PARAMETER :: NOMINAL_MAXX_IN_M = 1.9679466e-02_fp
  REAL(fp), PARAMETER :: NOMINAL_MAXX      = NOMINAL_MAXX_IN_M*M2CM

  ! Parameters for the resampled frequency grid
  REAL(fp), PARAMETER :: IASI_MIN_FREQUENCY = 645.0_fp
  REAL(fp), PARAMETER :: IASI_MAX_FREQUENCY = 2760.0_fp
  REAL(fp), PARAMETER :: IASI_D_FREQUENCY   = 0.25_fp
  REAL(fp), PARAMETER :: IASI_RESAMPLE_MAXX = TWO


  ! Band parameters
  ! ---------------
  INTEGER,  PARAMETER :: N_IASI_BANDS    = 3
  INTEGER,  PARAMETER :: N_IASI_CHANNELS = 8461

  ! Band frequencies
  REAL(fp), PARAMETER :: IASI_BAND_F1(N_IASI_BANDS) = (/  645.00_fp, 1210.00_fp, 2000.0_fp /)
  REAL(fp), PARAMETER :: IASI_BAND_F2(N_IASI_BANDS) = (/ 1209.75_fp, 1999.75_fp, 2760.0_fp /)

  ! The IASI band names
  CHARACTER(*), PARAMETER :: IASI_BAND(N_IASI_BANDS) = (/ 'B1','B2','B3'/)

  ! The channel numbering for each band
  INTEGER, PARAMETER :: IASI_BAND_BEGIN_CHANNEL( N_IASI_BANDS) = (/    1, 2261, 5421 /)
  INTEGER, PARAMETER :: IASI_BAND_END_CHANNEL(   N_IASI_BANDS) = (/ 2260, 5420, 8461 /)
  INTEGER, PARAMETER :: N_IASI_CHANNELS_PER_BAND(N_IASI_BANDS) = (/ 2260, 3160, 3041 /)
  INTEGER, PARAMETER :: MAX_N_IASI_BAND_CHANNELS = 3160
  
  
CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       IASI_MaxX
!
! PURPOSE:
!       Pure function to return the IASI maximum optical path delay.
!
!         maxX = 
!
! CALLING SEQUENCE:
!       maxX = IASI_MaxX(nominal=nominal)
!
! OPTIONAL INPUT ARGUMENTS:
!       nominal:  Set this argument to return the nominal value of the IASI
!                 max. OPD rather than the computed one.
!                 If == 0, the computed value of maxX is returned,
!                            maxX = 0.5 * n_FFT *. laser_wavelength * cos(mean_field_angle)
!                    == 1, the nominal fixed value is returned,
!                            maxX = 1.9679466cm
!
! FUNCTION RESULT:
!       maxX:   Maximum optical path delay of the IASI instrument.
!               UNITS:      Centimetres (cm)
!               TYPE:       REAL(fp)
!               DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_MaxX(nominal) RESULT(maxX)
    INTEGER, OPTIONAL, INTENT(IN) :: nominal
    REAL(fp) :: maxX
    maxX = REAL((N_IASI_FFT/2),fp)*(LASER_WAVELENGTH/TWO)*COS(FIELD_ANGLE)
    IF ( PRESENT(nominal) ) THEN
      IF ( nominal == SET ) maxX = NOMINAL_MAXX
    END IF
  END FUNCTION IASI_MaxX


!--------------------------------------------------------------------------------
!
! NAME:
!       IASI_X
!
! PURPOSE:
!       Pure function to compute the IASI double-sided optical delay grid.
!
! CALLING SEQUENCE:
!       x = IASI_X(n)
!
! INPUT ARGUMENTS:
!       n:         The number of points in the double-sided interferogram
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       x:         IASI double-sided optical delay grid.
!                  UNITS:      Centimetres (cm)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1 (N_IASI_FFT=51200)
!
! COMMENTS:
!       The function result size (51200) is defined as the public named
!       parameter, N_IASI_FFT, in this module.
!
!       The output array looks like,
!
!          X=dx-maxX     X=0cm           X=maxX
!              |         (ZPD)             |
!              |           |               | 
!              v           v               v
!
!              x   x   x   #   o   o   o   o
!                                   
!                      --->|   |<---
!                           dx
!
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_X(n) RESULT(X)
    ! Arguments
    INTEGER, INTENT(IN) :: n
    ! Function result
    REAL(fp) :: X(n)
    ! Local variables
    REAL(fp) :: maxX
    INTEGER :: i, nHalf
    ! Get the number of positive delays
    nHalf = n/2
    ! Compute maximum optical delay
    maxX = NOMINAL_MAXX
    ! Fill the grid array
    X(nHalf:n) = (/(REAL(i,fp),i=0,nHalf)/)/REAL(nHalf,fp)
    X(1:nHalf-1) = -X(n-1:nHalf+1:-1)
    X = X*maxX
  END FUNCTION IASI_X


!--------------------------------------------------------------------------------
!
! NAME:
!       IASI_GFT
!
! PURPOSE:
!       Pure function to compute the IASI apodisation function for a given 
!       optical delay grid.
!
! CALLING SEQUENCE:
!       gft = IASI_GFT(x)
!
! INPUT ARGUMENTS:
!       x:         Double-sided optical delay grid.
!                  UNITS:      Centimetres (cm)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       gft:       IASI apodisation function.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Same as input x argument.
!
! COMMENTS:
!                  ln(2)
!       sigma = -----------; where 0.25 = GFT HWHM
!                PI . 0.25
!
!                                  2
!                (        [   x   ] )
!       gft = EXP( -ln(2).[-------] )
!                (        [ sigma ] )
!
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_GFT(x) RESULT(gft)
    ! Arguments
    REAL(fp), INTENT(IN) :: x(:)
    ! Function result
    REAL(fp) :: gft(SIZE(x))
    ! Local variables
    REAL(fp) :: sigma

    ! Compute the IASI GFT
    sigma = LN2/(PI*GFT_HWHM)
    WHERE ( ABS(x) <= NOMINAL_MAXX )
      gft = EXP(-LN2*(x/sigma)**2)
    ELSEWHERE
      gft = ZERO
    END WHERE
  END FUNCTION IASI_GFT
  

!--------------------------------------------------------------------------------
!
! NAME:
!       IASI_nPts
!
! PURPOSE:
!       Pure function to compute the number of spectral points in an IASI band.
!
! CALLING SEQUENCE:
!       n = IASI_nPts(Band)
!
! INPUT ARGUMENTS:
!       Band:      IASI band number (1, 2, or 3).
!                  If Band < 1, then 1 is used.
!                     Band > 3, then 3 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:         Number of spectral points for the specified IASI band.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
! COMMENTS:
!       From the CNES website, http://132.149.11.177/IASI, the IASI band
!       definitions are,
!
!         Band   Range (cm-¹)    Range (µm)  
!         -----------------------------------
!          1     645  to 1210    15.5 to 8.26
!          2     1210 to 2000    8.26 to 5   
!          3     2000 to 2760    5 to 3.62   
!
!       This function does not count the replicated end-points for bands 1 and 2.
!       That is, the end frequenciues for these two bands are 1209.75 and
!       1999.75cm-1 respectively.
!
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_nPts(iBand) RESULT(n)
    INTEGER, INTENT(IN) :: iBand
    INTEGER :: n
    INTEGER :: ib
    ib = MAX(MIN(iBand,N_IASI_BANDS),1)
    n = INT((IASI_BAND_F2(ib)-IASI_BAND_F1(ib))/IASI_D_FREQUENCY + ONEPOINT5)
  END FUNCTION IASI_nPts


!--------------------------------------------------------------------------------
!
! NAME:
!       IASI_F
!
! PURPOSE:
!       Pure function to compute the resampled frequency grid for an IASI band.
!
! CALLING SEQUENCE:
!       f = IASI_F(Band)
!
! INPUT ARGUMENTS:
!       Band:      IASI band number (1, 2, or 3).
!                  If Band < 1, then 1 is used.
!                     Band > 3, then 3 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       f:         The spectral frequency grid for the specified IASI band.
!                  UNITS:      Inverse centimetres (cm^-1)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!
! COMMENTS:
!       From the CNES website, http://132.149.11.177/IASI, the IASI band
!       definitions are,
!
!         Band   Range (cm-¹)    Range (µm)  
!         -----------------------------------
!          1     645  to 1210    15.5 to 8.26
!          2     1210 to 2000    8.26 to 5   
!          3     2000 to 2760    5 to 3.62   
!
!       This function does not replicate the end-points for bands 1 and 2.
!       That is, the end frequenciues for these two bands are 1209.75 and
!       1999.75cm-1 respectively.
!
!       The function, IASI_nPts(), can be used to compute the number of spectral
!       points in each band.
!
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_F(iBand) RESULT(f)
    INTEGER, INTENT(IN) :: iBand
    REAL(fp) :: f(IASI_nPts(iBand))
    INTEGER :: i, ib, n
    ib = MAX(MIN(iBand,N_IASI_BANDS),1)
    n = IASI_nPts(ib)
    DO i = 1, n
      f(i) = IASI_BAND_F1(ib) + (IASI_D_FREQUENCY*REAL(i-1,fp))
    END DO
  END FUNCTION IASI_F
  
  
!--------------------------------------------------------------------------------
!
! NAME:
!       IASI_Channels
!
! PURPOSE:
!       Pure function to compute the channel numbers for an IASI band.
!
! CALLING SEQUENCE:
!       ch = IASI_Channels(Band)
!
! INPUT ARGUMENTS:
!       Band:      IASI band number (1, 2, or 3).
!                  If Band < 1, then 1 is used.
!                     Band > 3, then 3 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch:        The channel numbers for the specified IASI band.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Rank-1
!
! COMMENTS:
!       The function, IASI_nPts(), can be used to compute the number of spectral
!       channels in each band.
!
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_Channels(iBand) RESULT(c)
    INTEGER, INTENT(IN) :: iBand
    INTEGER :: c(IASI_nPts(iBand))
    INTEGER :: i, ib, n
    ib = MAX(MIN(iBand,N_IASI_BANDS),1)
    n = 0
    DO i = 1, ib-1
      n = n + IASI_nPts(i)
    END DO
    c = (/(i,i=1,IASI_nPts(ib))/) + n
  END FUNCTION IASI_Channels

END MODULE IASI_Define
