!
! IASI_Define
!
! Module containing IASI instrument definitions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 08-Dec-2006
!                       paul.vandelst@noaa.gov
!

MODULE IASI_Define

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI, LN2
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Parameters
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
  ! Procedures
  PUBLIC :: IASI_MaxX
  PUBLIC :: IASI_X
  PUBLIC :: IASI_F
  PUBLIC :: IASI_ApodFunction
  PUBLIC :: IASI_nPts
  PUBLIC :: IASI_Channels
  PUBLIC :: IASI_DefineVersion
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: POINT5    = 0.5_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: ONEPOINT5 = 1.5_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PARAMETER :: HUNDRED   = 100.0_fp
  REAL(fp), PARAMETER :: M2CM      = HUNDRED
  

  ! Band parameters
  INTEGER,  PARAMETER :: N_IASI_BANDS    = 3
  INTEGER,  PARAMETER :: N_IASI_CHANNELS = 8461
  !..Band frequencies
  REAL(fp), PARAMETER :: IASI_BAND_F1(N_IASI_BANDS) = (/  645.00_fp, 1210.00_fp, 2000.0_fp /)
  REAL(fp), PARAMETER :: IASI_BAND_F2(N_IASI_BANDS) = (/ 1209.75_fp, 1999.75_fp, 2760.0_fp /)
  !..The channel numbering for each band
  INTEGER, PARAMETER :: IASI_BAND_BEGIN_CHANNEL( N_IASI_BANDS) = (/    1, 2261, 5421 /)
  INTEGER, PARAMETER :: IASI_BAND_END_CHANNEL(   N_IASI_BANDS) = (/ 2260, 5420, 8461 /)
  INTEGER, PARAMETER :: N_IASI_CHANNELS_PER_BAND(N_IASI_BANDS) = (/ 2260, 3160, 3041 /)
  INTEGER, PARAMETER :: MAX_N_IASI_BAND_CHANNELS = 3160
  !..Band names
  CHARACTER(*), PARAMETER :: IASI_BAND(N_IASI_BANDS) = (/ 'B1','B2','B3'/)
  
  ! Instrument parameters
  !..Gaussian function FWHM (cm^-1)
  REAL(fp), PARAMETER :: GFT_FWHM = POINT5
  REAL(fp), PARAMETER :: GFT_HWHM = GFT_FWHM/TWO
  !..Laser wavelength (m)
  REAL(fp), PARAMETER :: LASER_WAVELENGTH_IN_M = 1.537656349e-06_fp
  REAL(fp), PARAMETER :: LASER_WAVELENGTH      = LASER_WAVELENGTH_IN_M*M2CM
  !..Laser frequency (m^-1)
  REAL(fp), PARAMETER :: LASER_FREQUENCY   = ONE/LASER_WAVELENGTH_IN_M
  REAL(fp), PARAMETER :: NYQUIST_FREQUENCY = LASER_FREQUENCY/TWO
  !..Field angle (rad)
  REAL(fp), PARAMETER :: FIELD_ANGLE = 0.01605073_fp
  !..Number of double-sided FFT points
  INTEGER,  PARAMETER :: N_IASI_FFT(N_IASI_BANDS) = 51200
  !..Nominal maximum optical path delay for N_IASI_FFT (m)
  REAL(fp), PARAMETER :: NOMINAL_MAXX_IN_M(N_IASI_BANDS) = 1.9679466e-02_fp
  REAL(fp), PARAMETER :: NOMINAL_MAXX(N_IASI_BANDS)      = NOMINAL_MAXX_IN_M*M2CM

  ! Parameters for the resampled frequency grid
  REAL(fp), PARAMETER :: IASI_MIN_FREQUENCY = 645.0_fp
  REAL(fp), PARAMETER :: IASI_MAX_FREQUENCY = 2760.0_fp
  REAL(fp), PARAMETER :: IASI_D_FREQUENCY(N_IASI_BANDS)   = 0.25_fp
  REAL(fp), PARAMETER :: IASI_RESAMPLE_MAXX(N_IASI_BANDS) = TWO

  
CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_MaxX
!
! PURPOSE:
!       Pure function to return the IASI maximum optical path delay.
!
! CALLING SEQUENCE:
!       maxX = IASI_MaxX(band, nominal=nominal)
!
! INPUT ARGUMENTS:
!       band:     IASI band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       nominal:  Set this logical argument to return the nominal value of
!                 the IASI maximum OPD rather than the computed one.
!                 If == .FALSE., the computed value of maxX is returned,
!                                maxX = 0.5 * n_FFT *. laser_wavelength * COS(field_angle)
!                    == .TRUE.,  the nominal defined fixed value is returned
!                 UNITS:      N/A
!                 TYPE:       LOGICAL
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       maxX:     Maximum optical path delay of the IASI instrument.
!                 UNITS:      Centimetres (cm)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_MaxX(band,nominal) RESULT(maxX)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    LOGICAL, OPTIONAL, INTENT(IN) :: nominal
    ! Function result
    REAL(fp) :: maxX
    ! Variables
    INTEGER  :: ib
    LOGICAL  :: computed
    
    ! Setup
    ! ...Check band
    ib = MAX(MIN(band,N_IASI_BANDS),1)
    ! ...Check nominal argument
    computed = .TRUE.
    IF ( PRESENT(nominal) ) computed = .NOT. nominal
    
    ! Determine optical path delay
    IF ( computed ) THEN
      maxX = REAL((N_IASI_FFT(ib)/2),fp)*(LASER_WAVELENGTH/TWO)*COS(FIELD_ANGLE)
    ELSE
      maxX = NOMINAL_MAXX(ib)
    END IF
    
  END FUNCTION IASI_MaxX


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_X
!
! PURPOSE:
!       Pure function to compute the IASI double-sided optical delay grid.
!
! CALLING SEQUENCE:
!       x = IASI_X(band, n)
!
! INPUT ARGUMENTS:
!       band:      IASI band number (1, 2, or 3).
!                  If band < 1, then 1 is used.
!                     band > 3, then 3 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
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
!                  DIMENSION:  Rank-1 (n)
!
! COMMENTS:
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
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_X(band, n) RESULT(X)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    INTEGER, INTENT(IN) :: n
    ! Function result
    REAL(fp) :: X(n)
    ! Local variables
    REAL(fp) :: maxX
    INTEGER :: ib, i, nHalf

    ib = MAX(MIN(band,N_IASI_BANDS),1)
    ! Get the number of positive delays
    nHalf = n/2
    ! Compute maximum optical delay
    maxX = NOMINAL_MAXX(ib)
    ! Fill the grid array
    X(nHalf:n) = (/(REAL(i,fp),i=0,nHalf)/)/REAL(nHalf,fp)
    X(1:nHalf-1) = -X(n-1:nHalf+1:-1)
    X = X*maxX
  END FUNCTION IASI_X


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_ApodFunction
!
! PURPOSE:
!       Pure function to compute the IASI apodisation function for a given 
!       optical delay grid.
!
! CALLING SEQUENCE:
!       afn = IASI_ApodFunction(band, x)
!
! INPUT ARGUMENTS:
!       band:      IASI band number (1, 2, or 3).
!                  If band < 1, then 1 is used.
!                     band > 3, then 3 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!       x:         Double-sided optical delay grid.
!                  UNITS:      Centimetres (cm)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       afn:       IASI apodisation function.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Same as input x argument.
!
! COMMENTS:
!       The IASI apodisation function is a truncated Gaussian:
!
!                    ln(2)
!         sigma = -----------; where 0.25 = Gaussian HWHM
!                  PI . 0.25
!
!                                    2
!                  (        [   x   ] )
!         afn = EXP( -ln(2).[-------] )   for  |x| <= nominal MaxX
!                  (        [ sigma ] )
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_ApodFunction(band, x) RESULT(afn)
    ! Arguments
    INTEGER,  INTENT(IN) :: band
    REAL(fp), INTENT(IN) :: x(:)
    ! Function result
    REAL(fp) :: afn(SIZE(x))
    ! Local variables
    INTEGER  :: ib
    REAL(fp) :: sigma

    ib = MAX(MIN(band,N_IASI_BANDS),1)
    sigma = LN2/(PI*GFT_HWHM)
    WHERE ( ABS(x) <= NOMINAL_MAXX(ib) )
      afn = EXP(-LN2*(x/sigma)**2)
    ELSEWHERE
      afn = ZERO
    END WHERE
  END FUNCTION IASI_ApodFunction
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_nPts
!
! PURPOSE:
!       Pure function to compute the number of spectral points in an IASI band.
!
! CALLING SEQUENCE:
!       n = IASI_nPts(band)
!
! INPUT ARGUMENTS:
!       band:      IASI band number (1, 2, or 3).
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
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_nPts(band) RESULT(n)
    INTEGER, INTENT(IN) :: band
    INTEGER :: n
    INTEGER :: ib
    ib = MAX(MIN(band,N_IASI_BANDS),1)
    n = INT((IASI_BAND_F2(ib)-IASI_BAND_F1(ib))/IASI_D_FREQUENCY(ib) + ONEPOINT5)
  END FUNCTION IASI_nPts


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_F
!
! PURPOSE:
!       Pure function to compute the resampled frequency grid for an IASI band.
!
! CALLING SEQUENCE:
!       f = IASI_F(band)
!
! INPUT ARGUMENTS:
!       band:      IASI band number (1, 2, or 3).
!                  If band < 1, then 1 is used.
!                     band > 3, then 3 is used.
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
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_F(band) RESULT(f)
    INTEGER, INTENT(IN) :: band
    REAL(fp) :: f(IASI_nPts(band))
    INTEGER :: i, ib   
    ib = MAX(MIN(band,N_IASI_BANDS),1)
    DO i = 1, IASI_nPts(ib)
      f(i) = IASI_BAND_F1(ib) + (IASI_D_FREQUENCY(ib)*REAL(i-1,fp))
    END DO
  END FUNCTION IASI_F
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_Channels
!
! PURPOSE:
!       Pure function to compute the channel numbers for an IASI band.
!
! CALLING SEQUENCE:
!       ch = IASI_Channels(band)
!
! INPUT ARGUMENTS:
!       band:      IASI band number (1, 2, or 3).
!                  If band < 1, then 1 is used.
!                     band > 3, then 3 is used.
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
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_Channels(band) RESULT(ch)
    INTEGER, INTENT(IN) :: band
    INTEGER :: ch(IASI_nPts(band))
    INTEGER :: i, ib, n
    ib = MAX(MIN(band,N_IASI_BANDS),1)
    n = 0
    DO i = 1, ib-1
      n = n + IASI_nPts(i)
    END DO
    ch = (/(i,i=1,IASI_nPts(ib))/) + n
  END FUNCTION IASI_Channels


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL IASI_DefineVersion( Id )
!
! OUTPUTS:
!       Id:   Character string containing the version Id information
!             for the module.
!             UNITS:      N/A
!             TYPE:       CHARACTER(*)
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IASI_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE IASI_DefineVersion

END MODULE IASI_Define
