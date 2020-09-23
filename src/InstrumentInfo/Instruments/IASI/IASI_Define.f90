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
  PUBLIC :: N_IASI_PLATFORMS
  PUBLIC :: IASI_PLATFORM_NAME
  PUBLIC :: N_IASI_BANDS
  PUBLIC :: N_IASI_CHANNELS
  ! Procedures
  PUBLIC :: IASI_nFFT
  PUBLIC :: IASI_MaxX
  PUBLIC :: IASI_X
  PUBLIC :: IASI_F
  PUBLIC :: IASI_ApodFunction
  PUBLIC :: IASI_BeginF
  PUBLIC :: IASI_EndF
  PUBLIC :: IASI_dF
  PUBLIC :: IASI_BeginChannel
  PUBLIC :: IASI_EndChannel
  PUBLIC :: IASI_nPts
  PUBLIC :: IASI_Channels
  PUBLIC :: IASI_BandName
  PUBLIC :: IASI_DefineVersion
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: POINT5    = 0.5_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: ONEPOINT5 = 1.5_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PARAMETER :: HUNDRED   = 100.0_fp
  REAL(fp), PARAMETER :: M2CM      = HUNDRED
  

  ! Platform parameters
  INTEGER     , PARAMETER :: N_IASI_PLATFORMS = 3
  CHARACTER(*), PARAMETER :: IASI_PLATFORM_NAME(N_IASI_PLATFORMS) = &
    (/ 'metop-a', &
       'metop-b', &
       'metop-c' /)
      

  ! Instrument parameters
  ! ...Number of bands and channels
  INTEGER, PARAMETER :: N_IASI_BANDS    = 3
  INTEGER, PARAMETER :: N_IASI_CHANNELS = 8461
  ! ...Gaussian function FWHM (cm^-1)
  REAL(fp), PARAMETER :: GFT_FWHM = POINT5
  REAL(fp), PARAMETER :: GFT_HWHM = GFT_FWHM/TWO
  ! ...Laser wavelength (m)
  REAL(fp), PARAMETER :: LASER_WAVELENGTH_IN_M = 1.537656349e-06_fp
  REAL(fp), PARAMETER :: LASER_WAVELENGTH      = LASER_WAVELENGTH_IN_M*M2CM
  ! ...Laser frequency (m^-1)
  REAL(fp), PARAMETER :: LASER_FREQUENCY   = ONE/LASER_WAVELENGTH_IN_M
  ! ...Sampling and Nyquist frequencies
  REAL(fp), PARAMETER :: SAMPLING_FREQUENCY = LASER_FREQUENCY*TWO  ! Every zero crossing of laser signal
  REAL(fp), PARAMETER :: NYQUIST_FREQUENCY  = SAMPLING_FREQUENCY/TWO
  ! ...Field angle (rad)
  REAL(fp), PARAMETER :: FIELD_ANGLE = 0.01605073_fp
  ! ...Number of double-sided FFT points
  INTEGER,  PARAMETER :: N_FFT(N_IASI_BANDS) = 51200
  ! ...Nominal maximum optical path delay for N_IASI_FFT (m)
  REAL(fp), PARAMETER :: NOMINAL_MAXX_IN_M(N_IASI_BANDS) = 1.9679466e-02_fp
  REAL(fp), PARAMETER :: NOMINAL_MAXX(N_IASI_BANDS)      = NOMINAL_MAXX_IN_M*M2CM


  ! Band parameters
  ! ...Band names
  CHARACTER(*), PARAMETER :: BAND_NAME(N_IASI_BANDS) = (/ 'B1','B2','B3'/)
  ! ...Frequencies
  REAL(fp), PARAMETER :: BAND_F1(N_IASI_BANDS) = (/  645.00_fp, 1210.00_fp, 2000.0_fp /)
  REAL(fp), PARAMETER :: BAND_F2(N_IASI_BANDS) = (/ 1209.75_fp, 1999.75_fp, 2760.0_fp /)
  ! ...Channel numbering
  INTEGER, PARAMETER :: BEGIN_CHANNEL( N_IASI_BANDS) = (/    1, 2261, 5421 /)
  INTEGER, PARAMETER :: END_CHANNEL(   N_IASI_BANDS) = (/ 2260, 5420, 8461 /)
  INTEGER, PARAMETER :: N_CHANNELS_PER_BAND(N_IASI_BANDS) = (/ 2260, 3160, 3041 /)
  INTEGER, PARAMETER :: MAX_N_BAND_CHANNELS = 3160

  
  ! Parameters for the resampled frequency grid
  REAL(fp), PARAMETER :: MIN_FREQUENCY = 645.0_fp
  REAL(fp), PARAMETER :: MAX_FREQUENCY = 2760.0_fp
  REAL(fp), PARAMETER :: D_FREQUENCY(N_IASI_BANDS)    = 0.25_fp
  REAL(fp), PARAMETER :: RESAMPLED_MAXX(N_IASI_BANDS) = TWO

  
CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_nFFT
!
! PURPOSE:
!       Pure function to return the number of double-sided FFT points
!       for a IASI instrument band.
!
! CALLING SEQUENCE:
!       n = IASI_nFFT(band)
!
! INPUTS:
!       band:     IASI band number (1, 2, or 3).
!                 If band < 1, then 1 is used.
!                    band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:        Number of double-sided FFT points for the specified
!                 IASI band.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_nFFT(band) RESULT(n)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: n
    ! Local variables
    INTEGER :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)
    
    ! Select the number of points
    n = N_FFT(ib)
    
  END FUNCTION IASI_nFFT


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
!                 the IASI max. OPD rather than the resampled max. OPD.
!                 If == .FALSE., the resampled value returned,
!                    == .TRUE.,  the nominal value is returned
!                 If not specified, the resampled value of maxX is returned.
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
  PURE FUNCTION IASI_MaxX(band, nominal) RESULT(maxX)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    LOGICAL, OPTIONAL, INTENT(IN) :: nominal
    ! Function result
    REAL(fp) :: maxX
    ! Variables
    INTEGER  :: ib
    LOGICAL  :: resampled
    
    ! Setup
    ! ...Check band
    ib = MAX(MIN(band,N_IASI_BANDS),1)
    ! ...Check nominal argument
    resampled = .TRUE.
    IF ( PRESENT(nominal) ) resampled = .NOT. nominal

    ! Determine optical path delay
    IF ( resampled ) THEN
      maxX = RESAMPLED_MAXX(ib)
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
!       x = IASI_X(band, n, nominal=nominal)
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
! OPTIONAL INPUTS:
!       nominal:   Set this logical argument to use the nominal value of
!                  the IASI max. OPD rather than the resampled max. OPD.
!                  If == .FALSE., the resampled value returned,
!                     == .TRUE.,  the nominal value is returned
!                  If not specified, the resampled value of maxX is returned.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
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
  PURE FUNCTION IASI_X(band, n, nominal) RESULT(X)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    INTEGER,           INTENT(IN) :: n
    LOGICAL, OPTIONAL, INTENT(IN) :: nominal
    ! Function result
    REAL(fp) :: X(n)
    ! Local variables
    REAL(fp) :: maxX
    INTEGER :: i, nHalf
    INTEGER :: ib

    ib = MAX(MIN(band,N_IASI_BANDS),1)
    ! Get the number of positive delays
    nHalf = n/2
    ! Compute maximum optical delay
    maxX = IASI_MaxX(ib, nominal=nominal)
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
    REAL(fp) :: maxX
    REAL(fp) :: sigma

    ib = MAX(MIN(band,N_IASI_BANDS),1)
    maxX = IASI_MaxX(ib, nominal=.TRUE.)
    sigma = LN2/(PI*GFT_HWHM)
    WHERE ( ABS(x) <= maxX )
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
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_nPts(band) RESULT(n)
    INTEGER, INTENT(IN) :: band
    INTEGER :: n
    INTEGER :: ib
    REAL(fp) :: f1, f2, df

    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)
    
    ! Select frequencies and interval
    f1 = IASI_BeginF(ib)
    f2 = IASI_EndF(ib)
    df = IASI_dF(ib)
    
    ! Compute the points
    n = INT((f2-f1)/df + ONEPOINT5)

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
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_F(band) RESULT(f)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    REAL(fp) :: f(IASI_nPts(band))
    ! Local variables
    INTEGER :: i, ib   
    REAL(fp) :: f1, df
    
    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)
    
    ! Select begin frequency and interval
    f1 = IASI_BeginF(ib)
    df = IASI_dF(ib)

    ! Compute frequencies
    DO i = 1, IASI_nPts(ib)
      f(i) = f1 + (df*REAL(i-1,fp))
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
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_Channels(band) RESULT(ch)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: ch(IASI_nPts(band))
    ! Local variables
    INTEGER :: i, ib, ic1, ic2

    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)
    
    ! Select channel bounds
    ic1 = IASI_BeginChannel(ib)
    ic2 = IASI_EndChannel(ib)
    
    ! Construct channel array
    ch = (/(i, i=ic1,ic2)/)
    
  END FUNCTION IASI_Channels


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_BeginF
!
! PURPOSE:
!       Pure function to return the IASI band begin frequency.
!
! CALLING SEQUENCE:
!       f1 = IASI_BeginF(band)
!
! INPUTS:
!       band:     IASI band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       f1:       Begin frequency for the IASI band.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_BeginF(band) RESULT(f1)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    REAL(fp) :: f1
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)

    ! Retrieve the begin frequency
    f1 = BAND_F1(ib)
    
  END FUNCTION IASI_BeginF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_EndF
!
! PURPOSE:
!       Pure function to return the IASI band end frequency.
!
! CALLING SEQUENCE:
!       f2 = IASI_EndF(band)
!
! INPUTS:
!       band:     IASI band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       f2:       End frequency for the IASI band.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_EndF(band) RESULT(f2)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    REAL(fp) :: f2
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)

    ! Retrieve the begin frequency
    f2 = BAND_F2(ib)
    
  END FUNCTION IASI_EndF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_dF
!
! PURPOSE:
!       Pure function to return the IASI band frequency interval.
!
! CALLING SEQUENCE:
!       df = IASI_dF(band)
!
! INPUTS:
!       band:     IASI band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       df:       Frequency interval for the IASI band.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_dF(band) RESULT(df)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    REAL(fp) :: df
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)

    ! Retrieve the frequency interval
    df = D_FREQUENCY(ib)
    
  END FUNCTION IASI_dF



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_BeginChannel
!
! PURPOSE:
!       Pure function to return the IASI band begin channel number.
!
! CALLING SEQUENCE:
!       ch1 = IASI_BeginChannel(band)
!
! INPUTS:
!       band:     IASI band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch1:      Begin channel number for the IASI band.
!                 UNITS:      N/A
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_BeginChannel(band) RESULT(ch1)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: ch1
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)

    ! Retrieve the begin channel number
    ch1 = BEGIN_CHANNEL(ib)
    
  END FUNCTION IASI_BeginChannel


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_EndChannel
!
! PURPOSE:
!       Pure function to return the IASI band end channel number.
!
! CALLING SEQUENCE:
!       ch2 = IASI_EndChannel(band)
!
! INPUTS:
!       band:     IASI band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch2:      End channel number for the IASI band.
!                 UNITS:      N/A
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_EndChannel(band) RESULT(ch2)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: ch2
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)

    ! Retrieve the end channel number
    ch2 = END_CHANNEL(ib)
    
  END FUNCTION IASI_EndChannel


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_BandName
!
! PURPOSE:
!       Pure function to return the IASI band name string.
!
! CALLING SEQUENCE:
!       name = IASI_BandName(band)
!
! INPUTS:
!       band:     IASI band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       name:     String containing the IASI band name.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_BandName(band) RESULT(name)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    CHARACTER(LEN(BAND_NAME(1))) :: name
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_BANDS),1)

    ! Retrieve the band name
    name = BAND_NAME(ib)
    
  END FUNCTION IASI_BandName


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
