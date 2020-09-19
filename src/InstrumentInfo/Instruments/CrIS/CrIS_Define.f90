!
! CrIS_Define
!
! Module containing CRIS instrument definitions
!
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CIRA/CSU 27-Aug-2008
!                       Yong.Chen@noaa.gov
!

MODULE CrIS_Define

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI, LN2
  USE Apodisation_Utility  , ONLY: HAMMING, &
                                   BLACKMANHARRIS_3, &
                                   Apodisation_Function
  USE SPC_IFG_Utility      , ONLY: ComputeMeanDelta
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Public parameters
  PUBLIC :: HAMMING
  PUBLIC :: BLACKMANHARRIS_3
  PUBLIC :: N_CRIS_BANDS
  PUBLIC :: N_CRIS_CHANNELS
  ! Public module procedures
  PUBLIC :: CrIS_nFFT
  PUBLIC :: CrIS_MaxX
  PUBLIC :: CrIS_X
  PUBLIC :: CrIS_F
  PUBLIC :: CrIS_ApodFunction
  PUBLIC :: CrIS_BeginF
  PUBLIC :: CrIS_EndF
  PUBLIC :: CrIS_dF
  PUBLIC :: CrIS_BeginChannel
  PUBLIC :: CrIS_EndChannel
  PUBLIC :: CrIS_nPts
  PUBLIC :: CrIS_Channels
  PUBLIC :: CrIS_Remove_Guard_Channels
  PUBLIC :: CrIS_BandName
  PUBLIC :: CrIS_DefineVersion

  INTERFACE CrIS_Remove_Guard_Channels
    MODULE PROCEDURE integer_rgc
    MODULE PROCEDURE real_rgc
  END INTERFACE CrIS_Remove_Guard_Channels


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: POINT5    = 0.5_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: ONEPOINT5 = 1.5_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PARAMETER :: THREE     = 3.0_fp
  REAL(fp), PARAMETER :: HUNDRED   = 100.0_fp
  REAL(fp), PARAMETER :: M2CM      = HUNDRED

  ! Instrument parameters
  ! ...Number of bands and channels
  INTEGER, PARAMETER :: N_CRIS_BANDS = 3
  INTEGER, PARAMETER :: N_CRIS_CHANNELS = 1305
  ! ...Laser wavelength (m)
  REAL(fp), PARAMETER :: LASER_WAVELENGTH_IN_M = 1.550e-06_fp
  REAL(fp), PARAMETER :: LASER_WAVELENGTH      = LASER_WAVELENGTH_IN_M * M2CM
  ! ...Laser frequency (m^-1)
  REAL(fp), PARAMETER :: LASER_FREQUENCY   = ONE/LASER_WAVELENGTH_IN_M
  ! ...Sampling and Nyquist frequencies
  REAL(fp), PARAMETER :: SAMPLING_FREQUENCY = LASER_FREQUENCY*TWO  ! Every zero crossing of laser signal
  REAL(fp), PARAMETER :: NYQUIST_FREQUENCY  = SAMPLING_FREQUENCY/TWO
  ! ...Field angle (rad)
  REAL(fp), PARAMETER :: FIELD_ANGLE = 0.0168_fp
  ! ...Number of double-sided FFT points
  INTEGER,  PARAMETER :: N_FFT(N_CRIS_BANDS) =(/ 20736, 10560, 5200 /)
  ! ...Nominal maximum optical path delay for N_CRIS_FFT (m)
  REAL(fp), PARAMETER :: NOMINAL_MAXX_IN_M(N_CRIS_BANDS) = &
                         (/ 8.03520e-03_fp, &
                            4.09200e-03_fp, &
                            2.01500e-03_fp /)
  REAL(fp), PARAMETER :: NOMINAL_MAXX(N_CRIS_BANDS) = NOMINAL_MAXX_IN_M * M2CM

  ! Band parameters
  ! ...Band names
  CHARACTER(*), PARAMETER :: BAND_NAME(N_CRIS_BANDS) = (/ 'B1','B2','B3'/)
  ! ...Frequencies
  REAL(fp), PARAMETER :: BAND_F1(N_CRIS_BANDS) = (/  650.00_fp, 1210.00_fp, 2155.0_fp /)
  REAL(fp), PARAMETER :: BAND_F2(N_CRIS_BANDS) = (/ 1095.00_fp, 1750.00_fp, 2550.0_fp /)
  ! ...Channel numbering
  INTEGER, PARAMETER :: BEGIN_CHANNEL( N_CRIS_BANDS) = (/   1,  714, 1147 /)
  INTEGER, PARAMETER :: END_CHANNEL(   N_CRIS_BANDS) = (/ 713, 1146, 1305 /)
  INTEGER, PARAMETER :: N_CHANNELS_PER_BAND(N_CRIS_BANDS) = (/ 713,  433,  159 /)
  INTEGER, PARAMETER :: MAX_N_BAND_CHANNELS = 713
  ! ...Guard channel count
  INTEGER, PARAMETER :: N_GUARD_CHANNELS(2, N_CRIS_BANDS) = &
    RESHAPE((/ 76, 75, 48, 47, 21, 20 /), (/ 2, N_CRIS_BANDS /))
  ! ...Frequencies with guard channels
  REAL(fp), PARAMETER :: BAND_GF1(N_CRIS_BANDS) = (/  602.500_fp, 1150.000_fp, 2102.500_fp /)
  REAL(fp), PARAMETER :: BAND_GF2(N_CRIS_BANDS) = (/ 1141.875_fp, 1808.750_fp, 2600.000_fp /)
  ! ...Channel numbering including guard channels
  INTEGER, PARAMETER :: BEGIN_GCHANNEL( N_CRIS_BANDS) = (/   1,  865, 1393 /)
  INTEGER, PARAMETER :: END_GCHANNEL(   N_CRIS_BANDS) = (/ 864, 1392, 1592 /)
  INTEGER, PARAMETER :: N_GCHANNELS_PER_BAND(N_CRIS_BANDS) = (/ 864,  528,  200 /)
  INTEGER, PARAMETER :: MAX_N_BAND_GCHANNELS = 864


  ! Parameters for the resampled frequency grid
  REAL(fp), PARAMETER :: MIN_FREQUENCY = 650.0_fp
  REAL(fp), PARAMETER :: MAX_FREQUENCY = 2550.0_fp
  REAL(fp), PARAMETER :: D_FREQUENCY(N_CRIS_BANDS) = (/ 0.625_fp, 1.25_fp, 2.5_fp /)
  REAL(fp), PARAMETER :: RESAMPLED_MAXX(N_CRIS_BANDS)   = (/ 0.8_fp  , 0.4_fp , 0.2_fp /)


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_nFFT
!
! PURPOSE:
!       Pure function to return the number of double-sided FFT points
!       for a CrIS instrument band.
!
! CALLING SEQUENCE:
!       n = CrIS_nFFT(band)
!
! INPUTS:
!       band:     CrIS band number (1, 2, or 3).
!                 If band < 1, then 1 is used.
!                    band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:        Number of double-sided FFT points for the specified
!                 CrIS band.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_nFFT(band) RESULT(n)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: n
    ! Local variables
    INTEGER :: ib

    ! Setup
    ib = MAX(MIN(band,N_CRIS_BANDS),1)

    ! Select the number of points
    n = N_FFT(ib)

  END FUNCTION CrIS_nFFT


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_MaxX
!
! PURPOSE:
!       Pure function to return the CRIS maximum optical path delay.
!
! CALLING SEQUENCE:
!       maxX = CrIS_MaxX(band, nominal=nominal)
!
! INPUTS:
!       band:     CrIS band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       nominal:  Set this argument to return the nominal value of the CRIS
!                 max. OPD rather than the resampled max. OPD..
!                 If == .FALSE., the resampled value returned,
!                    == .TRUE.,  the nominal value is returned
!                 If not specified, the resampled value of maxX is returned.
!                 UNITS:      N/A
!                 TYPE:       LOGICAL
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       maxX:     Maximum optical path delay of the CRIS instrument.
!                 UNITS:      Centimetres (cm)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_MaxX(band, nominal) RESULT(maxX)
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
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    ! ...Check nominal argument
    resampled = .TRUE.
    IF ( PRESENT(nominal) ) resampled = .NOT. nominal

    ! Determine optical path delay
    IF ( resampled ) THEN
      maxX = RESAMPLED_MAXX(ib)
    ELSE
      maxX = NOMINAL_MAXX(ib)
    END IF

  END FUNCTION CrIS_MaxX


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_X
!
! PURPOSE:
!       Pure function to compute the CrIS double-sided optical delay grid.
!
! CALLING SEQUENCE:
!       x = CrIS_X(band, n, nominal=nominal)
!
! INPUTS:
!       band:      CrIS band number (1, 2, or 3).
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
!       nominal:  Set this argument to use the nominal value of the CRIS
!                 max. OPD rather than the resampled max. OPD..
!                 If == .FALSE., the resampled value is used. [DEFAULT]
!                    == .TRUE.,  the nominal value is used.
!                 If not specified, the resampled value of maxX is used.
!                 UNITS:      N/A
!                 TYPE:       LOGICAL
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       x:        CrIS double-sided optical delay grid.
!                 UNITS:      Centimetres (cm)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Rank-1 (n)
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
  PURE FUNCTION CRIS_X(band, n, nominal) RESULT(X)
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

    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    ! Get the number of positive delays
    nHalf = n/2
    ! Compute maximum optical delay
    maxX = CrIS_MaxX(ib, nominal=nominal)
    ! Fill the grid array
    X(nHalf:n) = (/(REAL(i,fp),i=0,nHalf)/)/REAL(nHalf,fp)
    X(1:nHalf-1) = -X(n-1:nHalf+1:-1)
    X = X*maxX
  END FUNCTION CRIS_X


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_ApodFunction
!
! PURPOSE:
!       Pure function to compute the CrIS apodisation function for a given
!       optical delay grid.
!
! CALLING SEQUENCE:
!       afn = CrIS_ApodFunction(band, x, apodType=apodType, nominal=nominal)
!
! INPUTS:
!       band:      CrIS band number (1, 2, or 3).
!                  If band < 1, then 1 is used.
!                     band > 3, then 3 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
!       x:         Double-sided optical delay grid.
!                  UNITS:      Centimetres (cm)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       apodType:  Set this argument to the defined parameter values to
!                  select the type of apodisation function.
!                     == HAMMING          for Hamming apodisation [DEFAULT]
!                     == BLACKMANHARRIS_3 for Blackman-Harris 3-term apodisation
!                     == BLACKMANHARRIS_4 for Blackman-Harris 4-term apodisation
!                  If not specified, or any other value is supplied, then
!                  the computed apodisation uses HAMMING.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       nominal:   Set this argument to use the nominal value of the CRIS
!                  max. OPD rather than the resampled max. OPD..
!                  If == .FALSE., the resampled value is used. [DEFAULT]
!                     == .TRUE.,  the nominal value is used.
!                  If not specified, the resampled value of maxX is used.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       afn:       CrIS apodisation function.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Same as input x argument.
!
! COMMENTS:
!       The Hamming apodization function is a reasonable and efficient
!       function to use in atmospheric remote sensing applications with
!       high signal-to-noise instruments, both because its channel response
!       function has side-lobes less than 1% of the central lobe and because
!       it has a well behaved analytic inverse transformation which
!       satisfies retrieval models and also allows apodized radiances to
!       be returned to their unapodized values.
!
!       The Blackman-Harris apodizations produce high lobe suppression functions
!       and can be considered among the top performers in commonly used FTIR
!       apodisation filters.
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_ApodFunction(band, x, apodType, nominal) RESULT(afn)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    REAL(fp),          INTENT(IN) :: x(:)
    INTEGER, OPTIONAL, INTENT(IN) :: apodType
    LOGICAL, OPTIONAL, INTENT(IN) :: nominal
    ! Function result
    REAL(fp) :: afn(SIZE(x))
    ! Local variables
    INTEGER  :: atype
    REAL(fp) :: xmax
    REAL(fp) :: xnorm(SIZE(x))
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    xmax = CrIS_MaxX(ib, nominal=nominal)
    xnorm = x/xmax
    ! ...Set apodisation type
    atype = HAMMING
    IF ( PRESENT(apodType) ) atype = apodType

    ! Compute apodisation function
    afn = Apodisation_Function(x,aType=aType,xMax=xmax)

  END FUNCTION CrIS_ApodFunction


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_nPts
!
! PURPOSE:
!       Pure function to compute the number of spectral points in an CrIS band.
!
! CALLING SEQUENCE:
!       n = CrIS_nPts(band, include_guard_channels)
!
! INPUTS:
!       band:                    CrIS band number (1, 2, or 3).
!                                If band < 1, then 1 is used.
!                                   band > 3, then 3 is used.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  SCALAR
!                                ATTRIBUTES: INTENT(IN)
!
!       include_guard_channels:  Set this logical switch to include the guard
!                                channels on either side of the band.
!                                If == .FALSE. no guard channels are used.
!                                   == .TRUE.  guard channels are used.
!                                UNITS:      N/A
!                                TYPE:       Logical
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:                       Number of spectral points for the specified
!                                CrIS band.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_nPts(band, include_guard_channels) RESULT(n)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    LOGICAL, INTENT(IN) :: include_guard_channels
    ! Function result
    INTEGER :: n
    ! Local variables
    INTEGER :: ib
    REAL(fp) :: f1, f2, df

    ! Setup
    ib = MAX(MIN(band,N_CRIS_BANDS),1)

    ! Select frequencies and interval
    f1 = CrIS_BeginF(ib, include_guard_channels=include_guard_channels)
    f2 = CrIS_EndF(  ib, include_guard_channels=include_guard_channels)
    df = CrIS_dF(ib)

    ! Compute the points
    n = INT((f2-f1)/df + ONEPOINT5)

  END FUNCTION CrIS_nPts


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_F
!
! PURPOSE:
!       Pure function to compute the resampled frequency grid for an CrIS band.
!
! CALLING SEQUENCE:
!       f = CrIS_F(band, include_guard_channels)
!
! INPUTS:
!       band:                    CRIS band number (1, 2, or 3).
!                                If band < 1, then 1 is used.
!                                   band > 3, then 3 is used.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  SCALAR
!                                ATTRIBUTES: INTENT(IN)
!
!       include_guard_channels:  Set this logical switch to include the guard
!                                channels on either side of the band.
!                                If == .FALSE. no guard channels are used.
!                                   == .TRUE.  guard channels are used.
!                                UNITS:      N/A
!                                TYPE:       Logical
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       f:                       The spectral frequency grid for the specified
!                                band.
!                                UNITS:      Inverse centimetres (cm^-1)
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-1
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_F(band, include_guard_channels) RESULT(f)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    LOGICAL, INTENT(IN) :: include_guard_channels
    ! Function result
    REAL(fp) :: f(CRIS_nPts(band, include_guard_channels))
    ! Local variables
    INTEGER :: i, ib
    REAL(fp) :: f1, df

    ! Setup
    ib = MAX(MIN(band,N_CRIS_BANDS),1)

    ! Select begin frequency and interval
    f1 = CrIS_BeginF(ib, include_guard_channels=include_guard_channels)
    df = CrIS_dF(ib)

    ! Compute frequencies
    DO i = 1, CrIS_nPts(ib,include_guard_channels)
      f(i) = f1 + (df*REAL(i-1,fp))
    END DO

  END FUNCTION CrIS_F


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_Channels
!
! PURPOSE:
!       Pure function to compute the channel numbers for an CrIS band.
!
! CALLING SEQUENCE:
!       ch = CrIS_Channels(band, include_guard_channels)
!
! INPUTS:
!       band:                    CRIS band number (1, 2, or 3).
!                                If band < 1, then 1 is used.
!                                   band > 3, then 3 is used.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  SCALAR
!                                ATTRIBUTES: INTENT(IN)
!
!       include_guard_channels:  Set this logical switch to include the guard
!                                channels on either side of the band.
!                                If == .FALSE. no guard channels are used.
!                                   == .TRUE.  guard channels are used.
!                                UNITS:      N/A
!                                TYPE:       Logical
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch:                      The channel numbers for the specified CrIS band.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Rank-1
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_Channels(band, include_guard_channels) RESULT(ch)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    LOGICAL, INTENT(IN) :: include_guard_channels
    ! Function result
    INTEGER :: ch(CrIS_nPts(band, include_guard_channels))
    ! Local variables
    INTEGER :: i, ib, ic1, ic2

    ! Setup
    ib = MAX(MIN(band,N_CRIS_BANDS),1)

    ! Select channel bounds
    ic1 = CrIS_BeginChannel(ib, include_guard_channels=include_guard_channels)
    ic2 = CrIS_EndChannel(ib, include_guard_channels=include_guard_channels)

    ! Construct channel array
    ch = (/(i, i=ic1,ic2)/)

  END FUNCTION CrIS_Channels


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_Remove_Guard_Channels
!
! PURPOSE:
!       Function to remove the guard channels from input for a CrIS band.
!
! CALLING SEQUENCE:
!       output_vector = CrIS_Remove_Guard_Channels(band, input_vector)
!
! INPUTS:
!       band:            CrIS band number (1, 2, or 3).
!                        If band < 1, then 1 is used.
!                           band > 3, then 3 is used.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  SCALAR
!                        ATTRIBUTES: INTENT(IN)
!
!       input_vector:    CrIS band-length vector for which the guard
!                        channel data is to be removed.
!                        UNITS:      N/A
!                        TYPE:       INTEGER or REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       output_vector:   Same data as input_vector but with the guard
!                        channels removed.
!                        Value set to all -1 if error occurs
!                        UNITS:      Variable
!                        TYPE:       Same as input_vector
!                        DIMENSION:  Rank-1
!
!:sdoc-:
!--------------------------------------------------------------------------------
  FUNCTION integer_rgc(band, input_vector) RESULT(output_vector)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    INTEGER, INTENT(IN) :: input_vector(:)
    ! Function result
    INTEGER :: output_vector(CRIS_nPts(band, include_guard_channels=.FALSE.))
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CrIS_Remove_Guard_Channels(INTEGER)'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: ib, n
    INTEGER :: i1, i2

    ! Setup
    output_vector = -1
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    ! ...Test input
    n = SIZE(input_vector)
    IF ( n /= N_GCHANNELS_PER_BAND(ib) ) THEN
      WRITE( msg,'("Input vector size (",i0,") inconsistent for CrIS band ",i0)') n, ib
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
    END IF

    ! Pick out the data
    i1 = N_GUARD_CHANNELS(1, ib) + 1
    i2 = n - N_GUARD_CHANNELS(2, ib)
    output_vector = input_vector(i1:i2)

  END FUNCTION integer_rgc

  FUNCTION real_rgc(band, input_vector) RESULT(output_vector)
    ! Arguments
    INTEGER , INTENT(IN) :: band
    REAL(fp), INTENT(IN) :: input_vector(:)
    ! Function result
    REAL(fp) :: output_vector(CRIS_nPts(band, include_guard_channels=.FALSE.))
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CrIS_Remove_Guard_Channels(REAL)'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: ib, n
    INTEGER :: i1, i2

    ! Setup
    output_vector = -1.0_fp
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    ! ...Test input
    n = SIZE(input_vector)
    IF ( n /= N_GCHANNELS_PER_BAND(ib) ) THEN
      WRITE( msg,'("Input vector size (",i0,") inconsistent for CrIS band ",i0)') n, ib
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
    END IF

    ! Pick out the data
    i1 = N_GUARD_CHANNELS(1, ib) + 1
    i2 = n - N_GUARD_CHANNELS(2, ib)
    output_vector = input_vector(i1:i2)

  END FUNCTION real_rgc


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_BeginF
!
! PURPOSE:
!       Pure function to return the CRIS band begin frequency.
!
! CALLING SEQUENCE:
!       f1 = CrIS_BeginF(band, include_guard_channels=include_guard_channels)
!
! INPUTS:
!       band:                    CrIS band number (1, 2, or 3).
!                                If Band < 1, then 1 is used.
!                                   Band > 3, then 3 is used.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  SCALAR
!                                ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       include_guard_channels:  Set this logical switch to include the guard
!                                channels on either side of the band.
!                                If == .FALSE. no guard channels are included. [DEFAULT]
!                                   == .TRUE.  guard channels are included.
!                                If not specified, no guard channels are used.
!                                UNITS:      N/A
!                                TYPE:       Logical
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       f1:                      Begin frequency for the CrIS band.
!                                UNITS:      Inverse centimetres (cm^-1)
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_BeginF(band, include_guard_channels) RESULT(f1)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    LOGICAL, OPTIONAL, INTENT(IN) :: include_guard_channels
    ! Function result
    REAL(fp) :: f1
    ! Variables
    INTEGER  :: ib
    LOGICAL  :: no_guard_channels

    ! Setup
    ! ...Check band
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    ! ...Check optional argument
    no_guard_channels = .TRUE.
    IF ( PRESENT(include_guard_channels) ) no_guard_channels = .NOT. include_guard_channels

    ! Retrieve the begin frequency
    IF ( no_guard_channels ) THEN
      f1 = BAND_F1(ib)
    ELSE
      f1 = BAND_GF1(ib)
    END IF

  END FUNCTION CrIS_BeginF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_EndF
!
! PURPOSE:
!       Pure function to return the CRIS band end frequency.
!
! CALLING SEQUENCE:
!       f2 = CrIS_EndF(band, include_guard_channels=include_guard_channels)
!
! INPUTS:
!       band:                    CrIS band number (1, 2, or 3).
!                                If Band < 1, then 1 is used.
!                                   Band > 3, then 3 is used.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  SCALAR
!                                ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       include_guard_channels:  Set this logical switch to include the guard
!                                channels on either side of the band.
!                                If == .FALSE. no guard channels are included. [DEFAULT]
!                                   == .TRUE.  guard channels are included.
!                                If not specified, no guard channels are used.
!                                UNITS:      N/A
!                                TYPE:       Logical
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       f2:                      End frequency for the CrIS band.
!                                UNITS:      Inverse centimetres (cm^-1)
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_EndF(band, include_guard_channels) RESULT(f2)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    LOGICAL, OPTIONAL, INTENT(IN) :: include_guard_channels
    ! Function result
    REAL(fp) :: f2
    ! Variables
    INTEGER  :: ib
    LOGICAL  :: no_guard_channels

    ! Setup
    ! ...Check band
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    ! ...Check optional argument
    no_guard_channels = .TRUE.
    IF ( PRESENT(include_guard_channels) ) no_guard_channels = .NOT. include_guard_channels

    ! Retrieve the begin frequency
    IF ( no_guard_channels ) THEN
      f2 = BAND_F2(ib)
    ELSE
      f2 = BAND_GF2(ib)
    END IF

  END FUNCTION CrIS_EndF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_dF
!
! PURPOSE:
!       Pure function to return the CrIS band frequency interval.
!
! CALLING SEQUENCE:
!       df = CrIS_dF(band)
!
! INPUTS:
!       band:     CrIS band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       df:       Frequency interval for the CrIS band.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_dF(band) RESULT(df)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    REAL(fp) :: df
    ! Variables
    INTEGER  :: ib

    ! Setup
    ! ...Check band
    ib = MAX(MIN(band,N_CRIS_BANDS),1)

    ! Retrieve the frequency interval
    df = D_FREQUENCY(ib)

  END FUNCTION CrIS_dF



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_BeginChannel
!
! PURPOSE:
!       Pure function to return the CRIS band begin channel number.
!
! CALLING SEQUENCE:
!       ch1 = CrIS_BeginChannel(band, include_guard_channels=include_guard_channels)
!
! INPUTS:
!       band:                    CrIS band number (1, 2, or 3).
!                                If Band < 1, then 1 is used.
!                                   Band > 3, then 3 is used.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  SCALAR
!                                ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       include_guard_channels:  Set this logical switch to include the guard
!                                channels on either side of the band.
!                                If == .FALSE. no guard channels are included. [DEFAULT]
!                                   == .TRUE.  guard channels are included.
!                                If not specified, no guard channels are used.
!                                UNITS:      N/A
!                                TYPE:       Logical
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       ch1:                     Begin channel number for the CrIS band.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_BeginChannel(band, include_guard_channels) RESULT(ch1)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    LOGICAL, OPTIONAL, INTENT(IN) :: include_guard_channels
    ! Function result
    INTEGER :: ch1
    ! Variables
    INTEGER  :: ib
    LOGICAL  :: no_guard_channels

    ! Setup
    ! ...Check band
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    ! ...Check optional argument
    no_guard_channels = .TRUE.
    IF ( PRESENT(include_guard_channels) ) no_guard_channels = .NOT. include_guard_channels

    ! Retrieve the begin channel number
    IF ( no_guard_channels ) THEN
      ch1 = BEGIN_CHANNEL(ib)
    ELSE
      ch1 = BEGIN_GCHANNEL(ib)
    END IF

  END FUNCTION CrIS_BeginChannel


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_EndChannel
!
! PURPOSE:
!       Pure function to return the CRIS band end channel number.
!
! CALLING SEQUENCE:
!       ch2 = CrIS_EndChannel(band, include_guard_channels=include_guard_channels)
!
! INPUTS:
!       band:                    CrIS band number (1, 2, or 3).
!                                If Band < 1, then 1 is used.
!                                   Band > 3, then 3 is used.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  SCALAR
!                                ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       include_guard_channels:  Set this logical switch to include the guard
!                                channels on either side of the band.
!                                If == .FALSE. no guard channels are included. [DEFAULT]
!                                   == .TRUE.  guard channels are included.
!                                If not specified, no guard channels are used.
!                                UNITS:      N/A
!                                TYPE:       Logical
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       ch2:                     End channel number for the CrIS band.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_EndChannel(band, include_guard_channels) RESULT(ch2)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    LOGICAL, OPTIONAL, INTENT(IN) :: include_guard_channels
    ! Function result
    INTEGER :: ch2
    ! Variables
    INTEGER  :: ib
    LOGICAL  :: no_guard_channels

    ! Setup
    ! ...Check band
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    ! ...Check optional argument
    no_guard_channels = .TRUE.
    IF ( PRESENT(include_guard_channels) ) no_guard_channels = .NOT. include_guard_channels

    ! Retrieve the end channel number
    IF ( no_guard_channels ) THEN
      ch2 = END_CHANNEL(ib)
    ELSE
      ch2 = END_GCHANNEL(ib)
    END IF

  END FUNCTION CrIS_EndChannel


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_BandName
!
! PURPOSE:
!       Pure function to return the CrIS band name string.
!
! CALLING SEQUENCE:
!       name = CrIS_BandName(band)
!
! INPUTS:
!       band:     CrIS band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       name:     String containing the CrIS band name.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_BandName(band) RESULT(name)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    CHARACTER(LEN(BAND_NAME(1))) :: name
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_CRIS_BANDS),1)

    ! Retrieve the band name
    name = BAND_NAME(ib)

  END FUNCTION CrIS_BandName


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CrIS_DefineVersion( Id )
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

  SUBROUTINE CrIS_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CrIS_DefineVersion

END MODULE CrIS_Define
