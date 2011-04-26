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
  USE FFT_Spectral_Utility , ONLY: HAMMING         , &
                                   BLACKMANHARRIS_3, &
                                   BLACKMANHARRIS_4, &
                                   ApodFunction

  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Public parameters
  PUBLIC :: N_CRIS_FFT
  PUBLIC :: CRIS_MIN_FREQUENCY
  PUBLIC :: CRIS_MAX_FREQUENCY
  PUBLIC :: CRIS_D_FREQUENCY
  PUBLIC :: CRIS_RESAMPLE_MAXX
  PUBLIC :: N_CRIS_BANDS
  PUBLIC :: N_CRIS_CHANNELS
  PUBLIC :: CRIS_BAND_F1
  PUBLIC :: CRIS_BAND_F2
  PUBLIC :: CRIS_BAND
  PUBLIC :: CRIS_BAND_BEGIN_CHANNEL
  PUBLIC :: CRIS_BAND_END_CHANNEL
  PUBLIC :: N_CRIS_CHANNELS_PER_BAND
  PUBLIC :: MAX_N_CRIS_BAND_CHANNELS
  ! ...Inherited public parameters
  PUBLIC :: HAMMING
  PUBLIC :: BLACKMANHARRIS_3
  PUBLIC :: BLACKMANHARRIS_4
  ! Public module procedures
  PUBLIC :: CrIS_MaxX
  PUBLIC :: CrIS_X
  PUBLIC :: CrIS_F
  PUBLIC :: CrIS_ApodFunction
  PUBLIC :: CrIS_nPts
  PUBLIC :: CrIS_Channels
  PUBLIC :: CrIS_DefineVersion
  
  
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
  REAL(fp), PARAMETER :: THREE     = 3.0_fp
  REAL(fp), PARAMETER :: HUNDRED   = 100.0_fp
  REAL(fp), PARAMETER :: M2CM      = HUNDRED


  ! Band parameters
  INTEGER,  PARAMETER :: N_CRIS_BANDS    = 3
  INTEGER,  PARAMETER :: N_CRIS_CHANNELS = 1305
  ! ...Band frequencies
  REAL(fp), PARAMETER :: CRIS_BAND_F1(N_CRIS_BANDS) = (/  650.00_fp, 1210.00_fp, 2155.0_fp /)
  REAL(fp), PARAMETER :: CRIS_BAND_F2(N_CRIS_BANDS) = (/ 1095.00_fp, 1750.00_fp, 2550.0_fp /)
  ! ...The channel numbering for each band
  INTEGER, PARAMETER :: CRIS_BAND_BEGIN_CHANNEL( N_CRIS_BANDS) = (/   1,  714, 1147 /)
  INTEGER, PARAMETER :: CRIS_BAND_END_CHANNEL(   N_CRIS_BANDS) = (/ 713, 1146, 1305 /)
  INTEGER, PARAMETER :: N_CRIS_CHANNELS_PER_BAND(N_CRIS_BANDS) = (/ 713,  433,  159 /)
  INTEGER, PARAMETER :: MAX_N_CRIS_BAND_CHANNELS = 713
  ! ...Band names
  CHARACTER(*), PARAMETER :: CRIS_BAND(N_CRIS_BANDS) = (/ 'B1','B2','B3'/)

  
  ! Instrument parameters
  ! ...Laser wavelength (m)
  REAL(fp), PARAMETER :: LASER_WAVELENGTH_IN_M = 1.550e-06_fp
  REAL(fp), PARAMETER :: LASER_WAVELENGTH      = LASER_WAVELENGTH_IN_M * M2CM
  ! ...Laser frequency (m^-1)
  REAL(fp), PARAMETER :: LASER_FREQUENCY   = ONE/LASER_WAVELENGTH_IN_M
  REAL(fp), PARAMETER :: NYQUIST_FREQUENCY = LASER_FREQUENCY/TWO
  ! ...Field angle (rad)
  REAL(fp), PARAMETER :: FIELD_ANGLE = 0.0168_fp
  ! ...Number of double-sided FFT points
  INTEGER,  PARAMETER :: N_CRIS_FFT(N_CRIS_BANDS) =(/ 20736, 10560, 5200 /)
  ! ...Nominal maximum optical path delay for N_CRIS_FFT (m)
  REAL(fp), PARAMETER :: NOMINAL_MAXX_IN_M(N_CRIS_BANDS) = &
                         (/ 8.0340659e-03_fp, &
                            4.0914224e-03_fp, &
                            2.0147156e-03_fp /) 
  REAL(fp), PARAMETER :: NOMINAL_MAXX(N_CRIS_BANDS) = NOMINAL_MAXX_IN_M * M2CM

  ! Parameters for the resampled frequency grid
  REAL(fp), PARAMETER :: CRIS_MIN_FREQUENCY = 650.0_fp
  REAL(fp), PARAMETER :: CRIS_MAX_FREQUENCY = 2550.0_fp
  REAL(fp), PARAMETER :: CRIS_D_FREQUENCY(N_CRIS_BANDS)   = (/ 0.625_fp, 1.25_fp, 2.5_fp /)
  REAL(fp), PARAMETER :: CRIS_RESAMPLE_MAXX(N_CRIS_BANDS) = (/ 0.8_fp  , 0.4_fp , 0.2_fp /)
  
  
CONTAINS


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
! INPUT ARGUMENTS:
!       band:     CrIS band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       nominal:  Set this argument to return the nominal value of the CRIS
!                 max. OPD rather than the computed one.
!                 If == .FALSE., the computed value of maxX is returned,
!                                maxX = 0.5 * n_FFT *. laser_wavelength * COS(field_angle)
!                    == .TRUE.,  the nominal defined fixed value is returned
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
    LOGICAL  :: computed

    ! Setup
    ! ...Check band
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    ! ...Check nominal argument
    computed = .TRUE.
    IF ( PRESENT(nominal) ) computed = .NOT. nominal

    ! Determine optical path delay
    IF ( computed ) THEN
      maxX = REAL((N_CRIS_FFT(ib)/2),fp)*(LASER_WAVELENGTH/TWO)*COS(FIELD_ANGLE)
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
!       x = CrIS_X(band, n)
!
! INPUT ARGUMENTS:
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
! FUNCTION RESULT:
!       x:         CrIS double-sided optical delay grid.
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
  PURE FUNCTION CRIS_X(band,n) RESULT(X)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    INTEGER, INTENT(IN) :: n
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
    maxX = CRIS_RESAMPLE_MAXX(ib) 
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
!       afn = CrIS_ApodFunction(band, x, apodType = apodType)
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
!       funciton has side-lobes less than 1% of the central lobe and because
!       it has a well behaved analytic inverse transformation which 
!       satisfies retrieval models and also allows apodized radiances to
!       be returned to their unapodized values.
!       
!       The Blackman-Harris apodizations produce high lobe suppression functions
!       and can be considered among the top performers in commonly used FTIR
!       apodisation filters.
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_ApodFunction(band, x, apodType) RESULT(afn)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    REAL(fp),          INTENT(IN) :: x(:)
    INTEGER, OPTIONAL, INTENT(IN) :: apodType
    ! Function result
    REAL(fp) :: afn(SIZE(x))
    ! Local variables
    INTEGER :: atype
    REAL(fp) :: a0, a1, a2, a3
    REAL(fp) :: xnorm(SIZE(x))
    INTEGER :: ib
    
    ! Setup
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    xnorm = x/CRIS_RESAMPLE_MAXX(ib)
    ! ...Set apodisation type
    atype = -1 ! Force default
    IF ( PRESENT(apodType) ) atype = apodType


    ! Compute apodisation function
    SELECT CASE(atype)
    
      CASE(BLACKMANHARRIS_3)
        a0 = 0.42323_fp
        a1 = 0.49755_fp
        a2 = 0.07922_fp
        WHERE ( ABS(x) <= CRIS_RESAMPLE_MAXX(ib) )
          afn = a0 + a1*COS(PI*xnorm) + &
                     a2*COS(TWO*PI*xnorm)
        ELSE WHERE
          afn = ZERO
        END WHERE
       
      CASE(BLACKMANHARRIS_4)
        a0 = 0.35875_fp
        a1 = 0.48829_fp
        a2 = 0.14128_fp
        a3 = 0.01168_fp
        WHERE ( ABS(x) <= CRIS_RESAMPLE_MAXX(ib) )
          afn = a0 + a1*COS(PI*xnorm)     + &
                     a2*COS(TWO*PI*xnorm) + &
                     a3*COS(THREE*PI*xnorm)
        ELSE WHERE
          afn = ZERO
        END WHERE
     
      ! ...Hamming apodisation is the default 
      CASE DEFAULT
        a0 = 0.54_fp
        a1 = 0.46_fp
        WHERE ( ABS(x) <= CRIS_RESAMPLE_MAXX(ib) )
          afn = a0 + a1*COS(PI*xnorm)
        ELSE WHERE
          afn = ZERO
        END WHERE
      
    END SELECT
    
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
!       n = CrIS_nPts(band)
!
! INPUT ARGUMENTS:
!        band:    CrIS band number (1, 2, or 3).
!                 If band < 1, then 1 is used.
!                    band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:        Number of spectral points for the specified CrIS band.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
! COMMENTS:
!       From the CrIS ATBD 25 May 2001 Table 2, the CRIS band
!       definitions are,
!
!         Band   Range (cm-¹)    Range (µm)  
!         -----------------------------------
!          1     650  to 1095    15.4 to 9.1
!          2     1210 to 1750    8.3  to 5.7   
!          3     2155 to 2550    4.6  to 3.9   
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_nPts(band) RESULT(n)
    INTEGER, INTENT(IN) :: band
    INTEGER :: n
    INTEGER :: ib
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    n = INT((CRIS_BAND_F2(ib)-CRIS_BAND_F1(ib))/CRIS_D_FREQUENCY(ib) + ONEPOINT5)
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
!       f = CrIS_F(band)
!
! INPUT ARGUMENTS:
!       band:     CRIS band number (1, 2, or 3).
!                 If band < 1, then 1 is used.
!                    band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       f:        The spectral frequency grid for the specified CRIS band.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Rank-1
!
! COMMENTS:
!       From the CrIS ATBD 25 May 2001 Table 2, the CRIS band
!       definitions are,
!
!         Band   Range (cm-¹)    Range (µm)  
!         -----------------------------------
!          1     650  to 1095    15.4 to 9.1
!          2     1210 to 1750    8.3  to 5.7   
!          3     2155 to 2550    4.6  to 3.9   
!
!       The function, CrIS_nPts(), can be used to compute the number of spectral
!       points in each band.
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_F(band) RESULT(f)
    INTEGER, INTENT(IN) :: band
    REAL(fp) :: f(CRIS_nPts(band))
    INTEGER :: i, ib   
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    DO i = 1, CrIS_nPts(ib)
      f(i) = CRIS_BAND_F1(ib) + (CRIS_D_FREQUENCY(ib)*REAL(i-1,fp))
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
!       ch = CrIS_Channels(band)
!
! INPUT ARGUMENTS:
!       band:     CRIS band number (1, 2, or 3).
!                 If band < 1, then 1 is used.
!                    band > 3, then 3 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch:       The channel numbers for the specified CrIS band.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Rank-1
!
! COMMENTS:
!       The function, CrIS_nPts(), can be used to compute the number of spectral
!       channels in each band.
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION CrIS_Channels(band) RESULT(ch)
    INTEGER, INTENT(IN) :: band
    INTEGER :: ch(CrIS_nPts(band))
    INTEGER :: i, ib, n
    ib = MAX(MIN(band,N_CRIS_BANDS),1)
    n = 0
    DO i = 1, ib-1
      n = n + CrIS_nPts(i)
    END DO
    ch = (/(i,i=1,CrIS_nPts(ib))/) + n
  END FUNCTION CrIS_Channels


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
