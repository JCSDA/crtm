!
! CRTM_Parameters
!
! Module of parameter definitions for the CRTM.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP/EMC 31-Jul-2000
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Parameters


  ! ---------------------
  ! Module use statements
  ! ---------------------
  USE Type_Kinds, ONLY : fp=>fp_kind


  ! ------------------
  ! Default visibility
  ! ------------------
  ! Everything PRIVATE by default
  PRIVATE
  ! The MAX_N_CHANNELS methods
  PUBLIC :: CRTM_Set_Max_n_Channels
  PUBLIC :: CRTM_Reset_Max_n_Channels
  PUBLIC :: CRTM_Get_Max_n_Channels



  !#----------------------------------------------------------------------------#
  !#                    -- ALGORITHM INDEPENDENT PARAMETERS --                  #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------------------------
  ! Number of channels (for ALL satellites - really the number
  ! of satellites x number of channels USED per satellite)
  !
  ! This is also the number of lines in the satellite 
  ! information (satinfo) file used in the NCEP GDAS.
  !
  ! The number of channels that can be used is determined,
  ! and SET, during the model initialisation.
  !
  ! In this module it is a protected variable in that it can
  ! only be set, reset, or retrieved via the MAX_N_CHANNELS
  ! methods.
  ! ----------------------------------------------------------

  ! Accessed via SET_MAX_N_CHANNELS, RESET_MAX_N_CHANNELS,
  ! and GET_MAX_N_CHANNELS routines
  INTEGER, PRIVATE, PARAMETER :: RESET_VALUE = -1
  INTEGER, PRIVATE, SAVE      :: MAX_N_CHANNELS = RESET_VALUE


  ! -----------------------------------------------------
  ! The maximum number of atmospheric profiles and layers
  ! accepted. These values are arbitrary. Nothing magical
  ! -----------------------------------------------------
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PROFILES = 128
  INTEGER, PUBLIC, PARAMETER :: MAX_N_LAYERS   = 100


  ! -----------------
  ! Literal constants
  ! -----------------
  REAL(fp), PUBLIC, PARAMETER :: ZERO          =  0.0_fp
  REAL(fp), PUBLIC, PARAMETER :: ONE           =  1.0_fp
  REAL(fp), PUBLIC, PARAMETER :: TWO           =  2.0_fp
  REAL(fp), PUBLIC, PARAMETER :: THREE         =  3.0_fp
  REAL(fp), PUBLIC, PARAMETER :: FOUR          =  4.0_fp
  REAL(fp), PUBLIC, PARAMETER :: FIVE          =  5.0_fp
  REAL(fp), PUBLIC, PARAMETER :: TEN           = 10.0_fp
  REAL(fp), PUBLIC, PARAMETER :: POINT_25      =  0.25_fp
  REAL(fp), PUBLIC, PARAMETER :: POINT_5       =  0.5_fp
  REAL(fp), PUBLIC, PARAMETER :: POINT_75      =  0.75_fp
  REAL(fp), PUBLIC, PARAMETER :: ONEpointFIVE  =  1.5_fp
                                                                                                        

  ! -----------------------------
  ! Numerical precision/tolerance
  ! -----------------------------
  REAL(fp), PUBLIC, PARAMETER :: TOLERANCE = EPSILON( ONE )


  ! ---------------------------------------------
  ! Constant to allow degrees->radians conversion
  ! ---------------------------------------------
  REAL(fp), PUBLIC, PARAMETER :: PI = 3.141592653589793238462643383279_fp
  REAL(fp), PUBLIC, PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp


  ! -------------------------
  ! Direction flags
  !   DOWN == From TOA to SFC
  !   UP   == From SFC to TOA
  ! -------------------------
  INTEGER, PUBLIC, PARAMETER :: DOWN = 0
  INTEGER, PUBLIC, PARAMETER :: UP   = 1


  ! ------------------------
  ! Invalid sensor ID values
  ! ------------------------
  INTEGER, PUBLIC, PARAMETER :: INVALID_NCEP_SENSOR_ID   = -1
  INTEGER, PUBLIC, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PUBLIC, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  

  ! -------------------------
  ! Yes/No, Set/Unset flags
  ! -------------------------
  INTEGER, PUBLIC, PARAMETER :: NO      = 0, YES = 1
  INTEGER, PUBLIC, PARAMETER :: NOT_SET = 0, SET = 1



  !#----------------------------------------------------------------------------#
  !#                       -- AtmAbsorption PARAMETERS --                       #
  !#----------------------------------------------------------------------------#

  ! -------------------------------------
  ! Absorbers in the gas absorption model
  ! -------------------------------------

  ! The total number
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ABSORBERS = 3

  ! The indexing order of the absorbers
  INTEGER, PUBLIC, PARAMETER :: WET_ABSORBER_INDEX = 1
  INTEGER, PUBLIC, PARAMETER :: DRY_ABSORBER_INDEX = 2
  INTEGER, PUBLIC, PARAMETER :: OZO_ABSORBER_INDEX = 3

  ! The absorber index and name arrays
  INTEGER, PUBLIC, PARAMETER, DIMENSION( MAX_N_ABSORBERS ) :: &
    ABSORBER_INDEX = (/ WET_ABSORBER_INDEX, &
                        DRY_ABSORBER_INDEX, &
                        OZO_ABSORBER_INDEX /)

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( MAX_N_ABSORBERS ) :: &
    ABSORBER_NAME = (/ 'wet', &
                       'dry', &
                       'ozo' /)


  ! --------------------------------------
  ! Predictors in the gas absorption model
  ! --------------------------------------

  ! Standard predictors are absorber independent
  INTEGER, PUBLIC, PARAMETER :: MAX_N_STANDARD_PREDICTORS   = 11

  ! Integrated predictors are defined for EACH absoreber
  INTEGER, PUBLIC, PARAMETER :: MAX_N_INTEGRATED_PREDICTORS = 6

  ! The total number of predictors
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PREDICTORS = MAX_N_STANDARD_PREDICTORS + &
                                                   ( MAX_N_ABSORBERS * MAX_N_INTEGRATED_PREDICTORS )

  ! The number selected from the total to be
  ! used in the gas absorption algorithm
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PREDICTORS_USED = 6


  ! ----------------------------------------------
  ! Maximum number of polynomial orders for
  ! reconstructing the gas absorption coefficients
  ! ----------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_ORDERS = 10


  ! ----------------------------------------------
  ! The minimum absorber amount allowed based upon
  ! the smallest representable numbers.
  ! This value is equivalent to TINY(ONE)**0.25
  ! ----------------------------------------------

  REAL(fp), PUBLIC, PARAMETER :: MINIMUM_ABSORBER_AMOUNT = TEN**(-RANGE(ONE)/4)


  ! ---------------------------------------
  ! Numerical limits for the gas absorption
  ! coefficient reconstruction
  ! ---------------------------------------

  ! Numerical limits based on precision
!  REAL(fp), PUBLIC, PARAMETER :: LIMIT_EXP = 36.0436_fp   ! ABS( LOG( TOLERANCE ) )
!  REAL(fp), PUBLIC, PARAMETER :: LIMIT_LOG = 4.5e+15_fp   ! EXP( LIMIT_EXP )

  ! Numerical limits based on experiment.
  REAL(fp), PUBLIC, PARAMETER :: LIMIT_EXP = 20.0_fp
  REAL(fp), PUBLIC, PARAMETER :: LIMIT_LOG = 4.8e+08_fp   ! EXP( LIMIT_EXP )


  ! ---------------------------------------
  ! Top-Of-Atmosphere (TOA) pressure in hPa
  ! ---------------------------------------

  REAL(fp), PUBLIC, PARAMETER :: TOA_PRESSURE = 0.005_fp


  ! -------------------------------------------------------
  ! Reciprocal gravity (scaled by 100 for use with pressure
  ! in hPa) used in computing integrated absorber amounts
  ! -------------------------------------------------------

  REAL(fp), PUBLIC, PARAMETER :: RECIPROCAL_GRAVITY = ONE / 980.665_fp



  !#----------------------------------------------------------------------------#
  !#                       -- GeometryInfo PARAMETERS --                        #
  !#----------------------------------------------------------------------------#

  ! -----------------
  ! Distance defaults
  ! -----------------

  REAL(fp), PUBLIC, PARAMETER :: EARTH_RADIUS     = 6370.0_fp  ! Mean earth radius 
  REAL(fp), PUBLIC, PARAMETER :: SATELLITE_HEIGHT = 800.0_fp


  ! -----------------------------------
  ! Limits on sensor angles.
  ! - Azimuth angles that are multiples
  !   of 2pi are not accepted.
  ! -----------------------------------

  REAL(fp), PUBLIC, PARAMETER :: MAX_SENSOR_SCAN_ANGLE    = 65.0_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_SENSOR_ZENITH_ANGLE  = 65.0_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_SENSOR_AZIMUTH_ANGLE = 360.0_fp


  ! -------------------------------------------------
  ! Limits on source angles.
  ! - The maximum source zenith angle should
  !   be determined by the maximum angle secant
  !   used in generating the gas absorption model
  !   coefficients, i.e. a secant of 2.25 => 63.6deg.
  !   Users have requested the Value be 85deg which
  !   has a secant of ~11.47.
  ! - Azimuth angles that are multiples
  !   of 2pi are not accepted.
  ! -------------------------------------------------

  REAL(fp), PUBLIC, PARAMETER :: MAX_SOURCE_ZENITH_ANGLE  = 85.0_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_SECANT_SOURCE_ZENITH = 11.473711738554476_fp

  REAL(fp), PUBLIC, PARAMETER :: MAX_SOURCE_AZIMUTH_ANGLE = 360.0_fp


  ! ----------------------------------------
  ! Default diffusivity angle and secant
  ! ACOS( 3/5 ) in degrees is (~53.13)
  ! Used to approximate the downwelling flux
  ! ----------------------------------------

  REAL(fp), PUBLIC, PARAMETER :: DIFFUSIVITY_ANGLE  = 53.130102354156_fp
  REAL(fp), PUBLIC, PARAMETER :: DIFFUSIVITY_RADIAN = 0.927295218002_fp
  REAL(fp), PUBLIC, PARAMETER :: SECANT_DIFFUSIVITY = FIVE / THREE


  ! -----------------------------------------------------------
  ! Maximum flux angle definitions. Determined by the maximum
  ! angle secant used in generating the gas absorption model
  ! coefficients, i.e. a secant of 2.25 => 63.6deg. If the user
  ! inputs a value larger than this for the Flux_Zenith_Angle,
  ! the diffusivity angles are used instead
  ! -----------------------------------------------------------

  REAL(fp), PUBLIC, PARAMETER :: MAX_FLUX_ZENITH_ANGLE  = 63.612200038757_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_SECANT_FLUX_ZENITH = 2.25_fp





  !#----------------------------------------------------------------------------#
  !#            -- CloudScatter, RTSolution, AtmOptics PARAMETERS --            #
! This will be further broken down when it becomes more clear exactly where they are used.
  !#----------------------------------------------------------------------------#

  REAL(fp), PUBLIC, PARAMETER :: WATER_CONTENT_THRESHOLD = 0.000001_fp
  REAL(fp), PUBLIC, PARAMETER :: OPTICAL_DEPTH_THRESHOLD = 0.000001_fp

  REAL(fp), PUBLIC, PARAMETER :: BS_THRESHOLD  = 0.000001_fp  ! Was SCATTERING_ALBEDO_THRESHOLD
  REAL(fp), PUBLIC, PARAMETER :: SCATTERING_ALBEDO_THRESHOLD  = 0.000001_fp  ! Eventually replace this with BS_THRESHOLD


  INTEGER, PUBLIC, PARAMETER :: MAX_N_LEGENDRE_TERMS = 6 !10
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PHASE_ELEMENTS = 1
  INTEGER, PUBLIC, PARAMETER :: MAX_N_STREAMS = 20 
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ANGLES = 5 
  INTEGER, PUBLIC, PARAMETER :: MAX_N_STOKES = 4
  
  LOGICAL, PUBLIC, PARAMETER :: HGPHASE = .FALSE.
                                                                                                        


CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Set_Max_n_Channels
! 
! PURPOSE:
!       Subroutine to set the protected variable MAX_N_CHANNELS value in the
!       CRTM_Parameters module. This should *only* be done during the CRTM
!       initialisation.
!
! CALLING SEQUENCE:
!       CALL CRTM_Set_Max_n_Channels( Value )
!
! INPUT ARGUMENTS:
!       Value:        The maximum number of channels.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! SIDE EFFECTS:
!       This subroutines changes the value of the MAX_N_CHANNELS pseudo-parameter
!       in the CRTM_Parameters module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Set_Max_n_Channels( Value )
    INTEGER, INTENT( IN ) :: Value
    MAX_N_CHANNELS = Value
  END SUBROUTINE CRTM_Set_Max_n_Channels


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Reset_Max_n_Channels
! 
! PURPOSE:
!       Subroutine to reset the protected variable MAX_N_CHANNELS value in the
!       CRTM_Parameters module to an invalid value. This should *only* be done
!       during the CRTM destruction.
!
! CALLING SEQUENCE:
!       CALL CRTM_Reset_Max_n_Channels()
!
! SIDE EFFECTS:
!       This subroutines changes the value of the MAX_N_CHANNELS pseudo-parameter
!       in the CRTM_Parameters module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Reset_Max_n_Channels()
    MAX_N_CHANNELS = RESET_VALUE
  END SUBROUTINE CRTM_Reset_Max_n_Channels


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Get_Max_n_Channels
! 
! PURPOSE:
!       Subroutine to GET the protected variable MAX_N_CHANNELS value stored
!       in the CRTM_Parameters module.
!
! CALLING SEQUENCE:
!       CALL CRTM_Get_Max_n_Channels( Value, Is_Set )
!
! OUTPUT ARGUMENTS:
!       Value:        The maximum number of channels.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Is_Set:       Logical flag for determining whether or not the
!                     maximum number of channels has been set.
!                     If == .TRUE.  the MAX_N_CHANNELS protected variable is
!                                   set to a valid value.
!                        == .FALSE. the MAX_N_CHANNELS protected variable
!                                   value is invalid
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Get_Max_n_Channels( Value, Is_Set )
    INTEGER,           INTENT( OUT ) :: Value
    LOGICAL, OPTIONAL, INTENT( OUT ) :: Is_Set
    Value = MAX_N_CHANNELS
    IF ( PRESENT( Is_Set ) ) THEN
      IF ( Value /= RESET_VALUE ) THEN
        Is_Set = .TRUE.
      ELSE
        Is_Set = .FALSE.
      END IF
    END IF
  END SUBROUTINE CRTM_Get_Max_n_Channels

END MODULE CRTM_Parameters
