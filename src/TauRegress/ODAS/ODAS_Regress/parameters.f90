
MODULE Parameters

  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds, ONLY : fp_kind


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  ! ---------------------------
  ! Current number of absorbers
  ! ---------------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_ABSORBERS = 3


  ! --------------------
  ! Number of predictors.
  ! --------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_PREDICTORS_USED = 6

  INTEGER, PUBLIC, PARAMETER :: MAX_N_STANDARD_PREDICTORS   = 11
  INTEGER, PUBLIC, PARAMETER :: MAX_N_INTEGRATED_PREDICTORS = 6

  INTEGER, PUBLIC, PARAMETER :: MAX_N_PREDICTORS = MAX_N_STANDARD_PREDICTORS + &
                                                   ( MAX_N_ABSORBERS * MAX_N_INTEGRATED_PREDICTORS )


  ! -----------------------------------------
  ! Maximum number of polynomial orders for
  ! reconstructing transmittance coefficients
  ! -----------------------------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_ORDERS = 10


  ! ----------------------------------------------------------
  ! Number of channels (for ALL satellites - really the number
  ! of satellites x number of channels USED per satellite)
  !
  ! This is also the number of lines in the satellite 
  ! information file.
  !
  ! Eventually the MAX_N_PROFILES and MAX_N_LAYERS Values
  ! will be dynamic, i.e they will be defined by user inputs.
  ! For now, however, they're hardwired.
  ! --------------------------------------------------------

  ! -- Accessed via SET_<name>, RESET_<name>, and GET_<name> routines
  INTEGER, PRIVATE, PARAMETER :: RESET_VALUE = -1

  INTEGER, PRIVATE, SAVE      :: MAX_N_CHANNELS = RESET_VALUE
  INTEGER, PUBLIC,  PARAMETER :: MAX_N_PROFILES = 128
  INTEGER, PUBLIC,  PARAMETER :: MAX_N_LAYERS   = 100


  ! -----
  ! Flags
  ! -----

  ! -- Direction flags for transmittance calculation
  INTEGER, PUBLIC, PARAMETER :: DOWN = 0
  INTEGER, PUBLIC, PARAMETER :: UP   = 1


  ! --------------------
  ! Numerical parameters
  ! --------------------

  ! -- Numbers
  REAL( fp_kind ), PUBLIC, PARAMETER :: ZERO      = 0.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: ONE       = 1.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: TWO       = 2.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: THREE     = 3.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: FIVE      = 5.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: POINT_25  = 0.25_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: POINT_5   = 0.5_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: POINT_75  = 0.75_fp_kind

  ! -- Precision/tolerance
  REAL( fp_kind ), PUBLIC, PARAMETER :: TOLERANCE = EPSILON( ONE )

  ! -- Numerical limits
  REAL( fp_kind ), PUBLIC, PARAMETER :: LIMIT_EXP = 354.5981_fp_kind  ! ABS( LOG( TOLERANCE ) )
  REAL( fp_kind ), PUBLIC, PARAMETER :: LIMIT_LOG = 1.0e+154_fp_kind  ! EXP( LIMIT_EXP )



  ! -- Constant to allow degrees->radians conversion
  REAL( fp_kind ), PUBLIC, PARAMETER :: PI = 3.141592653589793238462643383279_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp_kind

  ! -- Top-Of-Atmosphere pressure in hPa
  REAL( fp_kind ), PUBLIC, PARAMETER :: TOA_PRESSURE = 0.005_fp_kind

  ! -- Reciprocal gravity (scaled by 100 for use with pressure in hPa)
  REAL( fp_kind ), PUBLIC, PARAMETER :: RECIPROCAL_GRAVITY = ONE / 980.665_fp_kind

  ! -- Default diffusivity angle secant = ACOS( 3/5 ) in degrees (~53.13)
  ! -- used to approximate the downwelling flux.
  REAL( fp_kind ), PUBLIC, PARAMETER :: SECANT_DIFFUSIVITY_ANGLE = FIVE / THREE

  ! -- Maximum flux angle secant definition. Determined by the maximum
  ! -- angle secant used in generating the transmittance model coefficients,
  ! -- i.e. a secant of 2.25 => 63.6deg. If the user inputs a value larger
  ! -- than this for the Secant_Flux_Angle, the SECANT_DIFFUSIVITY_ANGLE
  ! -- value is used instead.
  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SECANT_FLUX_ANGLE = 2.25_fp_kind

  ! -- Maximum solar zenith angle secant definition. Should be determined
  ! -- by the maximum angle secant used in generating the transmittance
  ! -- model coefficients, i.e. a secant of 2.25 => 63.6deg. Users have
  ! -- requested the Value be 85deg => secant of ~11.47.
  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SOLAR_ANGLE = 85.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SECANT_SOLAR_ANGLE = 11.473711738554476_fp_kind



  ! ---------------------
  ! Subprogram visibility
  ! ---------------------

  ! -- The MAX_N_CHANNELS methods
  PUBLIC :: Set_Max_n_Channels
  PUBLIC :: Reset_Max_n_Channels
  PUBLIC :: Get_Max_n_Channels


CONTAINS


  ! -------------------------------------------
  ! Subroutines to SET and GET the value of the 
  ! "pseudo-parameter" MAX_N_CHANNELS
  ! -------------------------------------------

  ! -- Set the value
  SUBROUTINE Set_Max_n_Channels( Value )
    INTEGER, INTENT( IN ) :: Value
    MAX_N_CHANNELS = Value
  END SUBROUTINE Set_Max_n_Channels

  ! -- REset the value
  SUBROUTINE Reset_Max_n_Channels()
    MAX_N_CHANNELS = RESET_VALUE
  END SUBROUTINE Reset_Max_n_Channels

  ! -- Get the value and test if it's been set
  SUBROUTINE Get_Max_n_Channels( Value, Is_Set )
    INTEGER, INTENT( OUT )           :: Value
    LOGICAL, INTENT( OUT ), OPTIONAL :: Is_Set
    Value = MAX_N_CHANNELS
    IF ( PRESENT( Is_Set ) ) THEN
      IF ( Value /= RESET_VALUE ) THEN
        Is_Set = .TRUE.
      ELSE
        Is_Set = .FALSE.
      END IF
    END IF
  END SUBROUTINE Get_Max_n_Channels

END MODULE Parameters


