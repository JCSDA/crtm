!--------------------------------------------------------------------------------
!M+
! NAME:
!       MONORTM_Input
!
! PURPOSE:
!       Module containing routines for creating MONORTM input files.
!
! CATEGORY:
!       MONORTM
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE MONORTM_input
!
! MODULES:
!       Type_Kinds:         Module with data type kind definitions.
!
!       File_Utility:       Module containing generic file utility routines
!
!       Message_Handler:    Module containing error handling definitions and
!                           routines.
!                           USEs: FILE_UTILITY module
!
! CONTAINS:
!       
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
!
!M-
!--------------------------------------------------------------------------------

MODULE MONORTM_Input

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE String_Utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------

  PRIVATE
  PUBLIC :: create_MONORTM_TAPE5


  ! -------------
  ! Derived types
  ! -------------

  TYPE, PUBLIC :: Calculation_Flags_type
    INTEGER :: hirac
    INTEGER :: lblf4
    INTEGER :: continuum
    INTEGER :: aerosol
    INTEGER :: emit
    INTEGER :: scan_flag
    INTEGER :: filter
    INTEGER :: plot_out
    INTEGER :: test
    INTEGER :: atm
    INTEGER :: merge_flag
    INTEGER :: laser
    INTEGER :: od_layer
    INTEGER :: xsection
    INTEGER :: od_mpts
    INTEGER :: od_npts
  END TYPE Calculation_Flags_type

  TYPE, PRIVATE :: LBLATM_Flags_type
    INTEGER         :: profile_type
    INTEGER         :: path_type
    INTEGER         :: n_layer_boundaries
    INTEGER         :: no_zero
    INTEGER         :: no_print
    INTEGER         :: n_molecules
    INTEGER         :: i_punch
    INTEGER         :: i_fixtype
    INTEGER         :: units
    REAL( fp_kind ) :: earth_radius
    REAL( fp_kind ) :: altitude_of_space
    REAL( fp_kind ) :: average_frequency
    REAL( fp_kind ) :: reference_latitude
  END TYPE LBLATM_Flags_type


  ! -----------------
  ! Module parameters
  ! -----------------
  
  ! -- Integer definitions for record 1.2
  INTEGER, PRIVATE, PARAMETER :: IHIRAC = 1
  INTEGER, PRIVATE, PARAMETER :: ICNTNM = 1
  INTEGER, PRIVATE, PARAMETER :: IATM   = 1
  INTEGER, PRIVATE, PARAMETER :: IXSECT = 0
  INTEGER, PRIVATE, PARAMETER :: ISPD   = 0
  
  ! -- Record 1.3 definition
  REAL( fp_kind ), PARAMETER :: DVSET = -1.0_fp_kind

  ! -- Floating point definitions for common constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO    =    0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE     =    1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TWO     =    2.0_fp_kind

  ! -- Tolerance value
  REAL( fp_kind ), PRIVATE, PARAMETER :: TOLERANCE = EPSILON( ZERO )

  ! -- Keyword set states
  INTEGER,         PRIVATE, PARAMETER :: UNSET = 0
  INTEGER,         PRIVATE, PARAMETER ::   SET = 1

  ! -- Range of number of absorbers
  INTEGER,         PRIVATE, PARAMETER :: MIN_N_ABSORBERS    =  7
  INTEGER,         PRIVATE, PARAMETER :: MAX_N_ABSORBERS    = 32
  INTEGER,         PRIVATE, PARAMETER :: MAX_N_XS_ABSORBERS = 35

  ! -- Maximum number of calculation LAYERS and profile LEVELS
  INTEGER,         PRIVATE, PARAMETER :: MAX_LAYER_BOUNDARIES = 200
  INTEGER,         PRIVATE, PARAMETER :: MAX_PROFILE_LEVELS   = 3400

  ! -- Maximum frequency and bandwidth
  REAL( fp_kind ), PRIVATE, PARAMETER :: MAX_FREQUENCY = 20000.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: MAX_BANDWIDTH =  2000.0_fp_kind

  ! -- Temperature of space in Kelvin
  REAL( fp_kind ), PRIVATE, PARAMETER :: COSMIC_BACKGROUND_TEMPERATURE = 2.736_fp_kind

  ! -- Calculation direction flags
  INTEGER,         PRIVATE, PARAMETER ::   UPWELLING_CALCULATION_DIR = 1
  INTEGER,         PRIVATE, PARAMETER :: DOWNWELLING_CALCULATION_DIR = 0

  ! -- Define defaults
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_CO2_PPMV     = 380.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_ZENITH_ANGLE = ZERO

  INTEGER,         PRIVATE, PARAMETER :: DEFAULT_CLIMATOLOGY_MODEL = 6  ! U.S. Std. Atm.
  INTEGER,         PRIVATE, PARAMETER ::     MIN_CLIMATOLOGY_MODEL = 1
  INTEGER,         PRIVATE, PARAMETER ::     MAX_CLIMATOLOGY_MODEL = 6

  ! -- Default calculation flags
  TYPE( Calculation_Flags_type ), PUBLIC, PARAMETER :: DEFAULT_CALCULATION_FLAGS = &
    Calculation_Flags_type( 1, & ! hirac       - Voight profile
                            1, & ! lblf4       - LBL bound is 25cm-1 for all layers
                            0, & ! continuum   - No continua
                            0, & ! aerosol     - No aerosols
                            0, & ! emit        - Optical depth calc.
                            0, & ! scan_flag   - No scanning function
                            0, & ! filter      - No filter function
                            0, & ! plot_out    - No plot output
                            0, & ! test        - No test option
                            1, & ! atm         - Call LBLATM
                            1, & ! merge_flag  - OD output for each layer
                            0, & ! laser       - No laser option
                            0, & ! od_layer    - Normal layering in OD calculation
                            0, & ! xsection    - No Xsections
                            0, & ! od_mpts     - No convolution OD output to TAPE6
                            0  ) ! od_npts     - No merge OD output to TAPE6

  ! -- Default LBLATM calculation flags
  TYPE( LBLATM_Flags_type ), PRIVATE, PARAMETER :: DEFAULT_LBLATM_FLAGS = &
    LBLATM_Flags_type(                0, & ! profile_type       - User supplied
                                      2, & ! path type          - Slant path from H1->H2
                                      0, & ! n_layer_boundaries - Generated internally (not yet at least)
                                      1, & ! no_zero            - Suppress zeroing of absorber amounts < 0.1% of total
                                      1, & ! no_print           - Short print out to TAPE6 (no refractive index information)
                        MIN_N_ABSORBERS, & ! n_molecules        - Number of molecular species
                                      1, & ! i_punch            - Layer data written to TAPE7
                                      0, & ! i_fixtype          - Suppresses some output to TAPE7
                                      0, & ! units              - Output molecules/cm^2) to TAPE7
                            0.0_fp_kind, & ! earth_radius       - Use default
                            0.0_fp_kind, & ! altitude of space  - Use default (100km)
                            0.0_fp_kind, & ! average frequency  - Use default calculation
                            0.0_fp_kind  ) ! Reference latitude - Use default
                       !DEFAULT_CO2_PPMV, & ! CO2 ppmv to use
                       !     0.0_fp_kind  ) ! Reference latitude - Use default


  ! -- Valid ranges for calculation flags
  INTEGER,         PRIVATE, PARAMETER ::     MIN_CONTINUUM_OPTION  = 0
  INTEGER,         PRIVATE, PARAMETER ::     MAX_CONTINUUM_OPTION  = 6

  ! -- Threshold value for variable pressure output format
  REAL( fp_kind ), PRIVATE, PARAMETER :: PRESSURE_FMT_THRESHOLD = ONE

  ! -- HITRAN absorber names
  CHARACTER( * ),  PRIVATE, PARAMETER, DIMENSION( MAX_N_ABSORBERS ) :: ABSORBER_NAME = &
    (/ 'H2O     ', 'CO2     ', 'O3      ', 'N2O     ', &
       'CO      ', 'CH4     ', 'O2      ', 'NO      ', &
       'SO2     ', 'NO2     ', 'NH3     ', 'HNO3    ', &
       'OH      ', 'HF      ', 'HCl     ', 'HBR     ', &
       'HI      ', 'ClO     ', 'OCS     ', 'H2CO    ', &
       'HOCl    ', 'N2      ', 'HCN     ', 'CH3Cl   ', &
       'H2O2    ', 'C2H2    ', 'C2H6    ', 'PH3     ', &
       'COF2    ', 'SF6     ', 'H2S     ', 'HCOOH   ' /)

  ! -- HITRAN molecular weights in g/mole
  REAL( fp_kind ), PRIVATE, PARAMETER, DIMENSION( MAX_N_ABSORBERS ) :: ABSORBER_MOLECULAR_WEIGHT = &
    (/ 18.0153_fp_kind,   44.0095_fp_kind,   47.9982_fp_kind,   44.0129_fp_kind, &
       28.0101_fp_kind,   16.0425_fp_kind,   31.9988_fp_kind,   30.0061_fp_kind, &
       64.0648_fp_kind,   46.0055_fp_kind,   17.0306_fp_kind,   63.0129_fp_kind, &
       17.0073_fp_kind,   20.0063_fp_kind,   36.4606_fp_kind,   80.9119_fp_kind, &
      127.9124_fp_kind,   51.4521_fp_kind,   60.0761_fp_kind,   30.0260_fp_kind, &
       52.4600_fp_kind,   28.0135_fp_kind,   27.0254_fp_kind,   50.4872_fp_kind, &
       34.0147_fp_kind,   26.0373_fp_kind,   30.0690_fp_kind,   33.9976_fp_kind, &
       66.0069_fp_kind,  146.0564_fp_kind,   34.0819_fp_kind,   46.0254_fp_kind /)

  ! -- Absorber unit check values
  INTEGER,         PRIVATE, PARAMETER ::      MIXING_RATIO_UNITS = 1  ! g/kg
  INTEGER,         PRIVATE, PARAMETER ::              PPMV_UNITS = 2
  INTEGER,         PRIVATE, PARAMETER ::  PARTIAL_PRESSURE_UNITS = 3  ! hPa
  INTEGER,         PRIVATE, PARAMETER ::      KMOL_PER_CM2_UNITS = 4
  INTEGER,         PRIVATE, PARAMETER :: MOLECULES_PER_CM3_UNITS = 5

  INTEGER,         PRIVATE, PARAMETER :: MIN_VALID_ABSORBER_UNIT = MIXING_RATIO_UNITS
  INTEGER,         PRIVATE, PARAMETER :: MAX_VALID_ABSORBER_UNIT = MOLECULES_PER_CM3_UNITS

  ! ----------------
  ! Module variables
  ! ----------------

  ! -- No place holder output
  LOGICAL, PRIVATE :: no_placeholder

CONTAINS

  FUNCTION create_MONORTM_TAPE5( pressure,             &  ! Input
                                temperature,          &  ! Input
                                absorber_amount,      &  ! Input
                                absorber_units,       &  ! Input
                                absorber_id,          &  ! Input
                                surface_altitude,     &  ! Input
                                begin_frequency,      &  ! Input
                                end_frequency,        &  ! Input
                                calculation_flags,    &  ! Optional input
                                absorber_format,      &  ! Optional input
                                co2ppmv,              &  ! Optional input
                                boundary_temperature, &  ! Optional input
                                climatology_model,    &  ! Optional input
                                downwelling,          &  ! Optional input
                                zenith_angle,         &  ! Optional input
                                observer_pressure,    &  ! Optional input
                                boundary_pressure,    &  ! Optional input
                                layer_boundaries,     &  ! Optional input
                                continuum_scale,      &  ! Optional input
                                xsection_name,        &  ! Optional input
                                xsection_pressure,    &  ! Optional input
                                xsection_amount,      &  ! Optional input
                                xsection_units,       &  ! Optional input
                                header,               &  ! Optional input
                                filename,             &  ! Optional input
                                placeholder,          &  ! Optional input
                                no_terminator,        &  ! Optional_input

                                message_log  )        &  ! Error messaging

                              RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ), DIMENSION( : ),              INTENT( IN ) :: pressure
    REAL( fp_kind ), DIMENSION( : ),              INTENT( IN ) :: temperature
    REAL( fp_kind ), DIMENSION( :, : ),           INTENT( IN ) :: absorber_amount
    CHARACTER( 1 ),  DIMENSION( : ),              INTENT( IN ) :: absorber_units
    INTEGER,         DIMENSION( : ),              INTENT( IN ) :: absorber_id
    REAL( fp_kind ),                              INTENT( IN ) :: surface_altitude
    REAL( fp_kind ),                              INTENT( IN ) :: begin_frequency
    REAL( fp_kind ),                              INTENT( IN ) :: end_frequency

    ! -- Optional Inputs
    TYPE( Calculation_Flags_type ),     OPTIONAL, INTENT( IN ) :: calculation_flags
    INTEGER,                            OPTIONAL, INTENT( IN ) :: absorber_format
    REAL( fp_kind ),                    OPTIONAL, INTENT( IN ) :: co2ppmv
    REAL( fp_kind ),                    OPTIONAL, INTENT( IN ) :: boundary_temperature
    INTEGER,                            OPTIONAL, INTENT( IN ) :: climatology_model
    INTEGER,                            OPTIONAL, INTENT( IN ) :: downwelling
    REAL( fp_kind ),                    OPTIONAL, INTENT( IN ) :: zenith_angle
    REAL( fp_kind ),                    OPTIONAL, INTENT( IN ) :: observer_pressure
    REAL( fp_kind ),                    OPTIONAL, INTENT( IN ) :: boundary_pressure
    REAL( fp_kind ), DIMENSION( : ),    OPTIONAL, INTENT( IN ) :: layer_boundaries
    REAL( fp_kind ), DIMENSION( 7 ),    OPTIONAL, INTENT( IN ) :: continuum_scale
    CHARACTER( 10 ), DIMENSION( : ),    OPTIONAL, INTENT( IN ) :: xsection_name
    REAL( fp_kind ), DIMENSION( : ),    OPTIONAL, INTENT( IN ) :: xsection_pressure
    REAL( fp_kind ), DIMENSION( :, : ), OPTIONAL, INTENT( IN ) :: xsection_amount
    CHARACTER( 1 ),  DIMENSION( : ),    OPTIONAL, INTENT( IN ) :: xsection_units
    CHARACTER( * ),                     OPTIONAL, INTENT( IN ) :: header
    CHARACTER( * ),                     OPTIONAL, INTENT( IN ) :: filename
    INTEGER,                            OPTIONAL, INTENT( IN ) :: placeholder
    INTEGER,                            OPTIONAL, INTENT( IN ) :: no_terminator

    ! -- Error handling filename
    CHARACTER( * ),                     OPTIONAL, INTENT( IN ) :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    

    ! -- Routine name
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Create_MONORTM_TAPE5'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: file_id
    INTEGER :: io_status

    INTEGER :: i, j, k, n 
    INTEGER :: n_levels 
    INTEGER :: n_absorbers 
    INTEGER :: pos
    INTEGER :: user_climatology_model 
    INTEGER :: calculation_direction 
    INTEGER :: xs_profile_flag
    INTEGER :: n_xs_levels
    INTEGER :: n_xs_absorbers

    TYPE( Calculation_Flags_type ) :: user_calculation_flags
    TYPE( LBLATM_Flags_type ) :: LBLATM_flags

    REAL( fp_kind ), DIMENSION( SIZE( pressure ) - 1 ) :: dp

    REAL( fp_kind ), DIMENSION( MAX_LAYER_BOUNDARIES ) :: user_layer_boundaries

    REAL( fp_kind ) :: bandwidth
    REAL( fp_kind ) :: user_co2ppmv
    REAL( fp_kind ) :: user_boundary_temperature
    REAL( fp_kind ) :: user_boundary_emissivity
    REAL( fp_kind ) :: user_zenith_angle
    REAL( fp_kind ) :: user_observer_pressure
    REAL( fp_kind ) :: temp_observer_pressure
    REAL( fp_kind ) :: user_boundary_pressure

    CHARACTER( LEN =  78 ) :: user_header
    CHARACTER( LEN = 128 ) :: tape5_filename
    CHARACTER( LEN =  50 ) :: record_number
    CHARACTER( LEN =  50 ) :: blank_line_format

    CHARACTER( LEN =  1 ) :: format_type
    CHARACTER( LEN = 10 ) :: data_format
    CHARACTER( LEN =  7 ) :: blank_format

    CHARACTER( LEN = MAX_N_ABSORBERS ) :: jchar_profile
    CHARACTER( LEN = MAX_N_ABSORBERS ) :: jchar_model

    CHARACTER( LEN = 15 ), DIMENSION( MAX_N_ABSORBERS ) :: level_amount

    INTEGER, DIMENSION( 8 ) :: date_and_time_values

integer, dimension(1) :: loc_index
integer :: nf
character(8) :: pfmt
character(3) :: advance
integer :: altitude_flag
logical :: terminator


    !#--------------------------------------------------------------------------#
    !#                   -- ASSIGN DEFAULT LBLATM FLAGS --                      #
    !#                                                                          #
    !#            The defaults may be changed by the user inputs                #
    !#--------------------------------------------------------------------------#

    LBLATM_Flags = DEFAULT_LBLATM_FLAGS
    


    !#--------------------------------------------------------------------------#
    !#                      -- CHECK MANDATORY INPUTS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------------------
    ! Check profile levels array sizes
    ! --------------------------------

    n_levels = SIZE( pressure )

    ! -- Too many
    IF ( n_levels > MAX_PROFILE_LEVELS ) THEN
      error_status = FAILURE
      WRITE( message, '( "Too many input PRESSURE levels. ", i4, " levels max." )' ) &
                      MAX_PROFILE_LEVELS
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    ! -- Inconsistent input
    IF ( SIZE( temperature ) /= n_levels ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'TEMPERATURE dimension inconsistent with PRESSURE.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    IF ( SIZE( absorber_amount, DIM = 1 ) /= n_levels ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'ABSORBER_AMOUNT dimension inconsistent with PRESSURE.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ------------------------------------------------
    ! Check that pressure is in ascending height order
    ! ------------------------------------------------

    dp = pressure( 1 : n_levels - 1 ) - pressure(  2 : n_levels )
    IF ( ANY( dp < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressures not in ascending height order.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ---------------------------------
    ! Check absorber number array sizes
    ! ---------------------------------

    n_absorbers = SIZE( absorber_amount, DIM = 2 )

    IF ( n_absorbers > MAX_N_ABSORBERS ) THEN
      WRITE( message, '( "Too many input absorbers. ", i4, " max." )' ) &
                      MAX_N_ABSORBERS
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    ! -- Inconsistent input
    IF ( SIZE( absorber_units ) /= n_absorbers ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'ABSORBER_UNITS dimension inconsistent with ABSORBER_AMOUNT.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    IF ( SIZE( absorber_id ) /= n_absorbers ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'ABSORBER_ID dimension inconsistent with ABSORBER_AMOUNT.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------
    ! Check absorber ID
    ! -----------------

    IF ( ANY( absorber_id < 1               ) .OR. &
         ANY( absorber_id > MAX_N_ABSORBERS )      ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid absorber ID specified', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    ! -- Assign the number of molecules to use in calculation
    LBLATM_Flags%n_molecules = MAX( MAXVAL( absorber_id ), MIN_N_ABSORBERS )


    ! -------------------------
    ! Check absorber unit flags
    ! -------------------------

    error_status = Check_Absorber_Units( absorber_units, &
                                         absorber_id,    &
                                         message_log = message_log )

    IF ( error_status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Absorber units check failed.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ---------------
    ! Frequency check
    ! ---------------

 !   error_status = Check_Frequency( begin_frequency, &
 !                                   end_frequency,   &
 !                                   message_log = message_log )

 !   IF ( error_status /= SUCCESS ) THEN
 !     CALL display_message( ROUTINE_NAME, &
 !                           'Frequency check failed.', &
 !                           error_status, &
 !                           message_log = message_log )
 !     RETURN
 !   END IF



    !#--------------------------------------------------------------------------#
    !#                       -- CHECK OPTIONAL INPUTS --                        #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Calculation flags
    ! -----------------

    ! -- Define default
    user_calculation_flags = DEFAULT_CALCULATION_FLAGS

    ! -- Check keyword
    IF ( PRESENT( calculation_flags ) ) THEN
      user_calculation_flags = calculation_flags
    END IF


    ! ---------------
    ! Absorber format
    ! ---------------

    ! -- Define defaults
    format_type  = ' '
    data_format  = '( es10.3 )'
    blank_format = '( 10x )'
    nf = 10

    ! -- Check keyowrd
    IF ( PRESENT( absorber_format ) ) THEN
      IF ( absorber_format == SET ) THEN
        format_type  = 'L'
        data_format  = '( es15.8 )'
        blank_format = '( 15x )'
        nf = 15
      END IF
    END IF


    ! ----------
    ! CO2 amount
    ! ---------- 

    ! -- Check keyowrd
  !  IF ( PRESENT( co2ppmv ) ) THEN
  !    LBLATM_Flags%co2ppmv = ABS( co2ppmv )
  !  END IF


    ! -----------------
    ! Climatology model
    ! -----------------

    ! -- Define default
    user_climatology_model = DEFAULT_CLIMATOLOGY_MODEL

    ! -- Check keyword
    IF ( PRESENT( climatology_model ) ) THEN

      ! -- Is specified model valid?
      IF ( climatology_model < MIN_CLIMATOLOGY_MODEL .OR. &
           climatology_model > MAX_CLIMATOLOGY_MODEL      ) THEN
        WRITE( message, '( "Invalid CLIMATOLOGY_MODEL input. Using value of ", i1 )' ) &
                        user_climatology_model
        CALL display_message( ROUTINE_NAME, &
                              message, &
                              INFORMATION, &
                              message_log = message_log )
      ELSE
        user_climatology_model = climatology_model
      END IF

    END IF


    ! ------------------------------------------------
    ! Calculation direction. Upwelling is the default.
    ! ------------------------------------------------

    ! -- Define default
    calculation_direction = UPWELLING_CALCULATION_DIR

    ! -- Check keyowrd
    IF ( PRESENT( downwelling ) ) THEN
      IF ( downwelling == SET ) calculation_direction = DOWNWELLING_CALCULATION_DIR
    END IF


    ! --------------------
    ! Boundary temperature
    ! --------------------

    ! -- Define default
    SELECT CASE ( calculation_direction )

      CASE ( DOWNWELLING_CALCULATION_DIR )
        user_boundary_temperature = COSMIC_BACKGROUND_TEMPERATURE

      CASE ( UPWELLING_CALCULATION_DIR )
        user_boundary_temperature = temperature( 1 )

    END SELECT

    ! -- Check keyword
    IF ( PRESENT( boundary_temperature ) ) THEN

      ! -- Is user value valid?
      IF ( boundary_temperature < ZERO ) THEN
        WRITE( message, '( "BOUNDARY_TEMPERATURE input < 0.0K. Using value of ", f7.3, "K" )' ) &
                        user_boundary_temperature
        CALL display_message( ROUTINE_NAME, &
                              message, &
                              INFORMATION, &
                              message_log = message_log )
      ELSE
        user_boundary_temperature = boundary_temperature
      END IF

    END IF


    ! ------------
    ! Zenith angle
    ! ------------

    ! -- Define default
    user_zenith_angle = DEFAULT_ZENITH_ANGLE

    ! -- Check keyowrd
    IF ( PRESENT( zenith_angle ) ) THEN
      user_zenith_angle = ABS( zenith_angle )
    END IF

    ! -- Modify based on calculation direction
    IF ( calculation_direction == UPWELLING_CALCULATION_DIR ) THEN
      user_zenith_angle = 180.0_fp_kind - user_zenith_angle
    END IF


    ! -----------------
    ! Observer pressure
    ! -----------------

    ! -- Define default
    SELECT CASE ( calculation_direction )

      CASE ( DOWNWELLING_CALCULATION_DIR )
        user_observer_pressure = pressure( 1 )         ! Lower boundary

      CASE ( UPWELLING_CALCULATION_DIR )
        user_observer_pressure = pressure( n_levels )  ! Upper boundary

    END SELECT

    ! -- Check keyword
    IF ( PRESENT( observer_pressure ) ) THEN

      ! -- Is user value valid?
      IF ( observer_pressure < ZERO ) THEN
        WRITE( message, '( "OBSERVER_PRESSURE input < 0.0hPa. Using value of ", f8.3, "hPa" )' ) &
                        user_observer_pressure
        CALL display_message( ROUTINE_NAME, &
                              message, &
                              INFORMATION, &
                              message_log = message_log )
      ELSE
        user_observer_pressure = observer_pressure
      END IF

    END IF


    ! -----------------
    ! Boundary pressure
    ! -----------------

    ! -- Define default
    SELECT CASE ( calculation_direction )

      CASE ( DOWNWELLING_CALCULATION_DIR )
        user_boundary_pressure = pressure( n_levels )  ! Upper boundary

      CASE ( UPWELLING_CALCULATION_DIR )
        user_boundary_pressure = pressure( 1 )         ! Lower boundary

    END SELECT

    ! -- Check keyword
    IF ( PRESENT( boundary_pressure ) ) THEN

      ! -- Is user value valid?
      IF ( boundary_pressure < ZERO ) THEN
        WRITE( message, '( "BOUNDARY_PRESSURE input < 0.0hPa. Using value of ", f8.3, "hPa" )' ) &
                        user_boundary_pressure
        CALL display_message( ROUTINE_NAME, &
                              message, &
                              INFORMATION, &
                              message_log = message_log )
      ELSE
        user_boundary_pressure = boundary_pressure
      END IF

    END IF


    ! --------------------------------------------------------
    ! Make sure observer and boundary pressures are consistent
    ! (in case *both* values were specified by the user)
    ! --------------------------------------------------------

    SELECT CASE ( calculation_direction )

      CASE ( DOWNWELLING_CALCULATION_DIR )
        temp_observer_pressure = MAX( user_observer_pressure, user_boundary_pressure )
        user_boundary_pressure = MIN( user_observer_pressure, user_boundary_pressure )
        user_observer_pressure = temp_observer_pressure

      CASE ( UPWELLING_CALCULATION_DIR )
        temp_observer_pressure = MIN( user_observer_pressure, user_boundary_pressure )
        user_boundary_pressure = MAX( user_observer_pressure, user_boundary_pressure )
        user_observer_pressure = temp_observer_pressure

    END SELECT


    ! ----------------------------
    ! Calculation layer boundaries
    ! ----------------------------

    IF ( PRESENT( layer_boundaries ) ) THEN

      ! -- No. of passed calculation levels
      LBLATM_Flags%n_layer_boundaries = SIZE( layer_boundaries )

      ! -- Too many?
      IF ( LBLATM_Flags%n_layer_boundaries > MAX_LAYER_BOUNDARIES ) THEN
        WRITE( message, '( "Too many layer boundaries, ", i4, ". Max. is ", i3, "." )' ) &
                        LBLATM_Flags%n_layer_boundaries, MAX_LAYER_BOUNDARIES
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Assign the calculation levels
      user_layer_boundaries( 1 : LBLATM_Flags%n_layer_boundaries ) = layer_boundaries

    ELSE

      ! -- No supplied calculation levels, check if profile boundaries o.k.
      IF ( n_levels > MAX_LAYER_BOUNDARIES ) THEN
        WRITE( message, '( "Too many profile levels to use as layer boundaries, ", i4, &
                          &". Max. is ", i3, "." )' ) &
                        n_levels, MAX_LAYER_BOUNDARIES
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Use the profile levels as layer boundaries
      LBLATM_Flags%n_layer_boundaries = n_levels
      user_layer_boundaries( 1 : LBLATM_Flags%n_layer_boundaries ) = pressure

    END IF


    ! ----------------
    ! Continuum option
    ! ----------------




    ! -----------------------
    ! Continuum scale factors
    ! -----------------------

    IF ( user_calculation_flags%continuum == 6 ) THEN
      IF ( PRESENT( continuum_scale ) ) THEN
        IF ( ANY( continuum_scale < ZERO ) ) THEN
          error_status = FAILURE
          CALL display_message( ROUTINE_NAME, &
                                'CONTINUUM_SCALE contains negative values.', &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF
      ELSE
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'CONTINUUM_SCALE not provided for continuum option 6', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF
    END IF


    ! ------------
    ! Merge option
    ! ------------




    ! -------------------------
    ! X-section molecule option
    ! -------------------------




    ! ---------------------
    ! Check XSECTION inputs
    ! ---------------------

    xsection_option: IF ( user_calculation_flags%xsection == 1 ) THEN

      ! -- Set profile flag to use model profile
      xs_profile_flag = 1


      ! ----------------------------------------------------
      ! Check XSECTION_NAME input. This *must* be specified.
      ! ----------------------------------------------------

      IF ( PRESENT( xsection_name ) ) THEN

        ! -- The number of XS absorbers
        n_xs_absorbers = SIZE( xsection_name )

        ! -- Too many?
        IF ( n_xs_absorbers > MAX_N_XS_ABSORBERS ) THEN
          WRITE( message, '( "Too many input X-section absorbers. ", i4, " max." )' ) &
                          MAX_N_XS_ABSORBERS
          error_status = FAILURE
          CALL display_message( ROUTINE_NAME, &
                                message, &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF

      ELSE

        ! -- Wasn't passed. Unrecoverable error
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'XSECTION_NAME not provided for xsection option 1', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF


      ! ---------------------------------------------
      ! Check all other XSECTION inputs. If *any* are
      ! specified, they *all* must be specified
      ! ---------------------------------------------

      xsection_user_profile: IF ( PRESENT( xsection_pressure ) .AND. &
                                  PRESENT( xsection_amount   ) .AND. &
                                  PRESENT( xsection_units    )       ) THEN

        ! -- Set profile flag to use supplied profile
        xs_profile_flag = 0


        ! -----------------------------
        ! Check XSECTION_PRESSURE input
        ! -----------------------------

        ! -- Check number of levels
        n_xs_levels = SIZE( xsection_pressure )
        IF ( n_xs_levels > MAX_PROFILE_LEVELS ) THEN
          error_status = FAILURE
          WRITE( message, '( "Too many input XSECTION_PRESSURE levels. ", i4, &
                            &" levels max." )' ) &
                          MAX_PROFILE_LEVELS
          CALL display_message( ROUTINE_NAME, &
                                message, &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF


        ! ---------------------------
        ! Check XSECTION_AMOUNT input
        ! ---------------------------

        ! -- Check number of levels
        IF ( SIZE( xsection_amount, DIM = 1 ) /= n_xs_levels ) THEN
          error_status = FAILURE
          CALL display_message( ROUTINE_NAME, &
                                'XSECTION_AMOUNT level dimension inconsistent '// &
                                'with XSECTION_PRESSURE.', &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF

        ! -- Check number of absorbers
        IF ( SIZE( xsection_amount, DIM = 2 ) /= n_xs_absorbers ) THEN
          error_status = FAILURE
          CALL display_message( ROUTINE_NAME, &
                                'XSECTION_AMOUNT absorber dimension inconsistent '// &
                                'with XSECTION_NAME.', &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF


        ! --------------------------
        ! Check XSECTION_UNITS input
        ! --------------------------

        ! -- Check number of absorbers
        IF ( SIZE( xsection_units ) /= n_xs_absorbers ) THEN
          error_status = FAILURE
          CALL display_message( ROUTINE_NAME, &
                                'XSECTION_UNITS absorber dimension inconsistent '// &
                                'with XSECTION_NAME.', &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF

        ! -- Check contents
        IF ( ANY( xsection_units /= 'A' ) .OR. ANY( xsection_units /= '1' ) ) THEN
          error_status = FAILURE
          CALL display_message( ROUTINE_NAME, &
                                'Invalid absorber unit flag specified in XSECTION_UNITS.', &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF

      ELSE ! xsection_user_profile

        ! -----------------------------------------------------
        ! Check if only some of the required inputs were passed
        ! -----------------------------------------------------

        IF ( PRESENT( xsection_pressure ) .OR. &
             PRESENT( xsection_amount   ) .OR. &
             PRESENT( xsection_units    )      ) THEN
          error_status = FAILURE
          CALL display_message( ROUTINE_NAME, &
                                'Must specify XSECTION_PRESSURE *and* '// &
                                'XSECTION_AMOUNT *and* XSECTION_UNITS.', &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF

      END IF xsection_user_profile

    END IF xsection_option


    ! -------------
    ! Header string
    ! -------------

    IF ( PRESENT( header ) ) THEN
      user_header = TRIM( header )
    ELSE
      CALL DATE_AND_TIME( values = date_and_time_values )
      WRITE( user_header, FMT = '( "MONORTM TAPE5. Created on ", &
                                  &i4,"/",i2.2,"/",i2.2," at ", &
                                  &i2.2,":",i2.2,":",i2.2 )' ) &
                          date_and_time_values( 1 : 3 ), &  ! Date
                          date_and_time_values( 5 : 7 )     ! Time
    END IF


    ! ---------------
    ! Output filename
    ! ---------------

    ! -- Define default
    tape5_filename = 'tape5.rdk'

    ! -- Check keyword
    IF ( PRESENT( filename ) ) THEN
      tape5_filename = TRIM( filename )
    END IF


    ! ----------------
    ! Placeholder flag
    ! ----------------

    no_placeholder = .TRUE.
    IF ( PRESENT( placeholder ) ) THEN
      IF ( placeholder == SET ) no_placeholder = .FALSE.
    END IF

    ! ----------
    ! Terminator
    ! ----------

    terminator = .TRUE.
    IF ( PRESENT( no_terminator ) ) THEN
      IF ( no_terminator == SET ) terminator = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- OPEN OUTPUT FILE --                          #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Obtain a free unit number
    ! -------------------------

    file_id = get_lun()


    ! -------------
    ! Open the file
    ! -------------

    OPEN( file_id, FILE   = tape5_filename, &
                   STATUS = 'REPLACE',      &
                   ACCESS = 'SEQUENTIAL',   &
                   FORM   = 'FORMATTED',    &
                   ACTION = 'WRITE',        &
                   IOSTAT = io_status       )

    IF ( io_status /= 0 ) THEN
      WRITE( message, '( "Error opening output TAPE5 file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( tape5_filename ), io_status
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- OUTPUT PRE-PROFILE TAPE 5 RECORDS --                  #
    !#--------------------------------------------------------------------------#

    ! -- Currently, pressure boundary output ONLY
    altitude_flag = UNSET



    ! --------------------------
    ! Header record (record 1.1)
    ! --------------------------

    record_number = '1.1'
    WRITE( file_id, FMT    = '( "$ ", a )', &
                    IOSTAT = io_status      ) &
                    user_header
    IF ( io_status /= 0 ) GOTO 1000
    
    ! write record 1.2
    record_number = '1.2'
    WRITE( file_id, FMT = '(I5,I10,I35,2I20)', &
                    IOSTAT = io_status    ) &
                    IHIRAC, ICNTNM, IATM, IXSECT, ISPD
                    
    ! write record 1.3
    record_number = '1.3'
    WRITE( file_id, FMT = '(E10.3)', &
                    IOSTAT = io_status ) &
                    DVSET

    error_status = Write_Record_1p2( file_id, &
                                     user_calculation_flags, &
                                     message_log = message_log )

    IF ( error_status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME,    &
                            'Error writing Record 1.2 to'//TRIM( tape5_filename ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF


    ! -------------------------------------
    ! Continuum scale factors (record 1.2a)
    ! -------------------------------------

    IF ( user_calculation_flags%continuum == 6 ) THEN
      record_number = '1.2a'

      IF ( no_placeholder ) THEN
        WRITE( file_id, FMT    = '( 7( f10.3 ) )', &
                        IOSTAT = io_status         ) &
                        continuum_scale
      ELSE
        ! -- Write place holder
        WRITE( file_id, FMT    = '( "CONTINUUM_SCALE" )', &
                        IOSTAT = io_status         )
      END IF

      IF ( io_status /= 0 ) GOTO 1000

    END IF


    ! -----------------------------
    ! Frequency limits (record 1.3)
    ! -----------------------------

    record_number = '1.3'

    IF ( io_status /= 0 ) GOTO 1000


    ! --------------------------------
    ! Boundary conditions (record 1.4)
    ! --------------------------------

    IF ( user_calculation_flags%emit == 1 ) THEN

      ! -- Set boundary emissivity
      user_boundary_emissivity = 1.0

      record_number = '1.4'
      WRITE( file_id, FMT    = '( 2( f10.3 ) )', &
                      IOSTAT = io_status         ) &
                      user_boundary_temperature, &
                      user_boundary_emissivity
      IF ( io_status /= 0 ) GOTO 1000

    END IF


    ! ------------------------------------
    ! LBLATM routine switches (record 3.1)
    ! ------------------------------------

    error_status = Write_Record_3p1( file_id, &
                                     LBLATM_Flags, &
                                     altitude_flag = altitude_flag, &
                                     message_log   = message_log )

    IF ( error_status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME,    &
                            'Error writing Record 3.1 to'//TRIM( tape5_filename ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF


    ! ----------------------------------
    ! Slant path parameters (record 3.2)
    ! ----------------------------------

    error_status = Write_Record_3p2( file_id,                  &
                                     user_observer_pressure,   &
                                     user_boundary_pressure,   &
                                     user_zenith_angle,        &
                                     altitude_flag = altitude_flag, &
                                     placeholder   = placeholder, &
                                     message_log   = message_log )
                                     
    IF ( error_status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME,    &
                            'Error writing Record 3.2 to'//TRIM( tape5_filename ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF


    ! --------------------------------
    ! Calculation levels (record 3.3B)
    ! --------------------------------

    error_status = Write_Record_3p3B( file_id, &
                                      user_layer_boundaries( 1:LBLATM_Flags%n_layer_boundaries ), &
                                      altitude_flag = altitude_flag, &
                                      message_log   = message_log )

    IF ( error_status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME,    &
                            'Error writing Record 3.3B to'//TRIM( tape5_filename ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF


    ! -------------------------------------------
    ! Output Records 3.4 - 3.6 (the profile data)
    ! -------------------------------------------

    error_status = Write_Record_3p4_to_3p6 ( file_id,                  &  ! Input
                                             LBLATM_Flags%n_molecules, &  ! Input
                                             user_climatology_model,   &  ! Input
                                             pressure,                 &  ! Input
                                             temperature,              &  ! Input
                                             absorber_amount,          &  ! Input
                                             absorber_id,              &  ! Input
                                             absorber_units,           &  ! Input
                                             surface_altitude,         &  ! Input

                                             absorber_format = absorber_format, &  ! Optional input
                                             altitude_flag   = altitude_flag,   &  ! Optional input
                                             message_log     = message_log )       ! Error messaging

    IF ( error_status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME,    &
                            'Error writing Records 3.4-3.6 to'//TRIM( tape5_filename ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- OUTPUT XSECTION DATA IF REQUIRED --                   #
    !#--------------------------------------------------------------------------#

    IF ( user_calculation_flags%xsection == 1 ) THEN

      error_status = Write_Record_3p7_to_3p8 ( file_id,           &  ! Input
                                               xs_profile_flag,   &  ! Input
                                               xsection_name,     &  ! Input

                                               xsection_pressure = xsection_pressure, &  ! Optional input
                                               xsection_amount   = xsection_amount,   &  ! Optional input
                                               xsection_units    = xsection_units,    &  ! Optional input
                                               message_log       = message_log )         ! Error messaging


      IF ( error_status /= SUCCESS ) THEN
        CALL display_message( ROUTINE_NAME,    &
                              'Error writing Records 3.7-3.8 to'//TRIM( tape5_filename ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- OUTPUT TERMINATION CHARACTER  --                    #
    !#--------------------------------------------------------------------------#

    IF ( terminator ) THEN
      record_number = 'Terminator'
      WRITE( file_id, FMT    = '( "%" )', &
                      IOSTAT = io_status  )
      IF ( io_status /= 0 ) GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CLOSE THE FILE  --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( file_id, STATUS = 'KEEP',   &
                    IOSTAT = io_status )

    IF ( io_status /= 0 ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error closing TAPE5 file. IOSTAT = ", i5 )' ) &
                      io_status
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                              -- DONE  --                                 #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS
    RETURN



    !#--------------------------------------------------------------------------#
    !#                      -- FILE IO ERROR HANDLING  --                       #
    !#--------------------------------------------------------------------------#

    1000 CONTINUE


    ! ---------------------------
    ! Construct I/O error message
    ! ---------------------------

    WRITE( message, '( "Error writing record ", a, " to file ", a, &
                      &". IOSTAT = ", i5 )' ) &
                    TRIM( record_number ), TRIM( tape5_filename ), io_status


    ! ----------------------------------------
    ! Output message and return with fail code
    ! ----------------------------------------

    error_status = FAILURE
    CALL display_message( ROUTINE_NAME,    &
                          TRIM( message ), &
                          error_status,    &
                          message_log = message_log )


  END FUNCTION create_MONORTM_TAPE5















  FUNCTION Check_Absorber_Units( absorber_units,  &  ! Input
                                 absorber_number, &  ! Input
                                 message_log )    &  ! Error messaging
                               RESULT ( error_status )


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( 1 ), DIMENSION( : ), INTENT( IN ) :: absorber_units
    INTEGER,        DIMENSION( : ), INTENT( IN ) :: absorber_number

    ! -- Error handling filename
    CHARACTER( * ),       OPTIONAL, INTENT( IN ) :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_Absorber_Units'

    ! -- Absorber unit check string
    CHARACTER( * ), PARAMETER :: VALID_ABSORBER_UNITS = 'ABCDEFGH123456'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: j, n_absorbers
    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- CHECK THE ABSORBER UNIT FLAGS --                     #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Get the numbner of absorbers
    ! ----------------------------

    n_absorbers = SIZE( absorber_units )


    ! -------------------
    ! Loop over absorbers
    ! -------------------

    j_absorber_loop: DO j = 1, n_absorbers


      ! -------------------------------------
      ! Find position of specified units flag
      ! -------------------------------------

      n = INDEX( VALID_ABSORBER_UNITS, strupcase( absorber_units( j ) ) )


      ! ----------------------------------
      ! Check if unit flag is at all valid
      ! ----------------------------------

      check_for_valid_flag: IF ( n /= 0 ) THEN

        ! -- Check if unit flag is valid for molecule type
        IF ( ( strupcase( absorber_units( j ) ) == 'F' .OR. &                  ! DP in K, H2O only
               strupcase( absorber_units( j ) ) == 'G' .OR. &                  ! DP in C, H2O only
               strupcase( absorber_units( j ) ) == 'H'      ) .AND. &          ! RH in %, H2O only
             ( absorber_number( j ) /= 1 )                          ) THEN     ! Is absorber H2O?

          error_status = FAILURE
          WRITE( message, '( "H2O absorber unit, ", a, &
                            &", specified for input absorber no. ", i2, "(", a, ")" )' ) &
                          absorber_units( j ), j, TRIM( ABSORBER_NAME( absorber_number( j ) ) )
          CALL display_message( ROUTINE_NAME, &
                                TRIM( message ), &
                                error_status, &
                                message_log = message_log )
          RETURN

        END IF

      ELSE

        ! -- Invalid units flag
        error_status = FAILURE
        WRITE( message, '( "Invalid absorber unit, ", a, &
                          &", specified for input absorber no. ", i2, "(", a, ")" )' ) &
                        absorber_units( j ), j, TRIM( ABSORBER_NAME( absorber_number( j ) ) )
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        RETURN

      END IF check_for_valid_flag

    END DO j_absorber_loop



    !#--------------------------------------------------------------------------#
    !#                        -- UNIT FLAGS ARE O.K. --                         #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION Check_Absorber_Units






  FUNCTION Check_Frequency( begin_frequency, &  ! Input
                            end_frequency,   &  ! Input
                            message_log )    &  ! Error messaging
                          RESULT ( error_status )


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ),          INTENT( IN ) :: begin_frequency
    REAL( fp_kind ),          INTENT( IN ) :: end_frequency

    ! -- Error handling filename
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_Frequency'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    REAL( fp_kind ) :: bandwidth



    !#--------------------------------------------------------------------------#
    !#                    -- CHECK THE FREQUENCY LIMITS --                      #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Calculate bandwidth
    ! -------------------

    bandwidth = end_frequency - begin_frequency


    ! -----------------------------------
    ! Check for zero or negative bandwith
    ! -----------------------------------

    IF ( bandwidth < TOLERANCE ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid input frequency range. V1 > V2 or V1 = V2.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -------------------------------
    ! Check for too large a bandwidth
    ! -------------------------------

    IF ( bandwidth > MAX_BANDWIDTH ) THEN
      error_status = FAILURE
      WRITE( message, '( "Invalid input frequency range. V2 - V1 > ", f6.1, "cm-1" )' ) &
                      MAX_BANDWIDTH
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! --------------------------------------
    ! Check for negative or too large inputs
    ! --------------------------------------

    IF ( begin_frequency < TOLERANCE .OR. &
         end_frequency   < TOLERANCE      ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid input frequencies. V1 and/or V2 <= 0.0cm-1', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    IF ( begin_frequency > MAX_FREQUENCY .OR. &
         end_frequency   > MAX_FREQUENCY      ) THEN
      error_status = FAILURE
      WRITE( message, '( "Invalid input frequency range. V1 and/or V2 > ", f7.1, "cm-1" )' ) &
                      MAX_FREQUENCY
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- FREQUENCIES ARE O.K. --                        #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION Check_Frequency






  FUNCTION Write_Record_1p2( file_id,           &  ! Input
                             Calculation_Flags, &  ! Input

                             message_log )      &  ! Error messaging
                           RESULT ( error_status )

    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                         INTENT( IN ) :: file_id
    TYPE( Calculation_Flags_type ),  INTENT( IN ) :: Calculation_Flags

    CHARACTER( * ),        OPTIONAL, INTENT( IN ) :: message_log


    ! ------
    ! Result
    ! ------

    INTEGER :: error_status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_Record_1p2'


    ! ---------
    ! Variables
    ! ---------

    CHARACTER( 256 ) :: message

    INTEGER :: io_status


    ! ---------------------------------
    ! Write the calculation flag record
    ! ---------------------------------

   ! WRITE( file_id, FMT    = '(" HI=",i1," F4=",i1," CN=",i1," AE=",i1," EM=",i1, &


    ! ----
    ! Done
    ! ----

    error_status = SUCCESS

  END FUNCTION Write_Record_1p2






  FUNCTION Write_Record_3p1( file_id,       &  ! Input
                             LBLATM_Flags,  &  ! Input

                             altitude_flag, &  ! Optional input
                             message_log )  &  ! Error messaging
                           RESULT ( error_status )

    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                    INTENT( IN ) :: file_id
    TYPE( LBLATM_Flags_type ),  INTENT( IN ) :: LBLATM_Flags

    INTEGER,          OPTIONAL, INTENT( IN ) :: altitude_flag
    CHARACTER( * ),   OPTIONAL, INTENT( IN ) :: message_log


    ! ------
    ! Result
    ! ------

    INTEGER :: error_status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_Record_3p1'


    ! ---------
    ! Variables
    ! ---------

    CHARACTER( 256 ) :: message

    TYPE( LBLATM_Flags_type ) :: LBLATM_Flags_copy
    INTEGER :: n_layer_boundaries
    INTEGER :: io_status


    ! ----------------------------------------------------
    ! Check if altitude keyword set. If not, the number of
    ! layer boundaries should be output as -ve to indicate
    ! boundaries are specified for pressure units
    ! ----------------------------------------------------

    LBLATM_Flags_copy  = LBLATM_Flags
    n_layer_boundaries = ABS( LBLATM_Flags_copy%n_layer_boundaries )
 
    ! -- Default is pressure boundary output....
    LBLATM_Flags_copy%n_layer_boundaries = -n_layer_boundaries
    ! -- .... unless the altitude_flag keyword is set
    IF ( PRESENT( altitude_flag ) ) THEN
      IF ( altitude_flag == SET ) THEN
        LBLATM_Flags_copy%n_layer_boundaries = n_layer_boundaries
      END IF
    END IF


    ! ----------------------------
    ! Write the LBLATM flag record
    ! ----------------------------

    WRITE( file_id, FMT    = '( 7( i5 ), i2, 1x, i2, 3( f10.3 ), f20.3 )', &
                    IOSTAT = io_status ) LBLATM_Flags_copy

    IF ( io_status /= 0 ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error writing Record 3.1. IOSTAT = ", i5 )' ) io_status
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      CLOSE( file_id )
      RETURN
    END IF


    ! ----
    ! Done
    ! ----

    error_status = SUCCESS

  END FUNCTION Write_Record_3p1






  FUNCTION Write_Record_3p2( file_id,        &  ! Input

                             observer_h1,    &  ! Input
                             endpoint_h2,    &  ! Input
                             zenith_angle,   &  ! Input

                             range_h1_to_h2, &  ! Optional input
                             beta_h1_to_h2,  &  ! Optional input
                             path_type,      &  ! Optional input
                             hobs,           &  ! Optional input

                             altitude_flag,  &  ! Optional input
                             Placeholder,    &  ! Optional input
                             message_log )   &  ! Error messaging
                           RESULT ( error_status )

    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                         INTENT( IN ) :: file_id

    REAL( fp_kind ),                 INTENT( IN ) :: observer_h1
    REAL( fp_kind ),                 INTENT( IN ) :: endpoint_h2
    REAL( fp_kind ),                 INTENT( IN ) :: zenith_angle

    REAL( fp_kind ),       OPTIONAL, INTENT( IN ) :: range_h1_to_h2
    REAL( fp_kind ),       OPTIONAL, INTENT( IN ) :: beta_h1_to_h2
    INTEGER,               OPTIONAL, INTENT( IN ) :: path_type
    REAL( fp_kind ),       OPTIONAL, INTENT( IN ) :: hobs

    INTEGER,               OPTIONAL, INTENT( IN ) :: altitude_flag
    INTEGER,               OPTIONAL, INTENT( IN ) :: Placeholder
    CHARACTER( * ),        OPTIONAL, INTENT( IN ) :: message_log


    ! ------
    ! Result
    ! ------

    INTEGER :: error_status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_Record_3p2'


    ! ---------
    ! Variables
    ! ---------

    CHARACTER( 256 ) :: message

    LOGICAL :: No_Placeholder
    LOGICAL :: pressure_output
    CHARACTER( 8 ) :: fmt
    INTEGER :: io_status


    ! --------------------------------
    ! Check if placeholder keyword set
    ! --------------------------------

    No_Placeholder = .TRUE.
    IF ( PRESENT( Placeholder ) ) THEN
      IF ( Placeholder == SET ) No_Placeholder = .FALSE.
    END IF


    ! -----------------------------------------------------------
    ! Check if altitude_flag keyword set. If so, layer boundaries
    ! are altitudes so variable format not required.
    ! -----------------------------------------------------------

    pressure_output = .TRUE.
    IF ( PRESENT( altitude_flag ) ) THEN
      IF ( altitude_flag == SET ) pressure_output = .FALSE.
    END IF


    ! ----------------------
    ! Output observer height
    ! ----------------------

    fmt = '(f10.3) '
    IF ( pressure_output ) THEN
      IF ( observer_h1 < PRESSURE_FMT_THRESHOLD ) fmt = '(es10.3)'
    END IF

    WRITE( file_id, FMT     = fmt,      &
                    ADVANCE = 'NO',     &
                    IOSTAT  = io_status ) observer_h1
    IF ( io_status /= 0 ) GOTO 1000


    ! -----------------------
    ! Output end-point height
    ! -----------------------

    fmt = '(f10.3) '
    IF ( pressure_output ) THEN
      IF ( endpoint_h2 < PRESSURE_FMT_THRESHOLD ) fmt = '(es10.3)'
    END IF

    WRITE( file_id, FMT     = fmt,      &
                    ADVANCE = 'NO',     &
                    IOSTAT  = io_status ) endpoint_h2
    IF ( io_status /= 0 ) GOTO 1000


    ! -------------------
    ! Output zenith angle
    ! -------------------

    fmt = '(f10.3) '
    IF ( No_Placeholder ) THEN
      WRITE( file_id, FMT     = fmt,      &
                      ADVANCE = 'NO',     &
                      IOSTAT  = io_status ) zenith_angle
    ELSE
      WRITE( file_id, FMT     = '( "   AAA.AAA" )', &
                      ADVANCE = 'NO', &
                      IOSTAT  = io_status )

    END IF

    IF ( io_status /= 0 ) GOTO 1000



    ! ------------------------------------------
    ! Output length of straight path from H1->H2
    ! ------------------------------------------

    IF ( PRESENT( range_h1_to_h2 ) ) THEN
      WRITE( file_id, FMT     = fmt,      &
                      ADVANCE = 'NO',     &
                      IOSTAT  = io_status ) range_h1_to_h2
    ELSE
      WRITE( file_id, FMT     = '( 10x )', &
                      ADVANCE = 'NO',      &
                      IOSTAT  = io_status  )
    END IF
    IF ( io_status /= 0 ) GOTO 1000


    ! --------------------------------------
    ! Output earth centred angle from H1->H2
    ! --------------------------------------

    IF ( PRESENT( beta_h1_to_h2 ) ) THEN
      WRITE( file_id, FMT     = fmt,      &
                      ADVANCE = 'NO',     &
                      IOSTAT  = io_status ) beta_h1_to_h2
    ELSE
      WRITE( file_id, FMT     = '( 10x )', &
                      ADVANCE = 'NO',      &
                      IOSTAT  = io_status  )
    END IF
    IF ( io_status /= 0 ) GOTO 1000


    ! ----------------
    ! Output path type
    ! ----------------

    IF ( PRESENT( path_type ) ) THEN
      WRITE( file_id, FMT     = '( i5, 5x )', &
                      ADVANCE = 'NO',         &
                      IOSTAT  = io_status     ) path_type
    ELSE
      WRITE( file_id, FMT     = '( 10x )', &
                      ADVANCE = 'NO',      &
                      IOSTAT  = io_status  )
    END IF
    IF ( io_status /= 0 ) GOTO 1000


    ! ---------------------------------------------------------------
    ! Output observer height for case where geometry goes above 120km
    ! ---------------------------------------------------------------

    IF ( PRESENT( hobs ) ) THEN
      WRITE( file_id, FMT     = fmt,      &
                      ADVANCE = 'YES',    &
                      IOSTAT  = io_status ) hobs
    ELSE
      WRITE( file_id, FMT     = '( 10x )', &
                      ADVANCE = 'YES',     &
                      IOSTAT  = io_status  )
    END IF
    IF ( io_status /= 0 ) GOTO 1000


    ! ----------------
    ! Successful write
    ! ----------------

    error_status = SUCCESS
    RETURN


    ! --------------
    ! Error occurred
    ! --------------

    1000 CONTINUE
    error_status = FAILURE
    WRITE( message, '( "Error writing Record 3.2. IOSTAT = ", i5 )' ) io_status
    CALL display_message( ROUTINE_NAME, &
                          TRIM( message ), &
                          error_status, &
                          message_log = message_log )
    CLOSE( file_id )

  END FUNCTION Write_Record_3p2






  FUNCTION Write_Record_3p3B ( file_id,          &  ! Input
                               layer_boundaries, &  ! Input
                               altitude_flag,    &  ! Optional input
                               message_log )     &  ! Error messaging
                             RESULT ( error_status )

    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                         INTENT( IN ) :: file_id
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN ) :: layer_boundaries
    INTEGER,               OPTIONAL, INTENT( IN ) :: altitude_flag
    CHARACTER( * ),        OPTIONAL, INTENT( IN ) :: message_log


    ! ------
    ! Result
    ! ------

    INTEGER :: error_status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_Record_3p3B'


    ! ---------
    ! Variables
    ! ---------

    CHARACTER( 256 ) :: message

    INTEGER :: n_layer_boundaries, k
    LOGICAL :: pressure_output
    CHARACTER( 8 ) :: fmt
    CHARACTER( 3 ) :: advance
    INTEGER :: io_status


    ! ------------------------------------------------------
    ! Check if altitude keyword set. If so, layer boundaries
    ! are altitudes so variable format not required.
    ! ------------------------------------------------------

    pressure_output = .TRUE.
    IF ( PRESENT( altitude_flag ) ) THEN
      IF ( altitude_flag == SET ) pressure_output = .FALSE.
    END IF


    ! --------------------------
    ! Number of layer boundaries
    ! --------------------------

    n_layer_boundaries = SIZE( layer_boundaries )


    ! --------------------------
    ! Loop over layer boundaries
    ! --------------------------

    DO k = 1, n_layer_boundaries


      ! --------------------------------------------------
      ! Set the default output format and carriage control
      ! --------------------------------------------------

      fmt     = '(f10.3) '
      advance = 'NO '

  
      ! -------------------------------------------------------------
      ! Change output format if boundaries are pressures and < 1.0hPa
      ! -------------------------------------------------------------

      IF ( pressure_output ) THEN
        IF ( layer_boundaries( k ) < PRESSURE_FMT_THRESHOLD ) fmt = '(es10.3)'
      END IF


      ! ------------------------
      ! Set the carriage control
      ! ------------------------

      IF ( MOD( k, 8 ) == 0 .OR. k == n_layer_boundaries ) advance = 'YES'


      ! --------------
      ! Write the data
      ! --------------

      WRITE( file_id, FMT     = fmt,      &
                      ADVANCE = advance,  &
                      IOSTAT  = io_status ) layer_boundaries( k )
      IF ( io_status /= 0 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Error writing layer boundary #", i3, 1x, &
                          &"in record 3.3B. IOSTAT = ", i5 )' ) k, io_status
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        CLOSE( file_id )
        RETURN
      END IF

    END DO


    ! ----
    ! Done
    ! ----

    error_status = SUCCESS

  END FUNCTION Write_Record_3p3B






  FUNCTION Write_Record_3p4_to_3p6 ( file_id,           &  ! Input
                                     n_molecules,       &  ! Input
                                     climatology_model, &  ! Input
                                     pressure,          &  ! Input
                                     temperature,       &  ! Input
                                     absorber_amount,   &  ! Input
                                     absorber_id,       &  ! Input
                                     absorber_units,    &  ! Input
                                     surface_altitude,  &  ! Input

                                     absorber_format,   &  ! Optional input
                                     altitude_flag,     &  ! Optional input
                                     altitude,          &  ! Optional input
                                     message_log )      &  ! Error messaging
                                   RESULT ( error_status )

    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                                      INTENT( IN ) :: file_id
    INTEGER,                                      INTENT( IN ) :: n_molecules
    INTEGER,                                      INTENT( IN ) :: climatology_model
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN ) :: pressure
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN ) :: temperature
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN ) :: absorber_amount
    INTEGER,                   DIMENSION( : ),    INTENT( IN ) :: absorber_id
    CHARACTER( LEN = 1 ),      DIMENSION( : ),    INTENT( IN ) :: absorber_units
    REAL( fp_kind ),                              INTENT( IN ) :: surface_altitude

    
    INTEGER,         OPTIONAL,                    INTENT( IN ) :: absorber_format
    INTEGER,         OPTIONAL,                    INTENT( IN ) :: altitude_flag
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),    INTENT( IN ) :: altitude
    CHARACTER( * ),  OPTIONAL,                    INTENT( IN ) :: message_log


    ! ------
    ! Result
    ! ------

    INTEGER :: error_status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_Record_3p4_to_3p6'


    ! ---------
    ! Variables
    ! ---------

    CHARACTER( 256 ) :: message

    INTEGER :: w, d
    CHARACTER( LEN = 1 ) :: absorber_format_type
    CHARACTER( LEN = 8 ) :: absorber_data_format

    LOGICAL :: pressure_output

    INTEGER :: n_levels, k, n_levels_copy
    INTEGER :: n_absorbers, i, j

    CHARACTER( LEN = 1 )               :: jchar
    CHARACTER( LEN = MAX_N_ABSORBERS ) :: jchar_profile

    CHARACTER( LEN = 6 ) :: pressure_data_format

    CHARACTER( LEN = 15 ), DIMENSION( MAX_N_ABSORBERS ) :: level_amount

    INTEGER :: io_status



    ! --------------------------------
    ! Set the required absorber format
    ! --------------------------------

    ! -- Default is es10.3 output....
    w = 10
    d =  3
    absorber_format_type  = ' '

    ! -- .... unless the ABSORBER_FORMAT keyword is set - then es15.8
    IF ( PRESENT( absorber_format ) ) THEN
      IF ( absorber_format == SET ) THEN
        w = 15
        d =  8
        absorber_format_type  = 'L'
      END IF
    END IF

    ! -- Write the format descriptor
    WRITE( absorber_data_format, '( "(es", i2, ".", i1, ")" )' ) w, d


    ! ------------------------------------------------------
    ! Check if altitude keyword set. If so then:
    ! -- Profile level altitudes must be supplied, and
    ! -- Profile layer boundaries are specified as altitudes
    !    so variable format not required.
    ! ------------------------------------------------------

    ! -- Default is pressure boundary output....
    pressure_output = .TRUE.

    ! -- .... unless the ALTITUDE keyword is set.
    IF ( PRESENT( altitude_flag ) ) THEN
      IF ( altitude_flag == SET ) THEN
        IF ( .NOT. PRESENT( altitude ) ) THEN
          error_status = FAILURE
          CALL display_message( ROUTINE_NAME, &
                                'Must supply ALTITUDE if ALTITUDE_FLAG set.', &
                                error_status, &
                                message_log = message_log )
          CLOSE( file_id )
          RETURN
        END IF

        pressure_output = .FALSE.

      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- GET DIMENSIONS --                            #
    !#--------------------------------------------------------------------------#

    n_levels    = SIZE( pressure )
    n_absorbers = SIZE( absorber_id )


    ! -----------------
    ! Output Record 3.4
    ! -----------------

    n_levels_copy = n_levels
    IF ( pressure_output ) n_levels_copy = -n_levels

    WRITE( file_id, FMT    = '( i5 )', &
                    IOSTAT = io_status ) n_levels_copy

    IF ( io_status /= 0 ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error writing Record 3.4', &
                            error_status, &
                            message_log = message_log )
      CLOSE( file_id )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                    -- CONSTRUCT THE JCHAR STRINGS --                     #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------------
    ! Construct the default string - all climatology
    ! ----------------------------------------------

    WRITE( jchar, '( i1 )' ) climatology_model
    jchar_profile = REPEAT( jchar, n_molecules )


    ! -------------------------------------
    ! Replace the supplied absorber's units
    ! -------------------------------------

    DO j = 1, n_absorbers

      i = absorber_id( j )
      jchar_profile( i:i ) = strupcase( absorber_units( j ) )

    END DO



    !#--------------------------------------------------------------------------#
    !#                     -- OUTPUT PROFILE RECORDS --                         #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! USER provided profile data
    ! --------------------------

    k_level_loop: DO k = 1, n_levels


      ! ------------------------------
      ! Set the variable output format
      ! ------------------------------

      pressure_data_format = 'f10.3 '
      IF ( pressure( k ) < PRESSURE_FMT_THRESHOLD ) THEN
        pressure_data_format = 'es10.3'
      END IF


      boundary_format: IF ( pressure_output ) THEN


        ! --------------------------------------------
        ! Write Record 3.5 in pressure boundary format
        ! --------------------------------------------

        IF ( k > 1 ) THEN
          WRITE( file_id, FMT    = '( 10x, '//&
                                   pressure_data_format//&
                                   ', f10.3, 5x, "AA", 1x, a, 1x, a )', &
                          IOSTAT = io_status ) pressure( k ), &
                                               temperature( k ), &
                                               absorber_format_type, &
                                               jchar_profile( 1:n_molecules )
        ELSE
          WRITE( file_id, FMT    = '( f10.3, '//&
                                   pressure_data_format//&
                                   ', f10.3, 5x, "AA", 1x, a, 1x, a )', &
                          IOSTAT = io_status ) surface_altitude, &
                                               pressure( k ), &
                                               temperature( k ), &
                                               absorber_format_type, &
                                               jchar_profile( 1:n_molecules )
        END IF

      ELSE ! boundary_format


        ! --------------------------------------------
        ! Write Record 3.5 in altitude boundary format
        ! --------------------------------------------

        WRITE( file_id, FMT    = '( f10.3, '//&
                                 pressure_data_format//&
                                 ', f10.3, 5x, "AA", 1x, a, 1x, a )', &
                        IOSTAT = io_status ) altitude( k ), &
                                             pressure( k ), &
                                             temperature( k ), &
                                             absorber_format_type, &
                                             jchar_profile( 1:n_molecules )

      END IF boundary_format

      IF ( io_status /= 0 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Error writing Record 3.5 at level #", i4, &
                          &". IOSTAT = ", i5 )' ) k, io_status
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        CLOSE( file_id )
        RETURN
      END IF


      ! -------------------------------------------
      ! Fill the current level output "data" vector
      ! -------------------------------------------

      ! -- First clear it
      level_amount = ' '

      ! -- Loop over supplied absorbers
      DO j = 1, n_absorbers

        i = absorber_id( j )
        WRITE( level_amount( i ), FMT = absorber_data_format ) absorber_amount( k, j )

      END DO


      ! ---------------------------------------
      ! Write Record 3.6, 8 molecules at a time
      ! ---------------------------------------

      WRITE( file_id, FMT    = '( 8( a, : ) )', &
                      IOSTAT = io_status        ) ( level_amount( i )(1:w), i = 1, n_molecules )

      IF ( io_status /= 0 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Error writing Record 3.6 at level #", i4, &
                          &". IOSTAT = ", i5 )' ) k, io_status
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        CLOSE( file_id )
        RETURN
      END IF

    END DO k_level_loop



    !#--------------------------------------------------------------------------#
    !#                                -- DONE --                                #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION Write_Record_3p4_to_3p6






  FUNCTION Write_Record_3p7_to_3p8 ( file_id,           &  ! Input
                                     xs_profile_flag,   &  ! Input
                                     xsection_name,     &  ! Input

                                     xsection_pressure, &  ! Optional input
                                     xsection_amount,   &  ! Optional input
                                     xsection_units,    &  ! Optional input
                                     message_log )      &  ! Error messaging
                                   RESULT ( error_status )

    ! ---------
    ! Arguments
    ! ---------

    INTEGER,                                      INTENT( IN ) :: file_id
    INTEGER,                                      INTENT( IN ) :: xs_profile_flag
    CHARACTER( 10 ),           DIMENSION( : ),    INTENT( IN ) :: xsection_name

    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),    INTENT( IN ) :: xsection_pressure
    REAL( fp_kind ), OPTIONAL, DIMENSION( :, : ), INTENT( IN ) :: xsection_amount
    CHARACTER( 1 ),  OPTIONAL, DIMENSION( : ),    INTENT( IN ) :: xsection_units
    CHARACTER( * ),  OPTIONAL,                    INTENT( IN ) :: message_log


    ! ------
    ! Result
    ! ------

    INTEGER :: error_status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_Record_3p7_to_3p8'


    ! ---------
    ! Variables
    ! ---------

    CHARACTER( 256 ) :: message

    LOGICAL :: pressure_output

    INTEGER :: xs_convolution_flag
    INTEGER :: n_xs_absorbers, j
    INTEGER :: n_xs_levels, k
    INTEGER :: xs_boundary_flag

    CHARACTER( LEN = 10 ), DIMENSION( MAX_N_ABSORBERS ) :: level_amount

    INTEGER :: io_status

    CHARACTER( 10 ), DIMENSION( SIZE( xsection_name ) ) :: xsection_name_UPCASE



    !#--------------------------------------------------------------------------#
    !#                        -- OUTPUT STANDARD HEADER --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Pressure convolution of X-sections.
    ! This is not currently selectable
    ! -----------------------------------

    xs_convolution_flag = 0


    ! -------------------------------------
    ! Set the number of X-section absorbers
    ! -------------------------------------

    n_xs_absorbers = SIZE( xsection_name )


    ! -----------------
    ! Output Record 3.7
    ! -----------------

    WRITE( file_id, FMT    = '( 3i5 )', &
                    IOSTAT = io_status ) n_xs_absorbers, &
                                         xs_profile_flag, &
                                         xs_convolution_flag

    IF ( io_status /= 0 ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error writing Record 3.7', &
                            error_status, &
                            message_log = message_log )
      CLOSE( file_id )
      RETURN
    END IF


    ! -------------------
    ! Output Record 3.7.1
    ! -------------------

    ! -- Ensure molecule names are all upper case
    DO j = 1, n_xs_absorbers
      xsection_name_UPCASE( j ) = strupcase( xsection_name( j ) )
    END DO

    WRITE( file_id, FMT    = '( 7( a, : ), (/, 8(a, : ) ) )', &
                    IOSTAT = io_status ) xsection_name_UPCASE
    
    IF ( io_status /= 0 ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error writing Record 3.7.1', &
                            error_status, &
                            message_log = message_log )
      CLOSE( file_id )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- OUTPUT PROFILE DATA --                         #
    !#--------------------------------------------------------------------------#

    xs_user_profile: IF ( xs_profile_flag == 0 ) THEN


      ! ------------------------------
      ! Determine the number of levels
      ! ------------------------------

      n_xs_levels = SIZE( xsection_pressure )


      ! --------------------------------------
      ! Set the boundary type flag to pressure
      ! This is not currently selectable
      ! --------------------------------------

      xs_boundary_flag = 1


      ! ----------------
      ! Write Record 3.8
      ! ----------------

      WRITE( file_id, FMT = '( 2i5 )', &
                      IOSTAT = io_status ) n_xs_levels, xs_boundary_flag

      IF ( io_status /= 0 ) THEN
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Error writing Record 3.8', &
                              error_status, &
                              message_log = message_log )
        CLOSE( file_id )
        RETURN
      END IF


      ! ----------------
      ! Begin level loop
      ! ----------------

      k_level_loop: DO k = 1, n_xs_levels


        ! ------------------
        ! Write Record 3.8.1
        ! ------------------

        WRITE( file_id, FMT = '( f10.3, 5x, 35( a, : ) )', &
                        IOSTAT = io_status ) xsection_pressure( k ), &
                                             xsection_units( 1:n_xs_absorbers )

        IF ( io_status /= 0 ) THEN
          error_status = FAILURE
          WRITE( message, '( "Error writing Record 3.8.1 at level #", i4, &
                            &". IOSTAT = ", i5 )' ) k, io_status
          CALL display_message( ROUTINE_NAME, &
                                TRIM( message ), &
                                error_status, &
                                message_log = message_log )
          CLOSE( file_id )
          RETURN
        END IF


        ! -------------------------------------------
        ! Fill the current level output "data" vector
        ! -------------------------------------------

        ! -- First clear it
        level_amount = ' '

        ! -- Loop over supplied absorbers.
        DO j = 1, n_xs_absorbers

          IF ( xsection_units( j ) /= '1' ) THEN
            WRITE( level_amount( j ), FMT = '( es10.3 )' ) xsection_amount( k, j )
          END IF

        END DO


        ! -----------------------------------------------
        ! Write Record 3.8.2-3.8.N, 8 molecules at a time
        ! -----------------------------------------------

        WRITE( file_id, FMT    = '( 8( a, : ) )', &
                        IOSTAT = io_status        ) ( level_amount( j ), j = 1, n_xs_absorbers )

        IF ( io_status /= 0 ) THEN
          error_status = FAILURE
          WRITE( message, '( "Error writing Record 3.8.N at level #", i4, &
                            &". IOSTAT = ", i5 )' ) k, io_status
          CALL display_message( ROUTINE_NAME, &
                                TRIM( message ), &
                                error_status, &
                                message_log = message_log )
          CLOSE( file_id )
          RETURN
        END IF

      END DO k_level_loop

    END IF xs_user_profile


    !#--------------------------------------------------------------------------#
    !#                                -- DONE --                                #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION Write_Record_3p7_to_3p8


END MODULE MONORTM_Input


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/07/26 21:43:58 $
!
! $Revision: 655 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: MONORTM_Input.f90,v $
! Revision 2.7  2006/07/26 21:43:58  wd20pd
! Replacement of "Error_Handler" with "Message_Handler" in USE statements and
! in documentaiton blocks.
!
! Revision 2.6  2006/07/25 19:33:58  paulv
! - Updated to use new Utility modules.
! - Cosmetic changes to structure parameter declarations.
!
! Revision 2.5  2003/12/01 17:55:22  paulv
! - Added optional Placeholder argument to Write_Record_3p2() function call
!   to allow zenith angle specification to be replaced with "AAA.AAA" for
!   use in transmittance production.
!
! Revision 2.4  2003/07/21 21:08:49  paulv
! - Corrected a bug in the definition of the absorber names array. The number
!   of absorber name definitions was different from the specified dimension.
!   PGI compiler did not flag error, IBM compiler issued conformance error
!   message.
!
! Revision 2.3  2002/06/05 18:53:58  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
!
! Revision 2.2  2002/04/26 13:26:22  paulv
! - Changed default calculation flag values for CONTINUUM, EMIT, and MERGE_FLAG.
! - Added PLACEHOLDER and NO_TERMINATOR optional inputs.
!
! Revision 2.1  2002/04/24 22:33:09  paulv
! - New version.
! - Added derived types for the calculation and LBLATM flags.
! - Split out a lot of the record writes into separate functions.
! - Added X-section capability.
!
! Revision 1.2  2002/04/16 22:30:31  paulv
! - Update to synchronise repository.
!
! Revision 1.1  2002/04/16 18:51:54  paulv
! Initial checkin.
!
!
!
!
