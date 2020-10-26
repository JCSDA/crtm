

MODULE profile_conversion


  ! ------------
  ! Modules used
  ! ------------

  ! -- Define default floating point precision.
  USE type_kinds, ONLY: fp_kind

  ! -- Use (double precision) fundamental constants
  USE fundamental_constants, ONLY : NA => AVOGADRO_CONSTANT,    &
                                    R0 => MOLAR_GAS_CONSTANT,   &
                                    L0 => LOSCHMIDT_CONSTANT,   &
                                    P0 => STANDARD_ATMOSPHERE,  &
                                    T0 => STANDARD_TEMPERATURE, &
                                    G0 => STANDARD_GRAVITY,     &
                                    PI

  ! -- Module containing error code defintions and routines
  USE Message_handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------

  PRIVATE
  PUBLIC :: mw_air
  PUBLIC :: density
  PUBLIC :: svp_water
  PUBLIC :: svp_ice
  PUBLIC :: virtual_temperature
  PUBLIC :: saturation_mixing_ratio

  ! -- Conversion routines
  PUBLIC :: rh_to_mr,                   mr_to_rh
  PUBLIC :: mr_to_ppmv,                 ppmv_to_mr
  PUBLIC :: ppmv_to_pp,                 pp_to_ppmv
  PUBLIC :: pressure_to_number_density, number_density_to_pressure
  PUBLIC :: ppmv_to_number_density,     number_density_to_ppmv
  PUBLIC :: ppmv_to_kmol_per_cm2,       kmol_per_cm2_to_ppmv

  ! -- Level->layer routines
  PUBLIC :: effective_layer_tp
  PUBLIC :: geopotential_height
  PUBLIC :: create_sublevels
  PUBLIC :: integrate_sublevels


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE mw_air
    MODULE PROCEDURE mw_air_scalar
    MODULE PROCEDURE mw_air_rank1
  END INTERFACE ! mw_air

  INTERFACE density
    MODULE PROCEDURE density_scalar
    MODULE PROCEDURE density_rank1a   ! Scalar molecular weight
    MODULE PROCEDURE density_rank1b   ! Rank-1 molecular weight
  END INTERFACE ! density

  INTERFACE svp_water
    MODULE PROCEDURE svpw_scalar
    MODULE PROCEDURE svpw_rank1
  END INTERFACE ! svp_water

  INTERFACE svp_ice
    MODULE PROCEDURE svpi_scalar
    MODULE PROCEDURE svpi_rank1
  END INTERFACE ! svp_ice

  INTERFACE virtual_temperature
    MODULE PROCEDURE vt_scalar
    MODULE PROCEDURE vt_rank1
  END INTERFACE ! virtual_temperature

  INTERFACE saturation_mixing_ratio
    MODULE PROCEDURE smr_scalar
    MODULE PROCEDURE smr_rank1
  END INTERFACE ! saturation_mixing_ratio

  INTERFACE rh_to_mr
    MODULE PROCEDURE rh_to_mr_scalar
    MODULE PROCEDURE rh_to_mr_rank1
  END INTERFACE ! rh_to_mr

  INTERFACE mr_to_rh
    MODULE PROCEDURE mr_to_rh_scalar
    MODULE PROCEDURE mr_to_rh_rank1
  END INTERFACE ! mr_to_rh

  INTERFACE mr_to_ppmv
    MODULE PROCEDURE mr_to_ppmv_scalar
    MODULE PROCEDURE mr_to_ppmv_rank1
  END INTERFACE ! mr_to_ppmv

  INTERFACE ppmv_to_mr
    MODULE PROCEDURE ppmv_to_mr_scalar
    MODULE PROCEDURE ppmv_to_mr_rank1
  END INTERFACE ! ppmv_to_mr

  INTERFACE ppmv_to_pp
    MODULE PROCEDURE ppmv_to_pp_scalar
    MODULE PROCEDURE ppmv_to_pp_rank1
  END INTERFACE ! ppmv_to_pp

  INTERFACE pp_to_ppmv
    MODULE PROCEDURE pp_to_ppmv_scalar
    MODULE PROCEDURE pp_to_ppmv_rank1
  END INTERFACE ! pp_to_ppmv

  INTERFACE pressure_to_number_density
    MODULE PROCEDURE pp2nd_scalar
    MODULE PROCEDURE pp2nd_rank1
  END INTERFACE ! pressure_to_number_density

  INTERFACE number_density_to_pressure
    MODULE PROCEDURE nd2pp_scalar
    MODULE PROCEDURE nd2pp_rank1
  END INTERFACE ! number_density_to_pressure

  INTERFACE ppmv_to_number_density
    MODULE PROCEDURE ppmv2nd_scalar
    MODULE PROCEDURE ppmv2nd_rank1
  END INTERFACE ! ppmv_to_number_density

  INTERFACE number_density_to_ppmv
    MODULE PROCEDURE nd2ppmv_scalar
    MODULE PROCEDURE nd2ppmv_rank1
  END INTERFACE ! number_density_to_ppmv

  INTERFACE ppmv_to_kmol_per_cm2
    MODULE PROCEDURE ppmv2kmol_scalar
    MODULE PROCEDURE ppmv2kmol_rank1
  END INTERFACE ! ppmv_to_kmol_per_cm2

  INTERFACE kmol_per_cm2_to_ppmv
    MODULE PROCEDURE kmol2ppmv_scalar
    MODULE PROCEDURE kmol2ppmv_rank1
  END INTERFACE ! kmol_per_cm2_to_ppmv


  ! ----------------------
  ! Module wide parameters
  ! ----------------------

  ! -- Numerical constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: TOLERANCE = EPSILON( 0.0_fp_kind )
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO      = 0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE       = 1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TWO       = 2.0_fp_kind

  ! -- Molecular weights
  REAL( fp_kind ), PRIVATE, PARAMETER :: MW_H2O    = 18.01528_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: MW_DRYAIR = 28.9648_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: EPS       = MW_H2O / MW_DRYAIR

  ! -- Conversion factors
  REAL( fp_kind ), PRIVATE, PARAMETER :: CELSIUS_TO_KELVIN = T0
  REAL( fp_kind ), PRIVATE, PARAMETER :: G_TO_KG           = 1.0e-03_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: KG_TO_G           = 1.0e+03_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: PA_TO_HPA         = 1.0e-02_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: HPA_TO_PA         = 1.0e+02_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: PPMV_TO_PPV       = 1.0e-06_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: PPV_TO_PPMV       = 1.0e+06_fp_kind


  ! -- Water vapor unit specifiers
  INTEGER, PRIVATE, PARAMETER :: MR_H2O_FLAG   = 1  ! H2O units in g/kg
  INTEGER, PRIVATE, PARAMETER :: PPMV_H2O_FLAG = 2  ! H2O units in ppmv
  INTEGER, PRIVATE, PARAMETER :: PP_H2O_FLAG   = 3  ! H2O units in hPa  (partial pressure)


  ! ----------------
  ! Module variables
  ! ----------------

  CHARACTER( 128 ), PRIVATE :: message


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ANY, COS, EPSILON, EXP, LOG, PRESENT, SIN, SIZE, SQRT

CONTAINS






  FUNCTION mw_air_scalar ( pressure,          &  ! Input
                           water_vapor,       &  ! Input
                           water_vapor_units, &  ! Input
                           message_log )      &  ! Optional input
                         RESULT ( mw_air )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: water_vapor
    INTEGER,         INTENT( IN )           :: water_vapor_units

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: mw_air


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'MW_AIR'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: min_unit, max_unit

    REAL( fp_kind ) :: ppmv_h2o, pp_h2o
    REAL( fp_kind ) :: d_mw_air



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------
 
    IF ( pressure < TOLERANCE .OR. water_vapor < ZERO ) THEN
      mw_air = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressures/water vapor < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF


    !--------------------------------------------------------------------------
    !             -- Convert water vapor units to partial pressure --
    !--------------------------------------------------------------------------
 
    SELECT CASE ( water_vapor_units )


      ! --------------
      ! Units are g/kg
      ! --------------

      CASE ( MR_H2O_FLAG )

        ! -- g/kg -> ppmv
        ppmv_h2o = mr_to_ppmv_scalar( water_vapor, MW_H2O, &
                                      message_log = message_log )

        ! -- ppmv -> pp
        pp_h2o   = ppmv_to_pp_scalar( pressure, ppmv_h2o, &
                                      message_log = message_log )


      ! --------------
      ! Units are ppmv
      ! --------------

      CASE ( PPMV_H2O_FLAG )      

        ! -- ppmv -> pp
        pp_h2o = ppmv_to_pp_scalar( pressure, water_vapor, &
                                    message_log = message_log )


      ! --------------------------
      ! Units are partial pressure
      ! --------------------------

      CASE ( PP_H2O_FLAG )
        pp_h2o = water_vapor


      ! --------------------
      ! How did this happen?
      ! --------------------

      CASE DEFAULT
        mw_air = -ONE
        CALL display_message( ROUTINE_NAME, &
                              'Invalid WATER_VAPOR_UNITS flag specified.', &
                              FAILURE, &
                              message_log = message_log )
        RETURN

    END SELECT
    

 
    !--------------------------------------------------------------------------
    !              -- Calculate effective molecular weight of air --
    !--------------------------------------------------------------------------
 
    ! ------------------------------------------------------------
    ! Calculate change to air molecular weight due to water vapour
    ! ------------------------------------------------------------

    d_mw_air = pp_h2o * ( MW_H2O - MW_DRYAIR ) / &
    !          -------------------------------
                        pressure


    ! ------------------------------
    ! Calculate air molecular weight
    ! ------------------------------

    mw_air = MW_DRYAIR + d_mw_air

  END FUNCTION mw_air_scalar




  FUNCTION mw_air_rank1 ( pressure,          &  ! Input
                          water_vapor,       &  ! Input
                          water_vapor_units, &  ! Input
                          message_log )      &  ! Optional input
                        RESULT ( mw_air )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: water_vapor
    INTEGER,         INTENT( IN )                 :: water_vapor_units

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: mw_air


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'MW_AIR'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_levels



    !---------------------------------------------------------------------------
    !                      -- Check size of input arrays --
    !---------------------------------------------------------------------------

    n_levels = SIZE( pressure )

    IF ( SIZE( water_vapor ) /= n_levels ) THEN
      mw_air = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !           -- Loop over levels and calculate the mw_air profile --
    !--------------------------------------------------------------------------
 
    DO i = 1, n_levels

      mw_air( i ) = mw_air_scalar( pressure( i ),            &
                                   water_vapor( i ),         &
                                   water_vapor_units,        &
                                   message_log = message_log )
      IF ( mw_air( i ) < ZERO ) RETURN

    END DO

  END FUNCTION mw_air_rank1







  FUNCTION density_scalar ( pressure,         &  ! Input
                            temperature,      &  ! Input
                            molecular_weight, &  ! Input
                            message_log )     &  ! Optional input
                         RESULT ( rho )



    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: temperature
    REAL( fp_kind ), INTENT( IN )           :: molecular_weight

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: rho


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'DENSITY'

    REAL( fp_kind ), PARAMETER :: SCALE_FACTOR = 0.1_fp_kind



    !--------------------------------------------------------------------------
    !                       -- Check input values --
    !--------------------------------------------------------------------------
 
    IF ( pressure         < TOLERANCE .OR. &
         temperature      < TOLERANCE .OR. &
         molecular_weight < TOLERANCE ) THEN
      rho = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressures/temperature/MW < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                       -- Calculate density --
    !
    ! Note that the universal gas constant, R0, is a module wide parameter.
    !--------------------------------------------------------------------------

    rho = SCALE_FACTOR * pressure * molecular_weight / &
    !                    ---------------------------
                             ( R0 * temperature ) 

  END FUNCTION density_scalar




  FUNCTION density_rank1a ( pressure,         &  ! Input
                            temperature,      &  ! Input
                            molecular_weight, &  ! Input
                            message_log )     &  ! Optional input
                          RESULT ( rho )



    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature
    REAL( fp_kind ), INTENT( IN )                 :: molecular_weight

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: rho


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'DENSITY'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_levels



    !--------------------------------------------------------------------------
    !                       -- Check input values --
    !--------------------------------------------------------------------------
 
    n_levels = SIZE( pressure )

    IF ( SIZE( temperature ) /= n_levels ) THEN
      rho = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input pressure/temperature array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                       -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, n_levels

      rho( i ) = density_scalar( pressure( i ), &
                                 temperature( i ), &
                                 molecular_weight, &
                                 message_log = message_log )

      IF ( rho( i ) < ZERO ) RETURN

    END DO

  END FUNCTION density_rank1a




  FUNCTION density_rank1b ( pressure,         &  ! Input
                            temperature,      &  ! Input
                            molecular_weight, &  ! Input
                            message_log )     &  ! Optional input
                          RESULT ( rho )



    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: molecular_weight

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: rho


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'DENSITY'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_levels



    !--------------------------------------------------------------------------
    !                       -- Check input values --
    !--------------------------------------------------------------------------
 
    n_levels = SIZE( pressure )

    IF ( SIZE( temperature      ) /= n_levels .OR. &
         SIZE( molecular_weight ) /= n_levels ) THEN
      rho = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                       -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, n_levels

      rho( i ) = density_scalar( pressure( i ), &
                                 temperature( i ), &
                                 molecular_weight( i ), &
                                 message_log = message_log )

      IF ( rho( i ) < ZERO ) RETURN

    END DO

  END FUNCTION density_rank1b











  FUNCTION svpw_scalar ( temperature,   &  ! Input
                         message_log  ) &  ! Optional input
                       RESULT ( svp )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: temperature

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: svp


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SVP_WATER'

    ! -- Coefficient data
    INTEGER, PARAMETER :: N_COEFFICIENTS = 8
    REAL( fp_kind ), PARAMETER, DIMENSION( 0:N_COEFFICIENTS ) :: COEFFICIENTS = &
      (/ 6.11583699e+00_fp_kind, 4.44606896e-01_fp_kind, 1.43177157e-02_fp_kind, &
         2.64224321e-04_fp_kind, 2.99291081e-06_fp_kind, 2.03154182e-08_fp_kind, &
         7.02620698e-11_fp_kind, 3.79534310e-14_fp_kind,-3.21582393e-16_fp_kind /)

    ! -- Valid temperature range
    REAL( fp_kind ), PARAMETER :: MIN_TEMPERATURE = 188.15_fp_kind
    REAL( fp_kind ), PARAMETER :: MAX_TEMPERATURE = 343.15_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i

    REAL( fp_kind ) :: t_celsius



    !--------------------------------------------------------------------------
    !                     -- Check input temperature range --
    !--------------------------------------------------------------------------
 
    IF ( temperature < MIN_TEMPERATURE .OR. &
         temperature > MAX_TEMPERATURE ) THEN
      WRITE( message, '( "Input temperature ", f6.2, &
                        &" outside valid range: ", &
                        &f6.2, "K < T < ", f6.2, "K" )' ) &
                      temperature, MIN_TEMPERATURE, MAX_TEMPERATURE
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            WARNING, &
                            message_log = message_log )
    ENDIF



    !--------------------------------------------------------------------------
    !                  -- Calculate saturation vapor pressure --
    !--------------------------------------------------------------------------

    t_celsius = temperature - CELSIUS_TO_KELVIN
    svp       = COEFFICIENTS( 0 )

    DO i = 1, N_COEFFICIENTS

      svp = svp + ( COEFFICIENTS(i) * ( t_celsius**i ) )

    END DO
 
  END FUNCTION svpw_scalar




  FUNCTION svpw_rank1 ( temperature,   &  ! Input
                        message_log  ) &  ! Optional input
                      RESULT ( svp )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( temperature ) ) :: svp


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SVP_WATER'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i



    !--------------------------------------------------------------------------
    !           -- Loop over levels and calculate the mw_air profile --
    !--------------------------------------------------------------------------
 
    DO i = 1, SIZE( temperature )

      svp( i ) = svpw_scalar( temperature( i ), &
                              message_log = message_log )
      IF ( svp( i ) < ZERO ) RETURN

    END DO

  END FUNCTION svpw_rank1








  FUNCTION svpi_scalar ( temperature,   &  ! Input
                         message_log  ) &  ! Optional input
                       RESULT ( svp )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )            :: temperature

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL :: message_log


    ! ------
    ! Result
    ! ------
 
    REAL( fp_kind ) :: svp


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SVP_ICE'

    ! -- Coefficient data
    INTEGER, PARAMETER :: N_COEFFICIENTS = 8
    REAL( fp_kind ), PARAMETER, DIMENSION( 0:N_COEFFICIENTS ) :: COEFFICIENTS = &
      (/ 6.09868993e+00_fp_kind, 4.99320233e-01_fp_kind, 1.84672631e-02_fp_kind, &
         4.02737184e-04_fp_kind, 5.65392987e-06_fp_kind, 5.21693933e-08_fp_kind, &
         3.07839583e-10_fp_kind, 1.05785160e-12_fp_kind, 1.61444444e-15_fp_kind /)

    ! -- Valid temperature range
    REAL( fp_kind ), PARAMETER :: MIN_TEMPERATURE = 183.15_fp_kind
    REAL( fp_kind ), PARAMETER :: MAX_TEMPERATURE = 273.15_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 128 ) :: message

    INTEGER :: i

    REAL( fp_kind ) :: t_celsius



    !--------------------------------------------------------------------------
    !                     -- Check input temperature range --
    !--------------------------------------------------------------------------
 
    IF ( temperature < MIN_TEMPERATURE .OR. &
         temperature > MAX_TEMPERATURE ) THEN
      WRITE( message, '( "Input temperature ", f6.2, &
                        &" outside valid range: ", &
                        &f6.2, "K < T < ", f6.2, "K" )' ) &
                      temperature, MIN_TEMPERATURE, MAX_TEMPERATURE
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            WARNING, &
                            message_log = message_log )
    ENDIF



    !--------------------------------------------------------------------------
    !                  -- Calculate saturation vapor pressure --
    !--------------------------------------------------------------------------

    t_celsius = temperature - CELSIUS_TO_KELVIN
    svp       = COEFFICIENTS( 0 )

    DO i = 1, N_COEFFICIENTS

      svp = svp + ( COEFFICIENTS(i) * ( t_celsius**i ) )

    END DO
 
  END FUNCTION svpi_scalar




  FUNCTION svpi_rank1 ( temperature,   &  ! Input
                        message_log  ) &  ! Optional input
                      RESULT ( svp )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ------
    ! Result
    ! ------
 
    REAL( fp_kind ), DIMENSION( SIZE( temperature ) ) :: svp


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SVP_ICE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i



    !--------------------------------------------------------------------------
    !           -- Loop over levels and calculate the mw_air profile --
    !--------------------------------------------------------------------------
 
    DO i = 1, SIZE( temperature )

      svp( i ) = svpi_scalar( temperature( i ),            &
                              message_log = message_log )
      IF ( svp( i ) < ZERO ) RETURN

    END DO

  END FUNCTION svpi_rank1






  FUNCTION vt_scalar ( temperature,       &  ! Input
                       water_vapor,       &  ! Input
                       water_vapor_units, &  ! Input
                       pressure,          &  ! Optional input
                       message_log )      &  ! Optional input
                     RESULT ( vt )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: temperature
    REAL( fp_kind ), INTENT( IN )           :: water_vapor
    INTEGER,         INTENT( IN )           :: water_vapor_units
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: pressure

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: vt


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'VIRTUAL_TEMPERATURE'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: mr_h2o, ppmv_h2o



    !--------------------------------------------------------------------------
    !                          -- Check input --
    !--------------------------------------------------------------------------
 
    ! ------------------
    ! Check input values
    ! ------------------

    IF ( temperature < TOLERANCE .OR. water_vapor < ZERO ) THEN
      vt = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input temperature/water vapor < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !        -- Convert water vapor units to mixing ratio g/g (or kg/kg) --
    !--------------------------------------------------------------------------
 
    SELECT CASE ( water_vapor_units )


      ! --------------
      ! Units are g/kg
      ! --------------

      CASE ( MR_H2O_FLAG )

        ! -- Scale g/kg -> kg/kg
        mr_h2o = G_TO_KG * water_vapor


      ! --------------
      ! Units are ppmv
      ! --------------

      CASE ( PPMV_H2O_FLAG )      

        ! -- ppmv -> kg/kg
        mr_h2o = G_TO_KG * ppmv_to_mr_scalar( water_vapor, MW_H2O, &
                                              message_log = message_log )


      ! --------------------------
      ! Units are partial pressure
      ! --------------------------

      CASE ( PP_H2O_FLAG )

        ! -- Check that the total pressure argument was passed
        IF ( .NOT. PRESENT( pressure ) ) THEN
          vt = -ONE
          CALL display_message( ROUTINE_NAME, &
                                'Must supply total pressure if input water '//&
                                'vapor units are hPa (partial pressure).', &
                                FAILURE, &
                                message_log = message_log )
          RETURN
        END IF

        ! -- pp -> ppmv
        ppmv_h2o = pp_to_ppmv_scalar( pressure, water_vapor, &
                                      message_log = message_log )

        ! -- ppmv -> kg/kg
        mr_h2o = G_TO_KG * ppmv_to_mr_scalar( water_vapor, MW_H2O, &
                                              message_log = message_log )


      ! ------------------
      ! Invalid units flag
      ! ------------------

      CASE DEFAULT
        vt = -ONE
        CALL display_message( ROUTINE_NAME, &
                              'Invalid WATER_VAPOR_UNITS flag specified.', &
                              FAILURE, &
                              message_log = message_log )
        RETURN

    END SELECT


    ! ---------------------------
    ! Cechk result of conversions
    ! ---------------------------

    IF ( mr_h2o < ZERO ) THEN    
      vt = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Error converting water vapor units to g/kg', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    END IF



    !--------------------------------------------------------------------------
    !                -- Calculate virtual temperature --
    !--------------------------------------------------------------------------

    vt = temperature *      ( EPS + mr_h2o )      / &
    !                  --------------------------
                       ( EPS * ( ONE + mr_h2o ) )


  END FUNCTION vt_scalar 




  FUNCTION vt_rank1 ( temperature,       &  ! Input
                      water_vapor,       &  ! Input
                      water_vapor_units, &  ! Input
                      pressure,          &  ! Optional input
                      message_log )      &  ! Optional input
                    RESULT ( vt )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : )           :: temperature
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : )           :: water_vapor
    INTEGER,         INTENT( IN )                           :: water_vapor_units
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ), OPTIONAL :: pressure

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),                 OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( temperature ) ) :: vt


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'VIRTUAL_TEMPERATURE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_levels




    !--------------------------------------------------------------------------
    !                          -- Check input --
    !--------------------------------------------------------------------------
 
    n_levels = SIZE( temperature )


    ! -----------------------
    ! Check input array sizes
    ! -----------------------

    IF ( SIZE( water_vapor ) /= n_levels ) THEN
      vt = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input temperature/water_vapor array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ------------------------------
    ! Check if pressure array needed
    ! ------------------------------

    IF ( water_vapor_units == PP_H2O_FLAG ) THEN

      ! -- Yes. If present, check size
      IF ( PRESENT( pressure ) ) THEN

        IF ( SIZE( pressure ) /= n_levels ) THEN
          vt = -ONE
          CALL display_message( ROUTINE_NAME, &
                                'Inconsistent input temperature/pressure array sizes.', &
                                FAILURE, &
                                message_log = message_log )
          RETURN
        END IF

      ! -- No pressure array!
      ELSE
        vt = -ONE
        CALL display_message( ROUTINE_NAME, &
                              'Must supply total pressure if input water '//&
                              'vapor units are hPa (partial pressure).', &
                              FAILURE, &
                              message_log = message_log )
        RETURN
      END IF

    END IF



    !--------------------------------------------------------------------------
    !          -- Loop over levels and calculate virtual temperature --
    !--------------------------------------------------------------------------

    DO i = 1, n_levels

      vt( i ) = vt_scalar( temperature( i ),            &
                           water_vapor( i ),            &
                           water_vapor_units,           &
                           pressure    = pressure( i ), &
                           message_log = message_log    )
      IF ( vt( i ) < ZERO ) RETURN

    END DO

  END FUNCTION vt_rank1







  FUNCTION smr_scalar( pressure,        &  ! Input
                       temperature,     &  ! Input
                       ice_temperature, &  ! Optional Input
                       min_pressure,    &  ! Optional Input
                       message_log )    &  ! Optional Input
                     RESULT( smr )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: temperature
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: ice_temperature
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: min_pressure

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: smr


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SATURATION_MIXING_RATIO'

    ! -- Default minimum pressure is 50hPa
    REAL( fp_kind ), PARAMETER :: DEFAULT_MIN_PRESSURE = 50.0_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: use_ice_t

    REAL( fp_kind ) :: ice_t
    REAL( fp_kind ) :: min_p
    REAL( fp_kind ) :: svp
    REAL( fp_kind ) :: dp



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    ! ------------
    ! Check values
    ! ------------

    IF ( pressure     < TOLERANCE .OR. &
         temperature  < TOLERANCE ) THEN
      smr = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input argument values < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ------------------------
    ! Check optional arguments
    ! ------------------------

    IF ( PRESENT( min_pressure ) ) THEN
      min_p = min_pressure
    ELSE
      min_p = DEFAULT_MIN_PRESSURE
    END IF


    IF ( PRESENT( ice_temperature ) ) THEN
      ice_t     = ice_temperature
      use_ice_t = 1
    ELSE
      ice_t     = ZERO
      use_ice_t = 0
    END IF



    !--------------------------------------------------------------------------
    !           -- Only do calculations below the minimum pressure --
    !--------------------------------------------------------------------------

    pressure_check: IF ( pressure > min_p ) THEN


      ! -----------------------------------
      ! Calculate saturation vapor pressure
      ! -----------------------------------

      IF ( use_ice_t == 1 .AND. temperature < ice_t ) THEN

        ! -- Vapor pressure over ice if required
        svp = svpi_scalar( temperature, &
                           message_log = message_log )

      ELSE

        ! -- Otherwise, over water
        svp = svpw_scalar( temperature, &
                           message_log = message_log )

      END IF


      ! ---------------------------------------------
      ! Calculate saturation mixing ratio only if the
      ! total pressure is greater than the saturation
      ! vapor pressure.
      ! ---------------------------------------------

      dp = pressure - svp

      IF ( dp > ZERO ) THEN
        smr = KG_TO_G * EPS * svp / dp
      ELSE
        smr = ZERO
      END IF

    ELSE

      smr = ZERO

    END IF pressure_check


  END FUNCTION smr_scalar




  FUNCTION smr_rank1( pressure,        &  ! Input
                      temperature,     &  ! Input
                      ice_temperature, &  ! Optional Input
                      min_pressure,    &  ! Optional Input
                      message_log )    &  ! Optional Input
                    RESULT( smr )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature
    REAL( fp_kind ), INTENT( IN ), OPTIONAL       :: ice_temperature
    REAL( fp_kind ), INTENT( IN ), OPTIONAL       :: min_pressure

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log



    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: smr


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'SATURATION_MIXING_RATIO'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_levels




    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n_levels = SIZE( pressure )

    IF ( SIZE( temperature ) /=  n_levels ) THEN
      smr = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input pressure/temperature array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF


    !--------------------------------------------------------------------------
    !                        -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, n_levels

      smr( i ) = smr_scalar( pressure( i ), &
                             temperature( i ), &
                             ice_temperature = ice_temperature, &
                             min_pressure    = min_pressure, &
                             message_log     = message_log )
      IF ( smr( i ) < ZERO ) RETURN

    END DO

  END FUNCTION smr_rank1







  FUNCTION rh_to_mr_scalar( pressure,          &  ! Input
                            temperature,       &  ! Input
                            relative_humidity, &  ! Input
                            ice_temperature,   &  ! Optional Input
                            min_pressure,      &  ! Optional Input
                            message_log )      &  ! Optional Input
                          RESULT( mr )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: temperature
    REAL( fp_kind ), INTENT( IN )           :: relative_humidity
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: ice_temperature
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: min_pressure

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: mr


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'RH_TO_MR'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: smr



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( pressure          < TOLERANCE .OR. &
         temperature       < TOLERANCE .OR. &
         relative_humidity < ZERO      ) THEN
      mr = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input argument values < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !            -- Calculate saturation mixing ratio in g/kg --
    !--------------------------------------------------------------------------

    smr = saturation_mixing_ratio( pressure, &
                                   temperature, &
                                   ice_temperature = ice_temperature, &
                                   min_pressure    = min_pressure )

    IF ( smr < ZERO ) THEN
      mr = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Error calculating saturation mixing ratio.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !               -- Calculate mixing ratio in g/kg --
    !--------------------------------------------------------------------------

    mr = 0.01_fp_kind * relative_humidity * smr

  END FUNCTION rh_to_mr_scalar




  FUNCTION rh_to_mr_rank1( pressure,          &  ! Input
                           temperature,       &  ! Input
                           relative_humidity, &  ! Input
                           ice_temperature,   &  ! Optional Input
                           min_pressure,      &  ! Optional Input
                           message_log )      &  ! Optional Input
                         RESULT( mr )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: relative_humidity
    REAL( fp_kind ), INTENT( IN ), OPTIONAL       :: ice_temperature
    REAL( fp_kind ), INTENT( IN ), OPTIONAL       :: min_pressure

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: mr


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'RH_TO_MR'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_levels



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n_levels = SIZE( pressure )

    IF ( SIZE( temperature       ) /= n_levels .OR. & 
         SIZE( relative_humidity ) /= n_levels ) THEN
      mr = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                     -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, n_levels

      mr( i ) = rh_to_mr_scalar( pressure( i ), &
                                 temperature( i ), &
                                 relative_humidity( i ), &
                                 message_log = message_log )
      IF ( mr( i ) < ZERO ) RETURN

    END DO

  END FUNCTION rh_to_mr_rank1






  FUNCTION mr_to_rh_scalar( pressure,        &  ! Input
                            temperature,     &  ! Input
                            mixing_ratio,    &  ! Input
                            ice_temperature, &  ! Optional Input
                            min_pressure,    &  ! Optional Input
                            message_log )    &  ! Optional Input
                          RESULT( rh )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: temperature
    REAL( fp_kind ), INTENT( IN )           :: mixing_ratio
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: ice_temperature
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: min_pressure

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: rh


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'MR_TO_RH'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: smr



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( pressure     < TOLERANCE .OR. &
         temperature  < TOLERANCE .OR. &
         mixing_ratio < ZERO      ) THEN
      rh = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input argument values < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !            -- Calculate saturation mixing ratio in g/kg --
    !--------------------------------------------------------------------------

    smr = saturation_mixing_ratio( pressure, &
                                   temperature, &
                                   ice_temperature = ice_temperature, &
                                   min_pressure    = min_pressure )

    IF ( smr < ZERO ) THEN
      rh = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Error calculating saturation mixing ratio.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                 -- Calculate relative humidity in % --
    !--------------------------------------------------------------------------

    IF ( smr > ZERO ) THEN
      rh = 100.0_fp_kind * mixing_ratio / &
      !                    ------------
                               smr
    ELSE
      rh = ZERO
    END IF

  END FUNCTION mr_to_rh_scalar




  FUNCTION mr_to_rh_rank1( pressure,        &  ! Input
                           temperature,     &  ! Input
                           mixing_ratio,    &  ! Input
                           ice_temperature, &  ! Optional Input
                           min_pressure,    &  ! Optional Input
                           message_log )    &  ! Optional Input
                         RESULT( rh )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: mixing_ratio
    REAL( fp_kind ), INTENT( IN ), OPTIONAL       :: ice_temperature
    REAL( fp_kind ), INTENT( IN ), OPTIONAL       :: min_pressure

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: rh


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'MR_TO_RH'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_levels



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n_levels = SIZE( pressure )

    IF ( SIZE( temperature  ) /= n_levels .OR. & 
         SIZE( mixing_ratio ) /= n_levels ) THEN
      rh = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                     -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, n_levels

      rh( i ) = mr_to_rh_scalar( pressure( i ), &
                                 temperature( i ), &
                                 mixing_ratio( i ), &
                                 ice_temperature = ice_temperature, &
                                 min_pressure    = min_pressure, &
                                 message_log = message_log )
      IF ( rh( i ) < ZERO ) RETURN

    END DO

  END FUNCTION mr_to_rh_rank1







  FUNCTION mr_to_ppmv_scalar( mixing_ratio,     &  ! Input
                              molecular_weight, &  ! Input
                              message_log )     &  ! Optional input
                            RESULT( ppmv )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: mixing_ratio
    REAL( fp_kind ), INTENT( IN )           :: molecular_weight

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: ppmv


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'MR_TO_PPMV'

    REAL( fp_kind ), PARAMETER :: SCALE_FACTOR = 1.0e+03_fp_kind



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( mixing_ratio     < ZERO      .OR. &
         molecular_weight < TOLERANCE ) THEN
      ppmv = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input mixing ratio/molecular weight < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                    -- Convert ppmv to mixing_ratio --
    !--------------------------------------------------------------------------

    ppmv = SCALE_FACTOR * mixing_ratio * MW_DRYAIR / molecular_weight

  END FUNCTION mr_to_ppmv_scalar




  FUNCTION mr_to_ppmv_rank1( mixing_ratio,     &  ! Input
                             molecular_weight, &  ! Input
                             message_log )     &  ! Optional input
                           RESULT( ppmv )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: mixing_ratio
    REAL( fp_kind ), INTENT( IN )                 :: molecular_weight

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( mixing_ratio ) ) :: ppmv


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'MR_TO_PPMV'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i



    !--------------------------------------------------------------------------
    !                     -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, SIZE( mixing_ratio )

      ppmv( i ) = mr_to_ppmv_scalar( mixing_ratio( i ), &
                                     molecular_weight, &
                                     message_log = message_log )
      IF ( ppmv( i ) < ZERO ) RETURN

    END DO

  END FUNCTION mr_to_ppmv_rank1







  FUNCTION ppmv_to_mr_scalar( ppmv,             &  ! Input
                              molecular_weight, &  ! Input
                              message_log )     &  ! Optional input
                            RESULT( mr )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: ppmv
    REAL( fp_kind ), INTENT( IN )           :: molecular_weight

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: mr


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PPMV_TO_MR'

    REAL( fp_kind ), PARAMETER :: SCALE_FACTOR = 1.0e-03_fp_kind



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( ppmv             < ZERO      .OR. &
         molecular_weight < TOLERANCE ) THEN
      mr = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input mixing ratio/molecular weight < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                    -- Convert ppmv to mixing ratio --
    !--------------------------------------------------------------------------

    mr = SCALE_FACTOR * ppmv * molecular_weight / MW_DRYAIR

  END FUNCTION ppmv_to_mr_scalar




  FUNCTION ppmv_to_mr_rank1( ppmv,             &  ! Input
                             molecular_weight, &  ! Input
                             message_log )     &  ! Optional input
                           RESULT( mr )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: ppmv
    REAL( fp_kind ), INTENT( IN )                 :: molecular_weight

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( ppmv ) ) :: mr


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PPMV_TO_MR'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i



    !--------------------------------------------------------------------------
    !                     -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, SIZE( ppmv )

      mr( i ) = ppmv_to_mr_scalar( ppmv( i ), &
                                   molecular_weight, &
                                   message_log = message_log )
      IF ( mr( i ) < ZERO ) RETURN

    END DO

  END FUNCTION ppmv_to_mr_rank1







  FUNCTION ppmv_to_pp_scalar( pressure,     &  ! Input
                              ppmv,         &  ! Input
                              message_log ) &  ! Optional input
                            RESULT( pp )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: ppmv

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: pp


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PPMV_TO_PP'



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( pressure < TOLERANCE .OR. &
         ppmv     < ZERO      ) THEN
      pp = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressure/ppmv < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                   -- Convert ppmv to partial pressure --
    !--------------------------------------------------------------------------

    pp = PPMV_TO_PPV * ppmv * pressure

  END FUNCTION ppmv_to_pp_scalar




  FUNCTION ppmv_to_pp_rank1( pressure,     &  ! Input
                             ppmv,         &  ! Input
                             message_log ) &  ! Optional input
                           RESULT( pp )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: ppmv

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: pp


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PPMV_TO_PP'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_levels



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n_levels = SIZE( pressure )

    IF ( SIZE( ppmv ) /= n_levels ) THEN
      pp = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input pressure/ppmv array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                     -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, n_levels

      pp( i ) = ppmv_to_pp_scalar( pressure( i ), &
                                   ppmv( i ), &
                                   message_log = message_log )
      IF ( pp( i ) < ZERO ) RETURN

    END DO

  END FUNCTION ppmv_to_pp_rank1






  FUNCTION pp_to_ppmv_scalar( pressure,     &  ! Input
                              pp,           &  ! Input
                              message_log ) &  ! Optional input
                            RESULT( ppmv )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: pp

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: ppmv


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PP_TO_PPMV'



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( pressure < TOLERANCE .OR. &
         pp       < ZERO      ) THEN
      ppmv = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressure/pp < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                  -- Convert partial pressure to ppmv --
    !--------------------------------------------------------------------------

    ppmv = PPV_TO_PPMV * pp / pressure

  END FUNCTION pp_to_ppmv_scalar




  FUNCTION pp_to_ppmv_rank1( pressure,     &  ! Input
                             pp,           &  ! Input
                             message_log ) &  ! Optional input
                           RESULT( ppmv )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pp

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: ppmv


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PP_TO_PPMV'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_levels



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n_levels = SIZE( pressure )

    IF ( SIZE( pp ) /= n_levels ) THEN
      ppmv = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input pressure/pp array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                     -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, n_levels

      ppmv( i ) = pp_to_ppmv_scalar( pressure( i ), &
                                     pp( i ), &
                                     message_log = message_log )
      IF ( ppmv( i ) < ZERO ) RETURN

    END DO

  END FUNCTION pp_to_ppmv_rank1






  FUNCTION pp2nd_scalar( pressure,     &  ! Input
                         temperature,  &  ! Input
                         message_log ) &  ! Optional input
                       RESULT( number_density )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: temperature

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: number_density


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PRESSURE_TO_NUMBER_DENSITY'



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( pressure    < ZERO      .OR. &
         temperature < TOLERANCE      ) THEN
      number_density = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input partial pressure < 0, or temperature = 0.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !             -- Convert partial pressure to number density --
    !--------------------------------------------------------------------------

    number_density = HPA_TO_PA * pressure * L0 * T0 / ( temperature * P0 )

  END FUNCTION pp2nd_scalar




  FUNCTION pp2nd_rank1( pressure,     &  ! Input
                        temperature,  &  ! Input
                        message_log ) &  ! Optional input
                      RESULT( number_density )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: number_density


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PRESSURE_TO_NUMBER_DENSITY'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k, n



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n = SIZE( pressure )

    IF ( SIZE( temperature ) /= n ) THEN
      number_density = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input pressure/temperature array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !               -- Loop over elements of input arrays --
    !--------------------------------------------------------------------------

    DO k = 1, n

      number_density( k ) = pp2nd_scalar( pressure( k ), &
                                          temperature( k ), &
                                          message_log = message_log )
      IF ( number_density( k ) < ZERO ) RETURN

    END DO

  END FUNCTION pp2nd_rank1






  FUNCTION nd2pp_scalar( number_density, &  ! Input
                         temperature,    &  ! Input
                         message_log )   &  ! Optional input
                       RESULT( pressure )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: number_density
    REAL( fp_kind ), INTENT( IN )           :: temperature

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: pressure


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'NUMBER_DENSITY_TO_PRESSURE'



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( number_density < ZERO      .OR. &
         temperature    < TOLERANCE      ) THEN
      pressure = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input number density < 0, or temperature = 0.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                -- Convert number density to pressure --
    !--------------------------------------------------------------------------

    pressure = PA_TO_HPA * P0 * number_density * temperature / ( L0 * T0 )


  END FUNCTION nd2pp_scalar




  FUNCTION nd2pp_rank1( number_density, &  ! Input
                        temperature,    &  ! Input
                        message_log )   &  ! Optional input
                      RESULT( pressure )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: number_density
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( number_density ) ) :: pressure


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'NUMBER_DENSITY_TO_PRESSURE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k, n



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n = SIZE( number_density )

    IF ( SIZE( temperature ) /= n ) THEN
      pressure = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input number_density/temperature array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !               -- Loop over elements of input arrays --
    !--------------------------------------------------------------------------

    DO k = 1, n

      pressure( k ) = nd2pp_scalar( number_density( k ), &
                                    temperature( k ),    &
                                    message_log = message_log )
      IF ( pressure( k ) < ZERO ) RETURN

    END DO

  END FUNCTION nd2pp_rank1






  FUNCTION ppmv2nd_scalar( pressure,     &  ! Input
                           temperature,  &  ! Input
                           ppmv,         &  ! Input
                           message_log ) &  ! Optional input
                         RESULT( number_density )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: temperature
    REAL( fp_kind ), INTENT( IN )           :: ppmv

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: number_density


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PPMV_TO_NUMBER_DENSITY'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: total_density



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( pressure    < ZERO      .OR. &
         temperature < TOLERANCE .OR. &
         ppmv        < ZERO           ) THEN
      number_density = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressure,ppmv < 0, or temperature = 0.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                     -- Convert ppmv to number density --
    !--------------------------------------------------------------------------

    ! ----------------------------------
    ! Calculate total air number density
    ! ----------------------------------

    total_density = pp2nd_scalar( pressure, temperature,    &
                                  message_log = message_log )
    IF ( total_density < ZERO ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error calculating total number density.', &
                            FAILURE, &
                            message_log = message_log )
      number_density = -ONE
      RETURN
    END IF


    ! ------------------
    ! Convert ppmv -> nd
    ! ------------------

    number_density = PPMV_TO_PPV * ppmv * total_density

  END FUNCTION ppmv2nd_scalar




  FUNCTION ppmv2nd_rank1( pressure,     &  ! Input
                          temperature,  &  ! Input
                          ppmv,         &  ! Input
                          message_log ) &  ! Optional input
                        RESULT( number_density )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: ppmv

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: number_density


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PPMV_TO_NUMBER_DENSITY'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k, n



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n = SIZE( pressure )

    IF ( SIZE( temperature ) /= n .OR. &
         SIZE( ppmv        ) /= n      ) THEN
      number_density = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !               -- Loop over elements of input arrays --
    !--------------------------------------------------------------------------

    DO k = 1, n

      number_density( k ) = ppmv2nd_scalar( pressure( k ),    &
                                            temperature( k ), &
                                            ppmv( k ),        &
                                            message_log = message_log )
      IF ( number_density( k ) < ZERO ) RETURN

    END DO

  END FUNCTION ppmv2nd_rank1






  FUNCTION nd2ppmv_scalar( pressure,       &  ! Input
                           temperature,    &  ! Input
                           number_density, &  ! Input
                           message_log )   &  ! Optional input
                         RESULT( ppmv )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: temperature
    REAL( fp_kind ), INTENT( IN )           :: number_density

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: ppmv


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'NUMBER_DENSITY_TO_PPMV'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: total_density



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( pressure       < ZERO      .OR. &
         temperature    < TOLERANCE .OR. &
         number_density < ZERO           ) THEN
      ppmv = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressure, number density < 0, or temperature = 0.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                  -- Convert number density to ppmv --
    !--------------------------------------------------------------------------

    ! ----------------------------------
    ! Calculate total air number density
    ! ----------------------------------

    total_density = pp2nd_scalar( pressure, temperature,    &
                                  message_log = message_log )
    IF ( total_density < ZERO ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error calculating total number density.', &
                            FAILURE, &
                            message_log = message_log )
      ppmv = -ONE
      RETURN
    END IF


    ! ------------------
    ! Convert nd -> ppmv
    ! ------------------

    ppmv = PPV_TO_PPMV * number_density / total_density

  END FUNCTION nd2ppmv_scalar




  FUNCTION nd2ppmv_rank1( pressure,       &  ! Input
                          temperature,    &  ! Input
                          number_density, &  ! Input
                          message_log )   &  ! Optional input
                        RESULT( ppmv )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: number_density

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: ppmv


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'NUMBER_DENSITY_TO_PPMV'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k, n



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n = SIZE( pressure )

    IF ( SIZE( temperature    ) /= n .OR. &
         SIZE( number_density ) /= n      ) THEN
      ppmv = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF


    !--------------------------------------------------------------------------
    !               -- Loop over elements of input arrays --
    !--------------------------------------------------------------------------

    DO k = 1, n

      ppmv( k ) = nd2ppmv_scalar( pressure( k ),       &
                                  temperature( k ),    &
                                  number_density( k ), &
                                  message_log = message_log )
      IF ( ppmv( k ) < ZERO ) RETURN

    END DO

  END FUNCTION nd2ppmv_rank1






  FUNCTION ppmv2kmol_scalar( pressure,     &  ! Input
                             temperature,  &  ! Input
                             delta_z,      &  ! Input
                             ppmv,         &  ! Input
                             message_log ) &  ! Optional input
                           RESULT ( kmol_per_cm2 )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: temperature
    REAL( fp_kind ), INTENT( IN )           :: delta_z
    REAL( fp_kind ), INTENT( IN )           :: ppmv

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: kmol_per_cm2


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PPMV_TO_KMOL_PER_CM2'

    REAL( fp_kind ) :: SCALE_FACTOR = 1.0e-11_fp_kind



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( pressure       < TOLERANCE .OR. &
         temperature    < TOLERANCE .OR. &
         ABS( delta_z ) < TOLERANCE .OR. &
         ppmv           < ZERO           ) THEN
      kmol_per_cm2 = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input values  < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                  -- Convert ppmv to kmol.cm^-2 --
    !--------------------------------------------------------------------------

    kmol_per_cm2 = SCALE_FACTOR * pressure * ABS( delta_z ) * ppmv / &
    !              -----------------------
                    ( R0 * temperature )


  END FUNCTION ppmv2kmol_scalar




  FUNCTION ppmv2kmol_rank1( pressure,     &  ! Input
                            temperature,  &  ! Input
                            delta_z,      &  ! Input
                            ppmv,         &  ! Input
                            message_log ) &  ! Optional input
                          RESULT ( kmol_per_cm2 )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: delta_z
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: ppmv

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: kmol_per_cm2


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'PPMV_TO_KMOL_PER_CM2'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_layers



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n_layers = SIZE( pressure )

    IF ( SIZE( temperature ) /= n_layers .OR. & 
         SIZE( delta_z     ) /= n_layers .OR. & 
         SIZE( ppmv        ) /= n_layers ) THEN
      kmol_per_cm2 = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                     -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, n_layers

      kmol_per_cm2( i ) = ppmv2kmol_scalar( pressure( i ), &
                                            temperature( i ), &
                                            delta_z( i ), &
                                            ppmv( i ), &
                                            message_log = message_log )
      IF ( kmol_per_cm2( i ) < ZERO ) RETURN

    END DO

  END FUNCTION ppmv2kmol_rank1






  FUNCTION kmol2ppmv_scalar( pressure,     &  ! Input
                             temperature,  &  ! Input
                             delta_z,      &  ! Input
                             kmol_per_cm2, &  ! Input
                             message_log ) &  ! Optional input
                           RESULT ( ppmv )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN )           :: pressure
    REAL( fp_kind ), INTENT( IN )           :: temperature
    REAL( fp_kind ), INTENT( IN )           :: delta_z
    REAL( fp_kind ), INTENT( IN )           :: kmol_per_cm2

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: ppmv


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'KMOL_PER_CM2_TO_PPMV'

    REAL( fp_kind ) :: SCALE_FACTOR = 1.0e+11_fp_kind



    !--------------------------------------------------------------------------
    !                        -- Check input values --
    !--------------------------------------------------------------------------

    IF ( pressure     < TOLERANCE .OR. &
         temperature  < TOLERANCE .OR. &
         delta_z      < TOLERANCE .OR. &
         kmol_per_cm2 < ZERO      ) THEN
      ppmv = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Input values  < or = 0.0 found.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                  -- Convert kmol.cm^-2 to ppmv --
    !--------------------------------------------------------------------------

    
    ppmv = SCALE_FACTOR * R0 * temperature  * kmol_per_cm2 / &
    !      -------------------------------
               ( pressure * delta_z )


  END FUNCTION kmol2ppmv_scalar




  FUNCTION kmol2ppmv_rank1( pressure,     &  ! Input
                            temperature,  &  ! Input
                            delta_z,      &  ! Input
                            kmol_per_cm2, &  ! Input
                            message_log ) &  ! Optional input
                          RESULT ( ppmv )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: pressure
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: temperature
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: delta_z
    REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: kmol_per_cm2

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL       :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: ppmv


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'KMOL_PER_CM2_TO_PPMV'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_layers



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    n_layers = SIZE( pressure )

    IF ( SIZE( temperature  ) /= n_layers .OR. & 
         SIZE( delta_z      ) /= n_layers .OR. & 
         SIZE( kmol_per_cm2 ) /= n_layers ) THEN
      ppmv = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                     -- Loop over levels --
    !--------------------------------------------------------------------------

    DO i = 1, n_layers

      ppmv( i ) = kmol2ppmv_scalar( pressure( i ), &
                                    temperature( i ), &
                                    delta_z( i ), &
                                    kmol_per_cm2( i ), &
                                    message_log = message_log )
      IF ( ppmv( i ) < ZERO ) RETURN

    END DO

  END FUNCTION kmol2ppmv_rank1






  FUNCTION effective_layer_tp( pressure,                 &  ! Input
                               temperature,              &  ! Input
                               water_vapor,              &  ! Input
                               water_vapor_units,        &  ! Input
                               effective_pressure,       &  ! Output
                               effective_temperature,    &  ! Output
                               height,                   &  ! optional input
                               message_log )             &  ! Optional input
                             RESULT ( error_status )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )           :: pressure
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )           :: temperature
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )           :: water_vapor
    INTEGER,         INTENT( IN )                            :: water_vapor_units

    ! -- Output
    REAL( fp_kind ), INTENT( OUT ), DIMENSION( : )           :: effective_pressure
    REAL( fp_kind ), INTENT( OUT ), DIMENSION( : )           :: effective_temperature

    ! -- Optional input
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : ), OPTIONAL :: height

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),                  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'EFFECTIVE_LAYER_TP'

    REAL( fp_kind ), PARAMETER :: SCALE_FACTOR = 0.1_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 128 ) :: message

    INTEGER :: n_levels, n_layers
    INTEGER :: k

    REAL( fp_kind ) :: MWair
    REAL( fp_kind ) :: Rair, Rair_km1, layer_Rair
    REAL( fp_kind ) :: RHOair, RHOair_km1
    REAL( fp_kind ) :: dz
    REAL( fp_kind ) :: log_ratio
    REAL( fp_kind ) :: H_p, H_rho
    REAL( fp_kind ) :: sum_RHO, sum_p_RHO, sum_T_RHO



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    ! -----------------
    ! Input array sizes
    ! -----------------

    n_levels = SIZE( pressure )

    IF ( SIZE( temperature ) /= n_levels .OR. & 
         SIZE( water_vapor ) /= n_levels ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ------------------
    ! Output array sizes
    ! ------------------

    n_layers = n_levels - 1

    IF ( SIZE( effective_pressure    ) < n_layers .OR. & 
         SIZE( effective_temperature ) < n_layers ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Output arrays to small to hold result.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ------------------
    ! Input array values
    ! ------------------

    IF ( ANY( pressure < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressures < or = 0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    IF ( ANY( temperature < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input temperatures < or = 0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    IF ( ANY( water_vapor < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input water vapor concentrations < or = 0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                  -- Calculate near surface level values --
    !--------------------------------------------------------------------------

    ! -- Molecular weight of air
    MWair = mw_air_scalar( pressure( 1 ), &
                           water_vapor( 1 ), &
                           water_vapor_units, &
                           message_log = message_log )
    IF ( MWair < ZERO ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error calculating MWair at level 1. Value = ", es13.6 )' ) &
                      MWair
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Calculate the gas "constant" in J/K/kg
    Rair_km1 = R0 / ( MWair * G_TO_KG )

    ! -- Air density
    RHOair_km1 = density_scalar( pressure( 1 ), &
                                 temperature( 1 ), &
                                 MWair, &
                                 message_log = message_log )

    IF ( RHOair_km1 < ZERO ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error calculating RHOair at level 1. Value = ", es13.6 )' ) &
                      RHOair_km1
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                         -- Loop over layers --
    !--------------------------------------------------------------------------

    ! ----------------
    ! Begin layer loop
    ! ----------------

    k_layer_loop: DO k = 1, n_layers


      ! -------------------------------------
      ! Calculate current top of layer values
      ! -------------------------------------

      ! -- MWair at current level
      MWair = mw_air_scalar( pressure( k+1 ), &
                             water_vapor( k+1 ), &
                             water_vapor_units, &
                             message_log = message_log )
      IF ( MWair < ZERO ) THEN
        error_status = FAILURE
        WRITE( message, '( "Error calculating MWair at level ", i4, ". Value = ", es13.6 )' ) &
                        k+1, MWair
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      ENDIF

      ! -- Gas "constant" at current level in J/K/kg
      Rair = R0 / ( MWair * G_TO_KG )

      ! -- Air density at current level
      RHOair = density_scalar( pressure( k+1 ), &
                               temperature( k+1 ), &
                               MWair, &
                               message_log = message_log )
      IF ( RHOair < ZERO ) THEN
        error_status = FAILURE
        WRITE( message, '( "Error calculating RHOair at level ", i4, ". Value = ", es13.6 )' ) &
                        k+1, RHOair
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      ENDIF


      ! ---------------------------------
      ! Calculate the layer scale heights
      ! ---------------------------------

      ! -- Calculate layer thicknesses
      dz = height( k+1 ) - height( k )

      ! -- Pressure scale height
      log_ratio = LOG( pressure( k+1 ) / pressure( k ) )
      H_p       = dz / log_ratio

      ! -- Density scale height
      log_ratio = LOG( RHOair / RHOair_km1 )
      H_rho     = dz / log_ratio


      ! ----------------------------------
      ! Calculate the effective quantities
      ! ----------------------------------

      ! -- Calculate the density integral
      sum_RHO = H_rho * ( RHOair - RHOair_km1 )

      ! -- Effective pressure
      sum_p_rho = ( ( H_p * H_rho ) / ( H_p + H_rho ) ) * &
                  ( ( pressure( k+1 ) * RHOair ) - ( pressure( k ) * RHOair_km1 ) )

      effective_pressure( k ) = sum_p_rho / sum_rho


      ! -- Calculate density weighted layer gas "constant"
      layer_Rair = ( ( Rair_km1 * RHOair_km1 ) + ( Rair * RHOair ) ) / &
      !            -------------------------------------------------
                                  ( RHOair_km1 + RHOair )


      ! -- Effective temperature
      sum_T_rho = ( H_p / layer_Rair ) * ( pressure( k+1 ) - pressure( k ) )

      effective_temperature( k ) = SCALE_FACTOR * sum_T_rho / sum_rho


      ! ---------------------------------------------------
      ! Save top boundary values for use as bottom boundary
      ! values for next layer
      ! ---------------------------------------------------

      Rair_km1   = Rair
      RHOair_km1 = RHOair

    END DO k_layer_loop



    !--------------------------------------------------------------------------
    !                               -- Done --
    !--------------------------------------------------------------------------

    error_status = SUCCESS

  END FUNCTION effective_layer_tp






  FUNCTION geopotential_height( pressure,                 &  ! Input
                                temperature,              &  ! Input
                                water_vapor,              &  ! Input
                                water_vapor_units,        &  ! Input
                                height,                   &  ! Output
                                surface_height,           &  ! Optional input
                                gravity_correction,       &  ! Optional input
                                latitude,                 &  ! Optional input
                                zonal_wind_velocity,      &  ! Optional input
                                meridional_wind_velocity, &  ! Optional input
                                message_log )             &  ! Optional input
                              RESULT( error_status )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )          :: pressure
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )          :: temperature
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )          :: water_vapor
    INTEGER,         INTENT( IN )                           :: water_vapor_units

    ! -- Output
    REAL( fp_kind ), INTENT( OUT ), DIMENSION( : )          :: height

    ! -- Optional input
    REAL( fp_kind ), INTENT( IN ),                  OPTIONAL :: surface_height
    INTEGER,         INTENT( IN ),                  OPTIONAL :: gravity_correction
    REAL( fp_kind ), INTENT( IN ),                  OPTIONAL :: latitude
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : ), OPTIONAL :: zonal_wind_velocity
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : ), OPTIONAL :: meridional_wind_velocity

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),                  OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'GEOPOTENTIAL_HEIGHT'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 128 ) :: message

    INTEGER :: n_levels
    INTEGER :: use_gravity
    INTEGER :: k, k1, k2, dk

    REAL( fp_kind ), DIMENSION( SIZE( pressure ) ) :: u, v

    REAL( fp_kind ) :: surface_z
    REAL( fp_kind ) :: MWair
    REAL( fp_kind ) :: Rair,   Rair_km1
    REAL( fp_kind ) :: RHOair, RHOair_km1
    REAL( fp_kind ) :: layer_Rair
    REAL( fp_kind ) :: layer_T
    REAL( fp_kind ) :: g
    REAL( fp_kind ) :: H
    REAL( fp_kind ) :: dz



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    ! -----------------
    ! Input array sizes
    ! -----------------

    n_levels = SIZE( pressure )

    IF ( SIZE( temperature ) /= n_levels .OR. & 
         SIZE( water_vapor ) /= n_levels ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! -----------------
    ! Output array size
    ! -----------------

    IF ( SIZE( height ) < n_levels  ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Output HEIGHT array too small to hold result.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ------------------
    ! Input array values
    ! ------------------

    IF ( ANY( pressure < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressures < or = 0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    IF ( ANY( temperature < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input temperatures < or = 0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    IF ( ANY( water_vapor < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input water vapor concentrations < or = 0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ------------------
    ! Optional arguments
    ! ------------------

    ! -- Surface height
    IF ( PRESENT( surface_height ) ) THEN
      surface_z = surface_height
    ELSE
      surface_z = ZERO
    END IF

    ! -- Gravity
    use_gravity = 0
    IF ( PRESENT( gravity_correction ) ) THEN

      IF ( gravity_correction /= 0 ) THEN

        use_gravity = 1

        ! -- Check zonal wind velocity
        u = ZERO
        IF ( PRESENT( zonal_wind_velocity ) ) THEN
          IF ( SIZE( zonal_wind_velocity ) /= n_levels ) THEN
            error_status = WARNING
            CALL display_message( ROUTINE_NAME, &
                                  'Input ZONAL_WIND_VELOCITY has inconsistent size. '//&
                                  'Setting to 0.0.', &
                                  error_status, &
                                  message_log = message_log )
          ELSE
            u = zonal_wind_velocity
          END IF
        END IF
          
        ! -- Check meridional wind velocity
        v = ZERO
        IF ( PRESENT( meridional_wind_velocity ) ) THEN
          IF ( SIZE( meridional_wind_velocity ) /= n_levels ) THEN
            error_status = WARNING
            CALL display_message( ROUTINE_NAME, &
                                  'Input MERIDIONAL_WIND_VELOCITY has inconsistent size. '//&
                                  'Setting to 0.0.', &
                                  error_status, &
                                  message_log = message_log )
          ELSE
            v = meridional_wind_velocity
          END IF
        END IF
      END IF
    END IF



    !--------------------------------------------------------------------------
    !                -- Determine order of input pressure --
    !--------------------------------------------------------------------------

    IF ( pressure( 2 ) < pressure ( 1 ) ) THEN

      ! -- Ascending, i.e. ground up
      k1 = 1
      k2 = n_levels
      dk = 1

    ELSE

      ! -- Descending, i.e. TOA down
      k1 = n_levels
      k2 = 1
      dk = -1

    END IF



    !--------------------------------------------------------------------------
    !                  -- Calculate near surface level values --
    !--------------------------------------------------------------------------

    ! -- Molecular weight of air
    MWair = mw_air_scalar( pressure( k1 ), &
                           water_vapor( k1 ), &
                           water_vapor_units, &
                           message_log = message_log )
    IF ( MWair < ZERO ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error calculating MWair at level ", i4, ". Value = ", es13.6 )' ) &
                      k1, MWair
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Calculate the gas "constant" in J/K/kg
    Rair_km1 = R0 / ( MWair * G_TO_KG )

    ! -- Air density
    RHOair_km1 = density_scalar( pressure( k1 ), &
                                 temperature( k1 ), &
                                 MWair, &
                                 message_log = message_log )

    IF ( RHOair_km1 < ZERO ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error calculating RHOair at level ", i4, ". Value = ", es13.6 )' ) &
                      k1, RHOair_km1
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                  -- Loop over levels in ground up order --
    !--------------------------------------------------------------------------

    ! -------------------------
    ! Assign first level height
    ! -------------------------

    height( k1 ) = surface_z


    ! ----------------
    ! Begin level loop
    ! ----------------

    k_level_loop: DO k = k1+dk, k2, dk


      ! ------------------------------
      ! Calculate current level values
      ! ------------------------------

      ! -- MWair at current level
      MWair = mw_air_scalar( pressure( k ), &
                             water_vapor( k ), &
                             water_vapor_units, &
                             message_log = message_log )
      IF ( MWair < ZERO ) THEN
        error_status = FAILURE
        WRITE( message, '( "Error calculating MWair at level ", i4, ". Value = ", es13.6 )' ) &
                        k, MWair
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      ENDIF

      ! -- Gas "constant" at current level in J/K/kg
      Rair = R0 / ( MWair * G_TO_KG )

      ! -- Air density at current level
      RHOair = density_scalar( pressure( k ), &
                               temperature( k ), &
                               MWair, &
                               message_log = message_log )
      IF ( RHOair < ZERO ) THEN
        error_status = FAILURE
        WRITE( message, '( "Error calculating RHOair at level ", i4, ". Value = ", es13.6 )' ) &
                        k, RHOair
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      ENDIF


      ! -----------------------------------------
      ! Calculate density weighted layer averages
      ! -----------------------------------------

      ! -- Gas "constant"
      layer_Rair = ( ( Rair_km1 * RHOair_km1 ) + ( Rair * RHOair ) ) / &
      !            -------------------------------------------------
                                  ( RHOair_km1 + RHOair )

      ! -- Temperature
      layer_T =  ( ( temperature( k-dk ) * RHOair_km1 ) + ( temperature( k ) * RHOair ) ) / &
      !          ------------------------------------------------------------------------
                                           ( RHOair_km1 + RHOair )


      ! -----------------------------------
      ! Calculate gravity value if required
      ! -----------------------------------

      IF ( use_gravity == 1 ) THEN

        ! -- Calculate gravity at layer lower boundary
        g = gravity( height( k-dk ), &
                     latitude                 = latitude, &
                     zonal_wind_velocity      = u( k-dk ), &
                     meridional_wind_velocity = v( k-dk ), &
                     message_log              = message_log )

        IF ( g < ZERO ) THEN
          error_status = FAILURE
          CALL display_message( ROUTINE_NAME, &
                                'Gravity calculation failed.', &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF

      ELSE

        ! -- Use standard gravity
        g = G0

      END IF


      ! --------------------------------------------------
      ! Calculate scale height. Here, if a gravity profile
      ! is used, we assume that the gravity at the lower
      ! layer boundary is a good approximation for that 
      ! in the middle of the layer.
      ! --------------------------------------------------

      H = layer_Rair * layer_T / g


      ! -------------------------
      ! Calculate layer thickness
      ! -------------------------

      dz = H * LOG( pressure( k-dk ) / pressure( k ) )


      ! ------------------
      ! Accumulate heights
      ! ------------------

      height( k ) = height( k-dk ) + dz


      ! ----------------------------------
      ! Save current level Rair and RHOair
      ! ----------------------------------

      Rair_km1   = Rair
      RHOair_km1 = RHOair

    END DO k_level_loop



    !--------------------------------------------------------------------------
    !                               -- Done --
    !--------------------------------------------------------------------------

    error_status = SUCCESS

  END FUNCTION geopotential_height






   FUNCTION gravity( height,                   &  ! Input
                     latitude,                 &  ! Optional input
                     zonal_wind_velocity,      &  ! Optional input
                     meridional_wind_velocity, &  ! Optional input
                     message_log               )  ! Optional input



    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), INTENT( IN )           :: height

    ! -- Optional input
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: latitude
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: zonal_wind_velocity
    REAL( fp_kind ), INTENT( IN ), OPTIONAL :: meridional_wind_velocity

    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ), OPTIONAL :: message_log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: gravity


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'GRAVITY'

    REAL( fp_kind ), PARAMETER :: TWO_PI = TWO * PI
    REAL( fp_kind ), PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp_kind

    ! -- Earth rotational speed in rad.s^-1. The number of seconds
    ! -- per day is for a sidereal day, the amount of time for the
    ! -- Earth to rotate 360 degrees: 23h, 56m, 4s. A solar day is
    ! -- 24h long. It makes next to no difference in the calculated
    ! -- gravity, but what the hell.
    REAL( fp_kind ), PARAMETER :: N_SECONDS_IN_DAY = 86164.0_fp_kind
    REAL( fp_kind ), PARAMETER :: OMEGA = TWO_PI / N_SECONDS_IN_DAY

    ! -- Earth radii in m.
    REAL( fp_kind ), PARAMETER :: Re_equatorial = 6.378388e+06_fp_kind ! == a
    REAL( fp_kind ), PARAMETER :: Re_polar      = 6.356911e+06_fp_kind ! == b

    ! -- Earth's flattening correction factor, (1 - b^2/a^2)
    REAL( fp_kind ), PARAMETER :: a2      = Re_equatorial**2
    REAL( fp_kind ), PARAMETER :: b2      = Re_polar**2
    REAL( fp_kind ), PARAMETER :: factor  = ONE - ( b2 / a2 )

    ! -- Data for normal gravity equation
    REAL( fp_kind ), PARAMETER :: G_NORMAL_EQUATOR      = 9.7803267715_fp_kind
    INTEGER,         PARAMETER :: N_NORMAL_COEFFICIENTS = 4
    REAL( fp_kind ), PARAMETER, DIMENSION( N_NORMAL_COEFFICIENTS ) :: &
                     G_COEFFS = (/ 5.2790414e-03_fp_kind, &
                                   2.32718e-05_fp_kind,   &
                                   1.262e-07_fp_kind,     &
                                   7.0e-10_fp_kind        /)

    ! -- This is the number of the above coefficients to use
    ! -- in the calculation. Don't really need all four.
    INTEGER,         PARAMETER :: N_COEFFICIENTS = 2


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i

    REAL( fp_kind ) :: lat
    REAL( fp_kind ) :: u, v
    REAL( fp_kind ) :: cos_lat, sin_lat
    REAL( fp_kind ) :: Re, Rtotal
    REAL( fp_kind ) :: sum_coeffs, g_normal
    REAL( fp_kind ) :: a_surface
    REAL( fp_kind ) :: a_z
    REAL( fp_kind ) :: g_surface



    !--------------------------------------------------------------------------
    !                        -- Check input --
    !--------------------------------------------------------------------------

    IF ( PRESENT( latitude ) ) THEN
      lat = latitude
    ELSE
      lat = ZERO
    END IF

    IF ( PRESENT( zonal_wind_velocity ) ) THEN
      u = zonal_wind_velocity
    ELSE
      u = ZERO
    END IF

    IF ( PRESENT( meridional_wind_velocity ) ) THEN
      v = meridional_wind_velocity
    ELSE
      v = ZERO
    END IF



    !--------------------------------------------------------------------------
    !                   -- Calculate trigonometric terms --
    !
    ! It's most likely faster to calculate the cosine squared terms here and
    ! modify the equations down the line to use the precalculated value rather
    ! than do cos**2 each time, but as it is now, this only occurs in two places
    ! (the calc for Re and a_surface, and a_z if the wind arguments aren't
    ! present) and the code reflects the equations as one would write them down,
    ! which makes me feel all warm and fuzzy on the inside. :o)
    !--------------------------------------------------------------------------

    cos_lat = COS( DEGREES_TO_RADIANS * lat )
    sin_lat = SIN( DEGREES_TO_RADIANS * lat )



    !--------------------------------------------------------------------------
    !                   -- Calculate radius terms --
    !--------------------------------------------------------------------------

    ! ---------------------------------------------
    ! Calculate the Earth's radius at this latitude
    ! ---------------------------------------------

    Re = SQRT(                b2                 / &
    !          ---------------------------------
               ( ONE - ( cos_lat**2 * factor ) ) )


    ! --------------------------------------------
    ! Calculate total distance from Earth's center
    ! --------------------------------------------

    Rtotal = Re + height

    IF ( Rtotal < TOLERANCE ) THEN
      gravity = -ONE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid height (<Re) input argument', &
                            FAILURE, &
                            message_log = message_log )
      RETURN
    END IF



    !--------------------------------------------------------------------------
    !               -- Calculate gravity at Earth's surface --
    !
    ! This uses the normal gravity equation. This equation provides gravity due
    ! to the mass *AND* rotation of the Earth.
    !--------------------------------------------------------------------------

    sum_coeffs = ZERO
    DO i = 1, N_COEFFICIENTS
      sum_coeffs = sum_coeffs + ( G_COEFFS( i ) * sin_lat**( 2*i ) )
    END DO

    g_normal = G_NORMAL_EQUATOR * ( ONE + sum_coeffs )



    !--------------------------------------------------------------------------
    !        -- Calculate centripetal acceleration at Earth's surface --
    !--------------------------------------------------------------------------

    a_surface = ( OMEGA * cos_lat )**2 * Re



    !--------------------------------------------------------------------------
    !         -- Calculate centripetal acceleration at height z --
    !--------------------------------------------------------------------------

    IF ( PRESENT( zonal_wind_velocity      ) .OR. &
         PRESENT( meridional_wind_velocity ) ) THEN

      a_z = ( ( ( OMEGA * Rtotal * cos_lat ) + u )**2 + v**2 ) / &
      !       ------------------------------------------------
                                        Rtotal

    ELSE

      a_z = ( OMEGA * cos_lat )**2 * Rtotal

    END IF



    !--------------------------------------------------------------------------
    !          -- Calculate the gravity at the Earth's surface,  --
    !          -- *removing* the effect of the Earth's rotation. --
    !--------------------------------------------------------------------------

    g_surface = g_normal + a_surface


    !--------------------------------------------------------------------------
    !       -- Calculate the gravitational acceleration at height z    --
    !       -- incorporating the centripetal acceleration at height z  --
    !--------------------------------------------------------------------------

    gravity = g_surface * ( Re/Rtotal )**2 - a_z

  END FUNCTION gravity






  FUNCTION create_sublevels( level_pressure,       &  ! Input
                             level_temperature,    &  ! Input
                             level_absorber,       &  ! Input
                             n_per_layer,          &  ! Input
                             sublevel_pressure,    &  ! Output
                             sublevel_temperature, &  ! Output
                             sublevel_absorber,    &  ! Output
                             message_log )         &  ! Optional input
                           RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )    :: level_pressure
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )    :: level_temperature
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( :, : ) :: level_absorber
    INTEGER,         INTENT( IN )                     :: n_per_layer

    REAL( fp_kind ), INTENT( OUT ), DIMENSION( : )    :: sublevel_pressure
    REAL( fp_kind ), INTENT( OUT ), DIMENSION( : )    :: sublevel_temperature
    REAL( fp_kind ), INTENT( OUT ), DIMENSION( :, : ) :: sublevel_absorber
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL          :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CREATE_SUBLEVELS'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 128 ) :: message

    INTEGER :: n_levels
    INTEGER :: n_sublevels
    INTEGER :: n_absorbers

    INTEGER :: i       ! Generic loop/index variable
    INTEGER :: j       ! Absorber index variable
    INTEGER :: k       ! Level index variable
    INTEGER :: n1, n2  ! Sublevel indices within *layer* k

    REAL( fp_kind ) :: xn_per_layer
    REAL( fp_kind ) :: dx

    REAL( fp_kind ), DIMENSION( n_per_layer ) :: xn

    REAL( fp_kind ), DIMENSION( SIZE( level_pressure    ) ) :: level_ln_pressure
    REAL( fp_kind ), DIMENSION( SIZE( sublevel_pressure ) ) :: sublevel_ln_pressure



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

    ! --------------------
    ! Size of input arrays
    ! --------------------

    n_levels = SIZE( level_pressure )

    IF ( SIZE( level_temperature       ) /= n_levels .OR. &
         SIZE( level_absorber, DIM = 1 ) /= n_levels ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! -------------------
    ! Sublevel multiplier
    ! -------------------

    IF ( n_per_layer < 1 ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input N_PER_LAYER must be > 0.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ---------------------
    ! Size of output arrays
    ! ---------------------

    ! -- Calculate the number of sublevels
    n_sublevels = ( ( n_levels - 1 ) * n_per_layer ) + 1

    ! -- Can output arrays handle it?
    IF ( SIZE( sublevel_pressure          ) < n_sublevels .OR. &
         SIZE( sublevel_temperature       ) < n_sublevels .OR. &
         SIZE( sublevel_absorber, DIM = 1 ) < n_sublevels ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Output arrays not large enough to hold result.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Number of absorbers
    n_absorbers = SIZE( level_absorber, DIM = 2 )

    IF ( SIZE( sublevel_absorber, DIM = 2 ) < n_absorbers ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Output SUBLEVEL_ABSORBER array does not have '//&
                            'enough absorber dimension elements.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
    

    ! ---------------------------------
    ! Check input pressure array values
    ! ---------------------------------

    IF ( ANY( level_pressure < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressures < or = 0.0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !-------------------------------------------------------------------------------
    !               -- Calculate the log of the input pressure --
    !-------------------------------------------------------------------------------

    ! -- Don't really need the WHERE due to the 
    ! -- input pressure check, but just to be sure
    WHERE ( level_pressure > ZERO )
      level_ln_pressure = LOG( level_pressure )
    ELSEWHERE
      level_ln_pressure = ZERO
    END WHERE



    !-------------------------------------------------------------------------------
    !                          -- Interpolate data --
    !
    ! Here we assumes that temperature and absorber amount vary linearly with the 
    ! natural logarithm of pressure. Because the interpolation is done at equal
    ! intervals, rather than use a function, the interpolation code is inline.
    ! It's simpler.
    !-------------------------------------------------------------------------------

    ! -------------------------------------
    ! Fill the layer index array
    !
    ! xn = [ 0, 1, 2, ..... n_per_layer-1 ]
    ! -------------------------------------

    xn           = (/ ( REAL( i, fp_kind ), i = 0, n_per_layer - 1 ) /)
    xn_per_layer = REAL( n_per_layer, fp_kind )


    ! ------------------------------------------
    ! Loop over layers and linearly interpolate
    ! across layer k (between levels k and k+1)
    ! to n equally spaced sublevels.
    !
    !        x(k+1) - x(k)
    !   dx = -------------
    !              n
    ! so that
    !
    !   x = x(k) + ( xn*dx )
    !
    ! where xn and x are vectors.
    !
    ! Note that although the temperature and 
    ! absorber amount are linearly interpolated
    ! between levels, the interpolated values 
    ! are associated with the ln(P) interpolated
    ! values. So, the temperature/absorber
    ! interpolation is effectively exponential.
    ! ------------------------------------------

    layer_loop: DO k = 1, n_levels - 1

      ! -- Sublevel array indices
      n1 = ( ( k-1 ) * n_per_layer ) + 1
      n2 = n1 + n_per_layer - 1


      ! -- Interpolate ln(p)
      dx = ( level_ln_pressure( k+1 ) - level_ln_pressure( k ) ) / &
      !    -----------------------------------------------------
                                 xn_per_layer

      sublevel_ln_pressure( n1:n2 ) = level_ln_pressure( k ) + ( xn * dx )


      ! -- Interpolate T
      dx = ( level_temperature( k+1 ) - level_temperature( k ) ) / &
      !    -----------------------------------------------------
                                 xn_per_layer

      sublevel_temperature( n1:n2 ) = level_temperature( k ) + ( xn * dx )


      ! -- Interpolate absorber
      absorber_loop: DO j = 1, n_absorbers

        dx = ( level_absorber( k+1, j ) - level_absorber( k, j ) ) / &
        !    -----------------------------------------------------
                                   xn_per_layer

        sublevel_absorber( n1:n2, j ) = level_absorber( k, j ) + ( xn * dx )

      END DO absorber_loop


      ! -- Convert ln(p) -> p
      sublevel_pressure( n1:n2 ) = EXP( sublevel_ln_pressure( n1:n2 ) )

    END DO layer_loop


    ! -----------------
    ! Assign last level
    ! -----------------

    sublevel_pressure( n_sublevels )    = level_pressure( n_levels )
    sublevel_temperature( n_sublevels ) = level_temperature( n_levels )
    sublevel_absorber( n_sublevels, : ) = level_absorber( n_levels, : )



    !-------------------------------------------------------------------------------
    !                               -- Done --
    !-------------------------------------------------------------------------------

    error_status = SUCCESS

  END FUNCTION create_sublevels






  FUNCTION integrate_sublevels( sublevel_height,       &  ! Input
                                sublevel_pressure,     &  ! Input
                                sublevel_temperature,  &  ! Input
                                sublevel_absorber,     &  ! Input
                                n_per_layer,           &  ! Input

                                layer_pressure,        &  ! Output
                                layer_temperature,     &  ! Output
                                layer_absorber,        &  ! Output

                                message_log )          &  ! Optional input

                              RESULT ( error_status )



    !------------------------------------------------------------------------------
    !                        -- Type declarations --
    !------------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )    :: sublevel_height
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )    :: sublevel_pressure
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( : )    :: sublevel_temperature
    REAL( fp_kind ), INTENT( IN ),  DIMENSION( :, : ) :: sublevel_absorber
    INTEGER,         INTENT( IN )                     :: n_per_layer

    REAL( fp_kind ), INTENT( OUT ), DIMENSION( : )    :: layer_pressure
    REAL( fp_kind ), INTENT( OUT ), DIMENSION( : )    :: layer_temperature
    REAL( fp_kind ), INTENT( OUT ), DIMENSION( :, : ) :: layer_absorber
 
    ! -- Error handler message log
    CHARACTER( * ),  INTENT( IN ),  OPTIONAL          :: message_log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'INTEGRATE_SUBLEVELS'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 128 ) :: message

    INTEGER :: n_sublevels
    INTEGER :: n_layers
    INTEGER :: n_absorbers

    INTEGER :: j          ! Absorber index variable
    INTEGER :: k          ! Layer index variable
    INTEGER :: n, n1, n2  ! Sublevel loop/indices within *layer* k

    REAL( fp_kind ) :: sublevel_RHOair, sublevel_RHOair_nm1

    REAL( fp_kind ) :: sublayer_dz
    REAL( fp_kind ) :: sublayer_pressure
    REAL( fp_kind ) :: sublayer_temperature
    REAL( fp_kind ) :: sublayer_T_RHOair
    REAL( fp_kind ) :: sublayer_RHOair
    REAL( fp_kind ) :: layer_T_RHOair_sum
    REAL( fp_kind ) :: layer_RHOair_sum
    REAL( fp_kind ) :: sublayer_absorber ,sublayer_absorber_k
    REAL( fp_kind ), DIMENSION( SIZE( sublevel_absorber, DIM=2 ) ) :: layer_absorber_sum
    REAL( fp_kind ) :: layer_dz



    !-------------------------------------------------------------------------------
    !                            -- Check input --
    !-------------------------------------------------------------------------------

    ! --------------------
    ! Size of input arrays
    ! --------------------

    n_sublevels = SIZE( sublevel_pressure )

    IF ( SIZE( sublevel_temperature       ) /= n_sublevels .OR. &
         SIZE( sublevel_absorber, DIM = 1 ) /= n_sublevels ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ------------------------
    ! Check input array values
    ! ------------------------

    IF ( ANY( sublevel_pressure < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input pressures < or = 0.0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    IF ( ANY( sublevel_temperature < TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input temperatures < or = 0.0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Absorber amount can be = 0.0
    IF ( ANY( sublevel_absorber < ZERO ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input absorber amounts < 0.0 found.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! -------------------
    ! Sublevel multiplier
    ! -------------------

    IF ( n_per_layer < 1 ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input N_PER_LAYER must be > 0.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF


    ! ---------------------
    ! Size of output arrays
    ! ---------------------

    ! -- Calculate the number of output layers
    IF ( n_per_layer > 1 ) THEN
      n_layers = n_sublevels / n_per_layer
    ELSE
      n_layers = n_sublevels - 1
    END IF

    ! -- Can output arrays handle it?
    IF ( SIZE( layer_pressure          ) < n_layers .OR. &
         SIZE( layer_temperature       ) < n_layers .OR. &
         SIZE( layer_absorber, DIM = 1 ) < n_layers ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Output arrays not large enough to hold result.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF

    ! -- Number of absorbers
    n_absorbers = SIZE( sublevel_absorber, DIM = 2 )

    IF ( SIZE( layer_absorber, DIM = 2 ) < n_absorbers ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Output LAYER_ABSORBER array does not have '//&
                            'enough absorber dimension elements.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF
    


    !--------------------------------------------------------------------------
    !           -- Calculate initial level total number density --
    !--------------------------------------------------------------------------

    sublevel_RHOair_nm1 = pp2nd_scalar( sublevel_pressure( 1 ), &
                                        sublevel_temperature( 1 ), &
                                        message_log = message_log )
    IF ( sublevel_RHOair_nm1 < ZERO ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error calculating RHOair at sublevel 1', &
                            error_status, &
                            message_log = message_log )
      RETURN
    ENDIF



    !--------------------------------------------------------------------------
    !                    -- Begin integration loop over layers --
    !--------------------------------------------------------------------------


    ! ----------------
    ! Begin layer loop
    ! ----------------

    k_layer_loop: DO k = 1, n_layers


      ! -- Initialise sum variables
      layer_T_RHOair_sum      = ZERO
      layer_RHOair_sum        = ZERO
      layer_absorber_sum( : ) = ZERO

      ! -- Sublevel array indices
      n1 = ( ( k-1 ) * n_per_layer ) + 1
      n2 = n1 + n_per_layer - 1


      ! -------------------
      ! Loop over sublayers
      ! -------------------

      n_sublayer_loop: DO n = n1, n2


        ! ------------------------------------------------------
        ! Calculate current top of sublayer total number density
        ! ------------------------------------------------------

        sublevel_RHOair = pp2nd_scalar( sublevel_pressure( n+1 ), &
                                        sublevel_temperature( n+1 ), &
                                        message_log = message_log )
        IF ( sublevel_RHOair < ZERO ) THEN
          error_status = FAILURE
          WRITE( message, '( "Error calculating RHOair at sublevel ", i4 )' ) n+1
          CALL display_message( ROUTINE_NAME, &
                                TRIM( message ), &
                                error_status, &
                                message_log = message_log )
          RETURN
        ENDIF


        ! ----------------------------------------------------------------
        ! Perform the summation for the density weighted layer temperature
        ! by summing the T.rho subLAYER product and the subLAYER density
        ! normalising factor:
        !
        !
        !                       __ N(k)
        !                      \
        !                       >   Trho      [ units of kmol.cm^-2.ppmv^-1.K ]
        !                      /__
        !                          1
        ! layer_temperature = -----------
        !                       __ N(k)
        !                      \
        !                       >   rho       [ units of kmol.cm^-2.ppmv^-1 ]
        !                      /__
        !                          1
        !
        !
        !                       __ N(k)
        !                      \      1.0e-11
        !                       >    --------- * dz * p
        !                      /__       R
        !                          1
        !                   = ---------------------------
        !                       __ N(k)
        !                      \       1.0e-11
        !                       >     --------- * dz * p
        !                      /__      R . T
        !                          1
        !
        !
        !                       __ N(k)
        !                      \
        !                       >     dz . p
        !                      /__
        !                          1
        !                   = ----------------
        !                       __ N(k)
        !                      \      dz . p
        !                       >    --------
        !                      /__       T
        !                          1
        !
        ! in units of Kelvin
        !
        ! In the UMBC KLAYERS code, the numerator corresponds to the final
        ! TSUM value (with each sublayer value corresponding to RJUNK),
        ! the denominator to AJUNK, and the result to TLAY.
        ! ----------------------------------------------------------------

        ! -- Calculate sublayer thickness, dz
        sublayer_dz = ABS( sublevel_height( n+1 ) - sublevel_height( n ) )


        ! -- Calculate sublayer pressure, p
        sublayer_pressure =    ( sublevel_pressure( n+1 ) - sublevel_pressure( n ) ) / &
        !                   --------------------------------------------------------
                            LOG( sublevel_pressure( n+1 ) / sublevel_pressure( n ) )


        ! -- Calculate sublayer temperature, T
        sublayer_temperature = ( sublevel_temperature( n+1 )*sublevel_RHOair + sublevel_temperature( n )*sublevel_RHOair_nm1 ) / &
        !                      -----------------------------------------------------------------------------------------------
                                                           ( sublevel_RHOair + sublevel_RHOair_nm1 )


        ! -- Calculate the sublayer T.rho and rho variables
        sublayer_T_RHOair = sublayer_dz * sublayer_pressure
        sublayer_RHOair   = sublayer_T_RHOair / sublayer_temperature

        ! -- Sum the sublayer Trho and rho variables
        layer_T_RHOair_sum = layer_T_RHOair_sum + sublayer_T_RHOair
        layer_RHOair_sum   = layer_RHOair_sum   + sublayer_RHOair



        ! ---------------------------------------------------------
        ! Perform the summation for the integrated layer absorber
        ! amount:
        !
        !                   __ N(k)
        !                  \      Trho . ppmv
        ! layer_absorber =  >    -------------
        !                  /__        T
        !                      1
        !
        !                   __ N(k)
        !                  \      1.0e-11             ppmv
        !                =  >    --------- . dz . p .------
        !                  /__       R                 T
        !                      1
        !
        ! in units of kmol.cm^-2
        !
        ! This corresponds to ASUM (and eventually ALAY) in the
        ! UMBC KLAYERS code.
        ! ---------------------------------------------------------

        j_absorber_sum_loop: DO j = 1, n_absorbers

          ! -- Calculate simple average sublayer absorber in ppmv
          sublayer_absorber = 0.5_fp_kind * ( sublevel_absorber( n+1, j ) + sublevel_absorber( n, j ) )

          ! -- Convert to kmol.cm^-2 (shouldn't need to check result(?))
          sublayer_absorber_k = ppmv2kmol_scalar( sublayer_pressure, &
                                                  sublayer_temperature, &
                                                  sublayer_dz, &
                                                  sublayer_absorber, &
                                                  message_log = message_log )

          ! -- Sum the column density
          layer_absorber_sum( j ) = layer_absorber_sum( j ) + sublayer_absorber_k

        END DO j_absorber_sum_loop


        ! -------------------------------------------
        ! Save top boundary density for use as bottom
        ! boundary density for next layer
        ! -------------------------------------------

        sublevel_RHOair_nm1 = sublevel_RHOair


      END DO n_sublayer_loop


      ! -------------------------------
      ! Assign the average layer values
      ! -------------------------------

      layer_pressure( k )    =    ( sublevel_pressure( n2+1 ) - sublevel_pressure( n1 ) ) / &
      !                        ----------------------------------------------------------
                               LOG( sublevel_pressure( n2+1 ) / sublevel_pressure( n1 ) )

      layer_temperature( k ) = layer_T_RHOair_sum / layer_RHOair_sum

      layer_absorber( k, : ) = layer_absorber_sum( : )

    END DO k_layer_loop



    !--------------------------------------------------------------------------
    !                               -- Done --
    !--------------------------------------------------------------------------

    error_status = SUCCESS


  END FUNCTION integrate_sublevels

END MODULE profile_conversion

