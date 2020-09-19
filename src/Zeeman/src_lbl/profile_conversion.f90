!------------------------------------------------------------------------------
!M+
! NAME:
!       profile_conversion
!
! PURPOSE:
!       Module containing functions and routines to allow for conversion of
!       atmospheric profile concentration units.
!
! CATEGORY:
!       Profile Conversion
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE profile_conversion
!
! MODULES:
!       type_kinds:               Module containing definitions for kinds of
!                                 variable types.
!
!       fundamental_constants:    Module containing definitions for some
!                                 fundamental physical constants.
!
!       error_handler:            Module containing definitions of simple error
!                                 codes and error handling routines.
!
! CONTAINS:
!       mw_air:                      Function to calculate the water vapor weighted
!                                    molecular weight of air.
!
!       density:                     Function calculate air density using the ideal
!                                    gas law.
!
!       svp_water:                   Function to calculate the saturation vapor
!                                    pressure over water.
!
!       svp_ice:                     Function to calculate the saturation vapor
!                                    pressure over ice.
!
!       virtual_temperature:         Function to calculate the virtual temperature.
!
!       saturation_mixing_ratio:     Funciton calculate the saturation mixing ratio.
!
!       rh_to_mr:                    Function to convert water vapor amounts from
!                                    relative humidity to mixing ratio.
!
!       mr_to_rh:                    Function to convert water vapor amounts from
!                                    mixing ratio to relative humidity.
!
!       mr_to_ppmv:                  Function to convert gas amounts from mixing
!                                    ratio to parts-per-million by volume.
!
!       ppmv_to_mr:                  Function to convert gas amounts from parts-per-
!                                    million by volume to mixing ratio.
!
!       ppmv_to_pp:                  Function to convert gas amounts from parts-per-
!                                    million by volume to partial pressure.
!
!       pp_to_ppmv:                  Function to convert gas amounts from partial
!                                    pressure to parts-per-million by volume.
!
!       pressure_to_number_density:  Function to convert gas amounts from pressures
!                                    in hectoPascals to molecules/m^3.
!
!       number_density_to_pressure:  Function to convert gas amounts from number
!                                    densities in molecules/m^3 to hectoPascals.
!
!       ppmv_to_kmol_per_cm2:        Function to convert gas amounts from parts-per-
!                                    million by volume to kilomoles per cm^2.
!
!       kmol_per_cm2_to_ppmv:        Function to convert gas amounts from kilomoles
!                                    per cm^2 to parts-per-million by volume.
!
!       effective_layer_tp:          Function to calculate the effective (or density
!                                    weighted) temperature and pressure for an
!                                    atmospheric layer.
!
!       geopotential_height:         Function to calculate the geopotential height
!                                    for an input profile.
!
!       create_sublevels:            Function to create the sublevels required to
!                                    accurately integrate gas amounts within a layer.
!
!       integrate_sublevels:         Function to integrate the temperature and gas
!                                    amounts to produce average layer values.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-May-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2001 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------


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




!------------------------------------------------------------------------------
!S+
! NAME:
!       mw_air
!
! PURPOSE:
!       Function to calculate the effective molecular weight of air
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = mw_air( pressure,          &  ! Input
!                        water_vapor,       &  ! Input
!                        water_vapor_units, &  ! Input
!                        message_log )      &  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Atmospheric pressure
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       water_vapor:       Water vapor concentration
!                          UNITS:      g/kg, ppmv, or hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       water_vapor_units: Flag to select units of water vapor input.
!                          If = 1, units are g/kg
!                             = 2, units are ppmv
!                             = 3, units are hPa (partial pressure)
!                          UNITS:      None
!                          TYPE:       Integer
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the effective molecular weight of air in grams.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The input water vapor values are converted to partial pressure if
!       required. The change in the effective molecular weight of dry air
!       due to water vapor is given by:
!
!                      pp_h2o * ( MW_H2O - MW_DRYAIR )
!         d(mw_air) = ---------------------------------
!                               pressure
!
!       and the final result is given by:
!
!         mw_air = MW_DRYAIR + d(mw_air)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-May-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank-1 version
!##############################################################################

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       density
!
! PURPOSE:
!       Function to calculate gas density using the ideal gas law.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = density( pressure,          &  ! Input
!                         temperature,       &  ! Input
!                         molecular_weight,  &  ! Input
!                         message_log )      &  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Pressure of gas
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       temperature:       Temperature of gas
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       molecular_weigth:  Molecular weight of the gas.
!                          UNITS:      g.mol^-1
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the density in units of kg.m^-3
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The density is calculated using the ideal gas equation
!
!                    p . MW
!         density = --------
!                    R . T
!
!       where R = universal gas constant.
!
!       Units:
!       ------
!       pressure           : hPa == 100 Pa == 100 kg.m^-1.s^-2
!       molecular_weight   : g.mol^-1 == 0.001 kg.mol^-1
!       MOLAR_GAS_CONSTANT : J.K^-1.mol^-1 == kg.m^2.s^-2.K^-1.mol^-1
!       temperature        : K
!
!                  100 kg.m^-1.s^-2 . 0.001 kg.mol^-1
!       density = -----------------------------------
!                     kg.m^2.s^-2.K^-1.mol^-1 . K
!
!                  0.1 kg^2.m^-1.s^-2
!               = --------------------
!                      kg.m^2.s^-2
!
!               = 0.1 kg.m^-3
!
!       Thus the result is scaled by 0.1 to return density in units
!       of kg.m^-3.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-May-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                   Rank-1 version with scalar molecular weight
!##############################################################################

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



!##############################################################################
!                   Rank-1 version with Rank-1 molecular weight
!##############################################################################

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









!------------------------------------------------------------------------------
!S+
! NAME:
!       svp_water
!
! PURPOSE:
!       Function to calculate the saturation vapor pressure over water.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = svp_water( temperature,              &  ! Input
!                           message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       temperature:      Temperatures for which the saturation vapor
!                         pressure is required.
!                         UNITS:      Kelvin
!                         TYPE:       Floating point
!                         DIMENSION:  Scalar or Rank-1 (K x 1)
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       Character
!                         DIMENSION:  Scalar, LEN = *
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the saturation vapor pressure over water in units
!       of hPa.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       Valid temperature range is 188K - 343K (-85C - +70C). A warning is
!       reported if the input temperatures are outside this range.
!
! PROCEDURE:
!       Flatau,P.J., R.L.Walko, and W.R.Cotton, 1992: "Polynomial fits to
!         saturation vapor pressure", J.Appl.Met., v31, pp1507-1513
!
!                           __ N
!                          \            i
!         svp_water = c0 +  >   c(i) . T
!                          /__ 
!                             i=1
!
!       where the c(i) are the relative error norm coefficients obtained
!       from the reference above.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Apr-1998
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank-1 version
!##############################################################################

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






!------------------------------------------------------------------------------
!S+
! NAME:
!       svp_ice
!
! PURPOSE:
!       Function to calculate the saturation vapor pressure over ice
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = svp_ice( temperature,              &  ! Input
!                         message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       temperature:      Temperatures for which the saturation vapor
!                         pressure is required.
!                         UNITS:      Kelvin
!                         TYPE:       Floating point
!                         DIMENSION:  Scalar or Rank-1 (K x 1)
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       Character
!                         DIMENSION:  Scalar, LEN = *
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the saturation vapor pressure over ice in units
!       of hPa.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       Valid temperature range is 183K - 273K (-90C - 0C). An warning is
!       reported if the input temperatures are outside this range.
!
! PROCEDURE:
!       Flatau,P.J., R.L.Walko, and W.R.Cotton, 1992: "Polynomial fits to
!         saturation vapor pressure", J.Appl.Met., v31, pp1507-1513
!
!                         __ N
!                        \            i
!         svp_ice = c0 +  >   c(i) . T
!                        /__ 
!                           i=1
!
!       where the c(i) are the relative error norm coefficients obtained
!       from the reference above.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Apr-1998
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank-1 version
!##############################################################################

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




!------------------------------------------------------------------------------
!S+
! NAME:
!       virtual_temperature
!
! PURPOSE:
!       Function to calculate the virtual_temperature
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = virtual_temperature( temperature,              &  ! Input
!                                     water_vapor,              &  ! Input
!                                     water_vapor_units,        &  ! Input
!                                     pressure = pressure,      &  ! Optional input
!                                     message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       temperature:       Atmospheric temperature.
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
!       water_vapor:       Water vapor concentration
!                          UNITS:      g/kg, ppmv, or hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       water_vapor_units: Flag to select units of water vapor input.
!                          If = 1, units are g/kg
!                             = 2, units are ppmv
!                             = 3, units are hPa (partial pressure)
!                          UNITS:      None
!                          TYPE:       Integer
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       pressure:          Atmospheric pressure. This argument must be supplied
!                          if the input water vapor units are hPa (partial
!                          pressure)
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the virtual temperature in Kelvin.
!
!       If an error occurs, -1.0 is returned.
!
! MODULES:
!       None.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The virtual temperature, the temperature that dry air must have in
!       order to have the same density as moist air at the same pressure, is
!       calculated using:
!
!                  [    eps + w    ]
!         Tv = T * [ ------------- ]     .......................(1)
!                  [ eps ( 1 + w ) ]
!
!       where T   = temperature,
!             w   = water vapour mixing ratio, and
!             eps = ratio of the molecular weights of water and dry air.
!
!       An approximation to eqn.(1) is,
!
!                  [      1 - eps    ]
!         Tv = T * [ 1 + --------- w ]     .....................(2)
!                  [        eps      ]
!
!       however, depending on what accuracy is required (keeping in mind that
!       water vapor measurements are probably good to 2-5%), eqn.(2) can 
!       differ from (1) by around 0.06-0.08K near the surface.
!
!       If virtual temperature is used to calculate geopotential heights,
!       this difference can lead to errors of up to 0.6-0.7m.
!
!       So I took the easy but slightly more computationally expensive road
!       and use eqn.(1).
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-May-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank-1 version
!##############################################################################

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       saturation_mixing_ratio
!
! PURPOSE:
!       Function to calculate the saturation mixing ratio for a given
!       pressure and temperature
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = saturation_mixing_ratio( pressure,                          &  ! Input
!                                         temperature,                       &  ! Input
!                                         ice_temperature = ice_temperature, &  ! optional input
!                                         min_pressure    = min_pressure,    &  ! Optional input
!                                         message_log     = message_log      )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Total atmospheric pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       temperature:       Atmospheric temperature.
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ice_temperature:   Temperature below which the saturation vapor
!                          pressure over ice is used in the conversion.
!                          By default, only the saturation vapor pressure
!                          over water is used.
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!   
!       min_pressure:      Pressure value below which the saturation
!                          mixing ratio is not calculated. The default
!                          is 50hPa. Saturation mixing ratios below the
!                          minimum pressure are set to zero.
!
!                          Note that above 50mb, the saturation vapour pressure,
!                          which is based only on temperature, can exceed the
!                          total air pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!   
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the saturation mixing ratio in g/kg.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       svp_water:       Function to calculate the saturation vapor pressure
!                        over water.
!
!       svp_ice:         Function to calculate the saturation vapor pressure
!                        over ice
!
!       display_message: Subroutine to output messages
!                        SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The saturation mixing ratio can be defined as:
!
!                rho_vs
!          ws = --------     .....(1)
!                rho_d
!
!       where rho_vs = the partial density of water vapour required to 
!                      saturate air with respect to water at a temperature, T
!             rho_d  = the partial density of dry air.
!
!       Equation (1) can be rewritten as:
!
!                  es
!               ---------
!                R_v . T
!         ws = ------------
!                p - es
!               ---------
!                R_d . T
!
!               R_d       es
!            = ----- . --------
!               R_v     p - es
!
!               M_w       es
!            = ----- . --------     .....(2)
!               M_d     p - es
!
!       where M_w = molecular weight of water
!             M_d = molecular weight of dry air
!             es  = water vapor partial pressure
!             p   = total air pressure
!             R_d = gas constant for dry air
!             R_v = gas constant for water vapor
!
!       The units of equation (2) are:
!
!               g     hPa
!         ws = --- . -----
!               g     hPa
!
!                      g
!            = 1000.0 ----
!                      kg
!
!       A factor of 1000 is used to return values in units of g/kg.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-1998
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       rh_to_mr
!
! PURPOSE:
!       Function to convert relative humidity to mixing ratio
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = rh_to_mr( pressure,                          &  ! Input
!                          temperature,                       &  ! Input
!                          relative_humidity,                 &  ! Input
!                          ice_temperature = ice_temperature, &  ! optional input
!                          min_pressure    = min_pressure,    &  ! Optional input
!                          message_log     = message_log      )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Total atmospheric pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       temperature:       Atmospheric temperature.
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       relative_humidity: Atmospheric relative humidity.
!                          UNITS:      %
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       ice_temperature:   Temperature below which the saturation vapor
!                          pressure over ice is used in the conversion.
!                          By default, only the saturation vapor pressure
!                          over water is used.
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!   
!       min_pressure:      Pressure value below which the saturation
!                          mixing ratio is not calculated. The default
!                          is 50mb. Saturation mixing ratios below the
!                          minimum pressure are set to zero.
!
!                          Note that above 50mb, the saturation vapour pressure,
!                          which is based only on temperature, can exceed the
!                          total air pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!   
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the water vapor mixing ratio in g/kg.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       saturation_mixing_ratio: Function to calculate the saturation mixing
!                                ratio for a given pressure and temperature.
!
!       display_message:         Subroutine to output messages
!                                SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       Once the saturation mixing ratio is calculated the mixing ratio
!       corresponding to the input relative humidity is determined using:
!
!                       relative_humidity * saturation_mixing_ratio
!       mixing_ratio = ---------------------------------------------
!                                        100
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-1999
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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




!------------------------------------------------------------------------------
!S+
! NAME:
!       mr_to_rh
!
! PURPOSE:
!       Function to convert water vapor mixing ratio to relative humidity
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = mr_to_rh( pressure,                          &  ! Input
!                          temperature,                       &  ! Input
!                          mixing_ratio,                      &  ! Input
!                          ice_temperature = ice_temperature, &  ! optional input
!                          min_pressure    = min_pressure,    &  ! Optional input
!                          message_log     = message_log      )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Total atmospheric pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       temperature:       Atmospheric temperature.
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       mixing_ratio:      Water vapor mixing ratio.
!                          UNITS:      g/kg
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       ice_temperature:   Temperature below which the saturation vapor
!                          pressure over ice is used in the conversion.
!                          By default, only the saturation vapor pressure
!                          over water is used.
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!   
!       min_pressure:      Pressure value below which the saturation
!                          mixing ratio is not calculated. The default
!                          is 50mb. Saturation mixing ratios below the
!                          minimum pressure are set to zero.
!
!                          Note that above 50mb, the saturation vapour pressure,
!                          which is based only on temperature, can exceed the
!                          total air pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!   
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the relative humidity in %.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       saturation_mixing_ratio: Function to calculate the saturation mixing
!                                ratio for a given pressure and temperature.
!
!       display_message:         Subroutine to output messages
!                                SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       Once the saturation mixing ratio is calculated the relative humidity
!       corresponding to the input mixing ratio is determined using:
!
!                                         mixing_ratio
!       relative_humidity = 100.0 * -------------------------
!                                    saturation_mixing_ratio
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-1999
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       mr_to_ppmv
!
! PURPOSE:
!       Function to convert gas concentrations from mixing ratio in g/kg
!       to ppmv
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = mr_to_ppmv( mixing_ratio,             &  ! Input
!                            molecular_weight,         &  ! Input
!                            message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       mixing_ratio:      Mass mixing ratio of gas.
!                          UNITS:      g/kg
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       molecular_weight:  Molecular weight of gas species.
!                          UNITS:      g.mol^-1
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the gas concentration in ppmv.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       To convert mixing ratio to parts-per-million, the following
!       is used:
!
!         ppmv = 1000.0 * mr * MW_DRYAIR / molecular_weight
!
!       The factor of 1000 derives from the product of the g/kg to g/g
!       scale factor (0.001) and the "parts-per" to "parts-per-million"
!       scale factor (1.0e+06)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-May-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       ppmv_to_mr
!
! PURPOSE:
!       Function to convert gas concentrations from mixing ratio in g/kg
!       to ppmv
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = ppmv_to_mr( ppmv,                     &  ! Input
!                            molecular_weight,         &  ! Input
!                            message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       ppmv:      Mass mixing ratio of gas.
!                          UNITS:      g/kg
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       molecular_weight:  Molecular weight of gas species.
!                          UNITS:      g.mol^-1
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the gas concentration mixing ratio in g/kg.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       To convert ppmv to mixing ratio, the following is used:
!
!         mr = 0.001 * ppmv * molecular_weight / MW_DRYAIR
!
!       The factor of 1000 derives from the product of the g/g to g/kg
!       scale factor (1000) and the "parts-per-million" to "parts-per"
!       scale factor (1.0e-06)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-May-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       ppmv_to_pp
!
! PURPOSE:
!       Function to convert gas concentrations from ppmv to partial pressure
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = ppmv_to_pp( pressure,                 &  ! Input
!                            ppmv,                     &  ! Input
!                            message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Total pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       ppmv:              Concentration in ppmv.
!                          UNITS:      ppmv
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the gas partial pressure in hPa.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       To convert parts-per-million to partial pressure, the following
!       is used:
!
!         pp = pressure * 1.0e-06 * ppmv
!
!       The factor of 1.0e-06 converts the parts-per-million to parts-per
!       which is then used to scale the total pressure to get the partial
!       pressure.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-May-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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




!------------------------------------------------------------------------------
!S+
! NAME:
!       pp_to_ppmv
!
! PURPOSE:
!       Function to convert gas concentrations from partial pressure to ppmv
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = pp_to_ppmv( pressure,                 &  ! Input
!                            pp,                       &  ! Input
!                            message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Total pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       pp:                Gas partial pressure
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the gas concentration in ppmv.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       To convert partial pressure to parts-per-million, the following
!       is used:
!
!         ppmv = 1.0e+06 * pp / pressure
!
!       The factor of 1.0e+06 converts the parts-per to parts-per-million.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-May-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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




!------------------------------------------------------------------------------
!S+
! NAME:
!       pressure_to_number_density
!
! PURPOSE:
!       Function to convert gas concentrations in pressure units to 
!       number density.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = pressure_to_number_density( pressure,                 &  ! Input
!                                            temperature,              &  ! Input
!                                            message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Total or partial pressure to provide number density
!                          of air or specific gas species.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
!       temperature:       Atmospheric temperature
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the gas concentration in molecules/m^3.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The ideal gas law is
!
!         p.V = n.R.T
!
!       where,
!         p = pressure
!         V = volume
!         n = number of moles of the gas,
!         R = molar gas constant,
!         T = temperature.
!
!       This can be written in terms of the number of molecules, N,
!
!                 N
!         p.V = ---- . R.T                  ..............(1)
!                NA
!
!       where NA = Avogadro's constant (number of molecules in 1 mole.)
!
!       Eqn.(1) can be recast to provide the number of molecules in the 
!       volume, the number density, nd,
!
!               N     p.NA
!         nd = --- = ------  molecules/m^3  ..............(2)
!               V     R.T
!
!       At standard temperature and pressure (T0=273.15K, p0=101325Pa), this
!       number density is know as the Loschmidt constant, L0, the molecular
!       density of 1 mole of an ideal gas. Thus we have the generic form of
!       eqn.(2) and the STP form,
!
!               p0.NA
!         L0 = -------  molecules/m^3       ..............(3)
!               R.T0
!
!       Taking the ratio of eqns.(2) and (3) gives,
!       
!         nd    p.NA     R.T0
!         -- = ------ . -------
!         L0    R.T      p0.NA
!
!       and rearranging gives,
!
!                    p      T0
!         nd = L0 . ---- . ----  molecules/m^3
!                    p0     T 
!
!       The value of p0 used in this routine is expressed in pascals (Pa) whereas
!       the input partial pressure is expected to be hPa (more common unit). Thus
!       there is a factor of 100 to include,
!
!                    100.p     T0
!         nd = L0 . ------- . ----  molecules/m^3
!                     p0       T 
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Nov-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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




!------------------------------------------------------------------------------
!S+
! NAME:
!       number_density_to_pressure
!
! PURPOSE:
!       Function to convert gas concentrations from number density to pressure.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = number_density_to_pressure( number_density,           &  ! Input
!                                            temperature,              &  ! Input
!                                            message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       number_density:    Molecular density.
!                          UNITS:      molecules/m^3
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
!       temperature:       Atmospheric temperature
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the gas concentration in hectoPascals.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The ideal gas law is
!
!         p.V = n.R.T
!
!       where,
!         p = pressure
!         V = volume
!         n = number of moles of the gas,
!         R = molar gas constant,
!         T = temperature.
!
!       This can be written in terms of the number of molecules, N,
!
!                 N
!         p.V = ---- . R.T                  ..............(1)
!                NA
!
!       where NA = Avogadro's constant (number of molecules in 1 mole.)
!
!       Recasting eqn.(1) to provide the pressure,
!
!              nd
!         p = ---- . R.T  Pa                ..............(2)
!              NA
!
!       where nd = the number density in molecules/m^3 = N/V.
!
!       At standard temperature and pressure (T0=273.15K, p0=101325Pa), the
!       number density of eqn.(2) is known as the Loschmidt constant, L0,
!       the molecular density of 1 mole of an ideal gas. Thus we have the
!       generic form of eqn.(2) and the STP form,
!
!               L0
!         p0 = ---- . R.T0  Pa              ..............(3)
!               NA
!
!       Taking the ratio of eqns.(2) and (3) gives,
!       
!          p      nd     T
!         ---- = ---- . ----  Pa
!          p0     L0     T0
!
!       and rearranging gives,
!
!                   nd      T
!         p = p0 . ---- . ----  Pa
!                   L0     T0 
!
!       The value of p0 used in this routine is expressed in pascals (Pa) whereas
!       the output pressure is returned as hPa (more common unit). Thus there
!       is a factor of 100 to include,
!
!                          nd      T
!         p = 0.01 . p0 . ---- . ----  hPa
!                          L0     T0 
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Nov-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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




!------------------------------------------------------------------------------
!S+
! NAME:
!       ppmv_to_number_density
!
! PURPOSE:
!       Function to convert gas concentrations in units of ppmv to 
!       number density.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = ppmv_to_number_density( pressure,                 &  ! Input
!                                        temperature,              &  ! Input
!                                        ppmv,                     &  ! Input
!                                        message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Total atmospheric pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
!       temperature:       Atmospheric temperature
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
!       ppmv:              Gas concentration in ppmv.
!                          UNITS:      ppmv
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the gas concentration in molecules/m^3.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:             Subroutine to output messages
!                                    SOURCE: error_handler module
!
!       pressure_to_number_density:  Function to calculate number density
!                                    given the pressure.
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The total airs density is calculated,
!
!                          p      T0
!         total_nd = L0 . ---- . ----  molecules/m^3
!                          p0     T 
!
!       The supplied gas concentration in ppmv is then converted to
!       a number density using,
!
!         nd = 1.0e-06 * ppmv * total_nd  molecules/m^3
!
!       with the factor of 10^6 to convert from part-per-million to 
!       parts-per.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Nov-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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




!------------------------------------------------------------------------------
!S+
! NAME:
!       number_density_to_ppmv
!
! PURPOSE:
!       Function to convert gas concentrations from number density to ppmv.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = number_density_to_ppmv( pressure,                 &  ! Input
!                                        temperature,              &  ! Input
!                                        number_density,           &  ! Input
!                                        message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Total atmospheric pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
!       temperature:       Atmospheric temperature
!                          UNITS:      Kelvin
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
!       number_density:    Molecular density.
!                          UNITS:      molecules/m^3
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the gas concentration in ppmv.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:             Subroutine to output messages
!                                    SOURCE: error_handler module
!
!       pressure_to_number_density:  Function to calculate number density
!                                    given the pressure.
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The total airs density is calculated,
!
!                          p      T0
!         total_nd = L0 . ---- . ----  molecules/m^3
!                          p0     T 
!
!       The supplied gas number density, nd, is then converted to
!       ppmv using,
!                              nd
!         ppmv = 1.0e+06 * ----------
!                           total_nd
!
!       with the factor of 10^6 to convert from part-per- to 
!       parts-per-million.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Nov-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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




!------------------------------------------------------------------------------
!S+
! NAME:
!       ppmv_to_kmol_per_cm2
!
! PURPOSE:
!       Function to convert layer gas concentrations from ppmv to kmol.cm^-2.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = ppmv_to_kmol_per_cm2( pressure,                 &  ! Input
!                                      temperature,              &  ! Input
!                                      delta_z,                  &  ! Input
!                                      ppmv,                     &  ! Input
!                                      message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Average layer pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       temperature:       Average layer temperature
!                          UNITS:      Kelvin (K)
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       delta_z:           Layer thickness
!                          UNITS:      metres (m)
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       ppmv:              Average layer gas concentration
!                          UNITS:      ppmv
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the layer gas concentration in kmol.cm^-2
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The number density of a particular molecular species given it's ppmv
!       concentration is given by,
!
!                                     p     T0
!         nd = 1.0e-6 . ppmv . L0 . ---- . ----  molecules.m^-3
!                                    p0      T
!
!                                     p     T0       1
!            = 1.0e-6 . ppmv . L0 . ---- . ---- . -------  molecules.cm^-2.m^-1
!                                    p0      T     100^2
!
!       where,
!         p  = pressure (p0 = 101325Pa)
!         T  = temperature (T0 = 273.15K)
!         L0 = Loschmidt contant.
!
!       Given the layer thickness in metres, dz, we obtain the column
!       density, cd,
!
!                                           p     T0  
!         cd = 1.0e-10 . dz . ppmv . L0 . ---- . ----  molecules.cm^-2
!                                          p0      T
!
!       Dividing by Avogadro's constant, we get the column density in terms
!       of moles of the gas, and a further factor of 10^-3 gives kilomoles,
!
!                                     L0      p     T0  
!         cd = 1.0e-13 . dz . ppmv . ---- . ---- . ----  kmoles.cm^-2     .....(1)
!                                     Na     p0      T
!
!       The ideal gas law, at STP, can be written as,
!
!               L0
!         p0 = ---- . R . T0
!               Na
!
!       and rearranging to provide an expression for L0 gives,
!
!               p0 . Na
!         L0 = ---------     .....(2)
!               R . T0
!
!       Substituting eqn (2) into eqn (1) gives,
!
!                                       p
!         cd = 1.0e-13 . dz . ppmv . -------   kmoles.cm^-2
!                                     R . T
!
!       which involves less calculations than eqn (1) (assuming all the
!       constant values are kept separate...which they are). 
!
!       This assumes that pressure will be specified in Pascals (SI). The
!       pressure is input as hectoPascals so there is a factor of 100 to
!       include,
!
!                                     100.p
!         cd = 1.0e-13 . dz . ppmv . -------   kmoles.cm^-2
!                                     R . T
!
!       to give the equation used in this routine for the specified
!       input units,
!
!                                       p
!         cd = 1.0e-11 . dz . ppmv . -------   kmoles.cm^-2
!                                     R . T
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Jan-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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




!------------------------------------------------------------------------------
!S+
! NAME:
!       kmol_per_cm2_to_ppmv
!
! PURPOSE:
!       Function to convert layer gas concentrations from kmol.cm^-2 to ppmv.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = kmol_per_cm2_to_ppmv( pressure,                 &  ! Input
!                                      temperature,              &  ! Input
!                                      delta_z,                  &  ! Input
!                                      kmol_per_cm2,             &  ! Input
!                                      message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       pressure:          Average layer pressure.
!                          UNITS:      hPa
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       temperature:       Average layer temperature
!                          UNITS:      Kelvin (K)
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       delta_z:           Layer thickness
!                          UNITS:      metres (m)
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!
!       kmol_per_cm2:      Average layer gas concentration
!                          UNITS:      kmol.cm^-2
!                          TYPE:       Floating point
!                          DIMENSION:  Scalar or Rank-1 (K x 1)
!                          ATTRIBUTES: INTENT( IN )
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       Character
!                          DIMENSION:  Scalar, LEN = *
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns the layer gas concentration in ppmv
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:   Subroutine to output messages
!                          SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The number density of a particular molecular species given it's ppmv
!       concentration is given by,
!
!                                     p     T0
!         nd = 1.0e-6 . ppmv . L0 . ---- . ----  molecules.m^-3
!                                    p0      T
!
!                                     p     T0       1
!            = 1.0e-6 . ppmv . L0 . ---- . ---- . -------  molecules.cm^-2.m^-1
!                                    p0      T     100^2
!
!       where,
!         p  = pressure (p0 = 101325Pa)
!         T  = temperature (T0 = 273.15K)
!         L0 = Loschmidt contant.
!
!       Given the layer thickness in metres, dz, we obtain the column
!       density, cd,
!
!                                           p     T0  
!         cd = 1.0e-10 . dz . ppmv . L0 . ---- . ----  molecules.cm^-2
!                                          p0      T
!
!       Dividing by Avogadro's constant, we get the column density in terms
!       of moles of the gas, and a further factor of 10^-3 gives kilomoles,
!
!                                     L0      p     T0  
!         cd = 1.0e-13 . dz . ppmv . ---- . ---- . ----  kmoles.cm^-2     .....(1)
!                                     Na     p0      T
!
!       The ideal gas law, at STP, can be written as,
!
!               L0
!         p0 = ---- . R . T0
!               Na
!
!       and rearranging to provide an expression for L0 gives,
!
!               p0 . Na
!         L0 = ---------     .....(2)
!               R . T0
!
!       Substituting eqn (2) into eqn (1) gives,
!
!                                       p
!         cd = 1.0e-13 . dz . ppmv . -------   kmoles.cm^-2
!                                     R . T
!
!       which involves less calculations than eqn (1) (assuming all the
!       constant values are kept separate...which they are). 
!
!       This assumes that pressure will be specified in Pascals (SI). The
!       pressure is input as hectoPascals so there is a factor of 100 to
!       include,
!
!                                     100.p
!         cd = 1.0e-13 . dz . ppmv . -------   kmoles.cm^-2
!                                     R . T
!
!                                       p
!            = 1.0e-11 . dz . ppmv . -------   kmoles.cm^-2
!                                     R . T
!
!       for pressure in hectoPascals. Inverting this equation to provide 
!       the ppmv amount for a given column density gives the equation used
!       in this routine,
!
!                           cd . R . T
!         ppmv = 1.0e+11 . ------------
!                             p . dz
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Jan-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

!##############################################################################
!                              Scalar version
!##############################################################################

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



!##############################################################################
!                              Rank1 version
!##############################################################################

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       effective_layer_tp
!
! PURPOSE:
!       Function to calculate the effective atmospheric layer temperature and
!       pressure by weighting level values with the integrated layer density.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = effective_layer_tp( height,                   &  ! Input
!                                    pressure,                 &  ! Input
!                                    temperature,              &  ! Input
!                                    water_vapor,              &  ! Input
!                                    water_vapor_units,        &  ! Input
!                                    effective_pressure,       &  ! Output
!                                    effective_temperature,    &  ! Output
!                                    message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       height:                 Heights of the atmospheric levels.
!                               UNITS:      m
!                               TYPE:       Floating point
!                               DIMENSION:  Rank-1 (K x 1), K>1
!                               ATTRIBUTES: INTENT( IN )
!
!       pressure:               Pressure of the atmospheric levels.
!                               UNITS:      hPa
!                               TYPE:       Floating point
!                               DIMENSION:  Rank-1 (K x 1), K>1
!                               ATTRIBUTES: INTENT( IN )
!
!       temperature:            Temperature of the atmospheric levels.
!                               UNITS:      Kelvin
!                               TYPE:       Floating point
!                               DIMENSION:  Rank-1 (K x 1), K>1
!                               ATTRIBUTES: INTENT( IN )
!
!       water_vapor:            Water vapor concentration at the atmospheric levels
!                               UNITS:      g/kg, ppmv, or hPa
!                               TYPE:       Floating point
!                               DIMENSION:  Rank-1 (K x 1), K>1
!                               ATTRIBUTES: INTENT( IN )
!
!       water_vapor_units:      Flag to select units of water vapor input.
!                               If = 1, units are g/kg
!                                  = 2, units are ppmv
!                                  = 3, units are hPa (partial pressure)
!                               UNITS:      None
!                               TYPE:       Integer
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
!   
! OPTIONAL INPUT ARGUMENTS:
!       message_log:            Character string specifying a filename in which any
!                               messages will be logged. If not specified, or if an
!                               error occurs opening the log file, the default action
!                               is to output messages to standard output.
!                               UNITS:      None
!                               TYPE:       Character
!                               DIMENSION:  Scalar, LEN = *
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       effective_pressure:     Effective layer pressure.
!                               UNITS:      hPa
!                               TYPE:       Floating point
!                               DIMENSION:  Rank-1 (K-1 x 1)
!                               ATTRIBUTES: INTENT( OUT )
!
!       effective_temperature:  Effective layer temperature.
!                               UNITS:      Kelvin
!                               TYPE:       Floating point
!                               DIMENSION:  Rank-1 (K-1 x 1)
!                               ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the calculation was successful.
!                 = FAILURE an error occurred.
!
!       Error codes are defined in the error_handler module.
!
!
! CALLS:
!       mw_air:             Function to calculate the effective molecular
!                           weight of air weighted by the water vapor amount.
!
!       density:            Function to calculate gas density using the ideal
!                           gas law.
!
!       display_message:    Subroutine to output messages
!                           SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       Based on:
!
!       Gallery, W.O., F.X. Kneizys, and S.A. Clough, "Air mass computer
!         program for atmospheric transmittance/radiance calculation: FSCATM",
!         AFGL-TR-83-0065, 9 March 1983.
!
!       The effective pressure and temperature is defined as,
!
!         _    sum{ p.rho.dz }
!         p = -----------------     ..............................................(1)
!               sum{ rho.dz }
!
!       and
!
!         _    sum{ T.rho.dz }
!         T = -----------------     ..............................................(2)
!               sum{ rho.dz }
!
!       where dz == layer thickness.
!
!       Note that the quantity sum{ rho.dz } can also be referred to as the
!       column density.
!
!       The pressure and total density are both assumed to follow an exponential
!       profile with scale heights H_p and H_rho respectively. For a single layer
!       the numerator of eqn(1) can be written as,
!
!          l                  H_p.H_rho
!         sum{ p.rho.dz } = ------------- ( p(l-1).rho(l-1) - p(l).rho(l) )  .....(3)
!         l-1                H_p + H_rho
!
!       Similarly for the numerator of eqn(2) using the ideal gas law,
!       p = R_air.rho.T, we get
!
!          l                  H_p
!         sum{ T.rho.dz } = -------( p(l-1) - p(l) )     .........................(4)
!         l-1                R_air
!
!       and the denominator is given by,
!
!          l
!         sum{ rho.dz } = H_rho ( rho(l-1) - rho(l) )    .........................(5)
!         l-1
!
!       where the scale heights are defined as,
!
!                -( z(l) - z(l-1 ) )        
!         H_p = ---------------------     ........................................(6)
!                ln( p(l) / p(l-1) )        
!
!       and
!
!                    -( z(l) - z(l-1 ) )
!         H_rho = -------------------------     ..................................(7)
!                  ln( rho(l) / rho(l-1) )
!
!
!       Note that in eqn.(4) the gas constant is that for *air*, not *dry air*. To
!       determine this the effective molecular weight of air (as a function of pressure)
!       must be determined.
!
!       Breaking down the units of the components, 
!
!         units(p)  = hPa
!                   = 100 Pa
!                   = 100 N.m-2
!                   = 100 kg.m.s-2.m-2
!                   = 100 kg.m-1.s-2
!
!                          m2
!         units(eqn(3)) = ----( 100 kg.m-1.s-2  .  kg.m-3 )
!                          m 
!
!                       = 100 kg2.m-3.s-2
!
!                          m  . 100 kg.m-1.s-2
!         units(eqn(4)) = ---------------------
!                              J.g-1.K-1
!
!                          m  . 100 kg.m-1.s-2
!                       = ---------------------
!                           kg.m2.s-2.g-1.K-1
!
!                       = 100 K.g.m-2
!                       = 0.1 K.kg.m-2
!
!         units(eqn(5)) = m  .  kg.m-3
!                       = kg.m-2  
!                     
!       So the units of the final equations are:
!
!                          units(eqn(3))
!         units(eqn(1)) = ---------------
!                          units(eqn(5))  
!
!                          100 kg2.m-3.s-2
!                       = -----------------
!                             kg.m-2  
!
!                       = 100 kg.m-1.s-2
!                       = 100 kg.m.s-2.m-2
!                       = 100 N.m-2
!                       = 100 Pa
!                       = hPa
!
!                          units(eqn(4))
!         units(eqn(2)) = ---------------
!                          units(eqn(5))  
!
!                          0.1 K.kg.m-2
!                       = --------------
!                            kg.m-2  
!
!                       = 0.1 K
!
!       So the final temperatures must be multiplied by 0.1 to get units of K.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC, 03-May-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       geopotential_height
!
! PURPOSE:
!       Function to calculate geopotential height using the hypsometric
!       equation.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = geopotential_height( pressure,                                            &  ! Input
!                                     temperature,                                         &  ! Input
!                                     water_vapor,                                         &  ! Input
!                                     water_vapor_units,                                   &  ! Input
!                                     height,                                              &  ! Output
!                                     surface_height           = surface_height,           &  ! Optional input
!                                     gravity_correction       = gravity_correction,       &  ! Optional input
!                                     latitude                 = latitude,                 &  ! Optional input
!                                     zonal_wind_velocity      = zonal_wind_velocity,      &  ! Optional input
!                                     meridional_wind_velocity = meridional_wind_velocity, &  ! Optional input
!                                     message_log              = message_log               )  ! Optional input
!
!
! INPUT ARGUMENTS:
!       pressure:                  Pressure of the atmospheric levels.
!                                  UNITS:      hPa
!                                  TYPE:       Floating point
!                                  DIMENSION:  Rank-1 (K x 1)
!                                  ATTRIBUTES: INTENT( IN )
!
!       temperature:               Temperature of the atmospheric levels.
!                                  UNITS:      Kelvin
!                                  TYPE:       Floating point
!                                  DIMENSION:  Rank-1 (K x 1)
!                                  ATTRIBUTES: INTENT( IN )
!
!       water_vapor:               Water vapor concentration at the atmospheric levels
!                                  UNITS:      g/kg, ppmv, or hPa
!                                  TYPE:       Floating point
!                                  DIMENSION:  Rank-1 (K x 1)
!                                  ATTRIBUTES: INTENT( IN )
!
!       water_vapor_units:         Flag to select units of water vapor input.
!                                  If = 1, units are g/kg
!                                     = 2, units are ppmv
!                                     = 3, units are hPa (partial pressure)
!                                  UNITS:      None
!                                  TYPE:       Integer
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       surface_height:            Height corresponding to the first element of the
!                                  input arrays. If not specified, the default value
!                                  is 0.0m.
!                                  UNITS:      metres (m).
!                                  TYPE:       Floating point
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       gravity_correction:        Set this argument to use a gravity profile rather
!                                  than standard reference gravity in calculating
!                                  the geopotential heights. If PRESENT then,
!                                  if == 0, standard gravity used,
!                                     /= 0, gravity profile is calculated and used.
!                                  UNITS:      None.
!                                  TYPE:       Integer
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       latitude:                  Set this argument to the latitude corresponding
!                                  to the input profile location. This argument is
!                                  ignored if the GRAVITY_CORRECTION argument is
!                                  not set.
!                                  UNITS:      Degrees (not N or S hemisphere dep.).
!                                  TYPE:       Floating point
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       zonal_wind_velocity:       Set this argument to the zonal wind velocities
!                                  at the input profile levels. This argument is
!                                  ignored if the GRAVITY_CORRECTION argument is
!                                  not set.
!                                  UNITS:      m.s^-1, +ve W'ly, -ve E'ly
!                                  TYPE:       Floating point
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       meridional_wind_velocity:  Set this argument to the meridional wind velocities
!                                  at the input profile levels. This argument is
!                                  ignored if the GRAVITY_CORRECTION argument is
!                                  not set.
!                                  UNITS:      m.s^-1, +ve S'ly, -ve N'ly
!                                  TYPE:       Floating point
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       message_log:               Character string specifying a filename in which any
!                                  messages will be logged. If not specified, or if an
!                                  error occurs opening the log file, the default action
!                                  is to output messages to standard output.
!                                  UNITS:      None
!                                  TYPE:       Character
!                                  DIMENSION:  Scalar, LEN = *
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!
! OUTPUT ARGUMENTS:
!       height:                    Geopotential heights of the input pressure levels.
!                                  UNITS:      metres (m)
!                                  TYPE:       Floating point
!                                  DIMENSION:  Rank-1 (K x 1)
!                                  ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the calculation was successful.
!                 = FAILURE an error occurred.
!
!       Error codes are defined in the error_handler module.
!
!
! CALLS:
!       mw_air:             Function to calculate the effective molecular
!                           weight of air weighted by the water vapor amount.
!
!       density:            Function to calculate gas density using the ideal
!                           gas law.
!
!       gravity:            Function to calculate acceleration due to gravity
!                           as a function of latitude and height.
!
!       display_message:    Subroutine to output messages
!                           SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       Geopotential heights are calculated using the hypsometric equation:
!
!                   _   [  p1  ]
!         z2 - z1 = H.ln[ ---- ]     .....(1)
!                       [  p2  ]
!             _
!       where H     = scale height, 
!             p1,p2 = layer boundary pressures, and
!             z1,z2 = layer boundary heights.
!
!       and
!              
!         _    R_air * T 
!         H = -----------     .....(2)
!                  g     
!
!       where R_air = gas constant for moist air
!             T     = average temperature for an atmospheric layer,
!             g     = acceleration due to gravity
!
!       The gas constant for air is given by:
!
!                  1000 * R
!         R_air = ----------     .....(3)
!                   MW_air
!
!       where R0     = Molar gas constant
!             MW_air = Water vapor weighted molecular weight of air
!
!       and the factor of 1000 is for conversion of the molecular
!       weight from g.mol^-1 to kg.mol^-1.
!
!       Layer values for both R_air and T are determined from a density
!       weighted average of adjacent level values:
!
!                    X(k)*rho(k) + X(k-1)*rho(k-1)
!         layer_X = -------------------------------
!                         rho(k) + rho(k-1)
!
!       The use of an optional gravity profile was introduced to make 
!       this code parallel that of the UMBC KLAYERS altitude calculation
!       code.
!
!       Units analysis:
!       ---------------
!
!       Typical units of the quantities are,
!
!         units(T)      = K
!         units(g)      = m.s^-2
!         units(R0)     = J.K^-1.mol^-1 = kg.m^2.s^-2.K^-1.mol^-1
!         units(MW_air) = g.mol^-1
!
!       From eqn(3),
!
!                         kg.m^2.s^-2.K^-1.mol^-1
!         units(R_air) = -------------------------
!                               g.mol^-1
!
!                         1000 kg.m^2.s^-2.K^-1.mol^-1
!                      = ------------------------------
!                                 kg.mol^-1             
!
!                      = 1000 m^2.s^-2.K^-1
!
!       From eqn.(2),
!
!               _     1000 m^2.s^-2.K^-1 . K
!         units(H) = ------------------------
!                            m.s^-2
!
!                   = 1000 m
!
!       So the final heights must be multiplied by 1000 to get units of metres.
!
!
!       If you're wondering why I don't use the virtual temperature rather
!       than calculate MW_air and R_air, the reasons are two-fold:
!         1) To calculate a density using the ideal gas law, you need to
!            know either of these quantities, and
!         2) Eventually, the effect due to ozone will also be included.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Feb-1999
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

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





!------------------------------------------------------------------------------
!
! NAME:
!       gravity
!
! PURPOSE:
!       Function to calculate gravity as a function of height, latitude, and,
!       if supplied, zonal and meridional wind velocity.
!
!       Adapted from the UMBC GRAV.F function supplied with the AIRS RTA.
!
! CATEGORY:
!       Meterology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = gravity( height,                                              &  ! Input
!                         latitude                 = latitude,                 &  ! Optional input
!                         zonal_wind_velocity      = zonal_wind_velocity,      &  ! Optional input
!                         meridional_wind_velocity = meridional_wind_velocity, &  ! Optional input
!                         message_log              = message_log               )  ! Optional input
!
! INPUT ARGUMENTS:
!       height:                    Height at which the gravity value is required.
!                                  UNITS:      metres (m)
!                                  TYPE:       Floating point
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       latitude:                  Set this argument to the latitude at which
!                                  the gravity value is required.
!
!                                  If not defined, the default value is 0.0.
!                                  UNITS:      Degrees
!                                  TYPE:       Floating point
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       zonal_wind_velocity:       Set this argument to the zonal wind
!                                  velocities at the input height.
!
!                                  If not defined, the default value is 0.0.
!                                  UNITS:      m.s^-1, +ve W'ly, -ve E'ly
!                                  TYPE:       Floating point
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       meridional_wind_velocity:  Set this argument to the meridional wind
!                                  velocities at the input height.
!
!                                  If not defined, the default value is 0.0.
!                                  UNITS:      m.s^-1, +ve S'ly, -ve N'ly
!                                  TYPE:       Floating point
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       message_log:               Character string specifying a filename
!                                  in which any messages will be logged.
!                                  If not specified, or if an error occurs
!                                  opening the log file, the default action
!                                  is to output messages to standard output.
!                                  UNITS:      None
!                                  TYPE:       Character
!                                  DIMENSION:  Scalar, LEN = *
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The function returns the acceleration due to gravity in m.s^-2.
!
!       If an error occurs, -1.0 is returned.
!
! CALLS:
!       display_message:    Subroutine to output messages
!                           SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The gravity at some height, z, and latitude, lat, can be expressed as
!
!         g(z,lat) = g_surface(lat).f(lat,z) - a(lat,z)     .....(1)
!
!       where g_surface(lat) = gravity at the Earth's surface due to Earth's
!                              mass only,
!             f(lat,z)       = factor modifying surface gravity due to inverse
!                              square law,
!             a(lat,z)       = centripetal acceleration due to rotation of
!                              mass (atmosphere) at height, z
!
!
!       Normal gravity
!       --------------
!
!       The normal gravity is determined from the International Gravity
!       equation (Moritz, H., "Geodetic Reference System 1980", See: Geodesists
!       Handbook, Bulletin Geodesique, v62,1988),
!
!                                    __ 4
!                              (    \             2i      )
!         g_normal(lat) = g0 * ( 1 + >  c(i) . SIN  (lat) )     .....(2)
!                              (    /__                   ) 
!                                      i=1
!
!       where g0_normal = normal gravity at the equator,
!             c(i)      = coefficients.
!
!       Eqn.2 provides a value for gravity due both to the EArth's mass and
!       rotation, i.e. it contains a surface centripetal acceleration 
!       component,
!
!         g_normal(lat) = g_surface(lat) - a_surface(lat)
!
!       therefore, one gets,
!
!         g_surface(lat) = g_normal(lat) + a_surface(lat)     .....(3)
!
!
!       Centripetal acceleration
!       ------------------------
!
!       The centripetal acceleration at the Earth's surface can be expressed
!       as,
!
!                             V(lat)^2
!         a_surface( lat ) = ----------     .....(4)
!                              Re(lat)
!
!       where V(lat) = speed of rotation of the Earth's surface at a given
!                      latitude,
!
!         V(lat) = 2pi * w * Re(lat) * cos(lat)
!
!       where w = the angular velocity of Earth's surface in rev.s^-1
!       (1/86400 rev.s^-1) and 2pi*w is the same in rad.s^-1
!       (2pi / 86400 rad.s^-1)
!
!       and Re(lat) = Earth's radius at some latitude, lat,
!                         _________________________________________
!                        /
!                       /               2
!                      /               R (polar)
!                     / ------------------------------------------
!                    /                    (          2          )
!         Re(lat) = /            2        (        Re polar     )
!                  /      1 - cos (lat) * ( 1 - --------------- )
!                 /                       (        2            )
!                /                        (      Re equatorial  )
!              \/
!
!       Thus, eqn. (4) becomes
!
!                             ( 2pi * w * Re(lat) * cos(lat) )^2
!         a_surface( lat ) = ------------------------------------
!                                         Re(lat)
!
!                          = ( 2pi * w * cos(lat) )^2 * Re(lat)
!
!
!       To determine the centripetal acceleration at some height z, we
!       assume that the only thing preventing the air mass at z moving
!       at a different speed than that of the rotating Earth is the wind
!       parallel to the Earth's surface. Thus the rotational speed is
!       modified by the zonal, u(z), and meridional wind, v(z), speed,
!
!                     V(lat,z)^2
!         a(lat,z) = ------------     .....(5)
!                      R(lat,z)
!
!       where,
!                       _______________________________________________________
!                      /
!         V(lat,z) =  / [ ( 2pi * w * R(lat,z) * cos(lat) ) + u(z) ]^2 + v(z)^2
!                   \/
!
!       and,
!
!         R(lat,z) = Re(lat) + Z
!
!       So, like before, (5) becomes,
!
!                   [ ( 2pi * w * R(lat,z) * cos(lat) ) + u(z) ]^2 + v(z)^2
!       a(lat,z) = ---------------------------------------------------------
!                                          R(lat,z)
!
!
!       Variation of gravity with altitude
!       ----------------------------------
!
!       From eqn.1, the variation of gravitation due only to height above
!       the surface can be expressed as, 
!
!          g_surface(lat).f(lat,z)
!
!       where, using the inverse square law,
!
!                                  2
!                    (  Re(lat)   )
!         f(lat,z) = ( ---------- )     .....(6)
!                    (  R(lat,z)  )
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Jan-2001
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       create_sublevels
!
! PURPOSE:
!       Function to create the sublevels used to integrate input profiles
!       to obtain average layer quantities. This routine is called before
!       INTEGRATE_SUBLEVELS.
!
!       Adapted from the UMBC INTLEV.F function supplied with the AIRS RTA.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = create_sublevels( level_pressure,           &  ! Input
!                                  level_temperature,        &  ! Input
!                                  level_absorber,           &  ! Input
!                                  n_per_layer,              &  ! Input
!                                  sublevel_pressure,        &  ! Output
!                                  sublevel_temperature,     &  ! Output
!                                  sublevel_absorber,        &  ! Output
!                                  message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       level_pressure:        Pressure of the atmospheric levels.
!                              UNITS:      hPa
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-1 (K x 1)
!                              ATTRIBUTES: INTENT( IN )
!
!       level_temperature:     Temperature of the atmospheric levels.
!                              UNITS:      Kelvin
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-1 (K x 1)
!                              ATTRIBUTES: INTENT( IN )
!
!       level_absorber:        Absorber concentrations at the atmospheric levels
!                              UNITS:      Doesn't matter
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-2 (KxJ)
!                                          K == number of levels
!                                          J == number of absorbers
!                              ATTRIBUTES: INTENT( IN )
!
!       n_per_layer:           Number of sublevels to create in each layer.
!                              Value must be > or = 1.
!                              UNITS:      None
!                              TYPE:       Integer
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:           Character string specifying a filename
!                              in which any messages will be logged.
!                              If not specified, or if an error occurs
!                              opening the log file, the default action
!                              is to output messages to standard output.
!                              UNITS:      None
!                              TYPE:       Character
!                              DIMENSION:  Scalar, LEN = *
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       sublevel_pressure:     Pressure of the atmospheric levels.
!                              UNITS:      hPa
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-1 (Ks x 1)
!                              ATTRIBUTES: INTENT( IN )
!
!       sublevel_temperature:  Temperature of the atmospheric levels.
!                              UNITS:      Kelvin
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-1 (Ks x 1)
!                              ATTRIBUTES: INTENT( IN )
!
!       sublevel_absorber:     Absorber concentrations at the atmospheric levels
!                              UNITS:      Same as input
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-2 (Ks x J)
!                                          Ks == number of sublevels
!                                          J  == number of absorbers
!                              ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the calculation was successful.
!                 = FAILURE an error occurred.
!
!       Error codes are defined in the error_handler module.
!
!
! CALLS:
!       display_message:    Subroutine to output messages
!                           SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The assumption is made that temperature and absorber amount vary
!       linearly with ln(p). To improve the quadrature in integrating level
!       amounts to a layer value, each input layer, k, is split into N(k)
!       sublayers equally spaced in ln(p),
!
!                                 ln(p[k+1]) - ln(p[k]
!         ln(p[n+1]) - ln(p[n] = ---------------------
!                                          N(k)
!
!       given the pressures, p(1) - p(K) of the input levels.
!
!       Once the sublevels are defined, the level temperatures and absorber
!       amounts are linearly interpolated at the specific number of sublevels
!       and those interpolates are associated with the sublevel pressures.
!
!       The last corresponding level/sublevel is assigned explicitly.
!
!       Currently, N is independent of k. That is, the same number of sublevels
!       are created for each layer.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Jan-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       integrate_sublevels
!
! PURPOSE:
!       Function to integrate the sublevel values created by CREATE_SUBLEVELS
!       to provide average layer temperature and absorber amount. Layer pressure
!       is provided by default, i.e. not from integration.
!
!       Adapted from the UMBC INTEG.F function supplied with the AIRS RTA.
!
! CATEGORY:
!       Meteorology
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = integrate_sublevels( sublevel_height,       &  ! Input
!                                     sublevel_pressure,     &  ! Input
!                                     sublevel_temperature,  &  ! Input
!                                     sublevel_absorber,     &  ! Input
!                                     n_per_layer,           &  ! Input
!
!                                     layer_pressure,        &  ! Output
!                                     layer_temperature,     &  ! Output
!                                     layer_absorber,        &  ! Output
!
!                                     message_log )          &  ! Optional input
!
! INPUT ARGUMENTS:
!       sublevel_height:       Altitude of the atmospheric sublevels.
!                              UNITS:      metres (m)
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-1 (Ks x 1)
!                              ATTRIBUTES: INTENT( IN )
!
!       sublevel_pressure:     Pressure of the atmospheric sublevels.
!                              UNITS:      hPa
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-1 (Ks x 1)
!                              ATTRIBUTES: INTENT( IN )
!
!       sublevel_temperature:  Temperature of the atmospheric sublevels.
!                              UNITS:      Kelvin
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-1 (Ks x 1)
!                              ATTRIBUTES: INTENT( IN )
!
!       sublevel_absorber:     Absorber concentrations at the atmospheric
!                              sublevels
!                              UNITS:      ppmv
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-2 (Ks x J)
!                                          Ks == number of sublevels
!                                          J  == number of absorbers
!                              ATTRIBUTES: INTENT( IN )
!
!       n_per_layer:           Number of sublevel for each layer.
!                              Value must be > or = 1.
!                              UNITS:      None
!                              TYPE:       Integer
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       message_log:           Character string specifying a filename
!                              in which any messages will be logged.
!                              If not specified, or if an error occurs
!                              opening the log file, the default action
!                              is to output messages to standard output.
!                              UNITS:      None
!                              TYPE:       Character
!                              DIMENSION:  Scalar, LEN = *
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       layer_pressure:        Average pressure of the atmospheric layers
!                              UNITS:      hPa
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-1 (K-1 x 1)
!                              ATTRIBUTES: INTENT( IN )
!
!       layer_temperature:     Average temperature of the atmospheric layers
!                              UNITS:      Kelvin
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-1 (K-1 x 1)
!                              ATTRIBUTES: INTENT( IN )
!
!       layer_absorber:        Average absorber concentrations of the
!                              atmospheric layers
!                              UNITS:      kmol.cm^-2.
!                              TYPE:       Floating point
!                              DIMENSION:  Rank-2 (K-1 x J)
!                                          K-1 == number of layers
!                                          J   == number of absorbers
!                              ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status.
!
!       If result = SUCCESS the calculation was successful.
!                 = FAILURE an error occurred.
!
!       Error codes are defined in the error_handler module.
!
!
! CALLS:
!       pressure_to_number_density:  Function to the total number density.
!
!       ppmv_to_kmol_per_cm2:        Function to convert layer gas concentrations
!                                    from ppmv to kmol.cm^-2.
!
!       display_message:             Subroutine to output messages
!                                    SOURCE: error_handler module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The average layer pressure is simply determined using,
!
!                                 p(k) - p(k-1)
!         layer_pressure(k) = --------------------
!                              LOG( p(k)/p(k-1) )
!
!       The average layer temperature is determined by summing
!       the density weighted layer temperature T.rho subLAYER
!       across the sublayers and normalising by the sum of the
!       subLAYER density,
!
!                               __ N(k)
!                              \
!                               >   Trho      [ units of kmol.cm^-2.ppmv^-1.K ]
!                              /__
!                                  1
!         layer_temperature = -----------
!                               __ N(k)
!                              \
!                               >   rho       [ units of kmol.cm^-2.ppmv^-1 ]
!                              /__
!                                  1
!
!
!                               __ N(k)
!                              \      1.0e-11
!                               >    --------- * dz * p
!                              /__       R
!                                  1
!                           = ---------------------------
!                               __ N(k)
!                              \       1.0e-11
!                               >     --------- * dz * p
!                              /__      R . T
!                                  1
!
!
!                               __ N(k)
!                              \
!                               >     dz . p
!                              /__
!                                  1
!                           = ----------------
!                               __ N(k)
!                              \      dz . p
!                               >    --------
!                              /__       T
!                                  1
!
!      in units of Kelvin
!
!      In the UMBC KLAYERS code, the numerator corresponds to the final
!      TSUM value (with each sublayer value corresponding to RJUNK),
!      the denominator to AJUNK, and the result to TLAY.
!
!      The average layer absorber amount is determined by simply summing
!      the sublayer absorber amount across the sublayers,
!
!                          __ N(k)
!                         \      Trho . ppmv
!        layer_absorber =  >    -------------
!                         /__        T
!                             1
!
!                          __ N(k)
!                         \      1.0e-11             ppmv
!                       =  >    --------- . dz . p .------
!                         /__       R                 T
!                             1
!
!       in units of kmol.cm^-2
!
!       This corresponds to ASUM (and eventually ALAY) in the 
!       UMBC KLAYERS code.
!
!       Currently, N is independent of k. That is, the same number of sublevels
!       is assumed for each layer.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Jan-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

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

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2001/12/19 16:18:15 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: profile_conversion.f90,v $
! Revision 1.6  2001/12/19 16:18:15  paulv
! - Added ppmv<->number density conversion functions.
! - Added ppmv<->ppv conversion factors
! - Changed parameter named "SCALE" to "SCALE_FACTOR" in DENSITY funciton to
!   avoid confusion with the SCALE intrinsic.
! - Appended the kind type parameter, FP_KIND, to the coefficient definitions
!   in the SVP_WATER and SVP_ICE functions.
! - Added ICE_TEMPERATURE and MIN_PRESSURE optional arguments to the MR_TO_RH_SCALAR
!   call in the MR_TO_RH_RANK1 function. Somehow, they got left out.
! - Added SCALE_FACTOR parameter to MR_TO_PPMV and PPMV_TO_MR functions rather
!   than using an inline magic number.
! - Used ppmv<->ppv conversion factors rather than inline magic numbers in the
!   PPMV_TO_PP and PP_TO_PPMV functions.
! - Made sure that the N_PER_LAYER argument in CREATE_SUBLEVELS and INTEGRATE_SUBLEVLES
!   was a valid selection. For CREATE_SUBLEVELS(), this simply involved modifying
!   the valid number check to allow a value of 1 to be accepted. The INTEGRATE_SUBLEVELS()
!   function also required a change in how the final numbers of layers is calculated.
!   Previously, the code was:
!
!     n_layers = n_sublevels / n_per_layer
!
!   which, when n_per_layer = 1, produced a value of the number of original LEVELS
!   not LAYERS. This was changed to:
!
!     IF ( n_per_layer > 1 ) THEN
!       n_layers = n_sublevels / n_per_layer
!     ELSE
!       n_layers = n_sublevels - 1
!     END IF
!
!   to produce the correct number of layers for n_per_layer = 1.
!
! ******************************* IMPORTANT *********************************
! *************** CHANGES TO INTEGRATE_SUBLEVELS() INTERFACE ****************
!
! - Removed calls to MW_AIR() and DENSITY() in INTEGRATE_SUBLEVELS(). These
!   were replaced by a single call to PRESS_TO_NUMBER_DENSITY(). The density
!   weighting of the temperature profile is now performed using the density
!   in molecules.m^-3 rather than kg.m^-3. THIS MEANS THAT THE SPECIFICATION
!   OF THE WATER VAPOUR AMOUNT IS NO LONGER NEEDED AND HAS BEEN REMOVED FROM
!   THE FUNCTION INTERFACE. This will cause problems for folks who have integrated
!   this code into their own, but now the function will provide results even
!   if water vapour data are not available.
!
! - The capability to return the integrated amounts in units of PPMV, rather
!   than KMOL.CM^-2 has BEEN REMOVED. This conversion was only valid for the
!   case where the number of sublayers was 1.
!
!   The column density, cd, (in kmol.cm^-2) for a given LAYER k, split into N(k)
!   sublayers, is given by
!
!              __N(k)
!             \                            p(i)
!     cd(k) =  >  c . dz(i) . ppmv(i) . ----------     .....(1)
!             /__                        R . T(i)
!                i=1
!
!   where c is a constant, dz is the i'th sublayer's thickness and ppmv is a
!   simple average of the values for the LEVELs above and below.
!
!   Now,
!
!        p       nd   <- number density (units e.g. molecules.cm^-3)
!     ------- = ----
!      R . T     Na   <- Avogadro's #
!
!   Substituting this into (1) gives,
!
!                   __N(k)
!              c   \
!     cd(k) = ----  >  dz(i) . ppmv(i) . nd(i)
!              Na  /__
!                     i=1
!
!   If we assume for this set of sublayers that the mixing ratio, ppmv, is
!   constant (e.g. CO2 (mostly)), then,
!
!                         __N(k)
!              c . ppmv  \
!     cd(k) = ----------  >  dz(i) . nd(i)     .....(2)
!                 Na     /__
!                           i=1
!
!   So, from (2), it occurred to me that to be able to convert cd(k) from
!   kmol.cm^-2 BACK to to ppmv for layer k, then
!
!      __N(k)                    __N(k)        __N(k)
!     \                         \             \
!      >  dz(i) . nd(i)    ==    >  dz(i)  .   >  nd(i)
!     /__                       /__           /__
!        i=1                       i=1           i=1
!
!   which is most decidedly NOT true. Unless, of course, N(k) = 1.  The
!
!   So, now the only option for integrated absorber amount output units
!   is kmol.cm^-2.
!
! Revision 1.5  2001/12/13 15:58:43  paulv
! - Made all module parameters explicitly private. This is not required as
!   there is a "blanket" PRIVATE statement, but it makes me intent clear.
! - Corrected bug in checking the value of RHOair_km1 in the GEOPOTENTIAL_HEIGHT
!   and EFFECTIVE_LAYER_TP functions. The value of RHOair, rather than RHOair_km1,
!   was being checked:
!
!   Context code:
!     ! -- Air density
!     RHOair_km1 = density_scalar( pressure( 1 ), &
!                                  temperature( 1 ), &
!                                  MWair, &
!                                  message_log = message_log )
!
!   Old check:
!     IF ( RHOair < ZERO ) THEN
!       ...issue error message
!
!   New check
!     IF ( RHOair_km1 < ZERO ) THEN
!       ...issue error message
!
!   Some compilers didn't return an error as their default action is to
!   initialise variable to zero (so RHOair < ZERO wouldn't return an error).
!   Other compilers do not initialise by default so the incorrect test produced
!   the error message. Classic example of cut-and-paste bug.
!
! Revision 1.4  2001/12/12 17:35:57  paulv
! - Added PRESSURE_TO_NUMBER_DENSITY and NUMBER_DENSITY_TO_PRESSURE functions.
! - Modified USE fundamental_constants statement to include the STANDARD_TEMPERATURE
!   and LOSCHMIDT_CONSTANT values.
! - Replaced some "amgic" numbers (scale factors) in functions with parameterised
!   values. E.g. replaced 1000 scale factor with parameter G_TO_KG.
!
! Revision 1.3  2001/09/05 22:56:15  paulv
! - Updated documentation.
!
! Revision 1.2  2001/09/05 15:09:43  paulv
! - Changed USE, ONLY definitions to reflect changes in the TYPE_KINDS and
!   FUNDAMENTAL_CONSTANTS modules. Only fp_kind is used from the TYPE_KINDS
!   module and all the constants used from the FUNDAMENTAL_CONSTANTS module
!   no longer have the "D_" prefix as all the constants are now Double.
! - Input value check in p2k_scalar altered from:
!     IF ( pressure    < TOLERANCE .OR. &
!          temperature < TOLERANCE .OR. &
!          delta_z     < TOLERANCE .OR. &
!          ppmv        < ZERO      ) THEN
!   to
!     IF ( pressure       < TOLERANCE .OR. &
!          temperature    < TOLERANCE .OR. &
!          ABS( delta_z ) < TOLERANCE .OR. &
!          ppmv           < ZERO           ) THEN
!   This allows profile inputs of either direction (i.e. increasing or
!   decreasing height) to be processed.
! - Equation for conversion of ppmv to kmol.cm^-2 changed from
!     kmol_per_cm2 = SCALE_FACTOR * pressure * delta_z * ppmv / &
!     !              -----------------------
!                     ( R0 * temperature )
!   to
!     kmol_per_cm2 = SCALE_FACTOR * pressure * ABS( delta_z ) * ppmv / &
!     !              -----------------------
!                     ( R0 * temperature )
!   for the same reason.
! - Added optional OUTPUT_UNITS argument to the INTEGRATE_SUBLEVELS function.
!   This allows the user to select the output units as ppmv (default) or
!   kmol.cm^-2. Previously, kmol.cm^-2 was the default which made for a
!   relatively useless conversion as not many people use these units or are
!   familiar with them.
!
! Revision 1.1  2001/01/23 20:11:52  paulv
! Initial checkin.
!
!
