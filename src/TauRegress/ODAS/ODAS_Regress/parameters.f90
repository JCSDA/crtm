!------------------------------------------------------------------------------
!M+
! NAME:
!       parameters
!
! PURPOSE:
!       Module to hold RT model parameter constants
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE Parameters
!
! OUTPUTS:
!
!       Pseudo-parameters
!       -----------------
!
!       These values are not parameters in the Fortran sense in that they are
!       defined at run-time based on user inputs but once defined, they are
!       (or should be) invariant.
!
!       MAX_N_CHANNELS:              INTEGER defining the maximum number of
!                                    instrument channels. This defines the
!                                    valid channels for the valid satellites
!                                    that the user has selected.
!                                    Upon RTM initialisation and destruction
!                                    the value is set to -1.
!                                    The value of MAX_N_CHANNELS can only be
!                                    accessed through its own methods:
!                                      - Set_Max_n_Channels()
!                                      - Reset_Max_n_Channels()
!                                      - Get_Max_n_Channels()
!       
!
! MODULES:
!       None.
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP 31-Jul-2000
!                       pvandelst@ncep.noaa.gov
!
!
!  Copyright (C) 2000 Paul van Delst
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
!!!  INTEGER, PRIVATE :: MAX_N_PROFILES = RESET_VALUE
!!!  INTEGER, PRIVATE :: MAX_N_LAYERS   = RESET_VALUE


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

!!! Fortran 95 code follows !!!
!  REAL( fp_kind ), PUBLIC, PARAMETER :: LIMIT_EXP = ABS( LOG( TOLERANCE ) )
!  REAL( fp_kind ), PUBLIC, PARAMETER :: LIMIT_LOG = EXP( LIMIT_EXP )


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
!!! Fortran 95 code follows !!!
!  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SECANT_SOLAR_ANGLE = ONE / COS( DEGREES_TO_RADIANS * MAX_SOLAR_ANGLE )



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


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2004/03/02 21:02:19 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: parameters.f90,v $
! Revision 1.13  2004/03/02 21:02:19  paulv
! - Renamed MAX_N_LAYER_FUNCTIONS parameter to MAX_N_ORDERS.
! - Added a maximum secant flux angle parameter.
!
! Revision 1.12  2003/02/05 15:37:26  paulv
! - Removed the definition for the maximum number of absorber layers. No
!   longer needed in new algorithm
! - Defined some literal constants for exponential and logarithmic limits.
!
! Revision 1.11  2003/02/05 15:06:18  paulv
! - Updated the number of standard predictors.
! - Added additional literal constant definitions.
!
! Revision 1.10  2002/10/04 21:08:24  paulv
! - Updated the parameters
!     MAX_N_PREDICTORS_USED
!     MAX_N_STANDARD_PREDICTORS
!   for the new algorithm.
! - Added the parameter
!     MAX_N_LAYER_FUNCTIONS
!   for the new algorithm.
!
! Revision 1.9  2001/10/01 20:28:46  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 1.8  2001/08/31 21:09:45  paulv
! - Added the secant of the maximum solar angle as a parameter. The secant
!   value was calculated and expressed as an explicit number since Fortran 90
!   does not allow intrinsics that have other than integer results in a
!   parameter initialisation expression.
! - Changed  MAX_SOLAR_ZENITH_ANGLE name to MAX_SOLAR_ANGLE.
!
! Revision 1.7  2001/08/16 16:44:14  paulv
! - Updated documentation.
! - Changed MAX_N_CHANNELS attributes from PUBLIC to PRIVATE, SAVE. The value
!   of MAX_N_CHANNELS is now accessed via its public methods:
!     set_max_n_channels
!     reset_max_n_channels()
!    get_max_n_channels()
! - Added RESET_VALUE parameter.
! - Removed POINT_333 parameter.
!
! Revision 1.6  2001/07/12 16:46:12  paulv
! - Added PRIVATE statement to prevent definitions in module TYPE_KINDS
!   being available outside the scope of this module.
!
! Revision 1.5  2001/05/29 17:42:55  paulv
! - Now use TYPE_KINDS module parameter FP_KIND to set the floating point
!   data type. All REAL declarations are now typed with FP_KIND.
! - Added direction flags for transmittance calculation.
!
! Revision 1.4  2000/11/09 20:32:11  paulv
! - Removed MAX_N_CHANNELS as a parameter. It is now a "pseudo" parameter
!   in that it is determined by the number of channels for which coefficients
!   are defined.
! - Added some more numerical parameters.
!
! Revision 1.3  2000/08/31 19:36:33  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.2  2000/08/24 15:39:45  paulv
! - Changed the parameter name that references how many predictors of the
!   total set to use from MAX_N_PREDICTORS_TO_USE to MAX_N_PREDICTORS_USED.
!   I felt this would clarify (for me at least) that while the maximum
!   number of predictors is set, the number that is actually used can be
!   less than that.
! - Current maximum number of layers is 100. This is a temporary limit for
!   testing purposes.
! - The parameter RECIPROCAL_GRAVITY was removed from this module and placed
!   in the ABSORBER_PROFILE module where it is used.
!
! Revision 1.1  2000/08/08 16:57:21  paulv
! Initial checkin
!
!
!
!
