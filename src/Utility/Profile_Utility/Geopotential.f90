!--------------------------------------------------------------------------------
!M+
! NAME:
!       Geopotential
!
! PURPOSE:
!       Module containing routines for calculating geopotential heights.
!
! CATEGORY:
!       Profile Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Geopotential
!
! MODULES:
!       Type_Kinds:              Module containing definitions for kinds of
!                                variable types.
!
!       Fundamental_Constants:   Module containing definitions for some
!                                fundamental physical constants.
!                                USEs: TYPE_KINDS module
!
!       Message_Handler:           Module containing definitions of simple error
!                                codes and error handling routines.
!                                USEs: FILE_UTILITY module
!
!       Atmospheric_Properties:  Module containing utility routines to calculate
!                                various and sundry atmospheric properties.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!                                      PROFILE_UTILITY_PARAMETERS module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!       Geopotential_Height:     Function to calculate the geopotential height
!                                for an input profile.
!
!       PRIVATE subprograms
!       -------------------
!       Gravity:                 Function to calculate gravity as a function of
!                                height, latitude, and, if supplied, zonal and
!                                meridional wind velocity.
!                                Adapted and updated from the UMBC GRAV.F function
!                                supplied with the AIRS RTA.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-May-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2001 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any Later version.
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
!--------------------------------------------------------------------------------


MODULE Geopotential


  ! ------------
  ! Modules used
  ! ------------

  USE Type_Kinds, ONLY: fp_kind
  USE Message_Handler

  USE Fundamental_Constants, ONLY : R0 => MOLAR_GAS_CONSTANT, &
                                    G0 => STANDARD_GRAVITY, &
                                    PI

  USE Atmospheric_Properties


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Geopotential_Height


  ! ----------------------
  ! Module wide parameters
  ! ----------------------

  CHARACTER( * ),  PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: Geopotential.f90,v 1.8 2006/05/02 22:04:35 wd20pd Exp $'

  ! -- Numerical constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: TOLERANCE = EPSILON( 0.0_fp_kind )
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO      = 0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE       = 1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TWO       = 2.0_fp_kind

  ! -- Conversion factors
  REAL( fp_kind ), PRIVATE, PARAMETER :: G_TO_KG   = 1.0e-03_fp_kind

  ! -- Definition of keyword set flag
  INTEGER,         PRIVATE, PARAMETER :: SET = 1


CONTAINS





!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Gravity
!
! PURPOSE:
!       Function to calculate gravity as a function of height, latitude, and,
!       if supplied, zonal and meridional wind velocity.
!
!       Adapted and updated from the UMBC GRAV.F function supplied with the
!       AIRS RTA.
!
! CATEGORY:
!       Profile_Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       g = Gravity( Height,                                              &  ! Input
!                    Latitude                 = Latitude,                 &  ! Optional input
!                    Zonal_Wind_Velocity      = Zonal_Wind_Velocity,      &  ! Optional input
!                    Meridional_Wind_Velocity = Meridional_Wind_Velocity, &  ! Optional input
!                    Message_Log              = Message_Log               )  ! Optional input
!
! INPUT ARGUMENTS:
!       Height:                    Height at which the gravity value is required.
!                                  UNITS:      metres (m)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Latitude:                  Set this argument to the latitude at which
!                                  the gravity value is required.
!                                  If not defined, the default value is 0.0.
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Zonal_Wind_Velocity:       Set this argument to the zonal wind
!                                  velocities at the input height.
!                                  If not defined, the default value is 0.0.
!                                  UNITS:      m.s^-1, +ve W'ly, -ve E'ly
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Meridional_Wind_Velocity:  Set this argument to the meridional wind
!                                  velocities at the input height.
!                                  If not defined, the default value is 0.0.
!                                  UNITS:      m.s^-1, +ve S'ly, -ve N'ly
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:               Character string specifying a filename
!                                  in which any messages will be logged.
!                                  If not specified, or if an error occurs
!                                  opening the log file, the default action
!                                  is to output messages to standard output.
!                                  UNITS:      N/A
!                                  TYPE:       CHARACTER( * )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       g:                         The acceleration due to gravity.
!                                  If an error occurs, -1.0 is returned.
!                                  UNITS:      m.s^-2
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
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
!       Normal Gravity
!       --------------
!
!       The normal Gravity is determined from the International Gravity
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
!       Eqn.2 provides a value for gravity due both to the Earth's mass and
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
!         a_surface( Lat ) = ----------     .....(4)
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
!         a_surface( Lat ) = ------------------------------------
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
!--------------------------------------------------------------------------------

   FUNCTION Gravity( Height,                   &  ! Input
                     Latitude,                 &  ! Optional input
                     Zonal_Wind_Velocity,      &  ! Optional input
                     Meridional_Wind_Velocity, &  ! Optional input
                     Message_Log               )  ! Error messaging



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ),           INTENT( IN ) :: Height

    ! -- Optional input
    REAL( fp_kind ), OPTIONAL, INTENT( IN ) :: Latitude
    REAL( fp_kind ), OPTIONAL, INTENT( IN ) :: Zonal_Wind_Velocity
    REAL( fp_kind ), OPTIONAL, INTENT( IN ) :: Meridional_Wind_Velocity

    ! -- Error handler message log
    CHARACTER( * ),  OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    REAL( fp_kind ) :: Gravity


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Gravity'

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
    REAL( fp_kind ), PARAMETER :: Re_Equatorial = 6.378388e+06_fp_kind ! == a
    REAL( fp_kind ), PARAMETER :: Re_Polar      = 6.356911e+06_fp_kind ! == b

    ! -- Earth's flattening correction factor, (1 - b^2/a^2)
    REAL( fp_kind ), PARAMETER :: a2      = Re_Equatorial**2
    REAL( fp_kind ), PARAMETER :: b2      = Re_Polar**2
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

    REAL( fp_kind ) :: Lat
    REAL( fp_kind ) :: u, v
    REAL( fp_kind ) :: cos_Lat, sin_Lat
    REAL( fp_kind ) :: Re, Rtotal
    REAL( fp_kind ) :: Sum_Coeffs, g_Normal
    REAL( fp_kind ) :: a_Surface
    REAL( fp_kind ) :: a_Z
    REAL( fp_kind ) :: g_Surface



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Latitude ) ) THEN
      Lat = ABS( Latitude )
    ELSE
      Lat = ZERO
    END IF

    IF ( PRESENT( Zonal_Wind_Velocity ) ) THEN
      u = Zonal_Wind_Velocity
    ELSE
      u = ZERO
    END IF

    IF ( PRESENT( Meridional_Wind_Velocity ) ) THEN
      v = Meridional_Wind_Velocity
    ELSE
      v = ZERO
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CALCULATE TRIGONOMETRIC TERMS --                   #
    !#                                                                          #
    !# It's most likely faster to calculate the cosine squared terms here and   #
    !# modify the equations down the line to use the precalculated value rather #
    !# than do cos**2 each time, but as it is now, this only occurs in two      #
    !# places (the calc for Re and a_Surface, and a_Z if the wind arguments     #
    !# aren't present) and the code reflects the equations as one would write   #
    !# them down, which makes me feel all warm and fuzzy on the inside. :o)     #
    !#--------------------------------------------------------------------------#

    cos_Lat = COS( DEGREES_TO_RADIANS * Lat )
    sin_Lat = SIN( DEGREES_TO_RADIANS * Lat )



    !#--------------------------------------------------------------------------#
    !#                        -- CALCULATE RADIUS TERMS --                      #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------
    ! Calculate the Earth's radius at this Latitude
    ! ---------------------------------------------

    Re = SQRT(                b2                 / &
    !          ---------------------------------
               ( ONE - ( cos_Lat**2 * factor ) ) )


    ! --------------------------------------------
    ! Calculate total distance from Earth's center
    ! --------------------------------------------

    Rtotal = Re + Height

    IF ( Rtotal < TOLERANCE ) THEN
      Gravity = -ONE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid Height (<Re) input argument', &
                            FAILURE, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- CALCULATE GRAVITY AT EARTH'S SURFACE --                 #
    !#                                                                          #
    !# This uses the normal Gravity equation. This equation provides Gravity    #
    !# due to the mass *AND* rotation of the Earth.                             #
    !#                                                                          #
    !# Note that I don't use Horner's rule here. It adds an extra iteration and #
    !# the tests I performed showed no difference form the simple method below. #
    !#--------------------------------------------------------------------------#

    Sum_Coeffs = ZERO
    DO i = 1, N_COEFFICIENTS
      Sum_Coeffs = Sum_Coeffs + ( G_COEFFS( i ) * sin_Lat**( 2*i ) )
    END DO

    g_Normal = G_NORMAL_EQUATOR * ( ONE + Sum_Coeffs )



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE CENTRIPETAL ACCELERATION AT EARTH'S SURFACE --       #
    !#--------------------------------------------------------------------------#

    a_Surface = ( OMEGA * cos_Lat )**2 * Re



    !#--------------------------------------------------------------------------#
    !#            -- CALCULATE CENTRIPETAL ACCELERATION AT HEIGHT Z --          #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Zonal_Wind_Velocity      ) .OR. &
         PRESENT( Meridional_Wind_Velocity ) ) THEN

      a_Z = ( ( ( OMEGA * Rtotal * cos_Lat ) + u )**2 + v**2 ) / &
      !       ------------------------------------------------
                                        Rtotal

    ELSE

      a_Z = ( OMEGA * cos_Lat )**2 * Rtotal

    END IF



    !#--------------------------------------------------------------------------#
    !#            -- CALCULATE THE GRAVITY AT THE EARTH'S SURFACE, --           #
    !#            -- *REMOVING* THE EFFECT OF THE EARTH'S ROTATION.--           #
    !#--------------------------------------------------------------------------#

    g_Surface = g_Normal + a_Surface



    !#--------------------------------------------------------------------------#
    !#       -- CALCULATE THE GRAVITATIONAL ACCELERATION AT HEIGHT Z    --      #
    !#       -- INCORPORATING THE CENTRIPETAL ACCELERATION AT HEIGHT Z  --      #
    !#--------------------------------------------------------------------------#

    Gravity = g_Surface * ( Re/Rtotal )**2 - a_Z

  END FUNCTION Gravity





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Geopotential_Height
!
! PURPOSE:
!       Function to calculate geopotential height using the hypsometric
!       equation.
!
! CATEGORY:
!       Profile_Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Statusz = Geopotential_Height( Pressure,                                            &  ! Input
!                                            Temperature,                                         &  ! Input
!                                            Water_Vapor_Pressure,                                &  ! Input
!                                            Height,                                              &  ! Output
!                                            Surface_Height           = Surface_Height,           &  ! Optional input
!                                            Gravity_Correction       = Gravity_Correction,       &  ! Optional input
!                                            Latitude                 = Latitude,                 &  ! Optional input
!                                            Zonal_Wind_Velocity      = Zonal_Wind_Velocity,      &  ! Optional input
!                                            Meridional_Wind_Velocity = Meridional_Wind_Velocity, &  ! Optional input
!                                            RCS_Id                   = RCS_Id,                   &  ! Revision control
!                                            Message_Log              = Message_Log               )  ! Error messaing
!
!
! INPUT ARGUMENTS:
!       Pressure:                  Pressure of the atmospheric levels.
!                                  UNITS:      hectoPascals, hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K x 1)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Temperature:               Temperature of the atmospheric levels.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K x 1)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor_Pressure:      Water vapor partial pressure at the atmospheric levels
!                                  UNITS:      hectoPascals, hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K x 1)
!                                  ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Surface_Height:            Height corresponding to the first element of the
!                                  input arrays. If not specified, the default value
!                                  is 0.0m.
!                                  UNITS:      metres, m.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Gravity_Correction:        Set this argument to use a gravity profile rather
!                                  than standard reference gravity in calculating
!                                  the geopotential Heights. If PRESENT then,
!                                  if = 0, standard gravity used,
!                                     = 1, gravity profile is calculated and used.
!                                  UNITS:      N/A.
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Latitude:                  Set this argument to the Latitude corresponding
!                                  to the input profile location. This argument is
!                                  ignored if the Gravity_Correction argument is
!                                  NOT set. If not defined and required, the default
!                                  value is zero.
!                                  UNITS:      Degrees (not N or S hemisphere dep.).
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Zonal_Wind_Velocity:       Set this argument to the zonal wind velocities
!                                  at the input profile levels. This argument is
!                                  ignored if the Gravity_Correction argument is
!                                  not set. If not specified, the default value
!                                  is zero.
!                                  UNITS:      m.s^-1, +ve W'ly, -ve E'ly
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Meridional_Wind_Velocity:  Set this argument to the meridional wind velocities
!                                  at the input profile levels. This argument is
!                                  ignored if the Gravity_Correction argument is
!                                  not set. If not specified, the default value
!                                  is zero.
!                                  UNITS:      m.s^-1, +ve S'ly, -ve N'ly
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:               Character string specifying a filename in which any
!                                  messages will be logged. If not specified, or if an
!                                  error occurs opening the log file, the default action
!                                  is to output messages to standard output.
!                                  UNITS:      N/A
!                                  TYPE:       CHARACTER( * )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!
! OUTPUT ARGUMENTS:
!       Height:                    Geopotential Heights of the input Pressure levels.
!                                  UNITS:      metres, m
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K x 1)
!                                  ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:                    Character string containing the Revision Control
!                                  System Id field for the module.
!                                  UNITS:      N/A
!                                  TYPE:       CHARACTER( * )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:              The return value is an integer defining the
!                                  error status. The error codes are defined
!                                  in the ERROR_HANDLER module.
!                                  If == SUCCESS the geopotential calculation was
!                                                successful
!                                     == FAILURE an unrecoverable error occurred
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
! CALLS:
!       MW_Air:             Function to calculate the effective molecular
!                           weight of air weighted by the water vapor amount.
!                           SOURCE: ATMOSPHERIC_PROPERTIES module
!
!       Density:            Function to calculate gas density using the ideal
!                           gas law.
!                           SOURCE: ATMOSPHERIC_PROPERTIES module
!
!       Gravity:            Function to calculate acceleration due to Gravity
!                           as a function of Latitude and Height.
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
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
!--------------------------------------------------------------------------------

  FUNCTION Geopotential_Height( Pressure,                 &  ! Input
                                Temperature,              &  ! Input
                                Water_Vapor_Pressure,     &  ! Input
                                Height,                   &  ! Output
                                Surface_Height,           &  ! Optional input
                                Gravity_Correction,       &  ! Optional input
                                Latitude,                 &  ! Optional input
                                Zonal_Wind_Velocity,      &  ! Optional input
                                Meridional_Wind_Velocity, &  ! Optional input
                                RCS_Id,                   &  ! Revision control
                                Message_Log )             &  ! Error messaging
                              RESULT( Error_Status )


    !--------------------------------------------------------------------------
    !                        -- Type declarations --
    !--------------------------------------------------------------------------
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ),           DIMENSION( : ), INTENT( IN )  :: Pressure
    REAL( fp_kind ),           DIMENSION( : ), INTENT( IN )  :: Temperature
    REAL( fp_kind ),           DIMENSION( : ), INTENT( IN )  :: Water_Vapor_Pressure

    ! -- Output
    REAL( fp_kind ),           DIMENSION( : ), INTENT( OUT ) :: Height

    ! -- Optional input
    REAL( fp_kind ), OPTIONAL,                 INTENT( IN )  :: Surface_Height
    INTEGER,         OPTIONAL,                 INTENT( IN )  :: Gravity_Correction
    REAL( fp_kind ), OPTIONAL,                 INTENT( IN )  :: Latitude
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( IN )  :: Zonal_Wind_Velocity
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( IN )  :: Meridional_Wind_Velocity

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),  OPTIONAL,                 INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Geopotential_Height'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Use_Gravity

    INTEGER :: n_Levels
    INTEGER :: k, k1, k2, dk

    REAL( fp_kind ), DIMENSION( SIZE( Pressure ) ) :: u, v

    REAL( fp_kind ) :: Surface_Z
    REAL( fp_kind ) :: MWair
    REAL( fp_kind ) :: Rair,   Rair_km1
    REAL( fp_kind ) :: RHOair, RHOair_km1
    REAL( fp_kind ) :: layer_Rair
    REAL( fp_kind ) :: layer_T
    REAL( fp_kind ) :: g
    REAL( fp_kind ) :: H
    REAL( fp_kind ) :: dz



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Input array sizes
    ! -----------------

    n_Levels = SIZE( Pressure )

    IF ( SIZE( Temperature )          /= n_Levels .OR. & 
         SIZE( Water_Vapor_Pressure ) /= n_Levels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF


    ! -----------------
    ! Output array size
    ! -----------------

    IF ( SIZE( Height ) < n_Levels  ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Output HEIGHT array too small to hold result.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF


    ! ------------------
    ! Input array values
    ! ------------------

    IF ( ANY( Pressure < TOLERANCE ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Pressures < or = 0 found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    IF ( ANY( Temperature < TOLERANCE ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Temperatures < or = 0 found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    IF ( ANY( Water_Vapor_Pressure < ZERO ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input water vapor partial pressures < 0 found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF


    ! --------------
    ! Surface height
    ! --------------

    ! -- Default value is 0.0....
    Surface_Z = ZERO

    ! -- ...unless the surface height argument is present
    IF ( PRESENT( Surface_Height ) ) THEN
      Surface_Z = Surface_Height
    ELSE
      Surface_Z = ZERO
    END IF


    ! ---------------
    ! Gravity profile
    ! ---------------

    ! -- Default is NOT to calculate a gravity profile....
    Use_Gravity = .FALSE.

    ! -- ...unless the gravity correction argument is set.
    IF ( PRESENT( Gravity_Correction ) ) THEN

      IF ( Gravity_Correction == SET ) THEN

        Use_Gravity = .TRUE.

        ! -- Check zonal wind velocity
        u = ZERO
        IF ( PRESENT( Zonal_Wind_Velocity ) ) THEN
          IF ( SIZE( Zonal_Wind_Velocity ) /= n_Levels ) THEN
            Error_Status = WARNING
            CALL Display_Message( ROUTINE_NAME, &
                                  'Input ZONAL_WIND_VELOCITY has inconsistent size. '//&
                                  'Setting to 0.0.', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          ELSE
            u = Zonal_Wind_Velocity
          END IF
        END IF
          
        ! -- Check meridional wind velocity
        v = ZERO
        IF ( PRESENT( Meridional_Wind_Velocity ) ) THEN
          IF ( SIZE( Meridional_Wind_Velocity ) /= n_Levels ) THEN
            Error_Status = WARNING
            CALL Display_Message( ROUTINE_NAME, &
                                  'Input MERIDIONAL_WIND_VELOCITY has inconsistent size. '//&
                                  'Setting to 0.0.', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          ELSE
            v = Meridional_Wind_Velocity
          END IF
        END IF
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- DETERMINE ORDER OF INPUT PRESSURE --                 #
    !#--------------------------------------------------------------------------#

    IF ( Pressure( 2 ) < Pressure ( 1 ) ) THEN

      ! -- Ascending, i.e. ground up
      k1 = 1
      k2 = n_Levels
      dk = 1

    ELSE

      ! -- Descending, i.e. TOA down
      k1 = n_Levels
      k2 = 1
      dk = -1

    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE NEAR SURFACE LEVEL VALUES --                 #
    !#--------------------------------------------------------------------------#

    ! -- Molecular weight of air
    MWair = MW_Air( Pressure( k1 ), &
                    Water_Vapor_Pressure( k1 ), &
                    Message_Log = Message_Log )
    IF ( MWair < ZERO ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error calculating MWair at level ", i4, ". Value = ", es13.6 )' ) &
                      k1, MWair
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Calculate the gas "constant" in J/K/kg
    Rair_km1 = R0 / ( MWair * G_TO_KG )

    ! -- Air density
    RHOair_km1 = Density( Pressure( k1 ), &
                          Temperature( k1 ), &
                          MWair, &
                          Message_Log = Message_Log )

    IF ( RHOair_km1 < ZERO ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error calculating RHOair at level ", i4, ". Value = ", es13.6 )' ) &
                      k1, RHOair_km1
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                 -- LOOP OVER LEVELS IN GROUND UP ORDER --                #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Assign first level height
    ! -------------------------

    Height( k1 ) = Surface_Z


    ! ----------------
    ! Begin level loop
    ! ----------------

    Level_Loop: DO k = k1+dk, k2, dk


      ! ------------------------------
      ! Calculate current level values
      ! ------------------------------

      ! -- MWair at current level
      MWair = MW_Air( Pressure( k ), &
                      Water_Vapor_Pressure( k ), &
                      Message_Log = Message_Log )
      IF ( MWair < ZERO ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error calculating MWair at level ", i4, ". Value = ", es13.6 )' ) &
                        k, MWair
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      ENDIF

      ! -- Gas "constant" at current level in J/K/kg
      Rair = R0 / ( MWair * G_TO_KG )

      ! -- Air density at current level
      RHOair = Density( Pressure( k ), &
                        Temperature( k ), &
                        MWair, &
                        Message_Log = Message_Log )
      IF ( RHOair < ZERO ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error calculating RHOair at level ", i4, ". Value = ", es13.6 )' ) &
                        k, RHOair
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
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
      layer_T =  ( ( Temperature( k-dk ) * RHOair_km1 ) + ( Temperature( k ) * RHOair ) ) / &
      !          ------------------------------------------------------------------------
                                           ( RHOair_km1 + RHOair )


      ! -----------------------------------
      ! Calculate gravity value if required
      ! -----------------------------------

      IF ( Use_Gravity ) THEN

        ! -- Calculate gravity at layer lower boundary
        g = Gravity( Height( k-dk ), &
                     Latitude                 = Latitude, &
                     Zonal_Wind_Velocity      = u( k-dk ), &
                     Meridional_Wind_Velocity = v( k-dk ), &
                     Message_Log              = Message_Log )

        IF ( g < ZERO ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Gravity calculation failed.', &
                                Error_Status, &
                                Message_Log = Message_Log )
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

      dz = H * LOG( Pressure( k-dk ) / Pressure( k ) )


      ! ------------------
      ! Accumulate heights
      ! ------------------

      Height( k ) = Height( k-dk ) + dz


      ! ----------------------------------
      ! Save current level Rair and RHOair
      ! ----------------------------------

      Rair_km1   = Rair
      RHOair_km1 = RHOair

    END DO Level_Loop

  END FUNCTION Geopotential_Height

END MODULE Geopotential


!#------------------------------------------------------------------------------#
!#                         -- MODIFICATION HISTORY --                           #
!#------------------------------------------------------------------------------#
!
! $Id: Geopotential.f90,v 1.8 2006/05/02 22:04:35 wd20pd Exp $
!
! $Date: 2006/05/02 22:04:35 $
!
! $Revision: 1.8 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Geopotential.f90,v $
! Revision 1.8  2006/05/02 22:04:35  wd20pd
! - Replaced all references to Error_Handler with Message_Handler.
!
! Revision 1.7  2004/11/29 19:41:49  paulv
! - Removed unused variable declarations.
! - Added comment above calculation of normal gravity. Horner's rule not used
!   to sum normal gravity coefficients as tests indicated it made no difference.
!
! Revision 1.6  2004/11/18 19:07:07  paulv
! - Upgraded to Fortran-95
! - Updated header documentation.
!
! Revision 1.5  2003/05/22 15:45:01  paulv
! - Updated documentation.
!
! Revision 1.4  2003/01/21 20:03:21  paulv
! - When latitude is used in the Gravity function, the ABS value is now
!   used in the gravity profile calculation.
! - Updated documentation.
!
! Revision 1.3  2002/10/04 20:54:47  paulv
! - Removed water vapor units argument.
! - Other cosmetic changes.
!
! Revision 1.2  2002/09/20 16:23:07  paulv
! - Corrected interface to function calls.
! - Checkin prior to changes to some call interfaces. Incomplete.
!
! Revision 1.1  2002/08/30 22:48:12  paulv
! Initial checkin. Untested.
! - The contents of this module have been extracted from the old PROFILE_CONVERSION
!   module and split up into different categories of profile processing.
!
!
