!------------------------------------------------------------------------------
!M+
! NAME:
!       Fundamental_Constants
!
! PURPOSE:
!       Module containing various fundamental physical constants.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Fundamental_Constants
!
! MODULES:
!       Type_Kinds:      Module containing definitions for kinds of variable
!                        types
!
! OUTPUTS:
!       Irrational numbers
!       ------------------
!
!       PI:                        Value of pi.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       PI_RECIPROCAL:             Reciprocal value of pi, 1/pi.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       PI_SQUARED:                Squared value of pi, pi^2.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       PI_SQUARE_ROOT:            Square root value of pi, pi^0.5.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       PI_LN:                     Natural logarithm of pi, LN(pi).
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       PI_LOG10:                  Base-10 logarithm of pi, LOG(pi).
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       E:                         Value of e, base of the natural logarithm.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       E_RECIPROCAL:              Reciprocal value of e, 1/e.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       E_SQUARED:                 Squared value of e, e^2.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       E_LOG10:                   Base-10 logarithm of e, LOG(e).
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       Universal Constants
!       -------------------
!
!       SPEED_OF_LIGHT:            Speed of light in a vacuum, c
!                                  UNITS:      metres/second, m/s
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       PERMEABILITY:              Permeability of free space, mu0, where
!                                    mu0 = 4.PI x 10^-7
!                                  UNITS:      Newton/Ampere^2, N/A^2
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       PERMITTIVITY:              Permittivity of free space, epsilon0, where
!                                                    1
!                                    epsilon0 = -----------
!                                                mu0 . c^2
!                                  UNITS:      Farad/metre, F/m
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       PLANCK_CONSTANT:           Planck's constant, h
!                                  UNITS:      Joule.seconds, Js
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       GRAVITATIONAL_CONSTANT:    Universal, or Newtonian, gravitation
!                                  constant, G
!                                  UNITS:      m^3/(kg.s^2)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       Conversion Factors
!       ------------------
!
!       ELECTRON_VOLT:             Electron volt, the work required to move
!                                  one electron through a potential difference
!                                  of one volt.
!                                  UNITS:      Joules, J
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       UNIFIED_ATOMIC_MASS_UNIT:  Unified atomic mass unit, u. This is a
!                                  unit of mass equal to the mass of 1/12 of
!                                  a mole of Carbon-12 atoms
!                                  UNITS:      kg
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       STANDARD_ATMOSPHERE:       Standard atmospheric pressure.
!                                  UNITS:      Pascals, Pa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       STANDARD_TEMPERATURE:      Standard atmospheric temperature.
!                                  Note that the unit of thermodynamic temperature,
!                                  the Kelvin, is the fraction 1/273.16 of the
!                                  thermodynamic temperature of the triple point
!                                  of water. The standard temperature is the ice
!                                  point of water, 273.15, NOT the triple point.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       STANDARD_GRAVITY:          Standard acceleration of gravity.
!                                  UNITS:      m/s^2
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       Physicochemical Constants
!       -------------------------
!
!       AVOGADRO_CONSTANT:         Avogadro's number, N(A). The number of atoms or
!                                  molecules required such that the number of grams
!                                  of a substance equals its atomic mass. This number
!                                  of atoms or molecules is a mole.
!                                  UNITS:      mol^-1
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       MOLAR_GAS_CONSTANT:        Universal gas constant, R.
!                                  UNITS:      J/(mol.K)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       BOLTZMANN_CONSTANT:        Boltzmann's constant, k, where
!                                          R
!                                    k = ------
!                                         N(A)
!                                  UNITS:      J/K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       STEFAN_BOLTZMANN_CONSTANT: Stefan-Boltzmann constant, sigma, where
!                                              pi^2
!                                              ----.k^4
!                                               60                     h
!                                    sigma = ------------   ( hbar = ----- )
!                                             hbar^3.c^2              2pi
!                                  UNITS:      W/(m^2.K^4)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       C_1:                       First radiation constant for spectral
!                                  radiance, c1, where
!                                    c1 = 2.h.c^2
!                                  UNITS:      W.m^2.sr^-1
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       C_2:                       Second radiation constant, c2, where
!                                          h.c
!                                    c2 = -----
!                                           k
!                                  UNITS:      K.m
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       STP_MOLAR_VOLUME:          Molar volume, Vm, of an ideal gas at standard
!                                  temperature and pressure, where
!                                          R.T0
!                                    Vm = ------
!                                           P0
!                                  UNITS:      m^3/mol
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
!       LOSCHMIDT_CONSTANT:        The number density, n0, of one mole of an
!                                  ideal gas at standard temperature and
!                                  pressure, where
!                                          N(A).P0     N(A)      P0
!                                    n0 = --------- = ------ = ------
!                                           R.T0        Vm      k.T0
!                                  UNITS:      m^-3
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: PARAMETER, PUBLIC
!
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
!       The fundamental constants and equations used are taken from the
!       NIST Reference on Constants, Units, and Uncertainty website:
!
!         http://physics.nist.gov/cuu/Constants/
!
!       See also:
!
!         Mohr, P.J. and B.N. Taylor, "CODATA recommended values of the
!           fundamental physical constants: 1998", Reviews of Modern Physics, 
!           Vol.72, No.2, 2000.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-May-2000
!                       paul.vandelst@ssec.wisc.edu
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

MODULE Fundamental_Constants


  ! ------------
  ! Modules used
  ! ------------

  USE Type_Kinds, ONLY: fp_kind


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE

 

  !#----------------------------------------------------------------------------#
  !#                       -- LOCAL LITERAL CONSTANTS --                        #
  !#----------------------------------------------------------------------------#

  REAL( fp_kind ), PARAMETER, PRIVATE :: ONE = 1.0_fp_kind
  REAL( fp_kind ), PARAMETER, PRIVATE :: TWO = 2.0_fp_kind



  !#----------------------------------------------------------------------------#
  !#                -- IRRATIONAL NUMBERS AND ASSOCIATED BITS --                #
  !#----------------------------------------------------------------------------#

  REAL( fp_kind ), PARAMETER, PUBLIC :: PI             = 3.141592653589793238462643_fp_kind
  REAL( fp_kind ), PARAMETER, PUBLIC :: PI_RECIPROCAL  = 0.318309886183790671537767_fp_kind
  REAL( fp_kind ), PARAMETER, PUBLIC :: PI_SQUARED     = 9.869604401089358618834491_fp_kind
  REAL( fp_kind ), PARAMETER, PUBLIC :: PI_SQUARE_ROOT = 1.772453850905516027298167_fp_kind
  REAL( fp_kind ), PARAMETER, PUBLIC :: PI_LN          = 1.144729885849400174143427_fp_kind
  REAL( fp_kind ), PARAMETER, PUBLIC :: PI_LOG10       = 0.497149872694133854351268_fp_kind

  REAL( fp_kind ), PARAMETER, PUBLIC :: E              = 2.71828182845904523560287_fp_kind
  REAL( fp_kind ), PARAMETER, PUBLIC :: E_RECIPROCAL   = 0.367879441171442321595523_fp_kind
  REAL( fp_kind ), PARAMETER, PUBLIC :: E_SQUARED      = 7.389056098930650227230427_fp_kind
  REAL( fp_kind ), PARAMETER, PUBLIC :: E_LOG10        = 0.434294481903251827651129_fp_kind



  !#----------------------------------------------------------------------------#
  !#                            -- UNIVERAL CONSTANTS --                        #
  !#----------------------------------------------------------------------------#


  ! ----------------------------------------------
  ! Speed of light
  ! Symbol:c,  Units:m/s,  Rel.Uncert.(ppm): exact
  ! ----------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: SPEED_OF_LIGHT = 2.99792458e+08_fp_kind


  ! --------------------------------------------------
  ! Permeability of vacuum
  ! Symbol:mu0,  Units:N/A^2,  Rel.Uncert.(ppm): exact
  ! --------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: PERMEABILITY = PI * 4.0e-07_fp_kind


  ! -----------------------------------------------------
  ! Permittivity of vacuum
  ! Symbol:epsilon0,  Units:F/m,  Rel.Uncert.(ppm): exact
  ! -----------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: PERMITTIVITY =                ONE                  / &
                                       !              ------------------------------------
                                                      ( PERMEABILITY * SPEED_OF_LIGHT**2 )


  ! ---------------------------------------------
  ! Planck constant
  ! Symbol:h,  Units:Js,  Rel.Uncert.(ppm): 0.078
  ! ---------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: PLANCK_CONSTANT = 6.62606876e-34_fp_kind


  ! ----------------------------------------------------
  ! Gravitational constant
  ! Symbol:G,  Units:m^3/kg/s^2,  Rel.Uncert.(ppm): 1500
  ! ----------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: GRAVITATIONAL_CONSTANT = 6.673e-11_fp_kind



  !#----------------------------------------------------------------------------#
  !#                          -- CONVERSION FACTORS --                          #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------------
  ! Electron volt
  ! Symbol:eV,  Units:J,  Rel.Uncert.(ppm): 0.039
  ! ---------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: ELECTRON_VOLT = 1.602176462e-19_fp_kind


  ! ---------------------------------------------
  ! Unified atomic mass unit
  ! Symbol:u,  Units:kg,  Rel.Uncert.(ppm): 0.079
  ! ---------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: UNIFIED_ATOMIC_MASS_UNIT = 1.66053873e-27_fp_kind


  ! ----------------------------------------------
  ! Standard atmosphere
  ! Symbol:P0,  Units:Pa,  Rel.Uncert.(ppm): exact
  ! ----------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: STANDARD_ATMOSPHERE = 101325.0_fp_kind


  ! ----------------------------------------------------------------------
  ! Standard temperature
  ! Symbol:T0,  Units:Kelvin,  Rel.Uncert.(ppm): exact
  !
  ! Note that the unit of thermodynamic temperature, the Kelvin, is the
  ! fraction 1/273.16 of the thermodynamic temperature of the triple point
  ! of water. The standard temperature is the ice point of water, NOT the
  ! triple point, hence the 0.01K difference.
  ! ----------------------------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: STANDARD_TEMPERATURE = 273.15_fp_kind


  ! ------------------------------------------------
  ! Standard gravity
  ! Symbol:g,  Units:m/s^2,  Rel.Uncert.(ppm): exact
  ! ------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: STANDARD_GRAVITY = 9.80665_fp_kind



  !#----------------------------------------------------------------------------#
  !#                        -- PHYSICOCHEMICAL CONSTANTS --                     #
  !#----------------------------------------------------------------------------#

  ! -----------------------------------------------------
  ! Avogadro constant
  ! Symbol:N(A),  Units:mole^-1,  Rel.Uncert.(ppm): 0.079
  ! -----------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: AVOGADRO_CONSTANT = 6.02214199e+23_fp_kind


  ! -------------------------------------------------
  ! Molar gas constant
  ! Symbol:R,  Units:J/mole/K,  Rel.Uncert.(ppm): 1.7
  ! -------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: MOLAR_GAS_CONSTANT = 8.314472_fp_kind


  ! --------------------------------------------
  ! Boltzmann constant
  ! Symbol:k,  Units:J/K,  Rel.Uncert.(ppm): 1.7
  !
  !         R
  !   k = ------
  !        N(A)
  !
  !     = 1.3806503(24)e-23
  !
  ! --------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: BOLTZMANN_CONSTANT = MOLAR_GAS_CONSTANT / &
  !                                                          ------------------
                                                              AVOGADRO_CONSTANT


  ! ------------------------------------------------------
  ! Stefan-Boltzmann constant
  ! Symbol:sigma,  Units:W/m^2/K^4,  Rel.Uncert.(ppm): 7.0
  !
  !             PI^2
  !             ----.k^4
  !              60                     h
  !   sigma = ------------   ( hbar = ----- )
  !            hbar^3.c^2              2PI
  !
  !         = 5.670400(40)e-08
  !
  ! I just placed the value here due to the mathematical
  ! gymnastics required to calculate it directly.
  ! ------------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: STEFAN_BOLTZMANN_CONSTANT = 5.670400e-08_fp_kind


  ! -------------------------------------------------------
  ! First Planck function constant
  ! Symbol:c1,  Units:W.m^2.sr^-1,  Rel.Uncert.(ppm): 0.078
  !
  !   c1 = 2.h.c^2
  !
  !      = 1.191042722(93)e-16
  !
  ! -------------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: C_1 = TWO * PLANCK_CONSTANT * SPEED_OF_LIGHT**2


  ! ---------------------------------------------
  ! Second Planck function constant
  ! Symbol:c2,  Units:K.m,  Rel.Uncert.(ppm): 1.7
  !
  !         h.c
  !   c2 = -----
  !          k
  !
  !      = 1.4387752(25)e-02
  !
  ! ---------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: C_2 = PLANCK_CONSTANT * SPEED_OF_LIGHT / &
  !                                          ----------------------------------
                                                      BOLTZMANN_CONSTANT


  ! -----------------------------------------------------------------
  ! Molar volume of an ideal gas at standard temperature and pressure
  ! Symbol:Vm,  Units:m^3/mol,  Rel.Uncert.(ppm): 1.7
  !
  !         R.T0
  !   Vm = ------
  !          P0
  !
  !      = 2.2413996(39)e-02
  !
  ! -----------------------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: STP_MOLAR_VOLUME = ( MOLAR_GAS_CONSTANT * STANDARD_TEMPERATURE ) / &
  !                                                        ---------------------------------------------
                                                                        STANDARD_ATMOSPHERE


  ! ------------------------------------------------------------------
  ! Loschmidt constant: The number density of one mole of an ideal gas
  ! at standard temperature and pressure
  ! Symbol:n0,  Units:m^-3,  Rel.Uncert.(ppm): 1.7
  !
  !         N(A).P0
  !   n0 = ---------
  !          R.T0
  !
  !         N(A)
  !      = ------     .....(1)
  !          Vm
  !
  !      = 2.6867775(47)e+25
  !
  ! Alternatively, using the ideal gas law directly, we know,
  !
  !   P.V = n.k.T     .....(2)
  !
  ! For V = 1m^3 (unit volume), and P = P0, T = T0, then eqn.(2)
  ! becomes,
  !
  !   P0 = n0.k.T0
  !
  ! which rearranges to
  !
  !          P0  
  !   n0 = ------     .....(3)
  !         k.T0 
  !
  ! Equation (1) rather than eqn(3) is used here.
  ! ------------------------------------------------------------------

  REAL( fp_kind ), PARAMETER, PUBLIC :: LOSCHMIDT_CONSTANT = AVOGADRO_CONSTANT / &
  !                                                          -----------------
                                                              STP_MOLAR_VOLUME


END MODULE Fundamental_Constants


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Fundamental_Constants.f90,v 1.12 2004/12/01 22:50:10 paulv Exp $
!
! $Date: 2004/12/01 22:50:10 $
!
! $Revision: 1.12 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Fundamental_Constants.f90,v $
! Revision 1.12  2004/12/01 22:50:10  paulv
! - Corrected spelling errors in some parameter names.
!
! Revision 1.11  2004/12/01 21:13:52  paulv
! - Updated documentation.
! - Moved to Utility category.
!
! Revision 1.10  2004/11/10 20:27:40  paulv
! - Added some private literal constants.
! - Added some PI derivative values.
! - Added e and derivate values.
! - Added a bit more documentation regarding the Loschmidt constant.
!
! Revision 1.9  2003/05/22 20:00:29  paulv
! - Updated documentation.
!
! Revision 1.8  2002/10/08 16:55:07  paulv
! - Increased the number of digits for PI to a ridiculously large number. :o)
!
! Revision 1.7  2001/12/12 17:37:57  paulv
! - Rearranged definitions so parameters were defined before being used. Duh.
!
! Revision 1.6  2001/11/15 16:43:30  paulv
! - Changed Boltzmann constant to a calculated value.
! - Added molar gas volume and Loschmidt constant.
!
! Revision 1.5  2001/10/24 17:31:45  paulv
! - Changed all floating point kind types from DOUBLE to FP_KIND. Only the
!   FP_KIND type is "inherited" from the TYPE_KINDS module.
!
! Revision 1.4  2001/05/09 17:43:27  paulv
! - Corrected error in documentation.
!
! Revision 1.3  2001/05/09 17:39:42  paulv
! - All constant definitions are now double precision only.
!
! Revision 1.2  2000/12/18 21:15:47  paulv
! - Added single and double precision definitions.
!
! Revision 1.1  2000/05/03 18:36:27  paulv
! Initial checked-in version
!
!
