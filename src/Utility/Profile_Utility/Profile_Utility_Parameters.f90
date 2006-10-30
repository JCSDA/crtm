!------------------------------------------------------------------------------
!M+
! NAME:
!       Profile_Utility_Parameters
!
! PURPOSE:
!       Module containing parameters used in the profile utility modules.
!
! CATEGORY:
!       Profile Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Profile_Utility_Parameters
!
! MODULES:
!       Type_Kinds:               Module containing definitions for kinds of
!                                 variable types.
!
!       Fundamental_Constants:    Module containing definitions for some
!                                 fundamental physical constants.
!                                 USEs: TYPE_KINDS module
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
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

MODULE Profile_Utility_Parameters


  ! ------------
  ! Modules used
  ! ------------

  ! -- Define default floating point precision.
  USE Type_Kinds, ONLY: fp_kind

  ! -- Use only some fundamental constants
  USE Fundamental_Constants, ONLY : NA => AVOGADRO_CONSTANT,    &
                                    R0 => MOLAR_GAS_CONSTANT,   &
                                    L0 => LOSCHMIDT_CONSTANT,   &
                                    P0 => STANDARD_ATMOSPHERE,  &
                                    T0 => STANDARD_TEMPERATURE, &
                                    G0 => STANDARD_GRAVITY,     &
                                    PI


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------

  ! -- Everything is default private
  PRIVATE

  ! -- Make renamed fundamental constants public
  PUBLIC :: NA
  PUBLIC :: R0
  PUBLIC :: L0
  PUBLIC :: P0
  PUBLIC :: T0
  PUBLIC :: G0
  PUBLIC :: PI



  !#----------------------------------------------------------------------------#
  !#                        -- PARAMETER DEFINITIONS --                         #
  !#----------------------------------------------------------------------------#

  ! -------------------
  ! Numerical constants
  ! -------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: ZERO      = 0.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: ONE       = 1.0_fp_kind

  REAL( fp_kind ), PUBLIC, PARAMETER :: TOLERANCE = EPSILON( ONE )


  ! ------------------
  ! Conversion factors
  ! ------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: CELSIUS_TO_KELVIN = T0
  REAL( fp_kind ), PUBLIC, PARAMETER :: G_TO_KG           = 1.0e-03_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: KG_TO_G           = 1.0e+03_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: PA_TO_HPA         = 1.0e-02_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: HPA_TO_PA         = 1.0e+02_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: PPMV_TO_PPV       = 1.0e-06_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: PPV_TO_PPMV       = 1.0e+06_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: CM_TO_M           = 1.0e-02_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: M_TO_CM           = 1.0e+02_fp_kind


  ! ------------------------------------
  ! Units specifiers. Same as for LBLRTM
  ! ------------------------------------

  INTEGER, PUBLIC, PARAMETER :: PPMV_UNITS = 1  ! Units in ppmv
  INTEGER, PUBLIC, PARAMETER :: ND_UNITS   = 2  ! Units in cm^-3  (number density)
  INTEGER, PUBLIC, PARAMETER :: MR_UNITS   = 3  ! Units in g/kg   (mixing ratio)
  INTEGER, PUBLIC, PARAMETER :: MD_UNITS   = 4  ! Units in g.m^-3 (mass density)
  INTEGER, PUBLIC, PARAMETER :: PP_UNITS   = 5  ! Units in hPa    (partial pressure)


  ! ---------------------------------------------------
  ! Maximum number of molecular species (as for HITRAN)
  ! ---------------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_MOLECULAR_SPECIES = 32


  ! --------------------------------
  ! HITRAN molecular IDs and symbols
  ! --------------------------------

  INTEGER, PUBLIC, PARAMETER :: ID_H2O   = 1
  INTEGER, PUBLIC, PARAMETER :: ID_CO2   = 2
  INTEGER, PUBLIC, PARAMETER :: ID_O3    = 3
  INTEGER, PUBLIC, PARAMETER :: ID_N2O   = 4
  INTEGER, PUBLIC, PARAMETER :: ID_CO    = 5
  INTEGER, PUBLIC, PARAMETER :: ID_CH4   = 6
  INTEGER, PUBLIC, PARAMETER :: ID_O2    = 7

  INTEGER, PUBLIC, PARAMETER :: ID_NO    = 8
  INTEGER, PUBLIC, PARAMETER :: ID_SO2   = 9
  INTEGER, PUBLIC, PARAMETER :: ID_NO2   = 10
  INTEGER, PUBLIC, PARAMETER :: ID_NH3   = 11
  INTEGER, PUBLIC, PARAMETER :: ID_HNO3  = 12
  INTEGER, PUBLIC, PARAMETER :: ID_OH    = 13
  INTEGER, PUBLIC, PARAMETER :: ID_HF    = 14

  INTEGER, PUBLIC, PARAMETER :: ID_HCL   = 15
  INTEGER, PUBLIC, PARAMETER :: ID_HBR   = 16
  INTEGER, PUBLIC, PARAMETER :: ID_HI    = 17
  INTEGER, PUBLIC, PARAMETER :: ID_CLO   = 18
  INTEGER, PUBLIC, PARAMETER :: ID_OCS   = 19
  INTEGER, PUBLIC, PARAMETER :: ID_H2CO  = 20
  INTEGER, PUBLIC, PARAMETER :: ID_HOCL  = 21

  INTEGER, PUBLIC, PARAMETER :: ID_N2    = 22
  INTEGER, PUBLIC, PARAMETER :: ID_HCN   = 23
  INTEGER, PUBLIC, PARAMETER :: ID_CH3CL = 24
  INTEGER, PUBLIC, PARAMETER :: ID_H2O2  = 25
  INTEGER, PUBLIC, PARAMETER :: ID_C2H2  = 26
  INTEGER, PUBLIC, PARAMETER :: ID_C2H6  = 27
  INTEGER, PUBLIC, PARAMETER :: ID_PH3   = 28

  INTEGER, PUBLIC, PARAMETER :: ID_COF2  = 29
  INTEGER, PUBLIC, PARAMETER :: ID_SF6   = 30
  INTEGER, PUBLIC, PARAMETER :: ID_H2S   = 31
  INTEGER, PUBLIC, PARAMETER :: ID_HCOOH = 32


  CHARACTER( * ), PUBLIC, PARAMETER, &
                  DIMENSION( MAX_N_MOLECULAR_SPECIES ) :: MOLECULAR_SYMBOL = &
      (/ 'H2O  ','CO2  ','O3   ','N2O  ', &
         'CO   ','CH4  ','O2   ','NO   ', &
         'SO2  ','NO2  ','NH3  ','HNO3 ', &
         'OH   ','HF   ','HCL  ','HBR  ', &
         'HI   ','CLO  ','OCS  ','H2CO ', &
         'HOCL ','N2   ','HCN  ','CH3CL', &
         'H2O2 ','C2H2 ','C2H6 ','PH3  ', &
         'COF2 ','SF6  ','H2S  ','HCOOH' /)


  ! -----------------
  ! Molecular weights
  ! -----------------

  ! -- Weights of first seven HITRAN molecular species
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_H2O = 18.01528_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_CO2 = 44.00950_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_O3  = 47.99820_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_N2O = 44.01288_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_CO  = 28.01010_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_CH4 = 16.04246_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_O2  = 31.99880_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_N2  = 28.01348_fp_kind

  ! -- Weights of all 32 HITRAN molecular species
  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( MAX_N_MOLECULAR_SPECIES ) :: MOLECULAR_WEIGHT = &
    (/            MW_H2O,             MW_CO2,              MW_O3,             MW_N2O, &
                  MW_CO ,             MW_CH4,              MW_O2,   30.00614_fp_kind, &
        64.06480_fp_kind,   46.00554_fp_kind,   17.03056_fp_kind,   63.01288_fp_kind, &
        17.00734_fp_kind,   20.00634_fp_kind,   36.46064_fp_kind,   80.91194_fp_kind, &
       127.91241_fp_kind,   51.45210_fp_kind,   60.07610_fp_kind,   30.02598_fp_kind, &
        52.46004_fp_kind,             MW_N2 ,   27.02538_fp_kind,   50.48722_fp_kind, &
        34.01468_fp_kind,   26.03728_fp_kind,   30.06904_fp_kind,   33.99758_fp_kind, &
        66.00690_fp_kind,  146.05643_fp_kind,   34.08188_fp_kind,   46.02538_fp_kind /)

  ! -- Average molecular weight of dry air
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_DRYAIR = 28.9648_fp_kind

  ! -- Ratio of water vapor and dry air weights for conversion routines
  REAL( fp_kind ), PUBLIC, PARAMETER :: EPS       = MW_H2O / MW_DRYAIR


  ! -------------------------------
  ! Some derived physical constants
  ! -------------------------------

  ! -- Gas constant for dry air. Units are J.K-1.kg-1
  REAL( fp_kind ), PUBLIC, PARAMETER :: R_DRYAIR = R0 / ( MW_DRYAIR * G_TO_KG )

  ! -- Specific heat of dry air. Units are J.K-1.kg-1
  REAL( fp_kind ), PRIVATE, PARAMETER :: TWO       = 2.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: SEVEN     = 7.0_fp_kind
  REAL( fp_kind ), PUBLIC,  PARAMETER :: Cp_DRYAIR = ( SEVEN/TWO ) * R_DRYAIR


END MODULE Profile_Utility_Parameters


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Profile_Utility_Parameters.f90,v 1.7 2004/11/29 23:37:57 paulv Exp $
!
! $Date: 2004/11/29 23:37:57 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Profile_Utility_Parameters.f90,v $
! Revision 1.7  2004/11/29 23:37:57  paulv
! - Altered order of declaration of TOLERANCE parameter to use already declared
!   literal constant as EPSILON argument.
!
! Revision 1.6  2004/11/18 17:16:56  paulv
! - Updated header documentation
!
! Revision 1.5  2003/05/22 15:43:55  paulv
! - Updated documentation.
!
! Revision 1.4  2003/02/24 22:15:20  paulv
! - Added more molecular ID and weight definitions.
!
! Revision 1.3  2002/10/04 20:56:09  paulv
! - Added full list of HITRAN absorber molecular weights.
! - Replaced old units flags with those consistent with LBLRTM input.
!
! Revision 1.2  2002/09/20 16:23:45  paulv
! - Added gas constant and specific heat for dry air.
!
! Revision 1.1  2002/08/30 22:48:12  paulv
! Initial checkin. Untested.
! - The contents of this module have been extracted from the old PROFILE_CONVERSION
!   module and split up into different categories of profile processing.
!
!
!
