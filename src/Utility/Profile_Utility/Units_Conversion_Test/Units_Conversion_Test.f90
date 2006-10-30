!------------------------------------------------------------------------------
!P+
! NAME:
!       Units_Conversion_Test
!
! PURPOSE:
!       Program to test the routines in the Units_Conversion module
!
! CATEGORY:
!       Profile Utility
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
!       Message_Handler:               Module to define simple error codes and
!                                    handle error conditions
!                                    USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:       Module containing routines to perform
!                                    equality and relational comparisons on
!                                    floating point numbers.
!                                    USEs: TYPE_KINDS module
!
!       Profile_Utility_Parameters:  Module containing parameters used in the
!                                    profile utility modules.
!                                    USEs: TYPE_KINDS module
!                                          FUNDAMENTAL_CONSTANTS module
!
!       Geopotential:                Module containing routines for calculating
!                                    geopotential heights.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          FUNDAMENTAL_CONSTANTS module
!                                          ATMOSPHERIC_PROPERTIES module
!
!       Level_Layer_Conversion:      Module containing routines to convert
!                                    LEVEL atmospheric profile quantities
!                                    to LAYER quantities.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          PROFILE_UTILITY_PARAMETERS module
!                                          ATMOSPHERIC_PROPERTIES module
!                                          UNITS_CONVERSION module
!
!       Units_Conversion:            Module containing routines to convert
!                                    atmospheric profile concentration units.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          PROFILE_UTILITY_PARAMETERS module
!                                          ATMOSPHERIC_PROPERTIES module
!
! CONTAINS:
!       Test_Results:       Subroutine to compare the data returned from
!                           the conversion routines.
!
!       Output_Difference:  Subroutine to output data to screen when a
!                           comparison fails.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Nov-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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
!P-
!------------------------------------------------------------------------------

PROGRAM Units_Conversion_Test


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler
  USE Compare_Float_Numbers

  USE Profile_Utility_Parameters
  USE Geopotential
  USE Level_Layer_Conversion
  USE Units_Conversion


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Units_Conversion_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Units_Conversion_Test.f90,v 1.5 2006/05/02 22:04:35 wd20pd Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- The dimension
  INTEGER, PARAMETER :: N_LEVELS = 50

  ! -- The number of absorbers and their IDs
  INTEGER, PARAMETER :: N_ABSORBERS = 5

  INTEGER, PARAMETER, DIMENSION( N_ABSORBERS ) :: MOLECULE_ID = (/ ID_H2O, &
                                                                   ID_CO2, & 
                                                                   ID_O3,  &
                                                                   ID_CO,  &
                                                                   ID_CH4 /)        

  CHARACTER(*), PARAMETER, DIMENSION( N_ABSORBERS ) :: MOLECULE_NAME = (/ 'WATER_VAPOR    ', &
                                                                          'CARBON_DIOXIDE ', & 
                                                                          'OZONE          ', &
                                                                          'CARBON_MONOXIDE', &
                                                                          'METHANE        ' /)        

  ! -- The minimum pressure to test the relative humidity routines
  REAL( fp_kind ), PARAMETER :: MIN_PRESSURE = 50.0_fp_kind

  ! -- The profile data. Absorber amounts are in ppmv.
  ! -- Original UMBC profile #1 (Tropical Atm)
  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: PRESSURE = &
  (/ 1.01300e+03_fp_kind,  9.04000e+02_fp_kind,  8.05000e+02_fp_kind,  7.15000e+02_fp_kind, &
     6.33000e+02_fp_kind,  5.59000e+02_fp_kind,  4.92000e+02_fp_kind,  4.32000e+02_fp_kind, &
     3.78000e+02_fp_kind,  3.29000e+02_fp_kind,  2.86000e+02_fp_kind,  2.47000e+02_fp_kind, &
     2.13000e+02_fp_kind,  1.82000e+02_fp_kind,  1.56000e+02_fp_kind,  1.32000e+02_fp_kind, &
     1.11000e+02_fp_kind,  9.37000e+01_fp_kind,  7.89000e+01_fp_kind,  6.66000e+01_fp_kind, &
     5.65000e+01_fp_kind,  4.80000e+01_fp_kind,  4.09000e+01_fp_kind,  3.50000e+01_fp_kind, &
     3.00000e+01_fp_kind,  2.57000e+01_fp_kind,  1.76300e+01_fp_kind,  1.22000e+01_fp_kind, &
     8.52000e+00_fp_kind,  6.00000e+00_fp_kind,  4.26000e+00_fp_kind,  3.05000e+00_fp_kind, &
     2.20000e+00_fp_kind,  1.59000e+00_fp_kind,  1.16000e+00_fp_kind,  8.54000e-01_fp_kind, &
     4.56000e-01_fp_kind,  2.39000e-01_fp_kind,  1.21000e-01_fp_kind,  5.80000e-02_fp_kind, &
     2.60000e-02_fp_kind,  1.10000e-02_fp_kind,  4.40000e-03_fp_kind,  1.72000e-03_fp_kind, &
     6.88000e-04_fp_kind,  2.89000e-04_fp_kind,  1.30000e-04_fp_kind,  6.47000e-05_fp_kind, &
     3.60000e-05_fp_kind,  2.25000e-05_fp_kind /)

  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: TEMPERATURE = &
  (/ 299.70_fp_kind, 293.70_fp_kind, 287.70_fp_kind, 283.70_fp_kind, &
     277.00_fp_kind, 270.30_fp_kind, 263.60_fp_kind, 257.00_fp_kind, &
     250.30_fp_kind, 243.60_fp_kind, 237.00_fp_kind, 230.10_fp_kind, &
     223.60_fp_kind, 217.00_fp_kind, 210.30_fp_kind, 203.70_fp_kind, &
     197.00_fp_kind, 194.80_fp_kind, 198.80_fp_kind, 202.70_fp_kind, &
     206.70_fp_kind, 210.70_fp_kind, 214.60_fp_kind, 217.00_fp_kind, &
     219.20_fp_kind, 221.40_fp_kind, 227.00_fp_kind, 232.30_fp_kind, &
     237.70_fp_kind, 243.10_fp_kind, 248.50_fp_kind, 254.00_fp_kind, &
     259.40_fp_kind, 264.80_fp_kind, 269.60_fp_kind, 270.20_fp_kind, &
     263.40_fp_kind, 253.10_fp_kind, 236.00_fp_kind, 218.90_fp_kind, &
     201.80_fp_kind, 184.80_fp_kind, 177.10_fp_kind, 177.00_fp_kind, &
     184.30_fp_kind, 190.70_fp_kind, 212.00_fp_kind, 241.60_fp_kind, &
     299.70_fp_kind, 380.00_fp_kind /)

  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: WATER_VAPOR = &
  (/ 2.593e+04_fp_kind,  1.949e+04_fp_kind,  1.534e+04_fp_kind,  8.600e+03_fp_kind, &
     4.441e+03_fp_kind,  3.346e+03_fp_kind,  2.101e+03_fp_kind,  1.289e+03_fp_kind, &
     7.637e+02_fp_kind,  4.098e+02_fp_kind,  1.912e+02_fp_kind,  7.306e+01_fp_kind, &
     2.905e+01_fp_kind,  9.900e+00_fp_kind,  6.220e+00_fp_kind,  4.000e+00_fp_kind, &
     3.000e+00_fp_kind,  2.900e+00_fp_kind,  2.750e+00_fp_kind,  2.600e+00_fp_kind, &
     2.600e+00_fp_kind,  2.650e+00_fp_kind,  2.800e+00_fp_kind,  2.900e+00_fp_kind, &
     3.200e+00_fp_kind,  3.250e+00_fp_kind,  3.600e+00_fp_kind,  4.000e+00_fp_kind, &
     4.300e+00_fp_kind,  4.600e+00_fp_kind,  4.900e+00_fp_kind,  5.200e+00_fp_kind, &
     5.500e+00_fp_kind,  5.700e+00_fp_kind,  5.900e+00_fp_kind,  6.000e+00_fp_kind, &
     6.000e+00_fp_kind,  6.000e+00_fp_kind,  5.400e+00_fp_kind,  4.500e+00_fp_kind, &
     3.300e+00_fp_kind,  2.100e+00_fp_kind,  1.300e+00_fp_kind,  8.500e-01_fp_kind, &
     5.400e-01_fp_kind,  4.000e-01_fp_kind,  3.400e-01_fp_kind,  2.800e-01_fp_kind, &
     2.400e-01_fp_kind,  2.000e-01_fp_kind /)

  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: CARBON_DIOXIDE = &
  (/ 3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
     3.700e+02_fp_kind,  3.678e+02_fp_kind,  3.588e+02_fp_kind,  3.476e+02_fp_kind, &
     3.027e+02_fp_kind,  2.186e+02_fp_kind,  1.233e+02_fp_kind,  6.727e+01_fp_kind, &
     4.485e+01_fp_kind,  3.924e+01_fp_kind /)

  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: OZONE = &
  (/ 2.869e-02_fp_kind,  3.150e-02_fp_kind,  3.342e-02_fp_kind,  3.504e-02_fp_kind, &
     3.561e-02_fp_kind,  3.767e-02_fp_kind,  3.989e-02_fp_kind,  4.223e-02_fp_kind, &
     4.471e-02_fp_kind,  5.000e-02_fp_kind,  5.595e-02_fp_kind,  6.613e-02_fp_kind, &
     7.815e-02_fp_kind,  9.289e-02_fp_kind,  1.050e-01_fp_kind,  1.256e-01_fp_kind, &
     1.444e-01_fp_kind,  2.500e-01_fp_kind,  5.000e-01_fp_kind,  9.500e-01_fp_kind, &
     1.400e+00_fp_kind,  1.800e+00_fp_kind,  2.400e+00_fp_kind,  3.400e+00_fp_kind, &
     4.300e+00_fp_kind,  5.400e+00_fp_kind,  7.800e+00_fp_kind,  9.300e+00_fp_kind, &
     9.850e+00_fp_kind,  9.700e+00_fp_kind,  8.800e+00_fp_kind,  7.500e+00_fp_kind, &
     5.900e+00_fp_kind,  4.500e+00_fp_kind,  3.450e+00_fp_kind,  2.800e+00_fp_kind, &
     1.800e+00_fp_kind,  1.100e+00_fp_kind,  6.500e-01_fp_kind,  3.000e-01_fp_kind, &
     1.800e-01_fp_kind,  3.300e-01_fp_kind,  5.000e-01_fp_kind,  5.200e-01_fp_kind, &
     5.000e-01_fp_kind,  4.000e-01_fp_kind,  2.000e-01_fp_kind,  5.000e-02_fp_kind, &
     5.000e-03_fp_kind,  5.000e-04_fp_kind /)

  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: CARBON_MONOXIDE = &
  (/ 1.441e-01_fp_kind,  1.301e-01_fp_kind,  1.205e-01_fp_kind,  1.185e-01_fp_kind, &
     1.300e-01_fp_kind,  1.433e-01_fp_kind,  1.394e-01_fp_kind,  1.467e-01_fp_kind, &
     9.681e-02_fp_kind,  4.414e-02_fp_kind,  4.255e-02_fp_kind,  4.168e-02_fp_kind, &
     3.936e-02_fp_kind,  4.099e-02_fp_kind,  3.844e-02_fp_kind,  3.595e-02_fp_kind, &
     3.398e-02_fp_kind,  3.555e-02_fp_kind,  2.084e-02_fp_kind,  2.102e-02_fp_kind, &
     2.016e-02_fp_kind,  1.808e-02_fp_kind,  1.507e-02_fp_kind,  1.872e-02_fp_kind, &
     1.781e-02_fp_kind,  2.180e-02_fp_kind,  1.975e-02_fp_kind,  2.079e-02_fp_kind, &
     1.893e-02_fp_kind,  2.145e-02_fp_kind,  2.706e-02_fp_kind,  3.170e-02_fp_kind, &
     3.084e-02_fp_kind,  4.384e-02_fp_kind,  5.032e-02_fp_kind,  6.413e-02_fp_kind, &
     9.016e-02_fp_kind,  1.318e-01_fp_kind,  2.354e-01_fp_kind,  3.243e-01_fp_kind, &
     7.947e-01_fp_kind,  1.873e+00_fp_kind,  3.739e+00_fp_kind,  7.558e+00_fp_kind, &
     1.412e+01_fp_kind,  1.841e+01_fp_kind,  2.602e+01_fp_kind,  3.510e+01_fp_kind, &
     4.369e+01_fp_kind,  5.532e+01_fp_kind /)

  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: METHANE = &
  (/ 1.800e+00_fp_kind,  1.800e+00_fp_kind,  1.800e+00_fp_kind,  1.800e+00_fp_kind, &
     1.800e+00_fp_kind,  1.800e+00_fp_kind,  1.800e+00_fp_kind,  1.799e+00_fp_kind, &
     1.797e+00_fp_kind,  1.793e+00_fp_kind,  1.784e+00_fp_kind,  1.774e+00_fp_kind, &
     1.760e+00_fp_kind,  1.742e+00_fp_kind,  1.722e+00_fp_kind,  1.699e+00_fp_kind, &
     1.675e+00_fp_kind,  1.644e+00_fp_kind,  1.610e+00_fp_kind,  1.567e+00_fp_kind, &
     1.508e+00_fp_kind,  1.435e+00_fp_kind,  1.347e+00_fp_kind,  1.261e+00_fp_kind, &
     1.184e+00_fp_kind,  1.117e+00_fp_kind,  1.045e+00_fp_kind,  9.673e-01_fp_kind, &
     8.788e-01_fp_kind,  7.899e-01_fp_kind,  7.007e-01_fp_kind,  5.970e-01_fp_kind, &
     4.885e-01_fp_kind,  3.845e-01_fp_kind,  2.936e-01_fp_kind,  2.224e-01_fp_kind, &
     1.748e-01_fp_kind,  1.588e-01_fp_kind,  1.588e-01_fp_kind,  1.588e-01_fp_kind, &
     1.588e-01_fp_kind,  1.588e-01_fp_kind,  1.588e-01_fp_kind,  1.482e-01_fp_kind, &
     1.376e-01_fp_kind,  1.271e-01_fp_kind,  1.165e-01_fp_kind,  1.006e-01_fp_kind, &
     6.353e-02_fp_kind,  3.176e-02_fp_kind /)


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  INTEGER :: j, k

  REAL( fp_kind ), DIMENSION( N_LEVELS )   :: H2O_Pressure
  REAL( fp_kind ), DIMENSION( N_LEVELS )   :: H2O_ppmv
  REAL( fp_kind ), DIMENSION( N_LEVELS )   :: H2O_Number_Density
  REAL( fp_kind ), DIMENSION( N_LEVELS )   :: H2O_Mixing_Ratio
  REAL( fp_kind ), DIMENSION( N_LEVELS )   :: H2O_Specific_Amount
  REAL( fp_kind ), DIMENSION( N_LEVELS-1 ) :: H2O_Layer_ppmv
  REAL( fp_kind ), DIMENSION( N_LEVELS-1 ) :: H2O_kmolcm2

  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Height

  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: dPressure
  INTEGER                                :: pIdx

  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Profile_Data
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Mixing_Ratio, New_Mixing_Ratio
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Specific_Amount
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Relative_Humidity
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Partial_Pressure, New_Partial_Pressure
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: ppmv
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Number_Density
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Mass_Density

  REAL( fp_kind ), DIMENSION( N_LEVELS-1 ) :: Layer_Pressure
  REAL( fp_kind ), DIMENSION( N_LEVELS-1 ) :: Layer_Temperature
  REAL( fp_kind ), DIMENSION( N_LEVELS-1 ) :: Layer_Thickness
  REAL( fp_kind ), DIMENSION( N_LEVELS-1 ) :: Layer_ppmv, New_Layer_ppmv
  REAL( fp_kind ), DIMENSION( N_LEVELS-1 ) :: Layer_kmolcm2



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the Units_Conversion module routines." )' )
  WRITE( *, '(/5x, " $Revision: 1.5 $")' )
  WRITE( *, '( 5x, a )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                  -- GATHER THE VARIOUS WATER VAPOR BITS --                 #
  !#----------------------------------------------------------------------------#

  H2O_ppmv            = WATER_VAPOR

  H2O_Pressure        = PPMV_to_PP( PRESSURE, &
                                    WATER_VAPOR )

  H2O_Number_Density  = PPMV_to_ND( PRESSURE, &
                                    TEMPERATURE, &
                                    WATER_VAPOR )

  H2O_Mixing_Ratio    = PPMV_to_MR( WATER_VAPOR )

  H2O_Specific_Amount = MR_to_SA( H2O_Mixing_Ratio )



  !#----------------------------------------------------------------------------#
  !#          -- GET EVERYTHING SET UP FOR THE kmol/cm^2 CONVERSIONS --         #
  !#----------------------------------------------------------------------------#

  ! --------------------------------
  ! Compute the geopotential heights
  ! --------------------------------

  Error_Status = Geopotential_Height( PRESSURE, &
                                      TEMPERATURE, &
                                      H2O_Pressure, &
                                      Height )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error calculating the geopotential heights', &
                          Error_Status )
    STOP
  END IF


  ! ---------------------------
  ! Compute the layer thickness
  ! ---------------------------

  Layer_Thickness = Height( 2:N_LEVELS ) - Height( 1:N_LEVELS-1 )


  ! ----------------------------------------------------
  ! Compute the effective layer pressure and temperature
  ! ----------------------------------------------------

  Error_Status = Effective_Layer_TP( Height, &
                                     PRESSURE, &
                                     TEMPERATURE, &
                                     H2O_Pressure, &
                                     Layer_Pressure, &
                                     Layer_Temperature )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error calculating the effective layer pressure and temperature', &
                          Error_Status )
    STOP
  END IF


  ! --------------------------------------------------------
  ! Compute the water vapor kmol/cm2 for the other molecules
  ! --------------------------------------------------------

  H2O_Layer_ppmv = 0.5_fp_kind * ( H2O_ppmv(1:N_LEVELS-1) + H2O_ppmv(2:N_LEVELS) )

  H2O_kmolcm2 = PPMV_to_KMOL( Layer_Pressure, &
                              Layer_Temperature, &
                              Layer_Thickness, &
                              H2O_Layer_ppmv )



  !#----------------------------------------------------------------------------#
  !#                         -- LOOP OVER ABSORBER --                           #
  !#----------------------------------------------------------------------------#

  Absorber_Loop: DO j = 1, N_ABSORBERS



    !#--------------------------------------------------------------------------#
    !#                       -- LOAD AN ABSORBER PROFILE --                     #
    !#--------------------------------------------------------------------------#

    SELECT CASE ( MOLECULE_ID(j) )

      CASE ( ID_H2O )
        Profile_Data = WATER_VAPOR

      CASE ( ID_CO2 )
        Profile_Data = CARBON_DIOXIDE

      CASE ( ID_O3 )
        Profile_Data = OZONE

      CASE ( ID_CO )
        Profile_Data = CARBON_MONOXIDE

      CASE ( ID_CH4 )
        Profile_Data = METHANE

    END SELECT

    WRITE( *, '( //5x, "Molecule: ", a )' ) MOLECULE_NAME(j)



    !#--------------------------------------------------------------------------#
    !#               -- FIRST TEST ROUTINE PAIRS FOR CONSISTENCY --             #
    !#--------------------------------------------------------------------------#

    WRITE( *, '( /10x, "Testing conversion routines for consistency..." )' )


    ! -------------------------
    ! ppmv <-> partial pressure
    ! -------------------------

    ! -- ppmv -> pp
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      Partial_Pressure = PPMV_to_PP( PRESSURE, &
                                     Profile_Data )
    ELSE
      Partial_Pressure = PPMV_to_PP( PRESSURE, &
                                     Profile_Data, &
                                     Water_Vapor = H2O_Pressure )
    END IF

    ! -- pp -> ppmv
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      ppmv = PP_to_PPMV( PRESSURE, &
                         Partial_Pressure )
    ELSE
      ppmv = PP_to_PPMV( PRESSURE, &
                         PArtial_Pressure, &
                         Water_Vapor = H2O_ppmv )
    END IF

    ! -- Test the result
    CALL Test_Results( ppmv, Profile_Data, 'PP<->PPMV  ' )


    ! ---------------------
    ! ppmv <-> mixing ratio
    ! ---------------------

    ! -- ppmv -> mixing ratio
    Mixing_Ratio = PPMV_to_MR( Profile_Data, &
                               Molecule_ID = MOLECULE_ID(j) )

    ! -- Mixing ratio -> ppmv
    ppmv = MR_to_PPMV( Mixing_Ratio, &
                       Molecule_ID = MOLECULE_ID(j) )

    ! -- Test the result
    CALL Test_Results( ppmv, Profile_Data, 'MR<->PPMV  ' )


    ! ---------------------------------
    ! mixing ratio <-> specific amounts
    ! ---------------------------------

    ! -- Mixing ratio -> specific amount
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      Specific_Amount = MR_to_SA( Mixing_Ratio )
    ELSE
      Specific_Amount = MR_to_SA( Mixing_Ratio, &
                                  Water_Vapor = H2O_Specific_Amount )
    END IF


    ! -- Specific amount -> mixing ratio
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      New_Mixing_Ratio = SA_to_MR( Specific_Amount )
    ELSE
      New_Mixing_Ratio = SA_to_MR( Specific_Amount, &
                                   Water_Vapor = H2O_Mixing_Ratio )
    END IF

    ! -- Test the result
    CALL Test_Results( New_Mixing_Ratio, Mixing_Ratio, 'MR<->SA    ' )


    ! ---------------------------------
    ! Partial pressure <-> mixing ratio
    ! ---------------------------------

    ! -- mixing ratio -> pp
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      Partial_Pressure = MR_to_PP( PRESSURE, &
                                   Mixing_Ratio )
    ELSE
      Partial_Pressure = MR_to_PP( PRESSURE, &
                                   Mixing_Ratio, &
                                   Molecule_ID = MOLECULE_ID(j), &
                                   Water_Vapor = H2O_Pressure )
    END IF

    ! -- pp-> mixing ratio
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      New_Mixing_Ratio = PP_to_MR( PRESSURE, &
                                   Partial_Pressure )
    ELSE
      New_Mixing_Ratio = PP_to_MR( PRESSURE, &
                                   Partial_Pressure, &
                                   Molecule_ID = MOLECULE_ID(j), &
                                   Water_Vapor = H2O_Mixing_Ratio )
    END IF

    ! -- Test the result
    CALL Test_Results( New_Mixing_Ratio, Mixing_Ratio, 'MR<->PP    ' )


    ! -----------------------------------------------------
    ! Mixing ratio <-> relative humidity (Water vapor only)
    ! -----------------------------------------------------

    IF ( MOLECULE_ID(j) == ID_H2O ) THEN

      ! -- mixing ratio -> RH
      Relative_Humidity = MR_to_RH( PRESSURE, &
                                    TEMPERATURE, &
                                    Mixing_Ratio, &
                                    Min_Pressure = MIN_PRESSURE )

      ! -- RH -> mixing ratio
      New_Mixing_Ratio = RH_to_MR( PRESSURE, &
                                   TEMPERATURE, &
                                   Relative_Humidity, &
                                   Min_Pressure = MIN_PRESSURE )

      ! -- Only test the RH routines up to the minimum pressure
      dPressure = PRESSURE - MIN_PRESSURE
      pIdx = MINLOC( dPressure, MASK = dPressure > ZERO, DIM = 1 )
      CALL Test_Results( New_Mixing_Ratio(1:pIdx), Mixing_Ratio(1:pIdx), 'MR<->RH    ' )

    END IF


    ! ---------------------------------
    ! Partial pressure <-> mass density
    ! ---------------------------------

    ! -- Partial pressure -> mass density
    Mass_Density = PP_to_MD( Partial_Pressure, &
                             TEMPERATURE, &
                             Molecule_ID = MOLECULE_ID(j) )

    ! -- Mass density -> partial pressure
    New_Partial_Pressure = MD_to_PP( Mass_Density, &
                                    TEMPERATURE, &
                                    Molecule_ID = MOLECULE_ID(j) ) 

    ! -- Test the result
    CALL Test_Results( New_Partial_Pressure, Partial_Pressure, 'PP<->MD    ' )


    ! -----------------------------------
    ! Partial pressure <-> number density
    ! -----------------------------------

    ! -- Partial pressure -> number density
    Number_Density = PP_to_ND( Partial_Pressure, &
                               TEMPERATURE )

    ! -- Number density -> partial pressure
    New_Partial_Pressure = ND_to_PP( Number_Density, &
                                     TEMPERATURE ) 

    ! -- Test the result
    CALL Test_Results( New_Partial_Pressure, Partial_Pressure, 'PP<->ND    ' )


    ! -----------------------
    ! ppmv <-> number density
    ! -----------------------

    ! -- ppmv -> number density
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      Number_Density = PPMV_to_ND( PRESSURE, &
                                   TEMPERATURE, &
                                   Profile_Data )
    ELSE
      Number_Density = PPMV_to_ND( PRESSURE, &
                                   Temperature, &
                                   Profile_Data, &
                                   Water_Vapor = H2O_Number_Density )
    END IF

    ! -- Number density -> ppmv
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      ppmv = ND_to_PPMV( PRESSURE, &
                         TEMPERATURE, &
                         Number_Density )
    ELSE
      ppmv = ND_to_PPMV( PRESSURE, &
                         TEMPERATURE, &
                         Number_Density, &
                         Water_Vapor = H2O_ppmv )
    END IF

    ! -- Test the result
    CALL Test_Results( ppmv, Profile_Data, 'PPMV<->ND  ' )


    ! ---------------------
    ! ppmv <-> kmol_per_cm2
    ! ---------------------

    ! -- Compute the average ppmv for the layer
    Layer_ppmv = 0.5_fp_kind * ( Profile_Data(1:N_LEVELS-1) + Profile_Data(2:N_LEVELS) )

    ! -- ppmv -> kmol_per_cm2
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      Layer_kmolcm2 = PPMV_to_KMOL( Layer_Pressure, &
                                    Layer_Temperature, &
                                    Layer_Thickness, &
                                    Layer_ppmv )
    ELSE
      Layer_kmolcm2 = PPMV_to_KMOL( Layer_Pressure, &
                                    Layer_Temperature, &
                                    Layer_Thickness, &
                                    Layer_ppmv, &
                                    Water_Vapor = H2O_kmolcm2 )
    END IF


    ! -- kmol_per_cm2 -> ppmv
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      New_Layer_ppmv = KMOL_to_PPMV( Layer_Pressure, &
                                     Layer_Temperature, &
                                     Layer_Thickness, &
                                     Layer_kmolcm2 )
    ELSE
      New_Layer_ppmv = KMOL_to_PPMV( Layer_Pressure, &
                                     Layer_Temperature, &
                                     Layer_Thickness, &
                                     Layer_kmolcm2, &
                                     Water_Vapor = H2O_Layer_ppmv )
    END IF

    ! -- Test the result
    CALL Test_Results( New_Layer_ppmv, Layer_ppmv, 'PPMV<->KMOL' )



    !#--------------------------------------------------------------------------#
    !#                    -- CONVERT DATA UNITS IN A CHAIN --                   #
    !#--------------------------------------------------------------------------#

    WRITE( *, '( /10x, "Testing conversion routines in a chain...." )' )


    ! -------------------------------------
    ! Convert original data to mixing ratio
    ! -------------------------------------

    WRITE( *, '( 15x, "Converting PPMV -> MR...." )' )
    Mixing_Ratio = PPMV_to_MR( Profile_Data, &
                               Molecule_ID = MOLECULE_ID(j) )


    ! ----------------------------------------
    ! Convert mixing ratio to partial pressure
    ! ----------------------------------------

    WRITE( *, '( 15x, "Converting MR -> PP...." )' )
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      Partial_Pressure = MR_to_PP( PRESSURE, &
                                   Mixing_Ratio )
    ELSE
      Partial_Pressure = MR_to_PP( PRESSURE, &
                                   Mixing_Ratio, &
                                   Molecule_ID = MOLECULE_ID(j), &
                                   Water_Vapor = H2O_Pressure )
    END IF


    ! ------------------------------------------
    ! Convert partial pressure to number density
    ! ------------------------------------------

    WRITE( *, '( 15x, "Converting PP -> ND...." )' )
    Number_Density = PP_to_ND( Partial_Pressure, &
                               TEMPERATURE )


    ! ------------------------------
    ! Convert number density to ppmv
    ! ------------------------------

    WRITE( *, '( 15x, "Converting ND -> PPMV...." )' )
    IF ( MOLECULE_ID(j) == ID_H2O ) THEN
      ppmv = ND_to_PPMV( PRESSURE, &
                         TEMPERATURE, &
                         Number_Density )
    ELSE
      ppmv = ND_to_PPMV( PRESSURE, &
                         TEMPERATURE, &
                         Number_Density, &
                         Water_Vapor = H2O_ppmv )
    END IF


    ! ---------------
    ! Test the result
    ! ---------------

    CALL Test_Results( ppmv, Profile_Data, 'PPMV->MR->PP->ND->PPMV' )

    IF ( j < N_ABSORBERS ) THEN
      WRITE( *, '( /5x, "Press <ENTER> to continue..." )' )
      READ( *, '( a )' ) pn_fmt
    END IF

  END DO Absorber_Loop


CONTAINS


  !#----------------------------------------------------------------------------#
  !#               -- SUBROUTINE TO COMPARE THE DATA RETURNED --                #
  !#               --      FROM THE CONVERSION ROUTINES       --                #
  !#----------------------------------------------------------------------------#

  SUBROUTINE Test_Results( x, y, Comparison )
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN ) :: x, y
    CHARACTER( * ),                  INTENT( IN ) :: Comparison

    INTEGER, PARAMETER ::     MAX_ULPS = 10
    INTEGER, PARAMETER :: WARNING_ULPS = 5

    CHARACTER( 256 ) :: Message
    INTEGER          :: MaxIdx
    REAL( fp_kind )  :: MaxValue
    REAL( fp_kind )  :: MaxDiff
    REAL( fp_kind )  :: MaxDiffpc
    INTEGER          :: n, ULP
    LOGICAL          :: Equal
    INTEGER          :: Error_Status


    MaxDiff   = MAXVAL( ABS( x-y ) )
    MaxIdx    = MAXLOC( ABS( x-y ), DIM = 1 )
    MaxValue  = MAX( ABS(x(MaxIdx)), ABS(y(MaxIdx)) )
    MaxDiffpc = 100.0_fp_kind * MaxDiff / MaxValue

    Equal = .FALSE.
    ULP_Loop: DO n = 1, MAX_ULPS
      IF ( ALL( Compare_Float( x, y, ULP = n ) ) ) THEN
        Equal = .TRUE.
        ULP   = n
        EXIT ULP_Loop
      END IF
    END DO ULP_Loop

    IF ( Equal ) THEN
      IF ( ULP <= WARNING_ULPS ) THEN
        Error_Status = INFORMATION
      ELSE
        Error_Status = WARNING
      END IF

      WRITE( Message, '( a, " consistent. ULP = ", i2, ", %MAX(|Diff|) = ", es13.6 )' ) &
                      Comparison, ULP, MaxDiffpc
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
    ELSE
      WRITE( Message, '( a, " inconsistent.  %MAX(|Diff|) = ", es13.6 )' ) &
                      Comparison, MaxDiffpc
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      CALL Output_Difference( x, y, Comparison )
    END IF

  END SUBROUTINE Test_Results


  !#----------------------------------------------------------------------------#
  !#                 -- SUBROUTINE TO OUTPUT DATA TO SCREEN --                  #
  !#                 --       WHEN A COMPARISON FAILS       --                  #
  !#----------------------------------------------------------------------------#

  SUBROUTINE Output_Difference( New, Old, Units )
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN ) :: New, Old
    CHARACTER( * ),                  INTENT( IN ) :: Units

    REAL( fp_kind ), DIMENSION( SIZE( New) ) :: Difference
    REAL( fp_kind ), DIMENSION( SIZE( New) ) :: Percentage_Difference

    Difference = New - Old
    WHERE( New > ZERO )
      Percentage_Difference = 100.0_fp_kind * Difference / New
    ELSEWHERE
      Percentage_Difference = 999.0_fp_kind
    END WHERE

    WRITE( *, '( /5x, "DATA UNITS COMPARISON : ", a )' ) Units
    WRITE( *, '( "       NEW          ORIGINAL        Diff.         % Diff" )' )
    WRITE( *, '( 2x, 56("-") )' )
    DO k = 1, SIZE( New )
      WRITE( *, '( 3( 2x, es13.6 ), 2x, f10.5 )' ) &
                New(k), Old(k), &
                Difference(k), Percentage_Difference(k)
    END DO

    READ( *, * )

  END SUBROUTINE Output_Difference

END PROGRAM Units_Conversion_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Units_Conversion_Test.f90,v 1.5 2006/05/02 22:04:35 wd20pd Exp $
!
! $Date: 2006/05/02 22:04:35 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Units_Conversion_Test.f90,v $
! Revision 1.5  2006/05/02 22:04:35  wd20pd
! - Replaced all references to Error_Handler with Message_Handler.
!
! Revision 1.4  2004/11/29 18:46:35  paulv
! - Replaced ELSE WHERE with ELSEWHERE statement (no space between ELSE and
!   WHERE) in the internal subprogram Output_Differences. The former is
!   apparently not standard.
!
! Revision 1.3  2004/11/25 01:10:12  paulv
! - New version testing all the Units_Conversion module routines. The routines
!   are tested in tandem for consistency, and in a call chain.
!
! Revision 1.2  2004/11/22 18:48:06  paulv
! - Renamed code to Units_Conversion_Test.
!
! Revision 1.1  2004/11/09 18:05:31  paulv
! Initial checkin.
!
!
!
