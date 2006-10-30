!------------------------------------------------------------------------------
!P+
! NAME:
!       Effective_Layer_TP_Test
!
! PURPOSE:
!       Program to test the Effective_Layer_TP routine in the
!       Level_Layer_Conversion module.
!
! CATEGORY:
!       Profile Utility : Level_Layer_Conversion
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
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Nov-2004
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

PROGRAM Effective_Layer_TP_Test


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE Units_Conversion
  USE Geopotential
  USE Level_Layer_Conversion


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Effective_Layer_TP_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Effective_Layer_TP_Test.f90,v 1.2 2006/05/02 22:04:35 wd20pd Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- The dimension
  INTEGER, PARAMETER :: N_LEVELS = 50
  INTEGER, PARAMETER :: N_LAYERS = N_LEVELS-1

  ! -- The profile data.
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



  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  INTEGER :: k

  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: H2O_Pressure
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Height
  REAL( fp_kind ), DIMENSION( N_LAYERS ) :: Layer_Pressure
  REAL( fp_kind ), DIMENSION( N_LAYERS ) :: Layer_Temperature



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the Effective_Layer_TP routine in the" )' )
  WRITE( *, '(/5x, "   Level_Layer_Conversion module." )' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '( 5x, a )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#           -- CONVERT THE WATER VAPOR UNITS TO PARTIAL PRESSURE --          #
  !#----------------------------------------------------------------------------#

  H2O_Pressure = PPMV_to_PP( PRESSURE, &
                             WATER_VAPOR )



  !#----------------------------------------------------------------------------#
  !#         -- COMPUTE THE EFFECTIVE LAYER PRESSURE AND TEMPERATURE --         #
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


  !#----------------------------------------------------------------------------#
  !#                            -- OUTPUT RESULTS --                            #
  !#----------------------------------------------------------------------------#
 
  WRITE( *, '( /2x, "    P(Bot)      P(Top)      P(Eff)  &
                    &    T(Bot)      T(Top)      T(Eff)" )' )

  DO k = 1, N_LAYERS

    WRITE( *, '( 2x, 6( f12.6 ) )' ) &
              PRESSURE( k ), PRESSURE( k+1 ), Layer_Pressure( k ), &
              TEMPERATURE( k ), TEMPERATURE( k+1 ), Layer_Temperature( k )

  END DO

END PROGRAM Effective_Layer_TP_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Effective_Layer_TP_Test.f90,v 1.2 2006/05/02 22:04:35 wd20pd Exp $
!
! $Date: 2006/05/02 22:04:35 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Effective_Layer_TP_Test.f90,v $
! Revision 1.2  2006/05/02 22:04:35  wd20pd
! - Replaced all references to Error_Handler with Message_Handler.
!
! Revision 1.1  2004/11/29 18:07:53  paulv
! Initial checkin.
!
!
!
