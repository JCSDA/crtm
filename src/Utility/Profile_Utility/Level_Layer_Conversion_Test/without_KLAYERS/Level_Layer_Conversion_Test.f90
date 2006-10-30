!------------------------------------------------------------------------------
!
! NAME:
!       Level_Layer_Conversion_Test
!
! PURPOSE:
!       Program to test the Create_Sublevels and Integrate_Sublevels routines
!       in the Level_Layer_Conversion module.
!
! CATEGORY:
!       Profile Utility : Level_Layer_Conversion
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:              Module to hold specification kinds for
!                                variable declaration.
!
!       File_Utility:            Module containing generic file utility
!                                routines
!
!       Message_Handler:           Module to define simple error codes and
!                                handle error conditions.
!                                USEs: TYPE_KINDS module
!                                      FILE_UTILITY module
!
!       Units_Conversion:        Module containing routines to convert
!                                atmospheric profile concentration units.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!                                      PROFILE_UTILITY_PARAMETERS module
!                                      ATMOSPHERIC_PROPERTIES module
!
!       Geopotential:            Module containing routines for calculating
!                                geopotential heights.
!                                USEs: TYPE_KINDS module
!                                      FUNDAMENTAL_CONSTANTS module
!                                      ERROR_HANDLER module
!                                      ATMOSPHERIC_PROPERTIES module
!
!       Level_Layer_Conversion:  Module containing routines to convert
!                                LEVEL atmospheric profile quantities
!                                to LAYER quantities.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!                                      PROFILE_UTILITY_PARAMETERS module
!                                      ATMOSPHERIC_PROPERTIES module
!                                      UNITS_CONVERSION module
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
!       Profile_Conversion.asc: OUTPUT. Columnar ASCII column data file
!                               containing the output of the CREATE_SUBLEVELS
!                               and INTEGRATE_SUBLEVELS functions.
!     
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Dec-2000
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
!
!------------------------------------------------------------------------------

PROGRAM Level_Layer_Conversion_Test

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE Units_Conversion
  USE Geopotential
  USE Level_Layer_Conversion


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Level_Layer_Conversion_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Level_Layer_Conversion_Test.f90,v 1.6 2006/05/02 22:04:35 wd20pd Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Output filename
  CHARACTER( * ), PARAMETER :: FILENAME = 'Profile_Conversion.asc'

  ! -- The dimensions
  INTEGER, PARAMETER :: N_LEVELS    = 50
  INTEGER, PARAMETER :: N_LAYERS    = N_LEVELS - 1
  INTEGER, PARAMETER :: N_ABSORBERS = 6
  INTEGER, PARAMETER :: N_PER_LAYER = 10
  INTEGER, PARAMETER :: N_SUBLEVELS = ( N_LAYERS * N_PER_LAYER ) + 1

  ! -- Literal constants
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Location of water vapour in absorber array
  INTEGER, PARAMETER :: H2O_J_INDEX = 1


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: FileID
  INTEGER :: j, k
  INTEGER :: IO_Status
  INTEGER :: Error_Status

  ! -- Profile data
  REAL( fp_kind ), DIMENSION( N_LEVELS )              :: Pressure
  REAL( fp_kind ), DIMENSION( N_LEVELS )              :: Temperature
  REAL( fp_kind ), DIMENSION( N_LEVELS, N_ABSORBERS ) :: Absorber

  ! -- Outputs from Create_Sublevels
  REAL( fp_kind ), DIMENSION( N_SUBLEVELS )              :: Sublevel_p, Sublevel_t
  REAL( fp_kind ), DIMENSION( N_SUBLEVELS, N_ABSORBERS ) :: Sublevel_aa

  ! -- Output from PPMV_to_PP
  REAL( fp_kind ), DIMENSION( N_SUBLEVELS ) :: Water_Vapor_Pressure

  ! -- Outputs from Geopotential_Height
  REAL( fp_kind ), DIMENSION( N_SUBLEVELS ) :: Sublevel_z

  ! -- Outputs from Integrate_Sublevels
  REAL( fp_kind ), DIMENSION( N_LEVELS-1 )              :: Layer_p, Layer_t
  REAL( fp_kind ), DIMENSION( N_LEVELS-1, N_ABSORBERS ) :: Layer_aa




  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the Create_Sublevels and Integrate_Sublevels" )' )
  WRITE( *, '(/5x, "   routines in the Level_Layer_Conversion module." )' )
  WRITE( *, '(/5x, " $Revision: 1.6 $")' )
  WRITE( *, '( 5x, a )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#               -- LOAD U.S. STANDARD ATMOSPHERE PROFILE DATA --             #
  !#----------------------------------------------------------------------------#

  Pressure =  (/ &
    1.013e+03_fp_kind, 8.988e+02_fp_kind, 7.950e+02_fp_kind, 7.012e+02_fp_kind, 6.166e+02_fp_kind, &
    5.405e+02_fp_kind, 4.722e+02_fp_kind, 4.111e+02_fp_kind, 3.565e+02_fp_kind, 3.080e+02_fp_kind, &
    2.650e+02_fp_kind, 2.270e+02_fp_kind, 1.940e+02_fp_kind, 1.658e+02_fp_kind, 1.417e+02_fp_kind, &
    1.211e+02_fp_kind, 1.035e+02_fp_kind, 8.850e+01_fp_kind, 7.565e+01_fp_kind, 6.467e+01_fp_kind, &
    5.529e+01_fp_kind, 4.729e+01_fp_kind, 4.047e+01_fp_kind, 3.467e+01_fp_kind, 2.972e+01_fp_kind, &
    2.549e+01_fp_kind, 1.743e+01_fp_kind, 1.197e+01_fp_kind, 8.010e+00_fp_kind, 5.746e+00_fp_kind, &
    4.150e+00_fp_kind, 2.871e+00_fp_kind, 2.060e+00_fp_kind, 1.491e+00_fp_kind, 1.090e+00_fp_kind, &
    7.978e-01_fp_kind, 4.250e-01_fp_kind, 2.190e-01_fp_kind, 1.090e-01_fp_kind, 5.220e-02_fp_kind, &
    2.400e-02_fp_kind, 1.050e-02_fp_kind, 4.460e-03_fp_kind, 1.840e-03_fp_kind, 7.600e-04_fp_kind, &
    3.200e-04_fp_kind, 1.450e-04_fp_kind, 7.100e-05_fp_kind, 4.010e-05_fp_kind, 2.540e-05_fp_kind /)

  Temperature = (/ &
    288.20_fp_kind, 281.70_fp_kind, 275.20_fp_kind, 268.70_fp_kind, 262.20_fp_kind, &
    255.70_fp_kind, 249.20_fp_kind, 242.70_fp_kind, 236.20_fp_kind, 229.70_fp_kind, &
    223.30_fp_kind, 216.80_fp_kind, 216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, &
    216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, &
    216.70_fp_kind, 217.60_fp_kind, 218.60_fp_kind, 219.60_fp_kind, 220.60_fp_kind, &
    221.60_fp_kind, 224.00_fp_kind, 226.50_fp_kind, 230.00_fp_kind, 236.50_fp_kind, &
    242.90_fp_kind, 250.40_fp_kind, 257.30_fp_kind, 264.20_fp_kind, 270.60_fp_kind, &
    270.70_fp_kind, 260.80_fp_kind, 247.00_fp_kind, 233.30_fp_kind, 219.60_fp_kind, &
    208.40_fp_kind, 198.60_fp_kind, 188.90_fp_kind, 186.90_fp_kind, 188.40_fp_kind, &
    195.10_fp_kind, 208.80_fp_kind, 240.00_fp_kind, 300.00_fp_kind, 360.00_fp_kind /)

  ! -- Water vapour
  Absorber( :, 1 ) = (/ &
    7.745e+03_fp_kind, 6.071e+03_fp_kind, 4.631e+03_fp_kind, 3.182e+03_fp_kind, 2.158e+03_fp_kind, &
    1.397e+03_fp_kind, 9.254e+02_fp_kind, 5.720e+02_fp_kind, 3.667e+02_fp_kind, 1.583e+02_fp_kind, &
    6.996e+01_fp_kind, 3.613e+01_fp_kind, 1.906e+01_fp_kind, 1.085e+01_fp_kind, 5.927e+00_fp_kind, &
    5.000e+00_fp_kind, 3.950e+00_fp_kind, 3.850e+00_fp_kind, 3.825e+00_fp_kind, 3.850e+00_fp_kind, &
    3.900e+00_fp_kind, 3.975e+00_fp_kind, 4.065e+00_fp_kind, 4.200e+00_fp_kind, 4.300e+00_fp_kind, &
    4.425e+00_fp_kind, 4.575e+00_fp_kind, 4.725e+00_fp_kind, 4.825e+00_fp_kind, 4.900e+00_fp_kind, &
    4.950e+00_fp_kind, 5.025e+00_fp_kind, 5.150e+00_fp_kind, 5.225e+00_fp_kind, 5.250e+00_fp_kind, &
    5.225e+00_fp_kind, 5.100e+00_fp_kind, 4.750e+00_fp_kind, 4.200e+00_fp_kind, 3.500e+00_fp_kind, &
    2.825e+00_fp_kind, 2.050e+00_fp_kind, 1.330e+00_fp_kind, 8.500e-01_fp_kind, 5.400e-01_fp_kind, &
    4.000e-01_fp_kind, 3.400e-01_fp_kind, 2.800e-01_fp_kind, 2.400e-01_fp_kind, 2.000e-01_fp_kind /)

  ! -- Ozone
  Absorber( :, 2 ) = (/ &
    2.660e-02_fp_kind, 2.931e-02_fp_kind, 3.237e-02_fp_kind, 3.318e-02_fp_kind, 3.387e-02_fp_kind, &
    3.768e-02_fp_kind, 4.112e-02_fp_kind, 5.009e-02_fp_kind, 5.966e-02_fp_kind, 9.168e-02_fp_kind, &
    1.313e-01_fp_kind, 2.149e-01_fp_kind, 3.095e-01_fp_kind, 3.846e-01_fp_kind, 5.030e-01_fp_kind, &
    6.505e-01_fp_kind, 8.701e-01_fp_kind, 1.187e+00_fp_kind, 1.587e+00_fp_kind, 2.030e+00_fp_kind, &
    2.579e+00_fp_kind, 3.028e+00_fp_kind, 3.647e+00_fp_kind, 4.168e+00_fp_kind, 4.627e+00_fp_kind, &
    5.118e+00_fp_kind, 5.803e+00_fp_kind, 6.553e+00_fp_kind, 7.373e+00_fp_kind, 7.837e+00_fp_kind, &
    7.800e+00_fp_kind, 7.300e+00_fp_kind, 6.200e+00_fp_kind, 5.250e+00_fp_kind, 4.100e+00_fp_kind, &
    3.100e+00_fp_kind, 1.800e+00_fp_kind, 1.100e+00_fp_kind, 7.000e-01_fp_kind, 3.000e-01_fp_kind, &
    2.500e-01_fp_kind, 3.000e-01_fp_kind, 5.000e-01_fp_kind, 7.000e-01_fp_kind, 7.000e-01_fp_kind, &
    4.000e-01_fp_kind, 2.000e-01_fp_kind, 5.000e-02_fp_kind, 5.000e-03_fp_kind, 5.000e-04_fp_kind /)

  ! -- Nitrous oxide
  Absorber( :, 3 ) = (/ &
    3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, &
    3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.195e-01_fp_kind, &
    3.179e-01_fp_kind, 3.140e-01_fp_kind, 3.095e-01_fp_kind, 3.048e-01_fp_kind, 2.999e-01_fp_kind, &
    2.944e-01_fp_kind, 2.877e-01_fp_kind, 2.783e-01_fp_kind, 2.671e-01_fp_kind, 2.527e-01_fp_kind, &
    2.365e-01_fp_kind, 2.194e-01_fp_kind, 2.051e-01_fp_kind, 1.967e-01_fp_kind, 1.875e-01_fp_kind, &
    1.756e-01_fp_kind, 1.588e-01_fp_kind, 1.416e-01_fp_kind, 1.165e-01_fp_kind, 9.275e-02_fp_kind, &
    6.693e-02_fp_kind, 4.513e-02_fp_kind, 2.751e-02_fp_kind, 1.591e-02_fp_kind, 9.378e-03_fp_kind, &
    4.752e-03_fp_kind, 3.000e-03_fp_kind, 2.065e-03_fp_kind, 1.507e-03_fp_kind, 1.149e-03_fp_kind, &
    8.890e-04_fp_kind, 7.056e-04_fp_kind, 5.716e-04_fp_kind, 4.708e-04_fp_kind, 3.932e-04_fp_kind, &
    3.323e-04_fp_kind, 2.837e-04_fp_kind, 2.443e-04_fp_kind, 2.120e-04_fp_kind, 1.851e-04_fp_kind /)

  ! -- Carbon monoxide
  Absorber( :, 4 ) = (/ &
    1.500e-01_fp_kind, 1.450e-01_fp_kind, 1.399e-01_fp_kind, 1.349e-01_fp_kind, 1.312e-01_fp_kind, &
    1.303e-01_fp_kind, 1.288e-01_fp_kind, 1.247e-01_fp_kind, 1.185e-01_fp_kind, 1.094e-01_fp_kind, &
    9.962e-02_fp_kind, 8.964e-02_fp_kind, 7.814e-02_fp_kind, 6.374e-02_fp_kind, 5.025e-02_fp_kind, &
    3.941e-02_fp_kind, 3.069e-02_fp_kind, 2.489e-02_fp_kind, 1.966e-02_fp_kind, 1.549e-02_fp_kind, &
    1.331e-02_fp_kind, 1.232e-02_fp_kind, 1.232e-02_fp_kind, 1.307e-02_fp_kind, 1.400e-02_fp_kind, &
    1.498e-02_fp_kind, 1.598e-02_fp_kind, 1.710e-02_fp_kind, 1.850e-02_fp_kind, 2.009e-02_fp_kind, &
    2.220e-02_fp_kind, 2.497e-02_fp_kind, 2.824e-02_fp_kind, 3.241e-02_fp_kind, 3.717e-02_fp_kind, &
    4.597e-02_fp_kind, 6.639e-02_fp_kind, 1.073e-01_fp_kind, 1.862e-01_fp_kind, 3.059e-01_fp_kind, &
    6.375e-01_fp_kind, 1.497e+00_fp_kind, 3.239e+00_fp_kind, 5.843e+00_fp_kind, 1.013e+01_fp_kind, &
    1.692e+01_fp_kind, 2.467e+01_fp_kind, 3.356e+01_fp_kind, 4.148e+01_fp_kind, 5.000e+01_fp_kind /)

  ! - Methane
  Absorber( :, 5 ) = (/ &
    1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, &
    1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.699e+00_fp_kind, 1.697e+00_fp_kind, 1.693e+00_fp_kind, &
    1.685e+00_fp_kind, 1.675e+00_fp_kind, 1.662e+00_fp_kind, 1.645e+00_fp_kind, 1.626e+00_fp_kind, &
    1.605e+00_fp_kind, 1.582e+00_fp_kind, 1.553e+00_fp_kind, 1.521e+00_fp_kind, 1.480e+00_fp_kind, &
    1.424e+00_fp_kind, 1.355e+00_fp_kind, 1.272e+00_fp_kind, 1.191e+00_fp_kind, 1.118e+00_fp_kind, &
    1.055e+00_fp_kind, 9.870e-01_fp_kind, 9.136e-01_fp_kind, 8.300e-01_fp_kind, 7.460e-01_fp_kind, &
    6.618e-01_fp_kind, 5.638e-01_fp_kind, 4.614e-01_fp_kind, 3.631e-01_fp_kind, 2.773e-01_fp_kind, &
    2.100e-01_fp_kind, 1.650e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, &
    1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.400e-01_fp_kind, 1.300e-01_fp_kind, &
    1.200e-01_fp_kind, 1.100e-01_fp_kind, 9.500e-02_fp_kind, 6.000e-02_fp_kind, 3.000e-02_fp_kind /)

  ! -- Nitrogen
  Absorber( :, 6 ) = (/ &
    7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
    7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
    7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
    7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
    7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
    7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
    7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
    7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
    7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.80e+05_fp_kind, 7.79e+05_fp_kind, &
    7.77e+05_fp_kind, 7.74e+05_fp_kind, 7.70e+05_fp_kind, 7.65e+05_fp_kind, 7.60e+05_fp_kind /)



  !#----------------------------------------------------------------------------#
  !#                   -- INTERPOLATE THE DATA ONTO SublevelS --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Profile interpolation stage...." )' )

  Error_Status = Create_Sublevels( Pressure,    &  ! Input
                                   Temperature, &  ! Input
                                   Absorber,    &  ! Input
                                   N_PER_LAYER, &  ! Input
                                   Sublevel_p,  &  ! Output
                                   Sublevel_t,  &  ! Output
                                   Sublevel_aa  )  ! Output

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error in Create_Sublevels', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- CALCULATE GEOPOTENTIAL HEIGHTS --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Calculating geopotential height profile...." )' )


  ! ---------------------------------------------
  ! Convert water vapor ppmv to partial pressure
  ! for use with the Geopotential_Height function
  ! ---------------------------------------------

  Water_Vapor_Pressure = PPMV_to_PP( Sublevel_p, &
                                     Sublevel_aa(:,H2O_J_INDEX) )

  IF ( ANY( Water_Vapor_Pressure < ZERO ) ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error converting water vapor ppmv to partial pressure', &
                          FAILURE )
    STOP
  END IF


  ! -----------------
  ! Calculate heights
  ! -----------------

  Error_Status = Geopotential_Height( Sublevel_p,            &  ! Input
                                      Sublevel_t,            &  ! Input
                                      Water_Vapor_Pressure,  &  ! Input
                                      Sublevel_z,            &  ! Output
                                      Surface_Height = ZERO, &  ! Optional input
                                      Gravity_Correction = 1 )  ! Optional input

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error in Geopotential_Height', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                     -- INTEGRATE THE PROFILE DATA --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Profile integration stage...." )' )

  Error_Status = Integrate_Sublevels( Sublevel_z,  &  ! Input
                                      Sublevel_p,  &  ! Input
                                      Sublevel_t,  &  ! Input
                                      Sublevel_aa, &  ! Input
                                      N_PER_LAYER, &  ! Input
                                      H2O_J_INDEX, &  ! Input
                                      Layer_p,     &  ! Output
                                      Layer_t,     &  ! Output
                                      Layer_aa     )  ! Output

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error in Integrate_Sublevels', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- OUTPUT THE DATA FOR COMPARISON --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Writing output file ", a, "...." )' ) FILENAME


  ! ----------------------
  ! Get a free unit number
  ! ----------------------

  FileID = Get_Lun()

  IF ( FileID < 0 ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error obtaining output file unit number', &
                          FAILURE )
    STOP
  END IF

  ! -------------
  ! Open the file
  ! -------------

  OPEN( FileID, FILE   = FILENAME,&
                STATUS = 'REPLACE',   &
                FORM   = 'FORMATTED', &
                ACCESS = 'SEQUENTIAL', &
                IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error opening output file '//FILENAME, &
                          FAILURE )
    STOP
  END IF


  ! ----------------------------
  ! Output dimension information
  ! ----------------------------

  WRITE( FileID, FMT    = '( "! Dimensions; N_SUBLEVELS, N_LEVELS, N_ABSORBERS", &
                             &/, 3( 2x, i5 ) )', &
                 IOSTAT = IO_Status ) N_SUBLEVELS, N_LEVELS, N_ABSORBERS

  IF ( IO_Status /= 0 ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error writing dimension data to output file '//FILENAME, &
                          FAILURE )
    STOP
  END IF


  ! --------------------------------
  ! Output the interpolation results
  ! --------------------------------

  ! -- Write the header
  WRITE( FileID, FMT    = '( "     Z(km)   ",&
                            &"     PF(mb)   ",&
                            &"     T(K)    ",&
                            &"  H2O(ppmv)     ",&
                            &"  O3(ppmv)    ",&
                            &"  N2O(ppmv)     ",&
                            &"  CO(ppmv)    ",&
                            &"  CH4(ppmv)     ",&
                            &"  N2(ppmv)", &
                            &/, 128( "-" ) )', &
                 IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error writing interpolation results header to output file '//FILENAME, &
                          FAILURE )
    STOP
  END IF


  ! -- Write the sublevel data
  DO k = 1, N_SUBLEVELS

    WRITE( FileID, FMT    = '( 2x, f8.3, 4x, f12.7, 2x, 2x, f7.3, 6( 2x, es13.6 ) )', &
                   IOSTAT = IO_Status ) Sublevel_z( k )/1000.0_fp_kind, &
                                        Sublevel_p( k ), &
                                        Sublevel_t( k ), &
                                        ( Sublevel_aa( k, j ), j = 1, N_ABSORBERS )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing interpolation results for sublevel #", i6, &
                        &" to output file ", a )' ) k, FILENAME
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

  END DO


  ! ------------------------------
  ! Output the integration results
  ! ------------------------------

  ! -- Write the header
  WRITE( FileID, FMT    = '( 2/, "     PL(mb)  ",&
                             &"     TL(K)     ",&
                             &"  H2OL(kmol/cm2) ",&
                             &"  O3L(kmol/cm2)",&
                             &"  N2OL(kmol/cm2) ",&
                             &"  COL(kmol/cm2)",&
                             &"  CH4L(kmol/cm2) ",&
                             &"  N2L(kmol/cm2)", &
                             &/, 124( "-" ) )', &
                 IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error writing integration results header to output file '//FILENAME, &
                          FAILURE )
    STOP
  END IF

  ! -- Write the layer data
  DO k = 1, N_LEVELS - 1

    WRITE( FileID, FMT    = '( 2x, f10.5, 3x, 2x, f7.3, 3x, 6( 3x, es13.6 ) )', &
                   IOSTAT = IO_Status ) Layer_p( k ), &
                                        Layer_t( k ), &
                                        ( Layer_aa( k, j ), j = 1, N_ABSORBERS )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing integration results for layer #", i6, &
                        &" to output file ", a )' ) k, FILENAME
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

  END DO


  !#----------------------------------------------------------------------------#
  !#                      -- CLOSE THE OUTPUT DATA FILE --                      #
  !#----------------------------------------------------------------------------#

  CLOSE( FileID )

END PROGRAM Level_Layer_Conversion_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Level_Layer_Conversion_Test.f90,v 1.6 2006/05/02 22:04:35 wd20pd Exp $
!
! $Date: 2006/05/02 22:04:35 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Level_Layer_Conversion_Test.f90,v $
! Revision 1.6  2006/05/02 22:04:35  wd20pd
! - Replaced all references to Error_Handler with Message_Handler.
!
! Revision 1.5  2004/11/30 00:08:03  paulv
! - Added program header.
! - Removed unused variable declarations.
!
! Revision 1.4  2004/11/22 18:36:02  paulv
! - Updated to use new utility modules.
!
! Revision 1.3  2003/05/22 15:38:17  paulv
! - Added more output file checks. Changed Integrate_Sublevels() call to
!   reflect the addition of the H2O_J_Index argument.
!
! Revision 1.2  2002/11/27 15:11:50  paulv
! - Changes made for testing.
!
! Revision 1.1  2002/10/04 20:52:16  paulv
! Initial checkin.
!
!
!
!
!
