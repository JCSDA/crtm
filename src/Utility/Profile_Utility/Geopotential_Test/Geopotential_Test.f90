!------------------------------------------------------------------------------
!
! NAME:
!       Geopotential_Height_Test
!
! PURPOSE:
!       Test program for geopotential height calculation.
!
! CATEGORY:
!       Profile Utility
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
!                                USEs: Type_Kinds module
!                                      FILE_UTILITY module
!
!       Units_Conversion:        Module containing routines to convert
!                                atmospheric profile concentration units.
!                                USEs: Type_Kinds module
!                                      Message_Handler module
!                                      PROFILE_UTILITY_PARAMETERS module
!                                      ATMOSPHERIC_PROPERTIES module
!
!       Geopotential:            Module containing routines for calculating
!                                geopotential heights.
!                                USEs: Type_Kinds module
!                                      FUNDAMENTAL_CONSTANTS module
!                                      Message_Handler module
!                                      ATMOSPHERIC_PROPERTIES module
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
!       Geopotential_Test.asc: OUTPUT. ASCII data file containing
!                              the output of the GEOPOTENTIAL_HEIGHT()
!                              function calls.
!     
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
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

PROGRAM Geopotential_Test

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds, ONLY : fp_kind
  USE File_Utility
  USE Message_Handler

  USE Units_Conversion, ONLY : PPMV_to_PP
  USE Geopotential


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Geopotential_Test'
  CHARACTER( * ), PARAMETER :: RCS_ID = &
  '$Id: Geopotential_Test.f90,v 1.5 2006/05/02 22:04:35 wd20pd Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Output filename
  CHARACTER( * ), PARAMETER :: FILENAME = 'Geopotential_Test.asc'

  ! -- Define dimension
  INTEGER, PARAMETER :: N_LEVELS = 101

  ! -- Define test profile pressure
  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: PRESSURE = &
    (/ 1100.000_fp_kind,1070.917_fp_kind,1042.232_fp_kind,1013.948_fp_kind, 986.067_fp_kind, &
        958.591_fp_kind, 931.524_fp_kind, 904.866_fp_kind, 878.620_fp_kind, 852.788_fp_kind, &
        827.371_fp_kind, 802.371_fp_kind, 777.790_fp_kind, 753.628_fp_kind, 729.886_fp_kind, &
        706.565_fp_kind, 683.667_fp_kind, 661.192_fp_kind, 639.140_fp_kind, 617.511_fp_kind, &
        596.306_fp_kind, 575.525_fp_kind, 555.167_fp_kind, 535.232_fp_kind, 515.720_fp_kind, &
        496.630_fp_kind, 477.961_fp_kind, 459.712_fp_kind, 441.882_fp_kind, 424.470_fp_kind, &
        407.474_fp_kind, 390.893_fp_kind, 374.724_fp_kind, 358.966_fp_kind, 343.618_fp_kind, &
        328.675_fp_kind, 314.137_fp_kind, 300.000_fp_kind, 286.262_fp_kind, 272.919_fp_kind, &
        259.969_fp_kind, 247.409_fp_kind, 235.234_fp_kind, 223.441_fp_kind, 212.028_fp_kind, &
        200.989_fp_kind, 190.320_fp_kind, 180.018_fp_kind, 170.078_fp_kind, 160.496_fp_kind, &
        151.266_fp_kind, 142.385_fp_kind, 133.846_fp_kind, 125.646_fp_kind, 117.777_fp_kind, &
        110.237_fp_kind, 103.017_fp_kind,  96.114_fp_kind,  89.520_fp_kind,  83.231_fp_kind, &
         77.240_fp_kind,  71.540_fp_kind,  66.125_fp_kind,  60.990_fp_kind,  56.126_fp_kind, &
         51.528_fp_kind,  47.188_fp_kind,  43.100_fp_kind,  39.257_fp_kind,  35.650_fp_kind, &
         32.274_fp_kind,  29.121_fp_kind,  26.183_fp_kind,  23.453_fp_kind,  20.922_fp_kind, &
         18.585_fp_kind,  16.432_fp_kind,  14.456_fp_kind,  12.649_fp_kind,  11.004_fp_kind, &
          9.512_fp_kind,   8.165_fp_kind,   6.957_fp_kind,   5.878_fp_kind,   4.920_fp_kind, &
          4.077_fp_kind,   3.340_fp_kind,   2.701_fp_kind,   2.153_fp_kind,   1.687_fp_kind, &
          1.297_fp_kind,   0.975_fp_kind,   0.714_fp_kind,   0.506_fp_kind,   0.345_fp_kind, &
          0.224_fp_kind,   0.137_fp_kind,   0.077_fp_kind,   0.038_fp_kind,   0.016_fp_kind, &
          0.005_fp_kind /)

  ! -- Define test profile temperature
  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: TEMPERATURE = &
    (/ 304.043_fp_kind,302.630_fp_kind,301.199_fp_kind,299.749_fp_kind,298.280_fp_kind, &
       296.790_fp_kind,295.281_fp_kind,293.750_fp_kind,292.227_fp_kind,290.683_fp_kind, &
       289.118_fp_kind,287.590_fp_kind,286.540_fp_kind,285.475_fp_kind,284.395_fp_kind, &
       283.047_fp_kind,281.235_fp_kind,279.397_fp_kind,277.531_fp_kind,275.665_fp_kind, &
       273.782_fp_kind,271.870_fp_kind,269.939_fp_kind,268.020_fp_kind,266.071_fp_kind, &
       264.092_fp_kind,262.131_fp_kind,260.155_fp_kind,258.148_fp_kind,256.118_fp_kind, &
       254.067_fp_kind,251.983_fp_kind,249.880_fp_kind,247.807_fp_kind,245.698_fp_kind, &
       243.553_fp_kind,241.422_fp_kind,239.252_fp_kind,237.043_fp_kind,234.797_fp_kind, &
       232.509_fp_kind,230.178_fp_kind,227.958_fp_kind,225.700_fp_kind,223.408_fp_kind, &
       221.164_fp_kind,218.876_fp_kind,216.524_fp_kind,214.055_fp_kind,211.535_fp_kind, &
       209.083_fp_kind,206.692_fp_kind,204.249_fp_kind,201.792_fp_kind,199.292_fp_kind, &
       196.910_fp_kind,196.031_fp_kind,195.130_fp_kind,195.862_fp_kind,197.557_fp_kind, &
       199.289_fp_kind,201.053_fp_kind,202.874_fp_kind,204.840_fp_kind,206.863_fp_kind, &
       208.960_fp_kind,211.116_fp_kind,213.323_fp_kind,215.232_fp_kind,216.717_fp_kind, &
       218.157_fp_kind,219.623_fp_kind,221.135_fp_kind,222.759_fp_kind,224.456_fp_kind, &
       226.216_fp_kind,228.013_fp_kind,229.857_fp_kind,231.780_fp_kind,233.852_fp_kind, &
       236.043_fp_kind,238.355_fp_kind,240.821_fp_kind,243.424_fp_kind,246.229_fp_kind, &
       249.223_fp_kind,252.505_fp_kind,256.009_fp_kind,259.759_fp_kind,263.815_fp_kind, &
       267.901_fp_kind,269.940_fp_kind,268.260_fp_kind,264.528_fp_kind,258.953_fp_kind, &
       251.472_fp_kind,239.120_fp_kind,225.489_fp_kind,209.888_fp_kind,192.205_fp_kind, &
       178.174_fp_kind /)

  ! -- Define test profile water vapor in ppmv
  REAL( fp_kind ), PARAMETER, DIMENSION( N_LEVELS ) :: WATER_VAPOR_PPMV = &
    (/ 30590.990_fp_kind,29075.215_fp_kind,27539.310_fp_kind,25982.915_fp_kind,24405.609_fp_kind, &
       22806.964_fp_kind,21186.668_fp_kind,19544.166_fp_kind,18471.102_fp_kind,17403.377_fp_kind, &
       16320.759_fp_kind,15154.037_fp_kind,13385.207_fp_kind,11591.185_fp_kind, 9771.420_fp_kind, &
        8194.816_fp_kind, 7070.010_fp_kind, 5928.731_fp_kind, 4770.583_fp_kind, 4222.798_fp_kind, &
        3915.026_fp_kind, 3602.601_fp_kind, 3278.904_fp_kind, 2922.299_fp_kind, 2560.159_fp_kind, &
        2192.339_fp_kind, 1920.250_fp_kind, 1677.194_fp_kind, 1430.213_fp_kind, 1219.825_fp_kind, &
        1059.069_fp_kind,  895.642_fp_kind,  741.512_fp_kind,  632.000_fp_kind,  520.614_fp_kind, &
         408.258_fp_kind,  337.651_fp_kind,  265.787_fp_kind,  192.629_fp_kind,  153.473_fp_kind, &
         114.298_fp_kind,   74.393_fp_kind,   58.556_fp_kind,   43.271_fp_kind,   28.493_fp_kind, &
          21.983_fp_kind,   15.342_fp_kind,    9.639_fp_kind,    8.283_fp_kind,    6.898_fp_kind, &
           5.810_fp_kind,    5.006_fp_kind,    4.185_fp_kind,    3.715_fp_kind,    3.342_fp_kind, &
           2.996_fp_kind,    2.956_fp_kind,    2.915_fp_kind,    2.860_fp_kind,    2.797_fp_kind, &
           2.731_fp_kind,    2.663_fp_kind,    2.600_fp_kind,    2.600_fp_kind,    2.602_fp_kind, &
           2.628_fp_kind,    2.666_fp_kind,    2.751_fp_kind,    2.826_fp_kind,    2.888_fp_kind, &
           3.058_fp_kind,    3.210_fp_kind,    3.244_fp_kind,    3.335_fp_kind,    3.441_fp_kind, &
           3.551_fp_kind,    3.676_fp_kind,    3.816_fp_kind,    3.961_fp_kind,    4.086_fp_kind, &
           4.208_fp_kind,    4.336_fp_kind,    4.473_fp_kind,    4.618_fp_kind,    4.774_fp_kind, &
           4.939_fp_kind,    5.118_fp_kind,    5.312_fp_kind,    5.513_fp_kind,    5.664_fp_kind, &
           5.829_fp_kind,    5.957_fp_kind,    6.000_fp_kind,    6.000_fp_kind,    6.000_fp_kind, &
           5.943_fp_kind,    5.509_fp_kind,    4.847_fp_kind,    3.868_fp_kind,    2.623_fp_kind, &
           1.412_fp_kind /)



  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: FileID
  INTEGER :: IO_Status
  INTEGER :: Error_Status

  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Water_Vapor_Pressure

  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Height
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Height_with_Gravity_Correction
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Height_at_Latitude_60



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the Geopotential module. Three" )' )
  WRITE( *, '( 5x, " calculations are performed:" )' )
  WRITE( *, '( 5x, "   1) Calculation with standard gravity at the equator" )' )
  WRITE( *, '( 5x, "   2) Calculation with gravity profile correction at" )' )
  WRITE( *, '( 5x, "      the equator" )' )
  WRITE( *, '( 5x, "   3) Calculation with gravity profile correction at" )' )
  WRITE( *, '( 5x, "      60N latitude" )' )
  WRITE( *, '( 5x, " The initial heights and subsequent height differences" )' )
  WRITE( *, '( 5x, " are written to the file ", a )' ) TRIM( FILENAME )
  WRITE( *, '(/5x, " $Revision: 1.5 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                       -- OPEN THE OUTPUT DATA FILE --                      #
  !#----------------------------------------------------------------------------#


  ! ----------------------
  ! Get a free unit number
  ! ----------------------

  FileID = get_lun()

  IF ( FileID < 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
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
    CALL Display_Message( PROGRAM_NAME, &
                          'Error opening output file', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#             -- CONVERT WATER VAPOUR UNITS FROM ppmv to hPa --              #
  !#----------------------------------------------------------------------------#

  Water_Vapor_Pressure = PPMV_to_PP( PRESSURE, &
                                     WATER_VAPOR_PPMV )




  !#----------------------------------------------------------------------------#
  !#                  -- COMPUTE THE GEOPOTENTIAL HEIGHTS --                    #
  !#----------------------------------------------------------------------------#

  ! -------------------------------
  ! Calculate the plain old heights
  ! -------------------------------

  WRITE( *, '( /5x, "Computing geopotential heights..." )' )

  Error_Status = Geopotential_Height( PRESSURE, &
                                      TEMPERATURE, &
                                      Water_Vapor_Pressure, &
                                      Height )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Geopotential height calculation failed', &
                          FAILURE )
    STOP
  END IF

  ! -- Write them to the output file
  WRITE( FileID, '( 5x, "Z(plain) (m):" )' )
  WRITE( FileID, FMT    = '( 5f10.1 )', &
                 IOSTAT = IO_Status ) Height

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing geopotential heights to output file '//FILENAME, &
                          FAILURE )
    STOP
  END IF


  ! ------------------------------------------------------------
  ! Calculate the heights, but with a gravity profile correction
  ! ------------------------------------------------------------

  WRITE( *, '( /5x, "Computing geopotential heights with gravity correction..." )' )

  Error_Status = Geopotential_Height( PRESSURE, &
                                      TEMPERATURE, &
                                      Water_Vapor_Pressure, &
                                      Height_with_Gravity_Correction, &
                                      Gravity_Correction = 1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Geopotential height calculation with gravity correction failed', &
                          FAILURE )
    STOP
  END IF

  ! -- Write the differences to the output file
  WRITE( FileID, '( /5x, "Z(plain) - Z(gcorr) (m):" )' )
  WRITE( FileID, FMT    = '( 5f10.2 )', &
                 IOSTAT = IO_Status ) Height - Height_with_Gravity_Correction

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing height differences to output file '//FILENAME, &
                          FAILURE )
    STOP
  END IF


  ! ---------------------------------------------------
  ! Calculate the heights, but with the gravity profile
  ! correction and at a latitude of 60deg.
  ! ---------------------------------------------------

  WRITE( *, '( /5x, "Computing geopotential heights with gravity correction at 60deg. latitude..." )' )

  Error_Status = Geopotential_Height( PRESSURE, &
                                      TEMPERATURE, &
                                      Water_Vapor_Pressure, &
                                      Height_at_Latitude_60, &
                                      Gravity_Correction = 1, &
                                      Latitude = 60.0_fp_kind )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Geopotential height calculation @ 60deg. latitude failed', &
                          FAILURE )
    STOP
  END IF

  ! -- Write the differences to the output file
  WRITE( FileID, '( /5x, "Z(plain) - Z(gcorr@60N) (m):" )' )
  WRITE( FileID, FMT    = '( 5f10.2 )', &
                 IOSTAT = IO_Status ) Height - Height_at_Latitude_60

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing height differences to output file '//FILENAME, &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- CLOSE THE OUTPUT DATA FILE --                      #
  !#----------------------------------------------------------------------------#

  CLOSE( FileID )

END PROGRAM Geopotential_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Geopotential_Test.f90,v 1.5 2006/05/02 22:04:35 wd20pd Exp $
!
! $Date: 2006/05/02 22:04:35 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Geopotential_Test.f90,v $
! Revision 1.5  2006/05/02 22:04:35  wd20pd
! - Replaced all references to Error_Handler with Message_Handler.
!
! Revision 1.4  2004/11/22 19:44:33  paulv
! - Added program header.
!
! Revision 1.3  2003/05/22 20:46:05  paulv
! - Updated documentation.
! - Output file now prodcued from test program.
!
!
!
!
