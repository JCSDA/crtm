!------------------------------------------------------------------------------
!M+
! NAME:
!       Create_Solar_netCDF_File
!
! PURPOSE:
!       Program to read the ASCII format Kurucz solar source spectrum and
!       write it and a blackbody equivalent source function to a netCDF
!       format file.
!
! CATEGORY:
!       Solar_Spectrum
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
!       File_Utility:                Module containing generic file utility
!                                    routines
!
!       Error_Handler:               Module to define simple error codes and
!                                    handle error conditions
!                                    USEs: FILE_UTILITY module
!
!       Fundamental_Constants:       Module containing various fundamental physical
!                                    constants.
!                                    USEs: TYPE_KINDS module
!
!       Interpolate:                 Module containing interpolation routines
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       Planck_Functions:            Module containing Planck function routines
!                                    USEs: TYPE_KINDS module
!                                          FUNDAMENTAL_CONSTANTS module
!                                          ERROR_HANDLER module
!
!       Solar_Define:                Module defining the Solar data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       Solar_netCDF_IO:             Module containing routines to read and write
!                                    netCDF format Solar files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          SOLAR_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
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
!       Input:  ASCII data file containing the regularly spaced Kurucz solar
!               irradiance data.
!
!       Output: netCDF format file containing the Kurucz solar source spectrum
!               and a blackbody equivalent source function.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The input data from the ASCII file is read in and simply written
!       out after being scaled to mW/m^2.cm-1. The frequency grid of this
!       data is used to calculate an equivalent blackbody source function.
!
!       First the Planck radiance, Bsolar, using an equivalent blackbody
!       temperature, Tsolar, is calculated. This radiance spectrum is 
!       converted to irradiance by assuming an isotropic source:
!
!         Fsolar = !PI * Bsolar
!
!       The solar irradiance at the top of the Earth's atmosphere is
!       then estimated by multiplying by the ratio of the areas of the
!       Sun and the sphere described by the mean Earth-Sun distance:
!
!                               [    mean_solar_radius    ]2
!         Fsolar@TOA = Fsolar * [-------------------------]
!                               [ mean_earth_sun_distance ]
!
!       Note the 4pi factors in the area formulae cancel. The values
!       used in the geometric scaling are:
!
!         mean_solar_radius       = 6.599e+08m (visible disk, or photosphere)
!         mean_earth_sun_distance = 1.495979e+11m
!
!       from the Solar_Define module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Oct-2000
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

PROGRAM Create_Solar_netCDF_File


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler

  USE Interpolate

  USE Fundamental_Constants, ONLY: PI
  USE Planck_Functions

  USE Solar_Define
  USE Solar_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_Solar_netCDF_File'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Create_Solar_netCDF_File.f90,v 1.5 2004/08/31 18:17:07 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Solar source function interpolation parameters
  REAL( fp_kind ), PARAMETER :: BEGIN_FREQUENCY    = 500.0_fp_kind
  REAL( fp_kind ), PARAMETER :: END_FREQUENCY      = 3500.0_fp_kind
  REAL( fp_kind ), PARAMETER :: FREQUENCY_INTERVAL = 0.0025_fp_kind



  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Interpolation_RCS_Id 

  CHARACTER( 256 ) :: Solar_Filename
  INTEGER          :: Solar_FileID
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  REAL( fp_kind ) :: f1, f2, df
  INTEGER         :: n
  INTEGER :: l
  REAL( fp_kind ) :: Omega
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: Input_Frequency
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: Input_Irradiance
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: Output_Frequency
  TYPE( Solar_type ) :: Solar


  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to read the ASCII format Kurucz solar source     ")' )
  WRITE( *, '( 5x, "  spectrum and write it and a blackbody equivalent source ")' )
  WRITE( *, '( 5x, "  function to a netCDF format file.                       ")' )
  WRITE( *, '(/5x, " $Revision: 1.5 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                 -- READ THE ASCII SOLAR SOURCE DATA FILE --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter the ASCII format Kurucz solar source filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Solar_Filename


  ! ------------------------------
  ! Open the ASCII solar data file
  ! ------------------------------

  ! -- Get a free unit number
  Solar_FileID = Get_Lun()

  IF ( Solar_FileID < 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error obtaining file unit number for '//&
                          TRIM( Solar_Filename )//' read.', &
                          FAILURE )
    STOP
  END IF

  ! -- Open the file
  OPEN( Solar_FileID, FILE   = TRIM( Solar_Filename ), &
                      STATUS = 'OLD', &
                      ACCESS = 'SEQUENTIAL', &
                      FORM   = 'FORMATTED', &
                      ACTION = 'READ', &
                      IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                    TRIM( Solar_Filename ), IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! --------------------------
  ! Read the ASCII file header
  ! --------------------------

  READ( Solar_FileID, *, IOSTAT = IO_Status ) f1, f2, df, n

  IF ( IO_Status /= 0 ) THEN
    WRITE( Message, '( "Error reading ", a, " header. IOSTAT = ", i5 )' ) &
                    TRIM( Solar_Filename ), IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    CLOSE( Solar_FileID )
    STOP
  END IF


  ! ------------------------------
  ! Allocate the input data arrays
  ! ------------------------------

  ALLOCATE( Input_Frequency( n ), &
            Input_Irradiance( n ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating input solar data arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    CLOSE( Solar_FileID )
    STOP
  END IF


  ! ------------------------------
  ! Read the solar source function
  ! ------------------------------

  WRITE( *, '( /10x, "Reading ASCII Solar data file..." )' )

  DO l = 1, n

    READ( Solar_FileID, *, IOSTAT = IO_Status ) Input_Frequency( l ), &
                                                Input_Irradiance( l )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading point number ", i7, &
                        &" irradiance value from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      l, TRIM( Solar_Filename ), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      CLOSE( Solar_FileID )
      STOP
    END IF

  END DO

  WRITE( *, '( 10x, "...Done" )' )

  WRITE( *, '( 15x, "Input begin frequency:     ", f12.6, " cm-1", &
             &/15x, "Input end frequency:       ", f12.6, " cm-1", &
             &/15x, "Input number of values:    ", i7 )' ) &
            Input_Frequency(1), &
            Input_Frequency(n), &
            n


  ! --------
  ! Close it
  ! --------

  CLOSE( Solar_FileID )



  !#----------------------------------------------------------------------------#
  !#               -- INTERPOLATE THE INPUT SOLAR SOURCE DATA --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /10x, "Interpolating the input Solar data..." )' )


  ! -------------------------------------------
  ! Determine the number of interpolated points
  ! -------------------------------------------

  n =  NINT( ( END_FREQUENCY - BEGIN_FREQUENCY ) / FREQUENCY_INTERVAL ) + 1


  ! ---------------------------------------
  ! Create the interpolation frequency grid
  ! ---------------------------------------

  ! -- Allocate the array
  ALLOCATE( Output_Frequency( n ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating output solar data frequency array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    CLOSE( Solar_FileID )
    STOP
  END IF

  ! -- Create the frequency index increment
  Output_Frequency = (/ ( l-1, l = 1, n ) /) / REAL( n-1, fp_kind )

  ! -- Calculate the actual frequencies
  Output_Frequency = BEGIN_FREQUENCY + ( Output_Frequency * ( END_FREQUENCY - BEGIN_FREQUENCY ) )


  ! -----------------------------------
  ! Allocate the output Solar structure
  ! -----------------------------------

  Error_Status = Allocate_Solar( n, &
                                 Solar )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Solar structure.', &
                          FAILURE )
    STOP
  END IF


  ! ----------------------------------------------------
  ! Perform the interpolation.
  ! 3rd order polynomial == 4pt Lagrangian interpolation
  ! ----------------------------------------------------

  Error_Status = Polynomial_Interpolate( Input_Frequency, &
                                         Input_Irradiance, &
                                         Output_Frequency, &
                                         Solar%Irradiance, &
                                         order = 3, &
                                         RCS_Id = Interpolation_RCS_Id )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error interpolating the solar data.', &
                          Error_Status )
    STOP
  END IF

  WRITE( *, '( 10x, "...Done" )' )

  WRITE( *, '( 15x, "Output begin frequency:    ", f12.6, " cm-1", &
             &/15x, "Output end frequency:      ", f12.6, " cm-1", &
             &/15x, "Output frequency interval: ", es13.6, " cm-1", &
             &/15x, "Output number of values:   ", i7 )' ) &
            BEGIN_FREQUENCY, &
            END_FREQUENCY, &
            FREQUENCY_INTERVAL, &
            n


  ! -----------------------------------
  ! Assign the scalar frequency members
  ! of the Solar data structure
  ! -----------------------------------

  Solar%Begin_Frequency    = BEGIN_FREQUENCY
  Solar%End_Frequency      = END_FREQUENCY
  Solar%Frequency_Interval = FREQUENCY_INTERVAL



  !#----------------------------------------------------------------------------#
  !#      -- CALCULATE THE BLACKBODY SOLAR SOURCE IRRADIANCE SPECTRUM --        #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /10x, "Calculating the blackbody solar source irradiance..." )' )


  ! ---------------------------------------
  ! Calculate a blackbody radiance spectrum
  ! at the solar temperature
  ! ---------------------------------------

  Error_Status = Planck_Radiance( Output_Frequency, &
                                  Solar%Blackbody_Temperature, &
                                  Solar%Blackbody_Irradiance )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error calculating solar blackbody radiance spectrum.', &
                          Error_Status )
    STOP
  END IF


  ! -----------------------------------------------
  ! Calculate the blackbody solar irradiance at TOA
  ! -----------------------------------------------

  ! -- COmpute the geometry factor
  Omega = PI * ( Solar%Radius / Solar%Earth_Sun_Distance )**2

  ! -- Compute the irradiance
  Solar%Blackbody_Irradiance = Omega * Solar%Blackbody_Irradiance

  WRITE( *, '( 10x, "...Done" )' )



  !#----------------------------------------------------------------------------#
  !#                 -- DEALLOCATE THE TEMPORARY DATA ARRAYS --                 #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Input_Frequency, &
              Input_Irradiance, &
              Output_Frequency, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating temporary data data arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                       -- WRITE THE OUTPUT Solar FILE --                    #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /10x, "Writing the output netCDF Solar data file..." )' )

  Solar_Filename = 'solar.nc'

  Error_Status = Write_Solar_netCDF( Solar_Filename, &
                                     Solar, &
                   Title = 'Kurucz synthetic and blackbody extraterrestrial solar source functions.', &
                   History = PROGRAM_RCS_ID//'; '//&
                             TRIM( Interpolation_RCS_Id )//'; '//&
                             'AER extract_solar.f', &
                   Comment = '4-pt Lagrangian interpolated from original Kurucz data.', &
                   Source = 'Solar spectrum computed with a version of the model atmosphere program ATLAS', &
                   References = 'Kurucz, R.L., Synthetic infrared spectra, in Infrared Solar Physics, '//&
                                'IAU Symp. 154, edited by D.M. Rabin and J.T. Jefferies, Kluwer, Acad., '//&
                                'Norwell, MA, 1992.' )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing netCDF Solar file '//&
                          TRIM( Solar_Filename ), &
                          Error_Status )
    STOP
  END IF

  WRITE( *, '( 10x, "...Done" )' )



  !#----------------------------------------------------------------------------#
  !#                     -- DESTROY THE Solar STRUCTURE --                      #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_Solar( Solar )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Solar data structure.', &
                          Error_Status )
  END IF


END PROGRAM Create_Solar_netCDF_File


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Create_Solar_netCDF_File.f90,v 1.5 2004/08/31 18:17:07 paulv Exp $
!
! $Date: 2004/08/31 18:17:07 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Create_Solar_netCDF_File.f90,v $
! Revision 1.5  2004/08/31 18:17:07  paulv
! - Updated to use new Solar_Define, Solar_netCDF_IO, and Planck_Functions
!   modules.
!
! Revision 1.4  2003/02/11 22:49:01  paulv
! - Updated output file name.
!
! Revision 1.3  2003/02/11 22:16:38  paulv
! - Using new versions of Solar_Define and Solar_netCDF_IO modules.
!
! Revision 1.2  2002/08/23 13:42:50  paulv
! - Added blackbody irradiance calculation.
! - netCDF output file written.
!
! Revision 1.1  2002/08/22 20:21:51  paulv
! Initial checkin.
!
!
!
!
!
!
