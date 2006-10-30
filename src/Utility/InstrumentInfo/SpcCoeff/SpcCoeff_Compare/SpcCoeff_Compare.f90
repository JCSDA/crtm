!------------------------------------------------------------------------------
!P+
! NAME:
!       SpcCoeff_Compare
!
! PURPOSE:
!       Program to compare SpcCoeff data read from either netCDF or
!       CRTM Binary format files.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       File_Utility:               Module containing generic file utility
!                                   routines.
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       SpcCoeff_Define:            Module defining the SpcCoeff data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         COMPARE_FLOAT_NUMBERS module
!
!       SpcCoeff_Binary_IO:         Module containing routines to read and
!                                   write Binary format SpcCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         SPCCOEFF_DEFINE module
!
!       SpcCoeff_netCDF_IO:         Module containing routines to read and
!                                   write netCDF format SpcCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         SPCCOEFF_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
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
!       Input SpcCoeff data files.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
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

PROGRAM SpcCoeff_Compare

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO
  USE SpcCoeff_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_Compare'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: SpcCoeff_Compare.f90,v 2.1 2006/05/02 16:58:03 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Allowable SpcCoeff data types.
  ! -- Type codes MUST start at 1 and increment by 1.
  INTEGER,        PARAMETER :: N_TYPES = 2
  INTEGER,        PARAMETER :: SENSOR_TYPE   = 1
  INTEGER,        PARAMETER :: SPECTRAL_TYPE = 2
  INTEGER,        PARAMETER, DIMENSION( N_TYPES ) :: &
    TYPE_CODE = (/ SENSOR_TYPE, &
                   SPECTRAL_TYPE /)

  CHARACTER( * ), PARAMETER, DIMENSION( N_TYPES ) :: &
    TYPE_NAME = (/ 'Sensor  ', &
                   'Spectral' /)


  ! -- Allowable SpcCoeff file formats.
  ! -- Format codes MUST start at 1 and increment by 1.
  INTEGER,        PARAMETER :: N_FORMATS = 2
  INTEGER,        PARAMETER :: NETCDF_FORMAT = 1
  INTEGER,        PARAMETER :: BINARY_FORMAT = 2
  INTEGER,        PARAMETER, DIMENSION( N_FORMATS ) :: &
    FORMAT_CODE = (/ NETCDF_FORMAT, &
                     BINARY_FORMAT /)

  CHARACTER( * ), PARAMETER, DIMENSION( N_FORMATS ) :: &
    FORMAT_NAME = (/ 'netCDF', &
                     'Binary' /)


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: n

  INTEGER :: Data_Type

  CHARACTER( 256 ) :: Filename1
  CHARACTER( 256 ) :: Filename2

  INTEGER :: File1_Format
  INTEGER :: File2_Format

  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor1
  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor2

  TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Spectral1
  TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Spectral2



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to compare SpcCoeff data read from either netCDF", &
             &/5x, "   or CRTM Binary format files.")' )
  WRITE( *, '(/5x, " $Revision: 2.1 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                          -- ENTER THE FILE TYPES --                        #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT = '( /5x, "Select the data type" )' )
  DO n = 1, N_TYPES
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) TYPE_CODE(n), &
                                             TRIM( TYPE_NAME(n) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) Data_Type

  ! -- Invalid input
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input error for data type.', &
                          FAILURE )
    STOP
  END IF

  ! -- Invalid value
  IF ( .NOT. ANY( TYPE_CODE == Data_Type ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid data type.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                        -- ENTER THE FIRST FILENAME --                      #
  !#----------------------------------------------------------------------------#

  ! ----------------
  ! Get the filename
  ! ----------------

  WRITE( *, FMT     = '( /5x, "Enter the FIRST SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Filename1
  Filename1 = ADJUSTL( Filename1 )
 
  IF ( .NOT. File_Exists( TRIM( Filename1 ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( Filename1 )//' not found.', &
                          FAILURE )
    STOP
  END IF


  ! ------------------------
  ! Get the file format type
  ! ------------------------

  WRITE( *, FMT = '( /5x, "Select the format type for ", a )' ) TRIM( Filename1 )
  DO n = 1, N_FORMATS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) FORMAT_CODE(n), &
                                             TRIM( FORMAT_NAME(n) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) File1_Format

  ! -- Invalid input
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input error for '//TRIM( Filename1 )//' format type.', &
                          FAILURE )
    STOP
  END IF

  ! -- Invalid value
  IF ( .NOT. ANY( FORMAT_CODE == File1_Format ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid format type for '//TRIM( Filename1 )//'.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                       -- ENTER THE SECOND FILENAME --                      #
  !#----------------------------------------------------------------------------#

  ! ----------------
  ! Get the filename
  ! ----------------

  WRITE( *, FMT     = '( /5x, "Enter the SECOND SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Filename2
  Filename2 = ADJUSTL( Filename2 )
 
  IF ( .NOT. File_Exists( TRIM( Filename2 ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( Filename2 )//' not found.', &
                          FAILURE )
    STOP
  END IF


  ! ------------------------
  ! Get the file format type
  ! ------------------------

  WRITE( *, FMT = '( /5x, "Select the format type for ", a )' ) TRIM( Filename2 )
  DO n = 1, N_FORMATS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) FORMAT_CODE(n), &
                                             TRIM( FORMAT_NAME(n) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) File2_Format

  ! -- Invalid input
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input error for '//TRIM( Filename2 )//' format type.', &
                          FAILURE )
    STOP
  END IF

  ! -- Invalid value
  IF ( .NOT. ANY( FORMAT_CODE == File2_Format ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid format type for '//TRIM( Filename2 )//'.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                        -- COMPARE THE SpcCoeff FILES --                    #
  !#----------------------------------------------------------------------------#

  Data_Type_Select: SELECT CASE ( Data_Type )


    !#--------------------------------------------------------------------------#
    !#                    -- PROCESS THE SENSOR SpcCoeff FILES --               #
    !#--------------------------------------------------------------------------#

    CASE ( SENSOR_TYPE )


      ! -----------------------------------
      ! Read the FIRST SENSOR SpcCoeff file
      ! -----------------------------------

      WRITE( *, '( /5x, "Reading FIRST SENSOR SpcCoeff datafile ", a, " ..." )' ) TRIM( Filename1 )

      Sensor1_Format_Select: SELECT CASE ( File1_Format )

        CASE ( NETCDF_FORMAT )

          Error_Status = Read_SpcCoeff_netCDF( Filename1, &
                                               SpcCoeff_Sensor1 )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF SENSOR SpcCoeff file '//&
                                  TRIM( Filename1 ), &
                                  Error_Status )
            STOP
          END IF
       
        CASE ( BINARY_FORMAT )

          Error_Status = Read_SpcCoeff_Binary( Filename1, &
                                               SpcCoeff_Sensor1 )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading Binary SENSOR SpcCoeff file '//&
                                  TRIM( Filename1 ), &
                                  Error_Status )
            STOP
          END IF

      END SELECT Sensor1_Format_Select


      ! ------------------------------------
      ! Read the SECOND SENSOR SpcCoeff file
      ! ------------------------------------

      WRITE( *, '( /5x, "Reading SECOND SENSOR SpcCoeff datafile ", a, " ..." )' ) TRIM( Filename2 )

      Sensor2_Format_Select: SELECT CASE ( File2_Format )

        CASE ( NETCDF_FORMAT )

          Error_Status = Read_SpcCoeff_netCDF( Filename2, &
                                               SpcCoeff_Sensor2 )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF SENSOR SpcCoeff file '//&
                                  TRIM( Filename2 ), &
                                  Error_Status )
            STOP
          END IF
       
        CASE ( BINARY_FORMAT )

          Error_Status = Read_SpcCoeff_Binary( Filename2, &
                                               SpcCoeff_Sensor2 )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading Binary SENSOR SpcCoeff file '//&
                                  TRIM( Filename2 ), &
                                  Error_Status )
            STOP
          END IF

      END SELECT Sensor2_Format_Select


      ! -------------------------------------
      ! Compare the two SENSOR SpcCoeff files
      ! -------------------------------------

      WRITE( *, '( /5x, "Comparing the two SENSOR SpcCoeff structures ..." )' )

      Error_Status = Equal_SpcCoeff( SpcCoeff_Sensor1, SpcCoeff_Sensor2, Check_All = 1 )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Differences found in SENSOR SpcCoeff structure comparison.', &
                              Error_Status )
      ELSE
        CALL Display_Message( PROGRAM_NAME, &
                              'SENSOR SpcCoeff structures are equal.', &
                              INFORMATION )
      END IF


      ! ----------------------
      ! Destroy the structures
      ! ----------------------

      Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor1 )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Sensor1 structure.', &
                              WARNING )
      END IF


      Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor2 )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Sensor2 structure.', &
                              WARNING )
      END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PROCESS THE SPECTRAL SpcCoeff FILES --              #
    !#--------------------------------------------------------------------------#

    CASE ( SPECTRAL_TYPE )


      ! -----------------------------------
      ! Read the FIRST SPECTRAL SpcCoeff file
      ! -----------------------------------

      WRITE( *, '( /5x, "Reading FIRST SPECTRAL SpcCoeff datafile ", a, " ..." )' ) TRIM( Filename1 )

      Spectral1_Format_Select: SELECT CASE ( File1_Format )

        CASE ( NETCDF_FORMAT )

          Error_Status = Read_SpcCoeff_netCDF( Filename1, &
                                               SpcCoeff_Spectral1 )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF SPECTRAL SpcCoeff file '//&
                                  TRIM( Filename1 ), &
                                  Error_Status )
            STOP
          END IF
       
        CASE ( BINARY_FORMAT )

          Error_Status = Read_SpcCoeff_Binary( Filename1, &
                                               SpcCoeff_Spectral1 )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading Binary SPECTRAL SpcCoeff file '//&
                                  TRIM( Filename1 ), &
                                  Error_Status )
            STOP
          END IF

      END SELECT Spectral1_Format_Select


      ! --------------------------------------
      ! Read the SECOND SPECTRAL SpcCoeff file
      ! --------------------------------------

      WRITE( *, '( /5x, "Reading SECOND SPECTRAL SpcCoeff datafile ", a, " ..." )' ) TRIM( Filename2 )

      Spectral2_Format_Select: SELECT CASE ( File2_Format )

        CASE ( NETCDF_FORMAT )

          Error_Status = Read_SpcCoeff_netCDF( Filename2, &
                                               SpcCoeff_Spectral2 )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF SPECTRAL SpcCoeff file '//&
                                  TRIM( Filename2 ), &
                                  Error_Status )
            STOP
          END IF
       
        CASE ( BINARY_FORMAT )

          Error_Status = Read_SpcCoeff_Binary( Filename2, &
                                               SpcCoeff_Spectral2 )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading Binary SPECTRAL SpcCoeff file '//&
                                  TRIM( Filename2 ), &
                                  Error_Status )
            STOP
          END IF

      END SELECT Spectral2_Format_Select


      ! ---------------------------------------
      ! Compare the two SPECTRAL SpcCoeff files
      ! ---------------------------------------

      WRITE( *, '( /5x, "Comparing the two SPECTRAL SpcCoeff structures ..." )' )

      Error_Status = Equal_SpcCoeff( SpcCoeff_Spectral1, SpcCoeff_Spectral2, Check_All = 1 )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Differences found in SPECTRAL SpcCoeff structure comparison.', &
                              Error_Status )
      ELSE
        CALL Display_Message( PROGRAM_NAME, &
                              'SPECTRAL SpcCoeff structures are equal.', &
                              INFORMATION )
      END IF


      ! ----------------------
      ! Destroy the structures
      ! ----------------------

      Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral1 )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Spectral1 structure.', &
                              WARNING )
      END IF


      Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral2 )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Spectral2 structure.', &
                              WARNING )
      END IF

  END SELECT Data_Type_Select

END PROGRAM SpcCoeff_Compare


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SpcCoeff_Compare.f90,v 2.1 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 2.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SpcCoeff_Compare.f90,v $
! Revision 2.1  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 2.0  2005/09/27 17:55:12  paulv
! - New version for Sensor and Spectral SpcCoeff data types.
!
!
!
!
!
