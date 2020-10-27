!
! ODPS_NC2BIN
!
! Program to convert netCDF format ODPS files to the CRTM Binary
! format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!
!       modified by:     Yong Chen 27-Feb-2009
!                       Yong.Chen@noaa.gov
!

PROGRAM ODPS_NC2BIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility   , ONLY: File_Exists
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                             Program_Message, Display_Message
  USE ODPS_Define    , ONLY: ODPS_type, &
                             Destroy_ODPS, Equal_ODPS
  USE ODPS_Binary_IO , ONLY: Read_ODPS_Binary, Write_ODPS_Binary
  USE ODPS_netCDF_IO , ONLY: Read_ODPS_netCDF, Write_ODPS_netCDF
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'ODPS_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: BIN_Filename
  TYPE(ODPS_type) :: ODPS
  TYPE(ODPS_type) :: ODPS_Test


  ! Output prgram header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF format ODPS files to '//&
                        'their CRTM Binary format.', &
                        '$Revision$' )

  ! Get the input and output filenames
  WRITE( *, FMT     = '( /5x, "Enter the INPUT netCDF ODPS file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) NC_Filename
  NC_Filename = ADJUSTL( NC_Filename )
  IF ( .NOT. File_Exists( TRIM( NC_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( NC_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Binary ODPS file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) BIN_Filename
  BIN_Filename = ADJUSTL( BIN_FileNAME )

  ! Check that the netCDF file isn't accidentally overwritten
  IF ( TRIM( NC_Filename ) == TRIM( BIN_Filename ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Output filename is the same as the input filename!', &
                          FAILURE )
    STOP
  END IF

  ! Read the input netCDF file
  WRITE( *, '( /5x, "Reading netCDF ODPS data ..." )' )
  Error_Status = Read_ODPS_netCDF( NC_Filename, ODPS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF ODPS file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Write the binary file
  WRITE( *, '( /5x, "Writing Binary ODPS data ..." )' )
  Error_Status = Write_ODPS_Binary( BIN_Filename, ODPS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary ODPS file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Test read the binary data file
  WRITE( *, '( /5x, "Test reading the Binary ODPS data file ..." )' )
  Error_Status = Read_ODPS_Binary( BIN_Filename, ODPS_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary ODPS file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Compare the two structures
  WRITE( *, '( /5x, "Comparing the netCDF and Binary ODPS structures ..." )' )
  Error_Status = Equal_ODPS( ODPS_Test, ODPS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Differences found in netCDF and Binary '//&
                          'file ODPS structure comparison.', &
                          Error_Status )
  ELSE
    CALL Display_Message( PROGRAM_NAME, &
                          'netCDF and Binary file ODPS structures are equal.', &
                          INFORMATION )
  END IF

  ! Destroy the structures
  Error_Status = Destroy_ODPS( ODPS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODPS structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_ODPS( ODPS_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODPS_Test structure.', &
                          WARNING )
  END IF

END PROGRAM ODPS_NC2BIN
