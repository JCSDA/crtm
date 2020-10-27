!
! ODAS_NC2BIN
!
! Program to convert netCDF format ODAS files to the CRTM Binary
! format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM ODAS_NC2BIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility   , ONLY: File_Exists
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                             Program_Message, Display_Message
  USE ODAS_Define    , ONLY: ODAS_type, &
                             Destroy_ODAS, Equal_ODAS
  USE ODAS_Binary_IO , ONLY: Read_ODAS_Binary, Write_ODAS_Binary
  USE ODAS_netCDF_IO , ONLY: Read_ODAS_netCDF, Write_ODAS_netCDF
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'ODAS_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: BIN_Filename
  TYPE(ODAS_type) :: ODAS
  TYPE(ODAS_type) :: ODAS_Test


  ! Output prgram header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF format ODAS files to '//&
                        'their CRTM Binary format.', &
                        '$Revision$' )

  ! Get the input and output filenames
  WRITE( *, FMT     = '( /5x, "Enter the INPUT netCDF ODAS file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) NC_Filename
  NC_Filename = ADJUSTL( NC_Filename )
  IF ( .NOT. File_Exists( TRIM( NC_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( NC_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Binary ODAS file: " )', &
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
  WRITE( *, '( /5x, "Reading netCDF ODAS data ..." )' )
  Error_Status = Read_ODAS_netCDF( NC_Filename, ODAS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF ODAS file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Write the binary file
  WRITE( *, '( /5x, "Writing Binary ODAS data ..." )' )
  Error_Status = Write_ODAS_Binary( BIN_Filename, ODAS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary ODAS file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Test read the binary data file
  WRITE( *, '( /5x, "Test reading the Binary ODAS data file ..." )' )
  Error_Status = Read_ODAS_Binary( BIN_Filename, ODAS_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary ODAS file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Compare the two structures
  WRITE( *, '( /5x, "Comparing the netCDF and Binary ODAS structures ..." )' )
  Error_Status = Equal_ODAS( ODAS_Test, ODAS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Differences found in netCDF and Binary '//&
                          'file ODAS structure comparison.', &
                          Error_Status )
  ELSE
    CALL Display_Message( PROGRAM_NAME, &
                          'netCDF and Binary file ODAS structures are equal.', &
                          INFORMATION )
  END IF

  ! Destroy the structures
  Error_Status = Destroy_ODAS( ODAS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODAS structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_ODAS( ODAS_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODAS_Test structure.', &
                          WARNING )
  END IF

END PROGRAM ODAS_NC2BIN
