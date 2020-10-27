!
! ODPS_BIN2NC
!
! Program to convert netCDF format ODPS files to the CRTM Binary
! format.
!
!
! CREATION HISTORY:
!       Written by:     Yong Chen 27-Feb-2009
!                       Yong.Chen@noaa.gov
!

PROGRAM ODPS_BIN2NC

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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'ODPS_BIN2NC'
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
  WRITE( *, FMT     = '( /5x, "Enter the INPUT Binary ODPS file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) BIN_Filename
  BIN_Filename = ADJUSTL( BIN_FileNAME )
  IF ( .NOT. File_Exists( TRIM( BIN_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( BIN_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT netCDF ODPS file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) NC_Filename
  NC_Filename = ADJUSTL( NC_Filename )

  ! Check that the BIN file isn't accidentally overwritten
  IF ( TRIM( BIN_Filename ) == TRIM( NC_Filename ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Output filename is the same as the input filename!', &
                          FAILURE )
    STOP
  END IF

  ! Read the input binary file
  WRITE( *, '( /5x, "Reading Binary ODPS data ..." )' )
  Error_Status = Read_ODPS_Binary( BIN_Filename, ODPS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary ODPS file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Write the netCDF file
  WRITE( *, '( /5x, "Writeing netCDF ODPS data ..." )' )
  Error_Status = Write_ODPS_netCDF( NC_Filename, ODPS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing netCDF ODPS file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! Test read the netCDF data file
  WRITE( *, '( /5x, "Test reading the netCDF ODPS data file ..." )' )
  Error_Status = Read_ODPS_netCDF( NC_Filename, ODPS_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF ODPS file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Compare the two structures
  WRITE( *, '( /5x, "Comparing the Binary and netCDF ODPS structures ..." )' )
  Error_Status = Equal_ODPS( ODPS_Test, ODPS, Check_All=1  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Differences found in Binary and netCDF'//&
                          'file ODPS structure comparison.', &
                          Error_Status )
  ELSE
    CALL Display_Message( PROGRAM_NAME, &
                          'Binary and netCDF file ODPS structures are equal.', &
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

END PROGRAM ODPS_BIN2NC
