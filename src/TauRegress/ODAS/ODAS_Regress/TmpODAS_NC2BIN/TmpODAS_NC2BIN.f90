!
! TauCoeff_NC2BIN
!
! Program to convert netCDF format TauCoeff files to the CRTM Binary
! format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM TauCoeff_NC2BIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,       ONLY: File_Exists
  USE Message_Handler,    ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                Program_Message, Display_Message
  USE TauCoeff_Define,    ONLY: TauCoeff_type, &
                                Destroy_TauCoeff, Equal_TauCoeff
  USE TauCoeff_Binary_IO, ONLY: Read_TauCoeff_Binary, Write_TauCoeff_Binary
  USE TauCoeff_netCDF_IO, ONLY: Read_TauCoeff_netCDF, Write_TauCoeff_netCDF
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'TauCoeff_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: BIN_Filename
  TYPE(TauCoeff_type) :: TauCoeff
  TYPE(TauCoeff_type) :: TauCoeff_Test


  ! Output prgram header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF format TauCoeff files to '//&
                        'their CRTM Binary format.', &
                        '$Revision$' )

  ! Get the input and output filenames
  WRITE( *, FMT     = '( /5x, "Enter the INPUT netCDF TauCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) NC_Filename
  NC_Filename = ADJUSTL( NC_Filename )
  IF ( .NOT. File_Exists( TRIM( NC_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( NC_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Binary TauCoeff file: " )', &
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
  WRITE( *, '( /5x, "Reading netCDF TauCoeff data ..." )' )
  Error_Status = Read_TauCoeff_netCDF( NC_Filename, TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF TauCoeff file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Write the binary file
  WRITE( *, '( /5x, "Writing Binary TauCoeff data ..." )' )
  Error_Status = Write_TauCoeff_Binary( BIN_Filename, TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary TauCoeff file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Test read the binary data file
  WRITE( *, '( /5x, "Test reading the Binary TauCoeff data file ..." )' )
  Error_Status = Read_TauCoeff_Binary( BIN_Filename, TauCoeff_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary TauCoeff file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Compare the two structures
  WRITE( *, '( /5x, "Comparing the netCDF and Binary TauCoeff structures ..." )' )
  Error_Status = Equal_TauCoeff( TauCoeff_Test, TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Differences found in netCDF and Binary '//&
                          'file TauCoeff structure comparison.', &
                          Error_Status )
  ELSE
    CALL Display_Message( PROGRAM_NAME, &
                          'netCDF and Binary file TauCoeff structures are equal.', &
                          INFORMATION )
  END IF

  ! Destroy the structures
  Error_Status = Destroy_TauCoeff( TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_TauCoeff( TauCoeff_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff_Test structure.', &
                          WARNING )
  END IF

END PROGRAM TauCoeff_NC2BIN
