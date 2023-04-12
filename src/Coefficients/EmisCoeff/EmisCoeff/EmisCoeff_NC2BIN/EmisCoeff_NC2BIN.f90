!
! EmisCoeff_NC2BIN
!
! Program to convert netCDF format EmisCoeff files to the CRTM Binary
! format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Jul-2005
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM EmisCoeff_NC2BIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,        ONLY: File_Exists
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Program_Message, Display_Message
  USE EmisCoeff_Define,    ONLY: EmisCoeff_type, &
                                 Destroy_EmisCoeff, Equal_EmisCoeff
  USE EmisCoeff_Binary_IO, ONLY: Read_EmisCoeff_Binary, Write_EmisCoeff_Binary
  USE EmisCoeff_netCDF_IO, ONLY: Read_EmisCoeff_netCDF, Write_EmisCoeff_netCDF
  USE SignalFile_Utility
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'EmisCoeff_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: BIN_Filename
  TYPE(EmisCoeff_type) :: EmisCoeff
  TYPE(EmisCoeff_type) :: EmisCoeff_Test

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF format EmisCoeff files to '//&
                        'their CRTM Binary format.', &
                        '$Revision$' )

  ! Get the input and output filenames
  WRITE( *, FMT     = '( /5x, "Enter the INPUT netCDF EmisCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) NC_Filename
  NC_Filename = ADJUSTL( NC_Filename )
  IF ( .NOT. File_Exists( TRIM( NC_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( NC_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Binary EmisCoeff file: " )', &
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
  WRITE( *, '( /5x, "Reading netCDF EmisCoeff data ..." )' )
  Error_Status = Read_EmisCoeff_netCDF( NC_Filename, EmisCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF EmisCoeff file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Write the binary file
  WRITE( *, '( /5x, "Writing Binary EmisCoeff data ..." )' )
  Error_Status = Write_EmisCoeff_Binary( BIN_Filename, EmisCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary EmisCoeff file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Test read the binary data file
  WRITE( *, '( /5x, "Test reading the Binary EmisCoeff data file ..." )' )
  Error_Status = Read_EmisCoeff_Binary( BIN_Filename, EmisCoeff_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary EmisCoeff file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Compare the two structures
  WRITE( *, '( /5x, "Comparing the netCDF and Binary EmisCoeff structures ..." )' )
  Error_Status = Equal_EmisCoeff( EmisCoeff_Test, EmisCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Differences found in netCDF and Binary '//&
                          'file EmisCoeff structure comparison.', &
                          Error_Status )
  ELSE
    CALL Display_Message( PROGRAM_NAME, &
                          'netCDF and Binary file EmisCoeff structures are equal.', &
                          INFORMATION )
  END IF
  
  ! Create a signal file indicating success
  Error_Status = Create_SignalFile( BIN_Filename )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating signal file for '//TRIM(BIN_Filename), &
                          FAILURE )
    STOP
  END IF 

  ! Destroy the structures
  Error_Status = Destroy_EmisCoeff( EmisCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying EmisCoeff structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_EmisCoeff( EmisCoeff_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying EmisCoeff_Test structure.', &
                          WARNING )
  END IF

END PROGRAM EmisCoeff_NC2BIN
