!
! SpcCoeff_NC2BIN
!
! Program to convert netCDF format SpcCoeff files to the Binary format.
!
! FILES ACCESSED:
!       - Input netCDF SpcCoeff data file
!       - Output Binary format SpcCoeff file.
!
! SIDE EFFECTS:
!       The output file is overwritten if it already exists.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM SpcCoeff_NC2BIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE String_Utility
  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO
  USE SpcCoeff_netCDF_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status, IO_Status
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: BIN_Filename
  INTEGER :: n
  INTEGER :: Data_Type
  TYPE(SpcCoeff_type) :: SpcCoeff, SpcCoeff_Test


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF format SpcCoeff files to the '//&
                        ' CRTM Binary format.', &
                        '$Revision$' )


  ! Enter the filenames
  ! -------------------
  ! INPUT netCDF file
  WRITE( *,FMT='(/5x,"Enter the INPUT netCDF SpcCoeff file: ")',ADVANCE='NO' )
  READ( *,'(a)' ) NC_Filename
  NC_Filename = ADJUSTL(NC_Filename)
  IF ( .NOT. File_Exists( TRIM(NC_Filename) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM(NC_Filename)//' not found.', &
                          FAILURE )
    STOP
  END IF
 
  ! OUTPUT Binary file
  WRITE( *,FMT='(/5x,"Enter the OUTPUT Binary SpcCoeff file: ")',ADVANCE='NO' )
  READ( *,'(a)' ) BIN_Filename
  BIN_Filename = ADJUSTL(BIN_Filename)

  ! Check that the netCDF file isn't accidentally overwritten
  IF ( TRIM(NC_Filename) == TRIM(BIN_Filename) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Output filename is the same as the input filename!', &
                          FAILURE )
    STOP
  END IF


  ! Read the netCDF SpcCoeff file
  ! -----------------------------
  WRITE( *,'(/5x,"Reading netCDF SpcCoeff data ...")' )
  Error_Status = Read_SpcCoeff_netCDF( TRIM(NC_Filename), &
                                       SpcCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF SENSOR SpcCoeff file '//&
                          TRIM(NC_Filename), &
                          Error_Status )
    STOP
  END IF

   
  ! Write the Binary SpcCoeff file
  ! ------------------------------
  WRITE( *,'(/5x,"Writing Binary SpcCoeff data ...")' )
  Error_Status = Write_SpcCoeff_Binary( TRIM(BIN_Filename), &
                                        SpcCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary SpcCoeff file '//&
                          TRIM(BIN_Filename), &
                          Error_Status )
    STOP
  END IF


  ! Test read the Binary file
  ! -------------------------
  WRITE( *,'(/5x,"Test reading the Binary SpcCoeff data file ...")' )
  Error_Status = Read_SpcCoeff_Binary( TRIM(BIN_Filename), &
                                       SpcCoeff_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary SpcCoeff file '//&
                          TRIM(BIN_Filename), &
                          Error_Status )
    STOP
  END IF


  ! Compare the netCDF and Binary SENSOR structures
  ! -----------------------------------------------
  WRITE( *,'(/5x,"Comparing the netCDF and Binary SpcCoeff structures ...")' )
  Error_Status = Equal_SpcCoeff( SpcCoeff_Test, SpcCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Differences found in netCDF and Binary '//&
                          'file SpcCoeff structure comparison.', &
                          Error_Status )
  ELSE
    CALL Display_Message( PROGRAM_NAME, &
                          'netCDF and Binary file SpcCoeff structures are equal.', &
                          INFORMATION )
  END IF


  ! Destroy the structures
  ! ----------------------
  Error_Status = Destroy_SpcCoeff( SpcCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff structure.', &
                          WARNING )
  END IF
  Error_Status = Destroy_SpcCoeff( SpcCoeff_Test )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff_Test structure.', &
                          WARNING )
  END IF

END PROGRAM SpcCoeff_NC2BIN
