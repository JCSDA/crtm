!
! IRwaterCoeff_oldNC2newBIN
!
! Program to convert the old netCDF format IR water EmisCoeff files to
! the new CRTM IRwaterCoeff Binary format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 04-May-2012
!                       paul.vandelst@noaa.gov
!

PROGRAM IRwaterCoeff_oldNC2newBIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,        ONLY: File_Exists
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Program_Message, Display_Message
  USE EmisCoeff_Define,    ONLY: EmisCoeff_type, &
                                 Destroy_EmisCoeff, &
                                 Equal_EmisCoeff
  USE EmisCoeff_netCDF_IO, ONLY: Read_EmisCoeff_netCDF
  USE IRwaterCoeff_Define, ONLY: IRwaterCoeff_type, &
                                 OPERATOR(==), &
                                 IRwaterCoeff_Associated, &
                                 IRwaterCoeff_Destroy, &
                                 IRwaterCoeff_Create, &
                                 IRwaterCoeff_Inspect, &
                                 IRwaterCoeff_WriteFile, &
                                 IRwaterCoeff_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'IRwaterCoeff_oldNC2newBIN'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  CHARACTER(2000) :: title
  CHARACTER(2000) :: history
  CHARACTER(2000) :: comment
  INTEGER :: Error_Status
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: BIN_Filename
  TYPE(EmisCoeff_type) :: EmisCoeff
  TYPE(IRwaterCoeff_type) :: irwatercoeff(2)

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
    'Program to convert the old netCDF format IR water EmisCoeff files to '//&
    'the new CRTM IRwaterCoeff Binary format.', &
    '$Revision$' )


  ! Get the filenames
  ! ...Input
  WRITE( *, FMT='(/5x,"Enter the INPUT netCDF EmisCoeff file: ")', ADVANCE='NO' )
  READ( *,'(a)' ) NC_Filename
  NC_Filename = ADJUSTL(NC_Filename)
  IF ( .NOT. File_Exists( TRIM(NC_Filename) ) ) THEN
    msg = 'File '//TRIM( NC_Filename )//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Output
  WRITE( *, FMT='(/5x,"Enter the OUTPUT Binary IRwaterCoeff file: ")', ADVANCE='NO' )
  READ( *,'(a)' ) BIN_Filename
  BIN_Filename = ADJUSTL(BIN_FileNAME)


  ! Check that the netCDF file isn't accidentally overwritten
  IF ( TRIM(NC_Filename) == TRIM(BIN_Filename) ) THEN
    msg = 'Output filename is the same as the input filename!'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Read the input netCDF file
  WRITE( *,'(/5x,"Reading netCDF EmisCoeff data ...")' )
  Error_Status = Read_EmisCoeff_netCDF( &
                   NC_Filename, &
                   EmisCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    msg = 'Error reading netCDF EmisCoeff file '//TRIM(NC_Filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Create an instance of the new structure
  CALL IRwaterCoeff_Create( &
         irwatercoeff(1), &
         EmisCoeff%n_Angles, &
         EmisCoeff%n_Frequencies, &
         EmisCoeff%n_Wind_Speeds )
  IF ( .NOT. IRwaterCoeff_Associated( irwatercoeff(1) ) ) THEN
    msg = 'Error allocating IRwaterCoeff structure for writing.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Transfer over the data
  irwatercoeff(1)%Angle      = EmisCoeff%Angle
  irwatercoeff(1)%Frequency  = EmisCoeff%Frequency
  irwatercoeff(1)%Wind_Speed = EmisCoeff%Wind_Speed
  irwatercoeff(1)%Emissivity = EmisCoeff%Emissivity


  ! Get attributes for output file
  WRITE(*,'(/5x,"Enter TITLE attribute:")')
  READ(*,'(a)') title
  WRITE(*,'(/5x,"Enter HISTORY attribute:")')
  READ(*,'(a)') history
  WRITE(*,'(/5x,"Enter COMMENT attribute:")')
  READ(*,'(a)') comment


  ! Write the binary file
  WRITE( *,'(/5x,"Writing Binary IRwaterCoeff data ..." )' )
  Error_Status = IRwaterCoeff_WriteFile( &
                   irwatercoeff(1)  , &
                   BIN_Filename     , &
                   Title   = title  , &
                   History = PROGRAM_VERSION_ID//'; '//TRIM(history) , &
                   Comment = comment   )
  IF ( Error_Status /= SUCCESS ) THEN
    msg = 'Error writing Binary IRwaterCoeff file '//TRIM(BIN_Filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Test read the binary data file
  WRITE( *,'(/5x,"Test reading the Binary IRwaterCoeff data file ..." )' )
  Error_Status = IRwaterCoeff_ReadFile( &
                   irwatercoeff(2), &
                   BIN_Filename     )
  IF ( Error_Status /= SUCCESS ) THEN
    msg = 'Error reading Binary IRwaterCoeff file '//TRIM(BIN_Filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Check the binary write+read was successful
  IF ( .NOT. (irwatercoeff(1) == irwatercoeff(2)) ) THEN
    msg = 'IRwaterCoeff structures are not equal'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Clean up
  Error_Status = Destroy_EmisCoeff( EmisCoeff )
  CALL IRwaterCoeff_Destroy(irwatercoeff)

END PROGRAM IRwaterCoeff_oldNC2newBIN
