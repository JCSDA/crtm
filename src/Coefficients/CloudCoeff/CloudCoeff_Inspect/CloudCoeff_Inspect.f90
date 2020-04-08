!
! CloudCoeff_Inspect
!
! Program to inspect the contents of a CRTM Binary format CloudCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 20-Jun-2006
!                       paul.vandelst@noaa.gov
!

PROGRAM CloudCoeff_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,         ONLY: File_Exists
  USE Message_Handler,      ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CloudCoeff_Define,    ONLY: CloudCoeff_type, CloudCoeff_Destroy, &
                                  Inspect => CloudCoeff_Inspect
  USE CloudCoeff_Binary_IO, ONLY: CloudCoeff_Binary_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CloudCoeff_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &


  ! -----------------------
  ! Command line processing
  ! -----------------------
  ! Default definition
  LOGICAL, PARAMETER :: DEFAULT_PAUSE = .FALSE.
  ! Namelist definition of the command line arguments
  LOGICAL :: pause = DEFAULT_PAUSE
  NAMELIST /cmd/ pause
  ! Variable definitions used in parsing command line
  CHARACTER(2000) :: cmd_string='', arg_string=''
  CHARACTER(256)  :: io_msg
  INTEGER         :: io_stat
  INTEGER         :: n, n_cmd_args


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  CHARACTER(256) :: filename
  INTEGER :: err_stat
  TYPE(CloudCoeff_type) :: coeffs
  
  
  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM Binary format '//&
                        'CloudCoeff file to stdout.', &
                        '$Revision$' )

  
  ! Get command line arguments as a string ready for namelist
  n_cmd_args = COMMAND_ARGUMENT_COUNT()
  IF ( n_cmd_args > 0 ) THEN
    ! ...Extract individual arguments into string
    DO n = 1, n_cmd_args
      CALL GET_COMMAND_ARGUMENT(n, arg_string)
      cmd_string = TRIM(cmd_string)//' '//TRIM(arg_string)
    END DO
    ! ...Add namelist prefix and terminator
    cmd_string = '&cmd '//TRIM(cmd_string)//' /'
    ! ...Internal read of namelist
    READ(cmd_string, NML    = cmd    , &
                     IOSTAT = io_stat, &
                     IOMSG  = io_msg)
    IF (io_stat /= 0) THEN
      msg = 'Command line argument retrieval failed - '//TRIM(io_msg)
      CALL Display_Message(PROGRAM_NAME, msg, FAILURE); STOP
    END IF
  END IF

 
  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the Binary CloudCoeff filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)


  ! Read the binary data file
  err_stat = CloudCoeff_Binary_ReadFile( filename, coeffs )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading Binary CloudCoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, err_stat ); STOP
  END IF


  ! Display the contents
  CALL Inspect( coeffs, Pause=pause )


  ! Clean up
  CALL CloudCoeff_Destroy( coeffs )

END PROGRAM CloudCoeff_Inspect
