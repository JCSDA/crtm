!
! SignalFile_Utility
!
! Module containing routines to create and manipulate signal files
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-May-2007
!                       paul.vandelst@ssec.wisc.edu
!

MODULE SignalFile_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE File_Utility
  USE Message_Handler
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Create_SignalFile
  
  
CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Create_Signal_File
!
! PURPOSE:
!       Function to create a "signal" file that can be checked for in 
!       processing scripts. The presence of a signal file generally
!       indicates "something" completed successfully.
!
! CALLING SEQUENCE:
!       Error_Status = Create_SignalFile( Rootname               , &  ! Input
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Rootname:     Root of the name of the signal file to create. The
!                     created signal file will be this name appended with
!                     the string ".signal"
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the signal file creation was successful
!                        == FAILURE an unrecoverable error occured
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If the signal file already exists, it is overwritten.
!
!--------------------------------------------------------------------------------

  FUNCTION Create_SignalFile( Rootname   , &  ! Input
                              Message_Log) &  ! Error messaging
                            RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Rootname
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_SignalFile'
    ! Local variables
    INTEGER :: FileID
    INTEGER :: IO_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Create the signal file
    ! ----------------------
    ! Get a free unit number
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit for '//&
                            TRIM(Rootname)//' signal file.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Open the file
    OPEN( FileID, FILE  =TRIM(Rootname)//'.signal', &
                  STATUS='REPLACE', &
                  FORM  ='FORMATTED', &
                  ACCESS='SEQUENTIAL', &
                  IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//&
                            TRIM(Rootname)//' signal file.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Write the Rootname to the signal file
    ! -------------------------------------
    WRITE( FileID, FMT   ='(a)',  &
                   IOSTAT=IO_Status ) TRIM(Rootname)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing to '//&
                            TRIM(Rootname)//' signal file.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS='DELETE' )
    END IF


    ! Close the signal file
    ! ---------------------
    CLOSE( FileID, STATUS='KEEP' )

  END FUNCTION Create_SignalFile

END MODULE SignalFile_Utility
