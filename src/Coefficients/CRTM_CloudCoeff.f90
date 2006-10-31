!
! CRTM_CloudCoeff
!
! Module containing the shared CRTM scattering coefficient data
! (CloudCoeff) and their load/destruction routines. 
!
! PUBLIC DATA:
!       CloudC:  Data structure containing the cloud scattering
!                coefficient data
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure CloudC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_CloudCoeff

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Message_Handler,      ONLY: SUCCESS, FAILURE, Display_Message
  USE CloudCoeff_Define,    ONLY: CloudCoeff_type, Destroy_CloudCoeff                  
  USE CloudCoeff_Binary_IO, ONLY: Read_CloudCoeff_Binary
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Shared data
  PUBLIC :: CloudC
  ! Procedures
  PUBLIC :: CRTM_Load_CloudCoeff
  PUBLIC :: CRTM_Destroy_CloudCoeff


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: CRTM_CloudCoeff.f90,v 1.3 2006/06/23 23:18:46 wd20pd Exp $'


  ! ------------------------------------
  ! The shared CloudCoeff data structure
  ! ------------------------------------
  TYPE(CloudCoeff_type), SAVE :: CloudC


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Load_CloudCoeff
!
! PURPOSE:
!       Function to load the CloudCoeff scattering coefficient data into
!       the public data structure CloudC.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_CloudCoeff( CloudCoeff_File,                       &  ! Input
!                                            Quiet             = Quiet,             &  ! Optional input
!                                            Process_ID        = Process_ID,        &  ! Optional input
!                                            Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                            Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       CloudCoeff_File:    Name of the CRTM Binary format CloudCoeff file
!                           containing the scattering coefficient data.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:              Set this argument to suppress INFORMATION messages
!                           being printed to standard output (or the message
!                           log file if the Message_Log optional argument is
!                           used.) By default, INFORMATION messages are printed.
!                           If QUIET = 0, INFORMATION messages are OUTPUT.
!                              QUIET = 1, INFORMATION messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the CloudCoeff data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure CloudC.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Load_CloudCoeff( CloudCoeff_File,   &  ! Input
                                 Quiet,             &  ! Optional input
                                 Process_ID,        &  ! Optional input
                                 Output_Process_ID, &  ! Optional input
                                 Message_Log )      &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: CloudCoeff_File
    INTEGER,      OPTIONAL, INTENT(IN) :: Quiet
    INTEGER,      OPTIONAL, INTENT(IN) :: Process_ID
    INTEGER,      OPTIONAL, INTENT(IN) :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_CloudCoeff'
    ! Local variables
    CHARACTER(256) :: Process_ID_Tag

    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Process ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Read the CloudCoeff data file
    Error_Status = Read_CloudCoeff_Binary( TRIM( CloudCoeff_File ), &  ! Input
                                           CloudC,                  &  ! Output
                                           Quiet             = Quiet, &
                                           Process_ID        = Process_ID, &
                                           Output_Process_ID = Output_Process_ID, &
                                           Message_Log       = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading CloudCoeff data from '//&
                            TRIM( CloudCoeff_File )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Load_CloudCoeff


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_CloudCoeff
!
! PURPOSE:
!       Function to deallocate the public data structure CloudC containing
!       the CRTM CloudCoeff scattering coefficient data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_CloudCoeff( Process_ID  = Process_ID, &  ! Optional input
!                                               Message_Log = Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:       Set this argument to the MPI process ID that this
!                         function call is running under. This value is used
!                         solely for controlling message output. If MPI is not
!                         being used, ignore this argument.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the deallocation of the public CloudC data
!                                       structure was successful
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure CloudC.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_CloudCoeff( Process_ID,   &  ! Optional input
                                    Message_Log ) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    INTEGER,      OPTIONAL, INTENT(IN) :: Process_ID
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_CloudCoeff'
    ! Local variables
    CHARACTER(256) :: Process_ID_Tag

    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Process ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Destroy the structure
    Error_Status = Destroy_CloudCoeff( CloudC, &
                                       Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred deallocating the public CloudCoeff structure'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Destroy_CloudCoeff

END MODULE CRTM_CloudCoeff
