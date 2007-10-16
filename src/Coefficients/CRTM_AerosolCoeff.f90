!
! CRTM_AerosolCoeff
!
! Module containing the shared CRTM aerosol coefficients (AerosolCoeff)
! and their load/destruction routines. 
!
! PUBLIC DATA:
!       AeroC:  Data structure containing the aerosol bulk optical
!               properties data
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure AeroC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_AerosolCoeff

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module use
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, Display_Message
  USE AerosolCoeff_Define   , ONLY: AerosolCoeff_type, Destroy_AerosolCoeff
  USE AerosolCoeff_Binary_IO, ONLY: Read_AerosolCoeff_Binary
  USE CRTM_Parameters
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! The shared data
  PUBLIC :: AeroC
  ! Public routines in this module
  PUBLIC :: CRTM_Load_AerosolCoeff
  PUBLIC :: CRTM_Destroy_AerosolCoeff


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


  ! -----------------------------------
  ! The shared aerosol coefficient data
  ! -----------------------------------
  TYPE(AerosolCoeff_type), SAVE :: AeroC


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Load_AerosolCoeff
!
! PURPOSE:
!       Function to load the AerosolCoeff aerosol coefficient data into
!       the public data structure AeroC.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_AerosolCoeff( AerosolCoeff_File,                   &  ! Input
!                                              Quiet            =Quiet,             &  ! Optional input
!                                              Process_ID       =Process_ID,        &  ! Optional input
!                                              Output_Process_ID=Output_Process_ID, &  ! Optional input
!                                              Message_Log      =Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AerosolCoeff_File:  Name of the CRTM Binary format AerosolCoeff file
!                           containing the aerosol coefficient data.
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
!                           UNITS:      None
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      None
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
!                           UNITS:      None
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the AerosolCoeff data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure AeroC.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Load_AerosolCoeff( AerosolCoeff_File, &  ! Input
                                   Quiet            , &  ! Optional input
                                   Process_ID       , &  ! Optional input
                                   Output_Process_ID, &  ! Optional input
                                   Message_Log      ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: AerosolCoeff_File
    INTEGER,      OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,      OPTIONAL, INTENT(IN)  :: Process_ID
    INTEGER,      OPTIONAL, INTENT(IN)  :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_AerosolCoeff'
    ! Local variables
    CHARACTER(256) :: Process_ID_Tag

    ! Setup 
    Error_Status = SUCCESS
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT(Process_ID) ) THEN
      WRITE( Process_ID_Tag, '(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Read the AerosolCoeff data file
    Error_Status = Read_AerosolCoeff_Binary( TRIM(AerosolCoeff_File)            , &  ! Input
                                             AeroC                              , &  ! Output
                                             Quiet            =Quiet            , &
                                             Process_ID       =Process_ID       , &
                                             Output_Process_ID=Output_Process_ID, &
                                             Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading AerosolCoeff data from '//&
                            TRIM(AerosolCoeff_File)//TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Load_AerosolCoeff


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_AerosolCoeff
!
! PURPOSE:
!       Function to deallocate the public data structure AeroC containing
!       the CRTM AerosolCoeff aerosol coefficient data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_AerosolCoeff( Process_ID =Process_ID, &  ! Optional input
!                                                 Message_Log=Message_Log )  ! Error messaging
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
!                         If == SUCCESS the deallocation of the public AeroC data
!                                       structure was successful
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure AeroC.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_AerosolCoeff( Process_ID,   &  ! Optional input
                                      Message_Log ) &  ! Error messaging
                                    RESULT( Error_Status )
    ! Arguments
    INTEGER,      OPTIONAL, INTENT(IN)  :: Process_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_AerosolCoeff'
    ! Local variables
    CHARACTER(256) :: Process_ID_Tag

    ! Set up
    Error_Status = SUCCESS
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT(Process_ID) ) THEN
      WRITE( Process_ID_Tag, '(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Destroy the shared data structure
    Error_Status = Destroy_AerosolCoeff( AeroC, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred deallocating the public AerosolCoeff structure'//&
                            TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Destroy_AerosolCoeff

END MODULE CRTM_AerosolCoeff
