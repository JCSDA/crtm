!
! LBLRTM_Layer_IO
!
! Module containing routines to read, write, and manipulate the LBLRTM
! layer data structure.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!

MODULE LBLRTM_Layer_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE LBLRTM_Parameters
  USE LBLRTM_Utility
  USE LBLRTM_Fhdr_IO
  USE LBLRTM_Phdr_IO
  USE LBLRTM_Panel_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Destroy_LBLRTM_Layer
  PUBLIC :: Allocate_LBLRTM_Layer
  PUBLIC :: Assign_LBLRTM_Layer
  PUBLIC :: Read_LBLRTM_Layer
  PUBLIC :: Write_LBLRTM_Layer


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! Keyword set value
  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1

  ! Invalid values
  INTEGER,                PARAMETER :: IP_INVALID = -1
  REAL( LBLRTM_FP_KIND ), PARAMETER :: FP_INVALID = -1.0_LBLRTM_FP_KIND
  REAL( Double ),         PARAMETER :: DP_INVALID = -1.0_Double


  ! ----------------------------------
  ! Layer data derived type definition
  ! ----------------------------------
  TYPE, PUBLIC :: LBLRTM_Layer_type
    INTEGER :: n_Allocates = 0
    ! The LBLRTM file header
    TYPE(LBLRTM_Fhdr_type) :: Fhdr
    ! The spectral dimensions
    INTEGER :: n_Points = 0
    INTEGER :: n_Panels = 0
    ! The frequency defintions
    REAL( Double )         :: Begin_Frequency    = DP_INVALID
    REAL( Double )         :: End_Frequency      = DP_INVALID
    REAL( LBLRTM_FP_KIND ) :: Frequency_Interval = FP_INVALID
    ! The spectrum (n_Points x n_Panels)
    REAL( LBLRTM_FP_KIND ), DIMENSION(:,:), POINTER :: Spectrum => NULL()
  END TYPE LBLRTM_Layer_type


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_LBLRTM_Layer
!
! PURPOSE:
!       Subroutine to clear the scalar members of a LBLRTM_Layer structure.
!
! CALLING SEQUENCE:
!       CALL Clear_LBLRTM_Layer( LBLRTM_Layer ) ! Output
!
! OUTPUT ARGUMENTS:
!       LBLRTM_Layer:  LBLRTM_Layer structure for which the scalar members have
!                      been cleared.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Layer_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output LBLRTM_Layer argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Jun-2002
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_LBLRTM_Layer( LBLRTM_Layer )
    TYPE(LBLRTM_Layer_type), INTENT(IN OUT) :: LBLRTM_Layer
    CALL Clear_LBLRTM_Fhdr( LBLRTM_Layer%Fhdr )
    LBLRTM_Layer%Begin_Frequency    = DP_INVALID
    LBLRTM_Layer%End_Frequency      = DP_INVALID
    LBLRTM_Layer%Frequency_Interval = FP_INVALID
  END SUBROUTINE Clear_LBLRTM_Layer





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
!       Associated_LBLRTM_Layer
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       LBLRTM_Layer structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_LBLRTM_Layer( LBLRTM_Layer,       &  ! Input
!                                                     ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       LBLRTM_Layer:        LBLRTM_Layer structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       LBLRTM_Layer_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            LBLRTM_Layer structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the LBLRTM_Layer pointer members.
!                            .TRUE.  - if ALL the LBLRTM_Layer pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the LBLRTM_Layer pointer
!                                      members are associated.
!                            .FALSE. - some or all of the LBLRTM_Layer pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_LBLRTM_Layer( LBLRTM_Layer,  & ! Input
                                    ANY_Test )     & ! Optional input
                                  RESULT( Association_Status )
    ! Arguments
    TYPE(LBLRTM_Layer_type), INTENT(IN) :: LBLRTM_Layer
    INTEGER,         OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test

    ! ** NOTE: The ANY_Test optional argument is currently
    !          just a placeholder since there is only one
    !          pointer member in the LBLRTM_Layer structure.

    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! Test the structure member association
    ! -------------------------------------
    Association_Status = .FALSE.
    IF ( ASSOCIATED( LBLRTM_Layer%Spectrum ) ) THEN
      Association_Status = .TRUE.
    END IF

  END FUNCTION Associated_LBLRTM_Layer


!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_LBLRTM_Layer
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of LBLRTM_Layer
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_LBLRTM_Layer( LBLRTM_Layer,             &  ! Output
!                                            RCS_Id = RCS_Id,          &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
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
! OUTPUT ARGUMENTS:
!       LBLRTM_Layer: Re-initialized LBLRTM_Layer structure.
!                     UNITS:      N/A
!                     TYPE:       LBLRTM_Layer_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output LBLRTM_Layer argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_LBLRTM_Layer( LBLRTM_Layer, &  ! Output
                                 No_Clear,     &  ! Optional input
                                 RCS_Id,       &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    TYPE(LBLRTM_Layer_type), INTENT(IN OUT) :: LBLRTM_Layer
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_LBLRTM_Layer'
    ! Local variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise dimensions
    LBLRTM_Layer%n_Points = 0
    LBLRTM_Layer%n_Panels = 0

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_LBLRTM_Layer( LBLRTM_Layer )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_LBLRTM_Layer( LBLRTM_Layer ) ) RETURN


    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( LBLRTM_Layer%Spectrum, STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating LBLRTM_Layer pointer components. ", &
                        &"STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    LBLRTM_Layer%n_Allocates = LBLRTM_Layer%n_Allocates - 1
    IF ( LBLRTM_Layer%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      LBLRTM_Layer%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_LBLRTM_Layer


!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_LBLRTM_Layer
! 
! PURPOSE:
!       Function to allocate the pointer members of the LBLRTM_Layer
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_LBLRTM_Layer( n_Points,                 &  ! Input
!                                             n_Panels,                 &  ! Input
!                                             LBLRTM_Layer,             &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Points:     Number of spectral points dimension of LBLRTM_Layer
!                     structure pointer members.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Panels:     Number of panels dimension of LBLRTM_Layer
!                     structure pointer members.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       LBLRTM_Layer: LBLRTM_Layer structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       LBLRTM_Layer_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure pointer allocations were
!                                   successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output LBLRTM_Layer argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_LBLRTM_Layer( n_Points,     &  ! Input
                                  n_Panels,     &  ! Input
                                  LBLRTM_Layer, &  ! Output
                                  RCS_Id,       &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: n_Points
    INTEGER,                 INTENT(IN)     :: n_Panels
    TYPE(LBLRTM_Layer_type), INTENT(IN OUT) :: LBLRTM_Layer
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_LBLRTM_Layer'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    IF ( n_Points < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_POINTS must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( n_Panels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_PANELS must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_LBLRTM_Layer( LBLRTM_Layer, ANY_Test = SET ) ) THEN
      Error_Status = Destroy_LBLRTM_Layer( LBLRTM_Layer, &
                                           No_Clear = SET, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating LBLRTM_Layer pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! Allocate pointer members
    ! ------------------------
    ALLOCATE( LBLRTM_Layer%Spectrum( n_Points, n_Panels ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating LBLRTM_Layer data arraysS. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Initialise data
    ! ---------------
    ! Assign the dimension members
    LBLRTM_Layer%n_Points = n_Points
    LBLRTM_Layer%n_Panels = n_Panels

    ! Assign allocated arrays an invalid value
    LBLRTM_Layer%Spectrum = FP_INVALID 


    ! Increment and test the allocation counter
    LBLRTM_Layer%n_Allocates = LBLRTM_Layer%n_Allocates + 1
    IF ( LBLRTM_Layer%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      LBLRTM_Layer%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_LBLRTM_Layer


!------------------------------------------------------------------------------
!
! NAME:
!       Assign_LBLRTM_Layer
!
! PURPOSE:
!       Function to copy valid LBLRTM_Layer structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_LBLRTM_Layer( LBLRTM_Layer_in,          &  ! Input
!                                           LBLRTM_Layer_out,         &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       LBLRTM_Layer_in:  LBLRTM_Layer structure which is to be copied.
!                         UNITS:      N/A
!                         TYPE:       LBLRTM_Layer_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       LBLRTM_Layer_out: Copy of the input structure, LBLRTM_Layer_in.
!                         UNITS:      N/A
!                         TYPE:       LBLRTM_Layer_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the structure assignment was successful
!                            == FAILURE an error occurred
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output LBLRTM_Layer argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Assign_LBLRTM_Layer( LBLRTM_Layer_in,  &  ! Input
                                LBLRTM_Layer_out, &  ! Output
                                RCS_Id,           &  ! Revision control
                                Message_Log )     &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(LBLRTM_Layer_type), INTENT(IN)     :: LBLRTM_Layer_in
    TYPE(LBLRTM_Layer_type), INTENT(IN OUT) :: LBLRTM_Layer_out
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_LBLRTM_Layer'
    ! Local variables
    INTEGER :: i

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_LBLRTM_Layer( LBLRTM_Layer_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT LBLRTM_Layer pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_LBLRTM_Layer( LBLRTM_Layer_in%n_Points, &
                                          LBLRTM_Layer_in%n_Panels, &
                                          LBLRTM_Layer_out,         &
                                          Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output LBLRTM_Layer array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------
    ! Assign data
    ! -----------
    LBLRTM_Layer_out%Fhdr               = LBLRTM_Layer_in%Fhdr
    LBLRTM_Layer_out%Begin_Frequency    = LBLRTM_Layer_in%Begin_Frequency
    LBLRTM_Layer_out%End_Frequency      = LBLRTM_Layer_in%End_Frequency
    LBLRTM_Layer_out%Frequency_Interval = LBLRTM_Layer_in%Frequency_Interval
    LBLRTM_Layer_out%Spectrum           = LBLRTM_Layer_in%Spectrum

  END FUNCTION Assign_LBLRTM_Layer


!------------------------------------------------------------------------------
!
! NAME:
!       Read_LBLRTM_Layer
!
! PURPOSE:
!       Function to read a layer of data from an LBLRTM format file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_LBLRTM_Layer ( FileID,                                &  ! Input
!                                          Panel_Type,                            &  ! Input
!                                          LBLRTM_Layer,                          &  ! Output
!                                          EOF,                                   &  ! Output
!                                          Panel_Request     = Panel_Request,     &  ! Optional input
!                                          Diagnostic_Output = Diagnostic_Output, &  ! Optional input
!                                          RCS_Id            = RCS_Id,            &  ! Revision control
!                                          Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       FileID:             Logical unit number associated with LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Panel_Type:         Integer specifying the LBLRTM file type, i.e. single
!                           or double panel. Valid input values are defined in
!                           the LBLRTM_Parameters module.
!                             = LBLRTM_SINGLE_PANEL_TYPE:  Single panel file
!                             = LBLRTM_DOUBLE_PANEL_TYPE:  Double panel file
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Panel_Request:      Integer flag specifying which panels from a double
!                           panel file is wanted.
!                             = 1     : The first panel is returned
!                             = 2     : The second panel is returned
!                             = other : All panels are returned [DEFAULT]
!                           If not specified, ALL the data is returned.
!                           If specified with a single panel type, it is ignored.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Diagnostic_Output:  Integer flag specifying the level of diagnostic 
!                           output required.
!                             = 1     : Only the file header is output.
!                             = 2     : Both the file header and panel header(s)
!                                       are output.
!                             = other : No output is generated [DEFAULT]
!                           If not specified no output is generated.
!                           If the MESSAGE_LOG argument is also specified, the
!                           output is written to the log file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       LBLRTM_Layer:       LBLRTM_Layer structure containing the layer data.
!                           UNITS:      N/A
!                           TYPE:       LBLRTM_Layer_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
!       EOF:                Flag indicating end-of-file status for the LBLRTM
!                           format file after the read. Valid return values are
!                           defined in the LBLRTM_Parameters module.
!                             = LBLRTM_FILE_PTR_EOF:   End-of-file has been reached.
!                                                      The file is then closed.
!                             = LBLRTM_FILE_PTR_OK:    No EOF or EOL condition. File
!                                                      is positioned for further
!                                                      reading.
!                             = LBLRTM_FILE_PTR_UNDEF: An error occurred. The file is
!                                                      closed.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the LBLRTM layer data read was successful
!                              == FAILURE an error occurred
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output LBLRTM_Layer argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Read_LBLRTM_Layer ( FileID,            &  ! Input
                               Panel_Type,        &  ! Input
                               LBLRTM_Layer,      &  ! Output
                               EOF,               &  ! Output
                               Panel_Request,     &  ! Optional input
                               Diagnostic_Output, &  ! Optional input
                               RCS_Id,            &  ! Revision control
                               Message_Log )      &  ! Error messaging
                             RESULT ( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: FileID
    INTEGER,                 INTENT(IN)     :: Panel_Type
    TYPE(LBLRTM_Layer_type), INTENT(IN OUT) :: LBLRTM_Layer
    INTEGER,                 INTENT(OUT)    :: EOF
    INTEGER,      OPTIONAL,  INTENT(IN)     :: Panel_Request
    INTEGER,      OPTIONAL,  INTENT(IN)     :: Diagnostic_Output
    CHARACTER(*), OPTIONAL,  INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL,  INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_LBLRTM_Layer'
    ! Local variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: Fhdr_Output
    LOGICAL :: Phdr_Output
    INTEGER :: n_Panels_to_Read
    INTEGER :: n_Panels_to_Return
    INTEGER :: i_Panel, i_Save, i
    INTEGER :: n_Points, l1, l2
    INTEGER :: Panel_Chunk_Count
    TYPE(LBLRTM_Phdr_type) :: Phdr
    REAL( LBLRTM_FP_KIND ), DIMENSION( LBLRTM_MAX_PANEL_POINTS ) :: Panel

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check if file is open
    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      EOF          = LBLRTM_FILE_PTR_UNDEF
      CALL Display_Message( ROUTINE_NAME, &
                            'LBLRTM file is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Check the panel type
    ! --------------------
    IF ( Panel_Type /= LBLRTM_SINGLE_PANEL_TYPE .AND. &
         Panel_Type /= LBLRTM_DOUBLE_PANEL_TYPE       ) THEN
      Error_Status = FAILURE
      EOF          = LBLRTM_FILE_PTR_UNDEF
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid LBLRTM panel type.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! Check the panel request argument
    ! --------------------------------
    ! Default is to read all data
    n_Panels_to_Read   = LBLRTM_N_PANELS( Panel_Type )  ! Used for reading
    n_Panels_to_Return = LBLRTM_N_PANELS( Panel_Type )  ! Used for allocating
    i_Panel            = 3

    ! Otherwise, for double panel files, determine what
    ! data to return. Note that the PANEL_REQUEST argument
    ! is ignored if specified for a single panel file.
    IF ( PRESENT( Panel_Request ) .AND. Panel_Type == LBLRTM_DOUBLE_PANEL_TYPE ) THEN

      SELECT CASE ( Panel_Request )
        CASE ( 1 )
          ! Just want the first panel returned
          n_Panels_to_Return = 1
          i_Panel            = 1
        CASE ( 2 )
          ! Just want the second panel returned
          n_Panels_to_Return = 1
          i_Panel            = 2
        CASE DEFAULT
          ! Everything else returns BOTH
          n_Panels_to_Return = 2
          i_Panel            = 3
      END SELECT

    END IF


    ! Check the diagnostic output argument
    ! ------------------------------------
    ! Default is no diagnostic output...
    Fhdr_Output = .FALSE.
    Phdr_Output = .FALSE.
    ! ...unless the keyword is correctly set
    IF ( PRESENT( Diagnostic_Output ) ) THEN
      SELECT CASE ( Diagnostic_Output )
        CASE ( 1 )
          Fhdr_Output = .TRUE.
        CASE ( 2 )
          Fhdr_Output = .TRUE.
          Phdr_Output = .TRUE.
        CASE DEFAULT
          Fhdr_Output = .FALSE.
          Phdr_Output = .FALSE.
      END SELECT
    END IF


    ! Read the file header
    ! --------------------
    Error_Status = Read_LBLRTM_Fhdr( FileID, &
                                     LBLRTM_Layer%Fhdr, &
                                     EOF, &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      EOF = LBLRTM_FILE_PTR_UNDEF
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred reading file header.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check for EOF. Remember that an EOL is only
    ! flagged during a PANEL HEADER read.
    ! Error_Status returned from Read_LBLRTM_Fhdr,
    IF ( EOF == LBLRTM_FILE_PTR_EOF ) RETURN


    ! Output diagnostic output if required
    IF ( Fhdr_Output )  THEN
      CALL Print_LBLRTM_Fhdr( LBLRTM_Layer%Fhdr,        &
                              Message_Log = Message_Log )
    END IF


    ! Compute the number of spectral points in the layer
    ! --------------------------------------------------
    n_Points = Compute_n_Points( LBLRTM_Layer%Fhdr%Begin_Frequency,   &
                                 LBLRTM_Layer%Fhdr%End_Frequency,     &
                                 LBLRTM_Layer%Fhdr%Frequency_Interval )


    ! Allocate the layer structure
    ! ----------------------------
    Error_Status = Allocate_LBLRTM_Layer( n_Points,                 &
                                          n_Panels_to_Return,       &
                                          LBLRTM_Layer,             &
                                          Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      EOF = LBLRTM_FILE_PTR_UNDEF
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred allocating LBLRTM_Layer.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                          -- READ PANEL DATA --                           #
    !#                                                                          #
    !# Here it should be pointed out that the term "panel" refers to two sorts  #
    !# of different things.                                                     #
    !#                                                                          #
    !# A "single panel" or "double panel" file refers to an LBLRTM format file  #
    !# that contains one or two spectrally coincident data set(s) respectively. #
    !# E.g. a single panel file usually contains optical depths, and a double   #
    !# panel file usually contains radiances (first panel) and transmittances   #
    !# (second panel). So in this respect how many "panels" a file contains is  #
    !# indicative of how much data is in the file.                              #
    !#                                                                          #
    !# The other definition relates to how data is stored within a layer. Each  #
    !# layer spectrum (or spectra if it's a double panel file) is stored in a   #
    !# series of "chunks", also referred to as (you guessed it) "panels".       #
    !#                                                                          #
    !# Over the years of working with FASCODE/LBLRTM format data files I have   #
    !# conflated these two meanings. Sorry 'bout that. So, to be clear, the     #
    !# following "Read_Panel_Chunk" loop is for reading the data "chunk" panels.#
    !# These chunks are then transferred to either the first or second          #
    !# spectrum (panel) in the LBLRTM_Layer%Spectrum pointer array. Phew.       # 
    !#--------------------------------------------------------------------------#

    ! Initialise counters
    ! -------------------
    Panel_Chunk_Count = 0
    l1 = 1                ! Begin index of current panel chunk in output array


    ! Begin open loop
    ! ---------------
    Read_Panel_Chunk: DO

      ! Read a panel header
      ! -------------------
      
      ! Increment panel chunk count
      Panel_Chunk_Count = Panel_Chunk_Count + 1

      ! Read the panel chunk header
      Error_Status = Read_LBLRTM_Phdr( FileID, &
                                       Phdr, &
                                       EOF, &
                                       Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        EOF = LBLRTM_FILE_PTR_UNDEF
        WRITE( Message, '( "Error reading panel chunk #", i4, " header." )' ) &
                        Panel_Chunk_Count
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! Check for EOL or EOF
      IF ( EOF == LBLRTM_FILE_PTR_EOL .OR. &
           EOF == LBLRTM_FILE_PTR_EOF      ) EXIT Read_Panel_Chunk
      
      ! Output diagnostic output if required
      IF ( Phdr_Output )  THEN
        CALL Print_LBLRTM_Phdr( Phdr, &
                                Panel_Chunk_Count, &
                                Message_Log = Message_Log )
      END IF

      ! Determine end index of current panel chunk in output array
      l2 = l1 + Phdr%n_Points - 1
      IF ( l2 > n_Points ) THEN
        Error_Status = FAILURE
        EOF          = LBLRTM_FILE_PTR_UNDEF
        WRITE( Message, '( "End point index for panel chunk #", i4, &
                          &" (",i10,") exceeds the output array bounds (",i10,")." )' ) &
                        Panel_Chunk_Count, l2, n_Points
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! Read the required number of panel chunks
      ! for single or double panel format file
      ! ----------------------------------------

      ! But first initialise a little counter
      ! for the LBLRTM_Layer panel index
      i_Save = 0

      ! Now loop over panel chunks
      DO i = 1, n_Panels_to_Read

        ! Read the actual panel chunk data. The INT is required if
        ! the LBLRTM data was output in "double precision mode"
        Error_Status = Read_LBLRTM_Panel( FileID, &
                                          Panel,   &
                                          EOF,     &
                                          n_Points    = INT( Phdr%n_Points ), & 
                                          Message_Log = Message_Log    )

        IF ( Error_Status /= SUCCESS ) THEN
          EOF = LBLRTM_FILE_PTR_UNDEF
          WRITE( Message, '( "Error reading panel chunk #", i4, &
                            &" data in panel ", i1, "." )' ) &
                          Panel_Chunk_Count, i
          CALL Display_Message( ROUTINE_NAME, &
                                message, &
                                Error_Status, &
                                Message_Log = Message_Log )
          CLOSE( FileID )
          RETURN
        END IF

        ! Test for end-of-file
        IF ( EOF == LBLRTM_FILE_PTR_EOF ) EXIT Read_Panel_Chunk

        ! Save the data if required
        IF ( IAND( i, i_Panel ) /= 0 ) THEN
          i_Save = i_Save + 1
          LBLRTM_Layer%Spectrum(l1:l2, i_Save) = Panel( 1:Phdr%n_Points )
        END IF

      END DO

      ! Update the begin index for the output arrays
      l1 = l2 + 1

    END DO Read_Panel_Chunk


    ! Check the number of points read
    ! -------------------------------
    IF ( l2 /= n_Points ) THEN

      ! Issue warning if the difference is more than one point.
      ! A 1-point difference can be expected due to rounding
      ! in the utility function COMPUTE_N_POINTS().
      IF ( ABS( l2 - n_Points ) > 1 ) THEN
        Error_Status = WARNING
        WRITE( Message, '( i10, " points read, ", i10, " points expected." )' ) &
                        l2, n_Points
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      ! Ensure structure n_Points is correct
      LBLRTM_Layer%n_Points = l2

    END IF


    ! Copy frequency range
    ! --------------------
    LBLRTM_Layer%Begin_Frequency    = LBLRTM_Layer%Fhdr%Begin_Frequency
    LBLRTM_Layer%End_Frequency      = LBLRTM_Layer%Fhdr%Begin_Frequency + &
                                      ( REAL( l2 - 1, Double ) * &
                                        REAL( LBLRTM_Layer%Fhdr%Frequency_Interval, Double ) )
    LBLRTM_Layer%Frequency_Interval = REAL( LBLRTM_Layer%Fhdr%Frequency_Interval, Double )

  END FUNCTION Read_LBLRTM_Layer


!------------------------------------------------------------------------------
!
! NAME:
!       Write_LBLRTM_Layer
!
! PURPOSE:
!       Function to write a layer of data to an LBLRTM format file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_LBLRTM_Layer ( FileID,                        &  ! Input
!                                           LBLRTM_Layer,                  &  ! Input
!                                           Panel_Request = Panel_Request, &  ! Optional input
!                                           Write_EOL     = Write_EOL,     &  ! Optional input
!                                           RCS_Id        = RCS_Id,        &  ! Revision control
!                                           Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       FileID:             Logical unit number associated with LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       LBLRTM_Layer:       LBLRTM_Layer structure containing the layer data
!                           to write.
!                           UNITS:      N/A
!                           TYPE:       LBLRTM_Layer_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Panel_Request:      Integer flag specifying which panels from a double
!                           panel file are to be written.
!                             = 1     : The first panel is written
!                             = 2     : The second panel is written
!                             = other : Both panels are written. [DEFAULT]
!                           If not specified, ALL the data is written.
!                           If specified with a single panel LBLRTM layer type,
!                           it is ignored.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Write_EOL:          Set this keyword to write an End-Of-Level (EOL) marker
!                           to teh output LBLRTM file.
!                             = 0     : No EOL marker written
!                             = 1     : EOL marker is written
!                             = other : No EOL marker written [DEFAULT]
!                           If not specified, no EOL marker is written.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the LBLRTM layer data write was successful
!                              == FAILURE an error occurred
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Write_LBLRTM_Layer ( FileID,        &  ! Input
                                LBLRTM_Layer,  &  ! Input
                                Panel_Request, &  ! Optional input
                                Write_EOL,     &  ! Optional input
                                RCS_Id,        &  ! Revision control
                                Message_Log )  &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    INTEGER,                   INTENT(IN)  :: FileID
    TYPE(LBLRTM_Layer_type), INTENT(IN)  :: LBLRTM_Layer
    INTEGER,        OPTIONAL,  INTENT(IN)  :: Panel_Request
    INTEGER,        OPTIONAL,  INTENT(IN)  :: Write_EOL
    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,  INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_Layer'
    ! Local variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: EOL_Output
    INTEGER :: io_status
    INTEGER :: n_Panel_Chunks, j
    INTEGER :: i_Panel_Begin, i_Panel_End, i
    INTEGER :: l1, l2
    INTEGER :: Phdr_i_count
    REAL( Double ) :: Begin_Frequency
    REAL( Double ) ::   End_Frequency
    TYPE(LBLRTM_Phdr_type) :: Phdr

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check if file is open
    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'LBLRTM file is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Default is to write all data...
    i_Panel_Begin = 1
    i_Panel_End   = LBLRTM_Layer%n_Panels
    ! ...otherwise determine what data to write.
    IF ( PRESENT( Panel_Request ) ) THEN
      SELECT CASE ( Panel_Request )
        CASE ( 1 )
          ! Just write the first panel
          i_Panel_Begin = 1
          i_Panel_End   = 1
        CASE ( 2 )
          ! Just write the second panel
          i_Panel_Begin = 2
          i_Panel_End   = 2
        CASE DEFAULT
          ! Everything else writes everything
          i_Panel_Begin = 1
          i_Panel_End   = LBLRTM_Layer%n_Panels
      END SELECT
    END IF

    ! Default is no EOL output...
    EOL_Output = .FALSE.
    ! ...unless the keyword is correctly set
    IF ( PRESENT( Write_EOL ) ) THEN
      IF ( Write_EOL == SET ) EOL_Output = .TRUE.
    END IF


    ! Write the file header
    ! ---------------------
    Error_Status = Write_LBLRTM_Fhdr( FileID, &
                                      LBLRTM_Layer%Fhdr, &
                                      Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred writing file header.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                         -- WRITE PANEL DATA --                           #
    !#                                                                          #
    !# Here it should be pointed out that the term "panel" refers to two sort   #
    !# of different things.                                                     #
    !#                                                                          #
    !# A "single panel" or "double panel" file refers to an LBLRTM format file  #
    !# that contains one or two spectrally coincident data set(s) respectively. #
    !# E.g. a single panel file usually contains optical depths, and a double   #
    !# panel file usually contains radiances (first panel) and transmittances   #
    !# (second panel). So in this respect how many "panels" a file contains is  #
    !# indicative of how much data is in the file.                              #
    !#                                                                          #
    !# The other definition relates to how data is stored within a layer. Each  #
    !# layer spectrum (or spectra if it's a double panel file) is stored in a   #
    !# series of "chunks", also referred to as (you guessed it) "panels".       #
    !#                                                                          #
    !# Over the years of working with FASCODE/LBLRTM format data files I have   #
    !# conflated these two meanings. Sorry 'bout that. So, to be clear, the     #
    !# following "Write_Panel_Chunk" loop is for writing the data "chunk"       #
    !# panels.                                                                  # 
    !#--------------------------------------------------------------------------#

    ! The number of complete panels
    n_Panel_Chunks = ( LBLRTM_Layer%n_Points / LBLRTM_MAX_PANEL_POINTS )

    ! The left overs
    IF ( MOD( LBLRTM_Layer%n_Points, LBLRTM_MAX_PANEL_POINTS ) /= 0 ) THEN
      n_Panel_Chunks = n_Panel_Chunks + 1
    END IF

    ! Initialise panel begin index
    l1 = 1


    ! Begin panel write loop
    ! ----------------------
    Write_Panel_Chunk: DO j = 1, n_Panel_Chunks

      ! Write the pnale chunk header
      ! ----------------------------
      ! Calculate the panel end index
      l2 = MIN( l1 + LBLRTM_MAX_PANEL_POINTS - 1, LBLRTM_Layer%n_Points )

      ! Construct panel chunk header
      Begin_Frequency = LBLRTM_Layer%Begin_Frequency + &
                        ( REAL( l1 - 1, Double ) * LBLRTM_Layer%Frequency_Interval )
      End_Frequency   = LBLRTM_Layer%Begin_Frequency + &
                        ( REAL( l2 - 1, Double ) * LBLRTM_Layer%Frequency_Interval )
      Phdr = LBLRTM_Phdr_type( Begin_Frequency,                 &
                               End_Frequency,                   &
                               LBLRTM_Layer%Frequency_Interval, &
                               l2 - l1 + 1                      )

      ! Write the header
      Error_Status = Write_LBLRTM_Phdr( FileID, &
                                        Phdr, &
                                        Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing panel chunk #", i4, " header." )' ) j
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! Write the required number of panel chunks
      ! -----------------------------------------
      ! Now loop over panel chunks
      DO i = i_Panel_Begin, i_Panel_End

        ! Write the actual panel chunk data
        Error_Status = Write_LBLRTM_Panel( FileID, &
                                           REAL( LBLRTM_Layer%Spectrum(l1:l2,i), LBLRTM_FP_KIND ), &
                                           Message_Log = Message_Log    )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error writing panel chunk #", i4, &
                            &" data for panel ", i1, "." )' ) j, i
          CALL Display_Message( ROUTINE_NAME, &
                                message, &
                                Error_Status, &
                                Message_Log = Message_Log )
          CLOSE( FileID )
          RETURN
        END IF

      END DO

      ! Update the begin index for the input arrays
      l1 = l2 + 1

    END DO Write_Panel_Chunk


    ! Write an end-of-layer marker
    ! ----------------------------
    IF ( EOL_Output ) THEN
      Error_Status = Write_LBLRTM_EOL( FileID, &
                                       Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error writing EOL marker.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF
    END IF

  END FUNCTION Write_LBLRTM_Layer

END MODULE LBLRTM_Layer_IO
