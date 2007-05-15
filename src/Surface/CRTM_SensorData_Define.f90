!
! CRTM_SensorData_Define
!
! Module defining the CRTM SensorData SensorData structure and containing
! routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_SensorData_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE CRTM_Parameters      , ONLY: ZERO, SET, STRLEN, &
                                   INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID   
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_SensorData routines in this module
  PUBLIC :: CRTM_Associated_SensorData
  PUBLIC :: CRTM_Destroy_SensorData
  PUBLIC :: CRTM_Allocate_SensorData
  PUBLIC :: CRTM_Assign_SensorData
  PUBLIC :: CRTM_Equal_SensorData


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


  ! -------------------------------
  ! SensorData data type definition
  ! -------------------------------
  TYPE, PUBLIC :: CRTM_SensorData_type
    INTEGER :: n_Allocates = 0
    ! Dimension values
    INTEGER :: n_Channels = 0  ! L
    INTEGER :: StrLen = STRLEN
    ! The WMO sensor ID of the sensor for which the data is to be used
    INTEGER :: Sensor_ID = INVALID_WMO_SENSOR_ID
    ! The data sensor IDs and channels
    CHARACTER(STRLEN), DIMENSION(:), POINTER :: SensorData_ID    => NULL()  ! L
    INTEGER,           DIMENSION(:), POINTER :: WMO_Satellite_ID => NULL() ! L
    INTEGER,           DIMENSION(:), POINTER :: WMO_Sensor_ID    => NULL() ! L
    INTEGER,           DIMENSION(:), POINTER :: Sensor_Channel   => NULL() ! L
    ! The sensor brightness temperatures
    REAL(fp),          DIMENSION(:), POINTER :: Tb => NULL() ! L
  END TYPE CRTM_SensorData_type


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
!       CRTM_Clear_SensorData
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM SensorData structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_SensorData( SensorData) ! Output
!
! OUTPUT ARGUMENTS:
!       SensorData:  SensorData structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_SensorData_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output SensorData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_SensorData( SensorData )
    TYPE(CRTM_SensorData_type), INTENT(IN OUT) :: SensorData
    SensorData%StrLen    = STRLEN
    SensorData%Sensor_ID = INVALID_WMO_SENSOR_ID
  END SUBROUTINE CRTM_Clear_SensorData


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
!       CRTM_Associated_SensorData
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_SensorData structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_SensorData( SensorData       , &  ! Input
!                                                        ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       SensorData:  SensorData structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_SensorData_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    SensorData structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the SensorData pointer
!                            members.
!                            .TRUE.  - if ALL the SensorData pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the SensorData
!                                      pointer members are associated.
!                            .FALSE. - some or all of the SensorData pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_SensorData( SensorData, & ! Input
                                       ANY_Test  ) & ! Optional input
                                     RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_SensorData_type), INTENT(IN) :: SensorData
    INTEGER,          OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test

    ! Set up
    ! ------
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! Test the structure pointer member association
    ! ---------------------------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( SensorData%SensorData_ID    ) .AND. &
           ASSOCIATED( SensorData%WMO_Satellite_ID ) .AND. &
           ASSOCIATED( SensorData%WMO_Sensor_ID    ) .AND. &
           ASSOCIATED( SensorData%Sensor_Channel   ) .AND. &
           ASSOCIATED( SensorData%Tb               )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( SensorData%SensorData_ID    ) .OR. &
           ASSOCIATED( SensorData%WMO_Satellite_ID ) .OR. &
           ASSOCIATED( SensorData%WMO_Sensor_ID    ) .OR. &
           ASSOCIATED( SensorData%Sensor_Channel   ) .OR. &
           ASSOCIATED( SensorData%Tb               )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION CRTM_Associated_SensorData


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_SensorData
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of SensorData
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_SensorData( SensorData             , &  ! Output
!                                               RCS_Id     =RCS_Id     , &  ! Revision control
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       SensorData:   Re-initialized SensorData structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SensorData_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
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
!                     The error codes are defined in the ERROR_HANDLER module.
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
!       Note the INTENT on the output SensorData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_SensorData( SensorData , &  ! Output
                                    No_Clear   , &  ! Optional input
                                    RCS_Id     , &  ! Revision control
                                    Message_Log) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_SensorData_type), INTENT(IN OUT) :: SensorData
    INTEGER,          OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_SensorData'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status


    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Reinitialise the dimensions
    SensorData%n_Channels = 0

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL CRTM_Clear_SensorData( SensorData )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. CRTM_Associated_SensorData( SensorData ) ) RETURN


    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( SensorData%SensorData_ID   , &
                SensorData%WMO_Satellite_ID, &
                SensorData%WMO_Sensor_ID   , &
                SensorData%Sensor_Channel  , &
                SensorData%Tb              , &
                STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating CRTM_SensorData pointer components.", &
                        &" STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    SensorData%n_Allocates = SensorData%n_Allocates - 1
    IF ( SensorData%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i0 )' ) &
                      SensorData%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION CRTM_Destroy_SensorData


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_SensorData
! 
! PURPOSE:
!       Function to allocate the pointer members of a CRTM SensorData
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_SensorData( n_Channels             , &  ! Input
!                                                SensorData             , &  ! Output
!                                                RCS_Id     =RCS_Id     , &  ! Revision control
!                                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:   The number of channels in the SensorData structure.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SensorData:   SensorData structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       CRTM_SensorData_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
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
!                     The error codes are defined in the ERROR_HANDLER module.
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
!       Note the INTENT on the output SensorData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_SensorData( n_Channels , &  ! Input
                                     SensorData , &  ! Output
                                     RCS_Id     , &  ! Revision control
                                     Message_Log) &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    INTEGER                   , INTENT(IN)     :: n_Channels
    TYPE(CRTM_SensorData_type), INTENT(IN OUT) :: SensorData
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_SensorData'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Dimensions
    IF ( n_Channels < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels must be > or = 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    IF ( CRTM_Associated_SensorData( SensorData, ANY_Test=SET ) ) THEN
      Error_Status = CRTM_Destroy_SensorData( SensorData, &
                                              Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating SensorData pointer members.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! If the number of channels is 0, then we're done
    IF ( n_Channels == 0 ) RETURN


    ! Perform the allocation
    ! ----------------------
    ALLOCATE( SensorData%SensorData_ID( n_Channels ), &
              SensorData%WMO_Satellite_ID( n_Channels ), &
              SensorData%WMO_Sensor_ID( n_Channels ), &
              SensorData%Sensor_Channel( n_Channels ), &
              SensorData%Tb( n_Channels ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating SensorData data arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the dimensions and data
    ! ------------------------------------------
    SensorData%n_Channels = n_Channels
    SensorData%Sensor_ID  = INVALID_WMO_SENSOR_ID
    SensorData%SensorData_ID    = ' '
    SensorData%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    SensorData%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    SensorData%Sensor_Channel   = -1
    SensorData%Tb               = ZERO


    ! Increment and test allocation counter
    ! -------------------------------------
    SensorData%n_Allocates = SensorData%n_Allocates + 1
    IF ( SensorData%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      SensorData%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION CRTM_Allocate_SensorData


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_SensorData
!
! PURPOSE:
!       Function to copy valid SensorData structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_SensorData( SensorData_in          , &  ! Input
!                                              SensorData_out         , &  ! Output
!                                              RCS_Id     =RCS_Id     , &  ! Revision control
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SensorData_in:   SensorData structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SensorData_type
!                        DIMENSION:  Scalar OR Rank-1
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SensorData_out:  Copy of the input structure, SensorData_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SensorData_type
!                        DIMENSION:  Same as SensorData_in
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SensorData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Assign_SensorData( SensorData_in , &  ! Input
                                   SensorData_out, &  ! Output
                                   RCS_Id        , &  ! Revision control
                                   Message_Log   ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_SensorData_type), INTENT(IN)     :: SensorData_in
    TYPE(CRTM_SensorData_type), INTENT(IN OUT) :: SensorData_out
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_SensorData'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. CRTM_Associated_SensorData( SensorData_In ) ) THEN
      Error_Status = CRTM_Destroy_SensorData( SensorData_Out, &
                                              Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_SensorData pointer members.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
      END IF
      RETURN
    END IF


    ! Allocate the structure
    ! ----------------------
    Error_Status = CRTM_Allocate_SensorData( SensorData_in%n_Channels, &
                                             SensorData_out, &
                                             Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output CRTM_SensorData arrays.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign data
    ! -----------
    SensorData_out%Sensor_ID = SensorData_in%Sensor_ID
    SensorData_out%SensorData_ID    = SensorData_in%SensorData_ID
    SensorData_out%WMO_Sensor_ID    = SensorData_in%WMO_Sensor_ID
    SensorData_out%WMO_Satellite_ID = SensorData_in%WMO_Satellite_ID
    SensorData_out%Sensor_Channel   = SensorData_in%Sensor_Channel
    SensorData_out%Tb               = SensorData_in%Tb

  END FUNCTION CRTM_Assign_SensorData


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Equal_SensorData
!
! PURPOSE:
!       Function to test if two SensorData structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Equal_SensorData( SensorData_LHS         , &  ! Input
!                                             SensorData_RHS         , &  ! Input
!                                             ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                             Check_All  =Check_All  , &  ! Optional input
!                                             RCS_Id     =RCS_Id     , &  ! Optional output
!                                             Message_Log=Message_Log  )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!       SensorData_LHS:    SensorData structure to be compared; equivalent to the
!                          left-hand side of a lexical comparison, e.g.
!                            IF ( SensorData_LHS == SensorData_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CRTM_SensorData_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       SensorData_RHS:    SensorData structure to be compared to; equivalent to
!                          right-hand side of a lexical comparison, e.g.
!                            IF ( SensorData_LHS == SensorData_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CRTM_SensorData_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:         Unit of data precision used to scale the floating
!                          point comparison. ULP stands for "Unit in the Last Place,"
!                          the smallest possible increment or decrement that can be
!                          made using a machine's floating point arithmetic.
!                          Value must be positive - if a negative value is supplied,
!                          the absolute value is used. If not specified, the default
!                          value is 1.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:         Set this argument to check ALL the floating point
!                          channel data of the SensorData structures. The default
!                          action is return with a FAILURE status as soon as
!                          any difference is found. This optional argument can
!                          be used to get a listing of ALL the differences
!                          between data in SensorData structures.
!                          If == 0, Return with FAILURE status as soon as
!                                   ANY difference is found  *DEFAULT*
!                             == 1, Set FAILURE status if ANY difference is
!                                   found, but continue to check ALL data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the structures were equal
!                             == FAILURE - an error occurred, or
!                                        - the structures were different.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Equal_SensorData( SensorData_LHS, &  ! Input
                                  SensorData_RHS, &  ! Input
                                  ULP_Scale     , &  ! Optional input
                                  Check_All     , &  ! Optional input
                                  RCS_Id        , &  ! Revision control
                                  Message_Log   ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_SensorData_type), INTENT(IN)  :: SensorData_LHS
    TYPE(CRTM_SensorData_type), INTENT(IN)  :: SensorData_RHS
    INTEGER,          OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,          OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_SensorData'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: l

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Default precision is a single unit in last place
    ULP = 1
    ! ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT( ULP_Scale ) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF

    ! Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF

    ! Check the structure association status
    IF ( .NOT. CRTM_Associated_SensorData( SensorData_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SensorData_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. CRTM_Associated_SensorData( SensorData_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SensorData_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( SensorData_LHS%n_Channels /= SensorData_RHS%n_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Compare the values
    ! ------------------
    IF ( SensorData_LHS%Sensor_ID /= SensorData_RHS%Sensor_ID ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Sensor_ID values are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    DO l = 1, SensorData_LHS%n_Channels
      IF ( SensorData_LHS%SensorData_ID(l) /= SensorData_RHS%SensorData_ID(l) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'SensorData_ID values are different', &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    DO l = 1, SensorData_LHS%n_Channels
      IF ( SensorData_LHS%WMO_Satellite_ID(l) /= SensorData_RHS%WMO_Satellite_ID(l) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'WMO_Satellite_ID values are different', &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    DO l = 1, SensorData_LHS%n_Channels
      IF ( SensorData_LHS%WMO_Sensor_ID(l) /= SensorData_RHS%WMO_Sensor_ID(l) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'WMO_Sensor_ID values are different', &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    DO l = 1, SensorData_LHS%n_Channels
      IF ( SensorData_LHS%Sensor_Channel(l) /= SensorData_RHS%Sensor_Channel(l) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Sensor_Channel values are different', &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    DO l = 1, SensorData_LHS%n_Channels
      IF ( .NOT. Compare_Float( SensorData_LHS%Tb(l), &
                                SensorData_RHS%Tb(l), &
                                ULP = ULP ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Tb values are different', &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
  END FUNCTION CRTM_Equal_SensorData

END MODULE CRTM_SensorData_Define
