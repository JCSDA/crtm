!
! CRTM_ChannelInfo_Define
!
! Module defining the CRTM ChannelInfo data structure and containing
! routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_ChannelInfo_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds
  USE Message_Handler
  USE CRTM_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                             INVALID_WMO_SENSOR_ID   , &
                             SET, STRLEN
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure definition
  PUBLIC :: CRTM_ChannelInfo_type
  ! Structure procedures
  PUBLIC :: CRTM_Associated_ChannelInfo
  PUBLIC :: CRTM_Destroy_ChannelInfo
  PUBLIC :: CRTM_Allocate_ChannelInfo
  PUBLIC :: CRTM_Assign_ChannelInfo
  PUBLIC :: CRTM_Equal_ChannelInfo
  ! Utility procedures
  PUBLIC :: CRTM_nChannels_ChannelInfo
  PUBLIC :: CRTM_RCS_ID_ChannelInfo

  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE CRTM_Associated_ChannelInfo
    MODULE PROCEDURE Associated_Scalar
    MODULE PROCEDURE Associated_Rank1
  END INTERFACE CRTM_Associated_ChannelInfo

  INTERFACE CRTM_Destroy_ChannelInfo
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE CRTM_Destroy_ChannelInfo

  INTERFACE CRTM_Allocate_ChannelInfo
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE CRTM_Allocate_ChannelInfo

  INTERFACE CRTM_Assign_ChannelInfo
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_ChannelInfo

  INTERFACE CRTM_Equal_ChannelInfo
    MODULE PROCEDURE Equal_Scalar
    MODULE PROCEDURE Equal_Rank1
  END INTERFACE CRTM_Equal_ChannelInfo
  
  INTERFACE CRTM_nChannels_ChannelInfo
    MODULE PROCEDURE nChannels_Scalar
    MODULE PROCEDURE nChannels_Rank1
  END INTERFACE CRTM_nChannels_ChannelInfo
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! ChannelInfo scalar invalid value
  INTEGER, PARAMETER :: INVALID = -1


  ! --------------------------------
  ! ChannelInfo data type definition
  ! --------------------------------
  !:tdoc+:
  TYPE :: CRTM_ChannelInfo_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Channels = 0  ! L dimension
    ! Scalar data
    CHARACTER(STRLEN) :: Sensor_ID        = ' '
    INTEGER           :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER           :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER           :: Sensor_Index     = 0
    ! Array data
    INTEGER, POINTER :: Sensor_Channel(:) => NULL()  ! L
    INTEGER, POINTER :: Channel_Index(:)  => NULL()  ! L
  END TYPE CRTM_ChannelInfo_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Associated_ChannelInfo
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM ChannelInfo structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_ChannelInfo( ChannelInfo      , &
!                                                         ANY_Test=Any_Test  )
!
! INPUT ARGUMENTS:
!       ChannelInfo: ChannelInfo structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_ChannelInfo_type
!                    DIMENSION:  Scalar OR Rank-1 array
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    ChannelInfo structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the ChannelInfo pointer
!                            members.
!                            .TRUE.  - if ALL the ChannelInfo pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the ChannelInfo
!                                      pointer members are associated.
!                            .FALSE. - some or all of the ChannelInfo pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Same as input ChannelInfo argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Associated_Scalar( ChannelInfo, & ! Input
                              ANY_Test   ) & ! Optional input
                            RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo
    INTEGER,           OPTIONAL, INTENT(IN) :: ANY_Test
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


    ! Test the structure pointer association
    ! --------------------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( ChannelInfo%Sensor_Channel ) .AND. &
           ASSOCIATED( ChannelInfo%Channel_Index  )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( ChannelInfo%Sensor_Channel ) .OR. &
           ASSOCIATED( ChannelInfo%Channel_Index  )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_Scalar

  FUNCTION Associated_Rank1( ChannelInfo , & ! Input
                             ANY_Test    ) & ! Optional input
                           RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:)
    INTEGER,           OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status(SIZE(ChannelInfo))
    ! Local variables
    INTEGER :: n

    DO n = 1, SIZE(ChannelInfo)
      Association_Status(n) = Associated_Scalar(ChannelInfo(n), ANY_Test=ANY_Test)
    END DO

  END FUNCTION Associated_Rank1


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Destroy_ChannelInfo
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a CRTM
!       ChannelInfo data structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_ChannelInfo( ChannelInfo            , &
!                                                Message_Log=Message_Log  )
! 
! OUTPUT ARGUMENTS:
!       ChannelInfo:  Re-initialized ChannelInfo structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( ChannelInfo, &  ! Output
                           No_Clear   , &  ! Optional input
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo
    INTEGER,           OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_ChannelInfo(Scalar)'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Re-initialise the dimensions
    ChannelInfo%n_Channels = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL CRTM_Clear_ChannelInfo( ChannelInfo )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. CRTM_Associated_ChannelInfo( ChannelInfo ) ) RETURN


    ! Deallocate the array components
    ! -------------------------------
    DEALLOCATE( ChannelInfo%Sensor_Channel , &
                ChannelInfo%Channel_Index  , &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating ChannelInfo. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    ChannelInfo%n_Allocates = ChannelInfo%n_Allocates - 1
    IF ( ChannelInfo%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      ChannelInfo%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF
  END FUNCTION Destroy_Scalar

  FUNCTION Destroy_Rank1( ChannelInfo, &  ! Output
                          No_Clear   , &  ! Optional input
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo(:)
    INTEGER     ,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_ChannelInfo(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Reinitialise array
    ! ------------------
    DO n = 1, SIZE(ChannelInfo)
      Scalar_Status = Destroy_Scalar( ChannelInfo(n), &
                                      No_Clear=No_Clear, &
                                      Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i0, &
                          &" of ChannelInfo structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO

  END FUNCTION Destroy_Rank1
  
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Allocate_ChannelInfo
! 
! PURPOSE:
!       Function to allocate the pointer members of a CRTM ChannelInfo
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_ChannelInfo( n_Channels             , &
!                                                 ChannelInfo            , &
!                                                 Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       n_Channels:   The number of channels in the ChannelInfo structure.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:  ChannelInfo structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Same as input n_Channels argument
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Channels , &  ! Input
                            ChannelInfo, &  ! Output
                            Message_Log) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                     INTENT(IN)     :: n_Channels
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_ChannelInfo(Scalar)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check dimensions
    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If so, then destroy structure
    IF ( CRTM_Associated_ChannelInfo( ChannelInfo, ANY_Test=SET ) ) THEN
      Error_Status = CRTM_Destroy_ChannelInfo( ChannelInfo, &
                                               Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating ChannelInfo pointer members.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Allocate the arrays
    ! -------------------
    ALLOCATE( ChannelInfo%Sensor_Channel(n_Channels), &
              ChannelInfo%Channel_Index(n_Channels) , &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating ChannelInfo data arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign dimensions and initialise variables
    ! ------------------------------------------
    ChannelInfo%n_Channels = n_Channels

    ChannelInfo%Sensor_Channel = INVALID
    ChannelInfo%Channel_Index  = INVALID


    ! Increment and test the allocation counter
    ! -----------------------------------------
    ChannelInfo%n_Allocates = ChannelInfo%n_Allocates + 1
    IF ( ChannelInfo%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      ChannelInfo%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF
  END FUNCTION Allocate_Scalar

  FUNCTION Allocate_Rank1( n_Channels , &  ! Input
                           ChannelInfo, &  ! Output
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    INTEGER                    , INTENT(IN)     :: n_Channels(:)
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo(:)
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_ChannelInfo(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Array arguments must conform
    n = SIZE(ChannelInfo)
    IF ( SIZE(n_Channels) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels and ChannelInfo arrays have different dimensions', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Perform the allocation
    ! ----------------------
    DO i = 1, n
      Scalar_Status = Allocate_Scalar( n_Channels(i), &
                                       ChannelInfo(i), &
                                       Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i0, &
                          &" of ChannelInfo structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO

  END FUNCTION Allocate_Rank1


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Assign_ChannelInfo
!
! PURPOSE:
!       Function to copy valid CRTM ChannelInfo structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_ChannelInfo( ChannelInfo_in         , &
!                                               ChannelInfo_out        , &
!                                               Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       ChannelInfo_in:  ChannelInfo structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_ChannelInfo_type
!                        DIMENSION:  Scalar OR Rank-1 array
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ChannelInfo_out: Copy of the input structure, ChannelInfo_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_ChannelInfo_type
!                        DIMENSION:  Same as ChannelInfo_in argument
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
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
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( ChannelInfo_in , &  ! Input
                          ChannelInfo_out, &  ! Output
                          Message_Log    ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN)     :: ChannelInfo_in
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo_out
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_ChannelInfo(Scalar)'

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! ALL *input* pointers must be associated.
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. CRTM_Associated_ChannelInfo( ChannelInfo_In ) ) THEN
      Error_Status = CRTM_Destroy_ChannelInfo( ChannelInfo_Out, &
                                               Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating output ChannelInfo pointer members.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Allocate the structure
    ! ----------------------
    Error_Status = CRTM_Allocate_ChannelInfo( ChannelInfo_in%n_Channels, &
                                              ChannelInfo_out          , &
                                              Message_Log=Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output ChannelInfo arrays.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign data
    ! -----------
    ! Scalars
    ChannelInfo_out%Sensor_ID        = ChannelInfo_in%Sensor_ID
    ChannelInfo_out%WMO_Sensor_ID    = ChannelInfo_in%WMO_Sensor_ID
    ChannelInfo_out%WMO_Satellite_ID = ChannelInfo_in%WMO_Satellite_ID
    ChannelInfo_out%Sensor_Index     = ChannelInfo_in%Sensor_Index
    ! Arrays
    ChannelInfo_out%Sensor_Channel   = ChannelInfo_in%Sensor_Channel
    ChannelInfo_out%Channel_Index    = ChannelInfo_in%Channel_Index

  END FUNCTION Assign_Scalar

  FUNCTION Assign_Rank1( ChannelInfo_in , &  ! Input
                         ChannelInfo_out, &  ! Output
                         Message_Log    ) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN)     :: ChannelInfo_in(:)
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo_out(:)
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_ChannelInfo(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Output array must be large enough to handle input copy.
    n = SIZE(ChannelInfo_in)
    IF ( SIZE(ChannelInfo_out) < n ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("ChannelInfo_out array not large enough (",i0,&
                      &") to hold ChannelInfo_in data (",i0,").")' ) &
                      SIZE(ChannelInfo_out), n
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Perform the assignment
    ! ----------------------
    DO i = 1, n
      Scalar_Status = Assign_Scalar( ChannelInfo_in(i), &
                                     ChannelInfo_out(i), &
                                     Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i0, &
                          &" of ChannelInfo structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO

  END FUNCTION Assign_Rank1


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Equal_ChannelInfo
!
! PURPOSE:
!       Function to test if two ChannelInfo structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Equal_ChannelInfo( ChannelInfo_LHS        , &
!                                              ChannelInfo_RHS        , &
!                                              ULP_Scale  =ULP_Scale  , &
!                                              Check_All  =Check_All  , &
!                                              Message_Log=Message_Log  )
!
!
! INPUT ARGUMENTS:
!       ChannelInfo_LHS: ChannelInfo structure to be compared; equivalent
!                        to the left-hand side of a lexical comparison, e.g.
!                          IF ( ChannelInfo_LHS == ChannelInfo_RHS ).
!                        UNITS:      N/A
!                        TYPE:       CRTM_ChannelInfo_type
!                        DIMENSION:  Scalar OR Rank-1 array
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelInfo_RHS: ChannelInfo structure to be compared to; equivalent
!                        to the right-hand side of a lexical comparison, e.g.
!                          IF ( ChannelInfo_LHS == ChannelInfo_RHS ).
!                        UNITS:      N/A
!                        TYPE:       CRTM_ChannelInfo_type
!                        DIMENSION:  Same as ChannelInfo_LHS argument
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:      Unit of data precision used to scale the floating
!                       point comparison. ULP stands for "Unit in the Last Place,"
!                       the smallest possible increment or decrement that can be
!                       made using a machine's floating point arithmetic.
!                       Value must be positive - if a negative value is supplied,
!                       the absolute value is used. If not specified, the default
!                       value is 1.
!                       ** NOTE: This is a hook for future changes and is not used.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:      Set this argument to check ALL the floating point
!                       channel data of the ChannelInfo structures. The default
!                       action is return with a FAILURE status as soon as
!                       any difference is found. This optional argument can
!                       be used to get a listing of ALL the differences
!                       between data in ChannelInfo structures.
!                       If == 0, Return with FAILURE status as soon as
!                                ANY difference is found  *DEFAULT*
!                          == 1, Set FAILURE status if ANY difference is
!                                found, but continue to check ALL data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the structures were equal
!                          == FAILURE - an error occurred, or
!                                     - the structures were different.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Equal_Scalar( ChannelInfo_LHS, &  ! Input
                         ChannelInfo_RHS, &  ! Input
                         ULP_Scale  , &  ! Optional input
                         Check_All  , &  ! Optional input
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN)  :: ChannelInfo_LHS
    TYPE(CRTM_ChannelInfo_type), INTENT(IN)  :: ChannelInfo_RHS
    INTEGER,           OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,           OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_ChannelInfo(scalar)'
    CHARACTER(*), PARAMETER :: FFMT = 'es22.15'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: l

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Default precision is a single unit in last place
    ULP = 1
    ! ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT(ULP_Scale) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF

    ! Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT(Check_All) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF

    ! Check the structure association status
    IF ( .NOT. CRTM_Associated_ChannelInfo( ChannelInfo_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ChannelInfo_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. CRTM_Associated_ChannelInfo( ChannelInfo_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ChannelInfo_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( ChannelInfo_LHS%n_Channels /= ChannelInfo_RHS%n_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Compare the values
    ! ------------------
    IF ( ChannelInfo_LHS%Sensor_Id /= ChannelInfo_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Sensor_Id values are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ChannelInfo_LHS%WMO_Satellite_Id /= ChannelInfo_RHS%WMO_Satellite_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'WMO_Satellite_Id values are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ChannelInfo_LHS%WMO_Sensor_Id /= ChannelInfo_RHS%WMO_Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'WMO_Sensor_Id values are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ChannelInfo_LHS%Sensor_Index /= ChannelInfo_RHS%Sensor_Index ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Sensor_Index values are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    DO l = 1, ChannelInfo_LHS%n_Channels
      IF ( ChannelInfo_LHS%Sensor_Channel(l) /= ChannelInfo_RHS%Sensor_Channel(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Sensor_Channel values are different at index ",i0,&
                        &":",i0,", ",i0)' ) &
                       l, ChannelInfo_LHS%Sensor_Channel(l), &
                          ChannelInfo_RHS%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, ChannelInfo_LHS%n_Channels
      IF ( ChannelInfo_LHS%Channel_Index(l) /= ChannelInfo_RHS%Channel_Index(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Channel_Index values are different at index ",i0,&
                        &":",i0,", ",i0)' ) &
                       l, ChannelInfo_LHS%Channel_Index(l), &
                          ChannelInfo_RHS%Channel_Index(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
  END FUNCTION Equal_Scalar

  FUNCTION Equal_Rank1( ChannelInfo_LHS, &  ! Input
                        ChannelInfo_RHS, &  ! Output
                        ULP_Scale      , &  ! Optional input
                        Check_All      , &  ! Optional input
                        Message_Log    ) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN)  :: ChannelInfo_LHS(:)
    TYPE(CRTM_ChannelInfo_type), INTENT(IN)  :: ChannelInfo_RHS(:)
    INTEGER,           OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,           OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_ChannelInfo(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Check_Once
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT(Check_All) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF

    ! Dimensions
    n = SIZE(ChannelInfo_LHS)
    IF ( SIZE(ChannelInfo_RHS) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ChannelInfo_LHS and ChannelInfo_RHS arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Test for equality
    ! -----------------
    DO i = 1, n
      Scalar_Status = Equal_Scalar( ChannelInfo_LHS(i), &
                                    ChannelInfo_RHS(i), &
                                    ULP_Scale  =ULP_Scale, &
                                    Check_All  =Check_All, &
                                    Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error comparing element (",i0,")", &
                          &" of rank-1 ChannelInfo structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
  END FUNCTION Equal_Rank1


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_nChannels_ChannelInfo
!
! PURPOSE:
!       Function to return the number of channels defined in a ChannelInfo
!       structure or structure array
!
! CALLING SEQUENCE:
!       nChannels = CRTM_nChannels_ChannelInfo( ChannelInfo )
!
! INPUT ARGUMENTS:
!       ChannelInfo: ChannelInfo structure or structure which is to have its
!                    channels counted.
!                    UNITS:      N/A
!                    TYPE:       CRTM_ChannelInfo_type
!                    DIMENSION:  Scalar
!                                  or
!                                Rank-1
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       nChannels:   The number of defined channels in the input argument.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION nChannels_Scalar( ChannelInfo ) RESULT( nChannels )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo
    INTEGER :: nChannels
    nChannels = ChannelInfo%n_Channels
  END FUNCTION nChannels_Scalar
  
  FUNCTION nChannels_Rank1( ChannelInfo ) RESULT( nChannels )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:) ! N
    INTEGER :: nChannels
    nChannels = SUM(ChannelInfo%n_Channels)
  END FUNCTION nChannels_Rank1


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RCS_ID_ChannelInfo
!
! PURPOSE:
!       Subroutine to return the module RCS Id information.
!
! CALLING SEQUENCE:
!       CALL CRTM_RCS_Id_ChannelInfo( RCS_Id )
!
! OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_RCS_ID_ChannelInfo( RCS_Id )
    CHARACTER(*), INTENT(OUT) :: RCS_Id
    RCS_Id = MODULE_RCS_ID
  END SUBROUTINE CRTM_RCS_ID_ChannelInfo


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
!       CRTM_Clear_ChannelInfo
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM ChannelInfo structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_ChannelInfo( ChannelInfo)
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:  ChannelInfo structure for which the scalar members have
!                     been cleared.
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_ChannelInfo( ChannelInfo )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo
    ChannelInfo%Sensor_ID        = ' '
    ChannelInfo%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    ChannelInfo%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    ChannelInfo%Sensor_Index     = 0
  END SUBROUTINE CRTM_Clear_ChannelInfo
 
END MODULE CRTM_ChannelInfo_Define
