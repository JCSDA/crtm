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
                             SET, SL=>STRLEN
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! The derived type definition
  PUBLIC :: CRTM_ChannelInfo_type
  ! Definition functions
  PUBLIC :: CRTM_Associated_ChannelInfo
  PUBLIC :: CRTM_Destroy_ChannelInfo
  PUBLIC :: CRTM_Allocate_ChannelInfo
  PUBLIC :: CRTM_Assign_ChannelInfo
  PUBLIC :: CRTM_nChannels_ChannelInfo

  ! -------------------
  ! Procedure overloads
  ! -------------------
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
  ! ChannelInfo scalar invalid value
  INTEGER, PARAMETER :: INVALID = -1


  ! --------------------------------
  ! ChannelInfo data type definition
  ! --------------------------------
  TYPE :: CRTM_ChannelInfo_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Channels = 0  ! L dimension
    INTEGER :: StrLen = SL
    ! Scalar data
    CHARACTER(SL) :: Sensor_ID        = ' '
    INTEGER       :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER       :: Sensor_Index     = 0
    ! Array data
    INTEGER, POINTER :: Sensor_Channel(:) => NULL()  ! L
    INTEGER, POINTER :: Channel_Index(:)  => NULL()  ! L
  END TYPE CRTM_ChannelInfo_type


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
!       CRTM_Clear_ChannelInfo
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM ChannelInfo structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_ChannelInfo( ChannelInfo) ! Output
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_ChannelInfo( ChannelInfo )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo
    ChannelInfo%StrLen = SL
    ChannelInfo%Sensor_ID        = ' '
    ChannelInfo%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    ChannelInfo%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    ChannelInfo%Sensor_Index     = 0
  END SUBROUTINE CRTM_Clear_ChannelInfo





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
!       CRTM_Associated_ChannelInfo
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_ChannelInfo structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_ChannelInfo( ChannelInfo      , &  ! Input
!                                                         ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       ChannelInfo: ChannelInfo structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_ChannelInfo_type
!                    DIMENSION:  Scalar
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
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_ChannelInfo( ChannelInfo, & ! Input
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

  END FUNCTION CRTM_Associated_ChannelInfo


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_ChannelInfo
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a CRTM
!       ChannelInfo data structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_ChannelInfo( ChannelInfo            , &  ! Output
!                                                RCS_Id     =RCS_Id     , &  ! Revision control
!                                                Message_Log=Message_Log  )  ! Error messaging
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
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_ChannelInfo( ChannelInfo, &  ! Output
                                     No_Clear   , &  ! Optional input
                                     RCS_Id     , &  ! Revision control
                                     Message_Log) &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo
    INTEGER,           OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),      OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_ChannelInfo'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

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
      WRITE( Message, '( "Error deallocating CRTM_ChannelInfo. STAT = ", i0 )' ) &
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
  END FUNCTION CRTM_Destroy_ChannelInfo


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_ChannelInfo
! 
! PURPOSE:
!       Function to allocate the pointer members of a CRTM ChannelInfo
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_ChannelInfo( n_Channels             , &  ! Input
!                                                 ChannelInfo            , &  ! Output
!                                                 RCS_Id     =RCS_Id     , &  ! Revision control
!                                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:   The number of channels in the ChannelInfo structure.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:  ChannelInfo structure with allocated pointer members
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
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_ChannelInfo( n_Channels , &  ! Input
                                      ChannelInfo, &  ! Output
                                      RCS_Id     , &  ! Revision control
                                      Message_Log) &  ! Error messaging
                                    RESULT( Error_Status )
    ! Arguments
    INTEGER,                     INTENT(IN)     :: n_Channels
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo
    CHARACTER(*),      OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_ChannelInfo'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

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
                              'Error deallocating CRTM_ChannelInfo pointer members.', &
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
      WRITE( Message, '( "Error allocating CRTM_ChannelInfo data arrays. STAT = ", i0 )' ) &
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
  END FUNCTION CRTM_Allocate_ChannelInfo


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_ChannelInfo
!
! PURPOSE:
!       Function to copy valid CRTM ChannelInfo structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_ChannelInfo( ChannelInfo_in         , &  ! Input
!                                               ChannelInfo_out        , &  ! Output
!                                               RCS_Id     =RCS_Id     , &  ! Revision control
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ChannelInfo_in:  ChannelInfo structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_ChannelInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ChannelInfo_out: Copy of the input structure, ChannelInfo_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_ChannelInfo_type
!                        DIMENSION:  Scalar
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
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Assign_ChannelInfo( ChannelInfo_in , &  ! Input
                                    ChannelInfo_out, &  ! Output
                                    RCS_Id         , &  ! Revision control
                                    Message_Log    ) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN)     :: ChannelInfo_in
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo_out
    CHARACTER(*),      OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_ChannelInfo'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

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
                              'Error deallocating output CRTM_ChannelInfo pointer members.', &
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
                            'Error allocating output CRTM_ChannelInfo arrays.', &
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

  END FUNCTION CRTM_Assign_ChannelInfo


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_nChannels_ChannelInfo
!
! PURPOSE:
!       Function to return the number of channels defined in a ChannelInfo
!       structure or structure array
!
! CALLING SEQUENCE:
!       nChannels = CRTM_nChannels_ChannelInfo( ChannelInfo ) ! Input
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
  
END MODULE CRTM_ChannelInfo_Define
