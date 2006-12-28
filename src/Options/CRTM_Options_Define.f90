!
! CRTM_Options_Define
!
! Module defining the CRTM Options optional argument data structure
! and containing routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Options_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds,      ONLY: fp=>fp_kind
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, SET, NOT_SET, STRLEN
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure data type
  PUBLIC :: CRTM_Options_type
  ! Definition functions
  PUBLIC :: CRTM_Associated_Options
  PUBLIC :: CRTM_Destroy_Options
  PUBLIC :: CRTM_Allocate_Options
  PUBLIC :: CRTM_Assign_Options


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Destroy_Options
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE CRTM_Destroy_Options

  INTERFACE CRTM_Allocate_Options
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE CRTM_Allocate_Options

  INTERFACE CRTM_Assign_Options
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Options


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Options_Define.f90,v 1.5 2006/05/25 19:35:52 wd20pd Exp $'


  ! ----------------------------
  ! Options data type definition
  ! ----------------------------
  TYPE :: CRTM_Options_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Channels = 0  ! L dimension
    ! Index into channel-specific components
    INTEGER :: Channel = 0
    ! Emissivity optional arguments
    INTEGER                         :: Emissivity_Switch =  NOT_SET
    REAL(fp), DIMENSION(:), POINTER :: Emissivity        => NULL() ! L
    ! Direct reflectivity optional arguments
    INTEGER                         :: Direct_Reflectivity_Switch =  NOT_SET
    REAL(fp), DIMENSION(:), POINTER :: Direct_Reflectivity        => NULL() ! L
  END TYPE CRTM_Options_type


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
!       CRTM_Clear_Options
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM Options structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Options( Options ) ! Output
!
! OUTPUT ARGUMENTS:
!       Options:      Options structure for which the scalar members have
!                     been cleared.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Options( Options )
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options
    Options%n_Channels = 0
    Options%Emissivity_Switch = NOT_SET
    Options%Direct_Reflectivity_Switch = NOT_SET
  END SUBROUTINE CRTM_Clear_Options





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
!       CRTM_Associated_Options
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_Options structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Options( Options            , &  ! Input
!                                                     ANY_Test = Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       Options:             Options structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Options_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Options structure pointer members are associated.
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
!                            association status of the Options pointer
!                            members.
!                            .TRUE.  - if ALL the Options pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the Options
!                                      pointer members are associated.
!                            .FALSE. - some or all of the Options pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_Options( Options,   & ! Input
                                    ANY_Test ) & ! Optional input
                                  RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN) :: Options
    INTEGER,         OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test

    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF

    ! Test the structure pointer association
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( Options%Emissivity          ) .AND. &
           ASSOCIATED( Options%Direct_Reflectivity )       ) THEN 
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( Options%Emissivity          ) .OR. &
           ASSOCIATED( Options%Direct_Reflectivity )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION CRTM_Associated_Options


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_Options
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a CRTM
!       Options data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Options( Options                , &  ! Output
!                                            RCS_Id     =RCS_Id     , &  ! Revision control
!                                            Message_Log=Message_Log  )  ! Error messaging
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
! OUTPUT ARGUMENTS:
!       Options:      Re-initialized Options structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank-1
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Options    , &  ! Output
                           No_Clear   , &  ! Optional input
                           RCS_Id     , &  ! Revision control
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Options(Scalar)'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF

    ! Initialise the scalar members
    IF ( Clear ) CALL CRTM_Clear_Options( Options )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. CRTM_Associated_Options( Options ) ) RETURN

    ! Deallocate the pointer members
    DEALLOCATE( Options%Emissivity         , &
                Options%Direct_Reflectivity, &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating Options. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

    ! Decrement and test allocation counter
    Options%n_Allocates = Options%n_Allocates - 1
    IF ( Options%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i0 )' ) &
                      Options%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF
  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( Options    , &  ! Output
                          No_Clear   , &  ! Optional input
                          RCS_Id     , &  ! Revision control
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options(:)
    INTEGER,      OPTIONAL,  INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL,  INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL,  INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Options(Rank-1)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Loop over Options entries
    DO n = 1, SIZE( Options )
      Scalar_Status = Destroy_Scalar( Options(n)             , &
                                      No_Clear   =No_Clear   , &
                                      Message_Log=Message_Log  )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i0, &
                          &" of CRTM_Options structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO
  END FUNCTION Destroy_Rank1


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_Options
! 
! PURPOSE:
!       Function to allocate the pointer members of a CRTM Options
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Options( n_Channels,               &  ! Input
!                                             Options,                  &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:   Number of sensor channels
!                     Must be > 0
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       Options:      CRTM_Options structure with allocated pointer members.
!                     Upon allocation, all pointer members are initialized to
!                     a value of zero.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank-1
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Channels , &  ! Input
                            Options    , &  ! Output
                            RCS_Id     , &  ! Revision control
                            Message_Log) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                   INTENT(IN)     :: n_Channels
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Options(Scalar)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( CRTM_Associated_Options( Options, ANY_Test=SET ) ) THEN
      Error_Status = CRTM_Destroy_Options( Options, &
                                           No_Clear=SET, &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_Options pointer members.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Allocate the structure
    ALLOCATE( Options%Emissivity(n_Channels)         , &
              Options%Direct_Reflectivity(n_Channels), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTM_Options data arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign dimensions and initialise variables
    Options%n_Channels = n_Channels

    Options%Emissivity          = ZERO
    Options%Direct_Reflectivity = ZERO

    ! Increment and test the allocation counter
    Options%n_Allocates = Options%n_Allocates + 1
    IF ( Options%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i0 )' ) &
                      Options%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank1( n_Channels , &  ! Input,  scalar
                           Options    , &  ! Output, rank-1
                           RCS_Id     , &  ! Revision control
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: n_Channels
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options(:)
    CHARACTER(*), OPTIONAL,  INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL,  INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Options(Rank-1)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Loop over Options entries
    DO n = 1, SIZE(Options)
      Error_Status = Allocate_Scalar( n_Channels, & ! Input
                                      Options(n), & ! Output
                                      Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error allocating element #", i0, &
                          &" of rank-1 CRTM_Options structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END DO
  END FUNCTION Allocate_Rank1


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_Options
!
! PURPOSE:
!       Function to copy valid CRTM Options structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Options( Options_in             , &  ! Input
!                                           Options_out            , &  ! Output
!                                           RCS_Id     =RCS_Id     , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Options_in:      Options structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Options_type
!                        DIMENSION:  Scalar
!                                      OR
!                                    Rank-1
!                        ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       Options_out:     Copy of the input structure, Options_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Options_type
!                        DIMENSION:  Same as Options_in
!                        ATTRIBUTES: INTENT(IN OUT)
!
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
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( Options_in , &  ! Input
                          Options_out, &  ! Output
                          RCS_Id     , &  ! Revision control
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN)     :: Options_in
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options_out
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Options(Scalar)'

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. CRTM_Associated_Options( Options_In ) ) THEN
      Error_Status = CRTM_Destroy_Options( Options_Out, &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating output CRTM_Options pointer members.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Allocate the structure
    Error_Status = CRTM_Allocate_Options( Options_in%n_Channels  , &
                                          Options_out            , &
                                          Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CRTM_Options arrays.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign scalar data
    Options_out%Emissivity_Switch          = Options_in%Emissivity_Switch
    Options_out%Direct_Reflectivity_Switch = Options_in%Direct_Reflectivity_Switch

    ! Assign array data
    Options_out%Emissivity          = Options_in%Emissivity
    Options_out%Direct_Reflectivity = Options_in%Direct_Reflectivity

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Options_in , &  ! Input
                         Options_out, &  ! Output
                         RCS_Id     , &  ! Revision control
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN)     :: Options_in(:)
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options_out(:)
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Options(Rank-1)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: i, n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    n = SIZE( Options_in )
    IF ( SIZE( Options_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Options_in and Options_out arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Perform the assignment
    DO i = 1, n
      Error_Status = Assign_Scalar( Options_in(i)          , &
                                    Options_out(i)         , &
                                    Message_Log=Message_Log  )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error copying element #", i0, &
                          &" of CRTM_Options structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END DO
  END FUNCTION Assign_Rank1

END MODULE CRTM_Options_Define
