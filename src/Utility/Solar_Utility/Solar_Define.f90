!
! Solar_Define
!
! Module defining the Solar data structure and containing routines to 
! manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2002
!                       paul.vandelst@noaa.gov
!

MODULE Solar_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public types
  PUBLIC :: Solar_type
  ! Public procedures
  PUBLIC :: Associated_Solar
  PUBLIC :: Destroy_Solar
  PUBLIC :: Allocate_Solar
  PUBLIC :: Assign_Solar
  PUBLIC :: Equal_Solar
  PUBLIC :: CheckRelease_Solar
  PUBLIC :: Info_Solar
  PUBLIC :: Frequency_Solar


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id$'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Message character length
  INTEGER, PARAMETER :: ML = 512
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: SOLAR_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: SOLAR_VERSION = 1  ! This is just the data version.
  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10

  ! Physical constants
  ! ------------------
  ! Default Solar blackbody temperature in KELVIN
  REAL(Double), PARAMETER :: DEFAULT_BLACKBODY_TEMPERATURE = 5783.0_Double
  ! Default mean Solar radius in METRES
  REAL(Double), PARAMETER :: DEFAULT_RADIUS = 6.599e+08_Double
  ! Default mean Earth-Solar distance in METRES
  REAL(Double), PARAMETER :: DEFAULT_EARTH_SUN_DISTANCE = 1.495979e+11_Double



  ! --------------------------
  ! Solar data type definition
  ! --------------------------
  TYPE :: Solar_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER(Long) :: Release = SOLAR_RELEASE
    INTEGER(Long) :: Version = SOLAR_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Frequencies = 0
    ! Scalar components
    REAL(Double) :: Begin_Frequency       = ZERO
    REAL(Double) :: End_Frequency         = ZERO
    REAL(Double) :: Frequency_Interval    = ZERO
    REAL(Double) :: Blackbody_Temperature = DEFAULT_BLACKBODY_TEMPERATURE
    REAL(Double) :: Radius                = DEFAULT_RADIUS
    REAL(Double) :: Earth_Sun_Distance    = DEFAULT_EARTH_SUN_DISTANCE
    ! Array components
    REAL(Double), POINTER :: Frequency(:)            => NULL()
    REAL(Double), POINTER :: Irradiance(:)           => NULL()
    REAL(Double), POINTER :: Blackbody_Irradiance(:) => NULL()
  END TYPE Solar_type


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
!       Associated_Solar
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       Solar structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_Solar( Solar,              &  ! Input
!                                              ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Solar:               Solar structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       Solar_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Solar structure pointer members are associated.
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
!                            association status of the Solar pointer members.
!                            .TRUE.  - if ALL the Solar pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the Solar pointer
!                                      members are associated.
!                            .FALSE. - some or all of the Solar pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_Solar( Solar   , & ! Input
                             ANY_Test) & ! Optional input
                           RESULT( Association_Status )
    ! Arguments
    TYPE(Solar_type),  INTENT(IN) :: Solar
    INTEGER, OPTIONAL, INTENT(IN) :: ANY_Test
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

    ! Test the members that MUST be associated
    ! ----------------------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED(Solar%Frequency           ) .AND. &
           ASSOCIATED(Solar%Irradiance          ) .AND. &
           ASSOCIATED(Solar%Blackbody_Irradiance)       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED(Solar%Frequency           ) .OR. &
           ASSOCIATED(Solar%Irradiance          ) .OR. &
           ASSOCIATED(Solar%Blackbody_Irradiance)      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_Solar


!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_Solar
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a 
!       Solar data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_Solar( Solar                  , &  ! Output
!                                     RCS_Id     =RCS_Id,    , &  ! Revision control
!                                     Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       Solar:        Re-initialized Solar structure
!                     UNITS:      N/A
!                     TYPE:       Solar_type
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
!       Note the INTENT on the output Solar argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_Solar( Solar      , &  ! Output
                          No_Clear   , &  ! Optional input
                          RCS_Id     , &  ! Revision control
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(Solar_type)      , INTENT(IN OUT) :: Solar
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_Solar'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise the dimensions
    Solar%n_Frequencies = 0

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_Solar( Solar )

    ! If ALL components are NOT associated, do nothing
    IF ( .NOT. Associated_Solar( Solar ) ) RETURN


    ! Deallocate the regular arrays components
    ! ----------------------------------------
    DEALLOCATE( Solar%Frequency           , &
                Solar%Irradiance          , &
                Solar%Blackbody_Irradiance, &
                STAT=Allocate_Status      )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error deallocating Solar components. STAT = ",i0)') &
                     Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    Solar%n_Allocates = Solar%n_Allocates - 1
    IF ( Solar%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 0, Value = ",i0)' ) &
                      Solar%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF
  END FUNCTION Destroy_Solar


!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_Solar
! 
! PURPOSE:
!       Function to allocate the pointer members of the Solar data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_Solar( n_Frequencies          , &  ! Input 
!                                      Solar                  , &  ! Output
!                                      RCS_Id     =RCS_Id     , &  ! Revision control
!                                      Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Frquencies: Spectral dimension of the Solar structure pointer
!                     members. Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Solar:        Solar structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       Solar_type
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
!       Note the INTENT on the output Solar argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_Solar( n_Frequencies , &  ! Input
                           Solar         , &  ! Output
                           RCS_Id        , &  ! Revision control
                           Message_Log   ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Frequencies
    TYPE(Solar_type)      , INTENT(IN OUT) :: Solar
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_Solar'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimension input
    IF ( n_Frequencies < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_FREQUENCIES must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_Solar( Solar, ANY_Test = SET ) ) THEN
      Error_Status = Destroy_Solar( Solar, &
                                    No_Clear = SET, &
                                    Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating Solar pointer members.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF

    END IF


    ! Allocate the intrinsic type arrays
    ! ----------------------------------
    ALLOCATE(  Solar%Frequency( n_Frequencies ),            &
               Solar%Irradiance( n_Frequencies ),           &
               Solar%Blackbody_Irradiance( n_Frequencies ), &
               STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating Solar data arrays. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the dimensions and initialise arrays
    Solar%n_Frequencies = n_Frequencies

    Solar%Frequency            = ZERO
    Solar%Irradiance           = ZERO
    Solar%Blackbody_Irradiance = ZERO


    ! Increment and test allocation counter
    ! -------------------------------------
    Solar%n_Allocates = Solar%n_Allocates + 1
    IF ( Solar%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 1, Value = ",i0)' ) &
                      Solar%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Allocate_Solar


!------------------------------------------------------------------------------
!
! NAME:
!       Assign_Solar
!
! PURPOSE:
!       Function to copy valid Solar structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_Solar( Solar_in               , &  ! Input
!                                    Solar_out              , &  ! Output
!                                    RCS_Id     =RCS_Id     , &  ! Revision control
!                                    Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Solar_in:     Solar structure which is to be copied.
!                     UNITS:      N/A
!                     TYPE:       Solar_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Solar_out:    Copy of the input structure, Solar_in.
!                     UNITS:      N/A
!                     TYPE:       Solar_type
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
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Solar argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Assign_Solar( Solar_in   , &  ! Input
                         Solar_out  , &  ! Output
                         RCS_Id     , &  ! Revision control
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(Solar_type)      , INTENT(IN)     :: Solar_in
    TYPE(Solar_type)      , INTENT(IN OUT) :: Solar_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_Solar'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! ALL *input* pointers must be associated
    ! BUT the antenna correction AC component
    ! may not be.
    IF ( .NOT. Associated_Solar( Solar_In ) ) THEN
      Error_Status = Destroy_Solar( Solar_Out, &
                                    Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output Solar components.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
      END IF
      RETURN
    END IF

    
    ! Allocate the output structure
    ! -----------------------------
    Error_Status = Allocate_Solar( Solar_in%n_Frequencies, &
                                   Solar_out, &
                                   Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output Solar structure.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign components
    ! -----------------
    Solar_out%Release = Solar_in%Release
    Solar_out%Version = Solar_in%Version

    Solar_out%Begin_Frequency       = Solar_in%Begin_Frequency
    Solar_out%End_Frequency         = Solar_in%End_Frequency
    Solar_out%Frequency_Interval    = Solar_in%Frequency_Interval
    Solar_out%Blackbody_Temperature = Solar_in%Blackbody_Temperature
    Solar_out%Radius                = Solar_in%Radius
    Solar_out%Earth_Sun_Distance    = Solar_in%Earth_Sun_Distance

    Solar_out%Frequency            = Solar_in%Frequency
    Solar_out%Irradiance           = Solar_in%Irradiance
    Solar_out%Blackbody_Irradiance = Solar_in%Blackbody_Irradiance

  END FUNCTION Assign_Solar


!--------------------------------------------------------------------------------
!
! NAME:
!       Equal_Solar
!
! PURPOSE:
!       Function to test if two Solar structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_Solar( Solar_LHS              , &  ! Input
!                                   Solar_RHS              , &  ! Input
!                                   ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                   Check_All  =Check_All  , &  ! Optional input
!                                   RCS_Id     =RCS_Id     , &  ! Revision control
!                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Solar_LHS:     Solar structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( Solar_LHS == Solar_RHS ).
!                      UNITS:      N/A
!                      TYPE:       Solar_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       Solar_RHS:     Solar structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( Solar_LHS == Solar_RHS ).
!                      UNITS:      N/A
!                      TYPE:       Same as Solar_LHS
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:     Unit of data precision used to scale the floating
!                      point comparison. ULP stands for "Unit in the Last Place,"
!                      the smallest possible increment or decrement that can be
!                      made using a machine's floating point arithmetic.
!                      Value must be positive - if a negative value is supplied,
!                      the absolute value is used. If not specified, the default
!                      value is 1.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:     Set this argument to check ALL the floating point
!                      channel data of the Solar structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in Solar structures.
!                      If == 0, Return with FAILURE status as soon as
!                               ANY difference is found  *DEFAULT*
!                         == 1, Set FAILURE status if ANY difference is
!                               found, but continue to check ALL data.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structures were equal
!                         == FAILURE - an error occurred, or
!                                    - the structures were different.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Equal_Solar( Solar_LHS  , &  ! Input
                        Solar_RHS  , &  ! Input
                        ULP_Scale  , &  ! Optional input
                        Check_All  , &  ! Optional input
                        RCS_Id     , &  ! Revision control
                        Message_Log) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE(Solar_type)      , INTENT(IN)  :: Solar_LHS
    TYPE(Solar_type)      , INTENT(IN)  :: Solar_RHS
    INTEGER     , OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER     , OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_Solar'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: i

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

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
    IF ( .NOT. Associated_Solar( Solar_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Solar_LHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_Solar( Solar_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Solar_RHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check dimensions
    IF (Solar_LHS%n_Frequencies /= Solar_RHS%n_Frequencies ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check release and version
    ! -------------------------
    ! The release should *never* be different, but...
    IF ( Solar_LHS%Release /= Solar_RHS%Release ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Solar RELEASE values are different: ",i0," vs ",i0)') &
                    Solar_LHS%Release, Solar_RHS%Release
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF
    ! The version *could* be different
    IF ( Solar_LHS%Version /= Solar_RHS%Version ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Solar VERSION values are different: ",i0," vs ",i0)') &
                    Solar_LHS%Version, Solar_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    

    ! Check the scalar components
    ! ---------------------------
    IF ( .NOT. Compare_Float( Solar_LHS%Begin_Frequency, &
                              Solar_RHS%Begin_Frequency, &
                              ULP=ULP ) ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Solar Begin_Frequency values are different: ",es13.6,&
                     &" vs ",es13.6," (",es13.6,")")') &
                    Solar_LHS%Begin_Frequency,Solar_RHS%Begin_Frequency, &
                    Solar_LHS%Begin_Frequency-Solar_RHS%Begin_Frequency
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    IF ( .NOT. Compare_Float( Solar_LHS%End_Frequency, &
                              Solar_RHS%End_Frequency, &
                              ULP=ULP ) ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Solar End_Frequency values are different: ",es13.6,&
                     &" vs ",es13.6," (",es13.6,")")') &
                    Solar_LHS%End_Frequency,Solar_RHS%End_Frequency, &
                    Solar_LHS%End_Frequency-Solar_RHS%End_Frequency
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    IF ( .NOT. Compare_Float( Solar_LHS%Frequency_Interval, &
                              Solar_RHS%Frequency_Interval, &
                              ULP=ULP ) ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Solar Frequency_Interval values are different: ",es13.6,&
                     &" vs ",es13.6," (",es13.6,")")') &
                    Solar_LHS%Frequency_Interval,Solar_RHS%Frequency_Interval, &
                    Solar_LHS%Frequency_Interval-Solar_RHS%Frequency_Interval
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    IF ( .NOT. Compare_Float( Solar_LHS%Blackbody_Temperature, &
                              Solar_RHS%Blackbody_Temperature, &
                              ULP=ULP ) ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Solar Blackbody_Temperature values are different: ",es13.6,&
                     &" vs ",es13.6," (",es13.6,")")') &
                    Solar_LHS%Blackbody_Temperature,Solar_RHS%Blackbody_Temperature, &
                    Solar_LHS%Blackbody_Temperature-Solar_RHS%Blackbody_Temperature
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    IF ( .NOT. Compare_Float( Solar_LHS%Radius, &
                              Solar_RHS%Radius, &
                              ULP=ULP ) ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Solar Radius values are different: ",es13.6,&
                     &" vs ",es13.6," (",es13.6,")")') &
                    Solar_LHS%Radius,Solar_RHS%Radius, &
                    Solar_LHS%Radius-Solar_RHS%Radius
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    IF ( .NOT. Compare_Float( Solar_LHS%Earth_Sun_Distance, &
                              Solar_RHS%Earth_Sun_Distance, &
                              ULP=ULP ) ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Solar Earth_Sun_Distance values are different: ",es13.6,&
                     &" vs ",es13.6," (",es13.6,")")') &
                    Solar_LHS%Earth_Sun_Distance,Solar_RHS%Earth_Sun_Distance, &
                    Solar_LHS%Earth_Sun_Distance-Solar_RHS%Earth_Sun_Distance
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    
    ! Check the array components
    ! --------------------------
    DO i = 1, Solar_LHS%n_Frequencies
      IF ( .NOT. Compare_Float( Solar_LHS%Frequency(i), &
                                Solar_RHS%Frequency(i), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("Solar Frequency values are different at index (",1(1x,i0),"): ",&
                       &es13.6," vs ",es13.6," (",es13.6,")")') &
                       i, &
                       Solar_LHS%Frequency(i),Solar_RHS%Frequency(i), &
                       Solar_LHS%Frequency(i)-Solar_RHS%Frequency(i)
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    DO i = 1, Solar_LHS%n_Frequencies
      IF ( .NOT. Compare_Float( Solar_LHS%Irradiance(i), &
                                Solar_RHS%Irradiance(i), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("Solar Irradiance values are different at index (",1(1x,i0),"): ",&
                       &es13.6," vs ",es13.6," (",es13.6,")")') &
                       i, &
                       Solar_LHS%Irradiance(i),Solar_RHS%Irradiance(i), &
                       Solar_LHS%Irradiance(i)-Solar_RHS%Irradiance(i)
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    DO i = 1, Solar_LHS%n_Frequencies
      IF ( .NOT. Compare_Float( Solar_LHS%Blackbody_Irradiance(i), &
                                Solar_RHS%Blackbody_Irradiance(i), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("Solar Blackbody_Irradiance values are different at index (",1(1x,i0),"): ",&
                       &es13.6," vs ",es13.6," (",es13.6,")")') &
                       i, &
                       Solar_LHS%Blackbody_Irradiance(i),Solar_RHS%Blackbody_Irradiance(i), &
                       Solar_LHS%Blackbody_Irradiance(i)-Solar_RHS%Blackbody_Irradiance(i)
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

  END FUNCTION Equal_Solar


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckRelease_Solar
!
! PURPOSE:
!       Function to check the Solar Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_Solar( Solar                  , &  ! Input
!                                          RCS_Id     =RCS_Id     , &  ! Revision control
!                                          Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Solar:         Solar structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       Solar_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure Release value is valid.
!                         == FAILURE the structure Release value is NOT valid
!                                    and either a data file file or software
!                                    update is required.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION CheckRelease_Solar( Solar      , &  ! Input
                               RCS_Id     , &  ! Revision control
                               Message_Log) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    TYPE(Solar_type)      , INTENT(IN)  :: Solar
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_Solar'
    ! Local variables
    CHARACTER(ML) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check the release
    ! -----------------
    ! Check that release is not too old
    IF ( Solar%Release < SOLAR_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A Solar data update is needed. ", &
                        &"Solar release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      Solar%Release, SOLAR_RELEASE
      CALL Display_Message( TRIM(Routine_Name), &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that release is not too new
    IF ( Solar%Release > SOLAR_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A Solar software update is needed. ", &
                        &"Solar release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      Solar%Release, SOLAR_RELEASE
      CALL Display_Message( TRIM(Routine_Name), &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_Solar


!--------------------------------------------------------------------------------
!
! NAME:
!       Info_Solar
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the Solar data structure.
!
! CALLING SEQUENCE:
!       CALL Info_Solar( Solar        , &  ! Input
!                        Info         , &  ! Output
!                        RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       Solar:         Filled Solar structure.
!                      UNITS:      N/A
!                      TYPE:       Solar_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed Solar data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!--------------------------------------------------------------------------------

  SUBROUTINE Info_Solar( Solar , &  ! Input
                         Info  , &  ! Output
                         RCS_Id  )  ! Revision control
    ! Arguments
    TYPE(Solar_type)      , INTENT(IN)  :: Solar
    CHARACTER(*)          , INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Set up
    ! ------
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required data to the local string
    ! -------------------------------------------
    WRITE( LongString,'(a,1x,"Solar RELEASE.VERSION: ",i2,".",i2.2,2x,&
                      &"N_FREQUENCIES=",i0)' ) &
                      ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                      Solar%Release, Solar%Version, &
                      Solar%n_Frequencies
    
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_Solar


!------------------------------------------------------------------------------
!
! NAME:
!       Frequency_Solar
!
! PURPOSE:
!       Function to compute the frequency grid for a supplied Solar data
!       structure.
!
! CALLING SEQUENCE:
!       Error_Status = Frequency_Solar( Solar                  , &  ! In/Output
!                                       RCS_Id     =RCS_Id     , &  ! Revision control
!                                       Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Solar:        Solar structure with fields containing the begin and
!                     end frequencies of the frequency grid to compute.
!                     UNITS:      N/A
!                     TYPE:       Solar_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Solar:        Solar structure with the frequency component filled.
!                     UNITS:      N/A
!                     TYPE:       Solar_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
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
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the frequency grid calculation was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The FREQUENCY field of the input Solar structure is modified.
!
! RESTRICTIONS:
!       Solar structure must contain at least 2 points of frequency and response
!       data.
!
!------------------------------------------------------------------------------

  FUNCTION Frequency_Solar( Solar      , &  ! Input
                            RCS_Id     , &  ! Revision control
                            Message_Log) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    TYPE(Solar_type)      , INTENT(IN OUT) :: Solar
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Frequency_Solar'
    ! Local variables
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL pointers must be associated
    IF ( .NOT. Associated_Solar( Solar ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Solar pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of points
    n = Solar%n_Frequencies
    IF ( n < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocated Solar structure arrays must contain at least 2 points.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Compute the frequency grid
    ! --------------------------
    ! Construct a normalised grid of 0->1
    Solar%Frequency(1:n) = (/(REAL(i-1,Double), i=1,n)/) / REAL(n-1,Double)

    ! Scale it to the actual values
    Solar%Frequency(1:n) = Solar%Begin_Frequency + &
                           (Solar%Frequency(1:n) * ( Solar%End_Frequency-Solar%Begin_Frequency ))

  END FUNCTION Frequency_Solar


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
!       Clear_Solar
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Solar structure.
!
! CALLING SEQUENCE:
!       CALL Clear_Solar( Solar ) ! Output
!
! OUTPUT ARGUMENTS:
!       Solar:       Solar structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       Solar_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output Solar argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_Solar( Solar )
    TYPE(Solar_type), INTENT(IN OUT) :: Solar
    Solar%Begin_Frequency       = ZERO
    Solar%End_Frequency         = ZERO
    Solar%Frequency_Interval    = ZERO
    Solar%Blackbody_Temperature = DEFAULT_BLACKBODY_TEMPERATURE
    Solar%Radius                = DEFAULT_RADIUS
    Solar%Earth_Sun_Distance    = DEFAULT_EARTH_SUN_DISTANCE
  END SUBROUTINE Clear_Solar

END MODULE Solar_Define
