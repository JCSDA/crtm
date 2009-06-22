!
! ODAS_Define
!
! Module defining the ODAS (Optical Depth, Absorber Space) data structure and
! containing routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE ODAS_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE Sort_Utility,          ONLY: InsertionSort
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE

  ! Public types
  ! ------------
  PUBLIC :: ODAS_type

  ! Public procedures
  ! -----------------
  PUBLIC :: Associated_ODAS
  PUBLIC :: Destroy_ODAS
  PUBLIC :: Allocate_ODAS
  PUBLIC :: Assign_ODAS
  PUBLIC :: Concatenate_Channel_ODAS
  PUBLIC :: Concatenate_Absorber_ODAS
  PUBLIC :: Equal_ODAS
  PUBLIC :: CheckRelease_ODAS
  PUBLIC :: CheckAlgorithm_ODAS
  PUBLIC :: Info_ODAS
  
  ! Public parameters
  ! -----------------
  ! Sensor Id defaults
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  ! Allowable sensor type values and names
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME
  ! The Global unique algorithm ID
  PUBLIC :: ODAS_ALGORITHM


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! ODAS invalid values
  INTEGER,      PARAMETER :: IP_INVALID = -1
  REAL(Double), PARAMETER :: FP_INVALID = -1.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! String lengths
  INTEGER, PARAMETER :: SL = 20   ! Sensor Id
  INTEGER, PARAMETER :: ML = 256  ! Messages
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ODAS_RELEASE = 6  ! This determines structure and file formats.
  INTEGER, PARAMETER :: ODAS_VERSION = 4  ! This is just the data version.
  ! The optical depth algorithm Id
  INTEGER     , PARAMETER :: ODAS_ALGORITHM = 1
  CHARACTER(*), PARAMETER :: ODAS_ALGORITHM_NAME = 'ODAS'
  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10
  ! Invalid sensor ids
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! The instrument types
  INTEGER, PARAMETER :: N_SENSOR_TYPES     = 4
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  INTEGER, PARAMETER :: MICROWAVE_SENSOR   = 1
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2
  INTEGER, PARAMETER :: VISIBLE_SENSOR     = 3
  INTEGER, PARAMETER :: ULTRAVIOLET_SENSOR = 4
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_SENSOR_TYPES ) :: &
    SENSOR_TYPE_NAME = (/ 'Invalid    ', &
                          'Microwave  ', &
                          'Infrared   ', &
                          'Visible    ', &
                          'Ultraviolet' /)

  ! -------------------------
  ! ODAS data type definition
  ! -------------------------
  TYPE :: ODAS_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER(Long) :: Release = ODAS_RELEASE
    INTEGER(Long) :: Version = ODAS_VERSION
    ! Algorithm identifer
    INTEGER(Long) :: Algorithm = ODAS_ALGORITHM
    ! Dimensions
    INTEGER(Long) :: n_Orders     = 0    ! Iorder
    INTEGER(Long) :: n_Predictors = 0    ! Iuse
    INTEGER(Long) :: n_Absorbers  = 0    ! J
    INTEGER(Long) :: n_Channels   = 0    ! L
    ! Scalar components
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER(Long) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER(Long) :: Sensor_Type      = INVALID_SENSOR
    ! The actual sensor channel numbers
    INTEGER(Long), POINTER :: Sensor_Channel(:)      => NULL() ! L
    ! The absorber ID and absorber space values
    INTEGER(Long), POINTER :: Absorber_ID(:)         => NULL() ! J
    REAL(Double),  POINTER :: Alpha(:)               => NULL() ! J
    REAL(Double),  POINTER :: Alpha_C1(:)            => NULL() ! J
    REAL(Double),  POINTER :: Alpha_C2(:)            => NULL() ! J
    ! The polynomial order index array.
    ! This array identifies the order of the polynomial used to
    ! reconstruct the regression coefficients. For each predictor
    ! (Iuse), each absorber (J) and each channel (L) a different
    ! order of polynomial can be specified.
    INTEGER(Long), POINTER :: Order_Index(:,:,:)     => NULL() ! 0:Iuse x J x L
    ! The predictor index array.
    ! This array identifies which subset (Iuse) of the total number
    ! number of predictors are used to compute the absorption coefficient
    ! for absorber (J) and each channel (L). If Predictor_Index(0,:,:) is
    ! less than 0, this is an indication that there is NO absorption for
    ! the selected absorber in the current channel.
    INTEGER(Long), POINTER :: Predictor_Index(:,:,:) => NULL() ! 0:Iuse x J x L
    ! The array of coefficients
    REAL(Double),  POINTER :: C(:,:,:,:)             => NULL() ! 0:Iorder x 0:Iuse x J x L
  END TYPE ODAS_type


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
!       Associated_ODAS
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       ODAS structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_ODAS( ODAS             ,&  ! Input
!                                             ANY_Test=Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       ODAS:        ODAS structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       ODAS_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    ODAS structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the ODAS pointer members.
!                            .TRUE.  - if ALL the ODAS pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the ODAS pointer
!                                      members are associated.
!                            .FALSE. - some or all of the ODAS pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_ODAS( ODAS    , & ! Input
                            ANY_Test) & ! Optional input
                          RESULT( Association_Status )
    ! Arguments
    TYPE(ODAS_type)  , INTENT(IN) :: ODAS
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
      IF ( ASSOCIATED( ODAS%Sensor_Channel    ) .AND. &
           ASSOCIATED( ODAS%Absorber_ID       ) .AND. &
           ASSOCIATED( ODAS%Alpha             ) .AND. &
           ASSOCIATED( ODAS%Alpha_C1          ) .AND. &
           ASSOCIATED( ODAS%Alpha_C2          ) .AND. &
           ASSOCIATED( ODAS%Order_Index       ) .AND. &
           ASSOCIATED( ODAS%Predictor_Index   ) .AND. &
           ASSOCIATED( ODAS%C                 )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( ODAS%Sensor_Channel    ) .OR. &
           ASSOCIATED( ODAS%Absorber_ID       ) .OR. &
           ASSOCIATED( ODAS%Alpha             ) .OR. &
           ASSOCIATED( ODAS%Alpha_C1          ) .OR. &
           ASSOCIATED( ODAS%Alpha_C2          ) .OR. &
           ASSOCIATED( ODAS%Order_Index       ) .OR. &
           ASSOCIATED( ODAS%Predictor_Index   ) .OR. &
           ASSOCIATED( ODAS%C                 )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_ODAS


!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_ODAS
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of ODAS
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_ODAS( ODAS                   , &  ! Output
!                                    RCS_Id     =RCS_Id     , &  ! Revision control
!                                    Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       ODAS:         Re-initialized ODAS structure.
!                     UNITS:      N/A
!                     TYPE:       ODAS_type
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
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
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
!       Note the INTENT on the output ODAS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_ODAS( ODAS       , &  ! Output
                         No_Clear   , &  ! Optional input
                         RCS_Id     , &  ! Revision control
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(ODAS_type)       , INTENT(IN OUT) :: ODAS
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_ODAS'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise the dimensions
    ODAS%n_Orders     = 0
    ODAS%n_Predictors = 0
    ODAS%n_Absorbers  = 0
    ODAS%n_Channels   = 0

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_ODAS( ODAS )

    ! If ALL components are NOT associated, do nothing
    IF ( .NOT. Associated_ODAS( ODAS ) ) RETURN


    ! Deallocate the regular arrays components
    ! ----------------------------------------
    DEALLOCATE( ODAS%Sensor_Channel  , &
                ODAS%Absorber_ID     , &
                ODAS%Alpha           , &
                ODAS%Alpha_C1        , &
                ODAS%Alpha_C2        , &
                ODAS%Order_Index     , &
                ODAS%Predictor_Index , &
                ODAS%C               , &
                STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error deallocating ODAS components. STAT = ",i0)' ) &
                     Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    ODAS%n_Allocates = ODAS%n_Allocates - 1
    IF ( ODAS%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 0, Value = ",i0)' ) &
                      ODAS%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF
  END FUNCTION Destroy_ODAS


!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_ODAS
! 
! PURPOSE:
!       Function to allocate the pointer members of the ODAS
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_ODAS( n_Orders               , &  ! Input
!                                     n_Predictors           , &  ! Input
!                                     n_Absorbers            , &  ! Input
!                                     n_Channels             , &  ! Input
!                                     ODAS                   , &  ! Output
!                                     RCS_Id     =RCS_Id     , &  ! Revision control
!                                     Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Orders:     Maximum polynomial order used to reconstruct
!                     the prediction coefficients.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Predictors: Maximum number of predictors dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:  Number of absorbers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Channels:   Number of channels dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODAS:         ODAS structure with allocated
!                     pointer members
!                     UNITS:      N/A
!                     TYPE:       ODAS_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in
!                     which any messages will be logged. If not
!                     specified, or if an error occurs opening
!                     the log file, the default action is to
!                     output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
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
!       Note the INTENT on the output ODAS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_ODAS( n_Orders    , &  ! Input
                          n_Predictors, &  ! Input
                          n_Absorbers , &  ! Input
                          n_Channels  , &  ! Input
                          ODAS        , &  ! Output
                          RCS_Id      , &  ! Revision control
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Orders
    INTEGER               , INTENT(IN)     :: n_Predictors
    INTEGER               , INTENT(IN)     :: n_Absorbers
    INTEGER               , INTENT(IN)     :: n_Channels
    TYPE(ODAS_type)       , INTENT(IN OUT) :: ODAS
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_ODAS'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimension input
    IF ( n_Orders     < 1 .OR. &
         n_Predictors < 1 .OR. &
         n_Absorbers  < 1 .OR. &
         n_Channels   < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODAS dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_ODAS( ODAS, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_ODAS( ODAS, &
                                   No_Clear=SET, &
                                   Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating ODAS prior to reallocation.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Allocate the data arrays
    ! ------------------------
    ALLOCATE( ODAS%Sensor_Channel( n_Channels ), &
              ODAS%Absorber_ID( n_Absorbers ), &
              ODAS%Alpha( n_Absorbers ), &
              ODAS%Alpha_c1( n_Absorbers ), &
              ODAS%Alpha_c2( n_Absorbers ), &
              ODAS%Order_Index( 0:n_Predictors, n_Absorbers, n_Channels ), &
              ODAS%Predictor_Index( 0:n_Predictors, n_Absorbers, n_Channels ), &
              ODAS%C( 0:n_Orders, 0:n_Predictors, n_Absorbers, n_Channels ), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating ODAS data arrays. STAT = ",i0)' ) &
                     Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the dimensions and initialise arrays
    ODAS%n_Orders     = n_Orders
    ODAS%n_Predictors = n_Predictors
    ODAS%n_Absorbers  = n_Absorbers
    ODAS%n_Channels   = n_Channels

    ODAS%Sensor_Channel    = 0
    ODAS%Absorber_ID       = IP_INVALID
    ODAS%Alpha             = FP_INVALID
    ODAS%Alpha_c1          = FP_INVALID
    ODAS%Alpha_c2          = FP_INVALID
    ODAS%Order_Index       = IP_INVALID
    ODAS%Predictor_Index   = IP_INVALID
    ODAS%C                 = FP_INVALID


    ! Increment and test allocation counter
    ! -------------------------------------
    ODAS%n_Allocates = ODAS%n_Allocates + 1
    IF ( ODAS%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 1, Value = ",i0)' ) &
                     ODAS%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_ODAS


!------------------------------------------------------------------------------
!
! NAME:
!       Assign_ODAS
!
! PURPOSE:
!       Function to copy valid ODAS structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_ODAS( ODAS_in                , &  ! Input
!                                   ODAS_out               , &  ! Output
!                                   RCS_Id     =RCS_Id     , &  ! Revision control
!                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODAS_in:       ODAS structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODAS_out:      Copy of the input structure, ODAS_in.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure assignment was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output ODAS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Assign_ODAS( ODAS_in     , &  ! Input
                        ODAS_out    , &  ! Output
                        RCS_Id      , &  ! Revision control
                        Message_Log ) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE(ODAS_type)       , INTENT(IN)     :: ODAS_in
    TYPE(ODAS_type)       , INTENT(IN OUT) :: ODAS_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_ODAS'

    ! Set up
    ! ------
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_ODAS( ODAS_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODAS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_ODAS( ODAS_in%n_Orders    , &
                                  ODAS_in%n_Predictors, &
                                  ODAS_in%n_Absorbers , &
                                  ODAS_in%n_Channels  , &
                                  ODAS_out, &
                                  Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output ODAS arrays.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign intrinsic data types
    ! ---------------------------
    ODAS_out%Release   = ODAS_in%Release
    ODAS_out%Version   = ODAS_in%Version

    ODAS_out%Sensor_Id         = ODAS_in%Sensor_Id
    ODAS_out%Sensor_Type       = ODAS_in%Sensor_Type
    ODAS_out%WMO_Satellite_ID  = ODAS_in%WMO_Satellite_ID
    ODAS_out%WMO_Sensor_ID     = ODAS_in%WMO_Sensor_ID
    ODAS_out%Sensor_Channel    = ODAS_in%Sensor_Channel
    ODAS_out%Absorber_ID       = ODAS_in%Absorber_ID
    ODAS_out%Alpha             = ODAS_in%Alpha
    ODAS_out%Alpha_C1          = ODAS_in%Alpha_C1
    ODAS_out%Alpha_C2          = ODAS_in%Alpha_C2
    ODAS_out%Order_Index       = ODAS_in%Order_Index
    ODAS_out%Predictor_Index   = ODAS_in%Predictor_Index
    ODAS_out%C                 = ODAS_in%C

  END FUNCTION Assign_ODAS


!------------------------------------------------------------------------------
!
! NAME:
!       Concatenate_Channel_ODAS
!
! PURPOSE:
!       Function to concatenate two valid ODAS structures along
!       the channel dimension.
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_Channel_ODAS( ODAS1                  , &  ! Input/Output
!                                                ODAS2                  , &  ! Input
!                                                RCS_Id     = RCS_Id    , &  ! Revision control
!                                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODAS1:         First ODAS structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       ODAS2:         Second ODAS structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODAS1:         The concatenated ODAS structure. The order of
!                      concatenation is ODAS1,ODAS2 along the 
!                      channel dimension.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure concatenation was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The input ODAS1 argument contains the concatenated structure
!       data (in character-speak: ODAS1//ODAS2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input ODAS1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
!------------------------------------------------------------------------------

  FUNCTION Concatenate_Channel_ODAS( ODAS1      , &  ! Input/Output
                                     ODAS2      , &  ! Input
                                     RCS_Id     , &  ! Revision control
                                     Message_Log) &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    TYPE(ODAS_type)       , INTENT(IN OUT)  :: ODAS1
    TYPE(ODAS_type)       , INTENT(IN)      :: ODAS2
    CHARACTER(*), OPTIONAL, INTENT(OUT)     :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)      :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Concatenate_Channel_ODAS'
    ! Local variables
    INTEGER :: Destroy_Status
    INTEGER :: n_Channels, l1, l2
    TYPE(ODAS_type) :: ODAS_Tmp

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check structures
    IF ( .NOT. Associated_ODAS( ODAS1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODAS1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_ODAS( ODAS2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODAS2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Compare structure release/version
    IF ( ODAS1%Release /= ODAS2%Release ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODAS Release values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( ODAS1%Version /= ODAS2%Version ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODAS Version values are different.', &
                            WARNING, &
                            Message_Log=Message_Log )

    END IF

    ! Check non-channel dimensions
    IF ( ODAS1%n_Orders     /= ODAS2%n_Orders     .OR. &
         ODAS1%n_Predictors /= ODAS2%n_Predictors .OR. &
         ODAS1%n_Absorbers  /= ODAS2%n_Absorbers       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-channel ODAS dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the sensor ID values
    IF ( ODAS1%Sensor_ID        /= ODAS2%Sensor_ID        .OR. &
         ODAS1%WMO_Satellite_ID /= ODAS2%WMO_Satellite_ID .OR. &
         ODAS1%WMO_Sensor_ID    /= ODAS2%WMO_Sensor_ID         ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODAS sensor ID values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the sensor type value
    IF ( ODAS1%Sensor_Type /= ODAS2%Sensor_Type  ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODAS sensor type values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
 
    ! Reallocate the first structure
    ! ------------------------------
    ! Copy it...
    Error_Status = Assign_ODAS( ODAS1, ODAS_Tmp, &
                                Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying ODAS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! ... now destroy it ...
    Error_Status = Destroy_ODAS( ODAS1, &
                                 Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ODAS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! ... and now re-allocate it for all channels
    n_Channels   = ODAS_Tmp%n_Channels + ODAS2%n_Channels
    Error_Status = Allocate_ODAS( ODAS_Tmp%n_Orders, &
                                  ODAS_Tmp%n_Predictors, &
                                  ODAS_Tmp%n_Absorbers, &
                                  n_Channels, &
                                  ODAS1, &
                                  Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating ODAS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the non-channel array data
    ! ---------------------------------
    ODAS1%Version           = MAX(ODAS_Tmp%Version, ODAS2%Version)
    ODAS1%Sensor_ID         = ODAS_Tmp%Sensor_ID
    ODAS1%WMO_Satellite_ID  = ODAS_Tmp%WMO_Satellite_ID
    ODAS1%WMO_Sensor_ID     = ODAS_Tmp%WMO_Sensor_ID
    ODAS1%Sensor_Type       = ODAS_Tmp%Sensor_Type
    ODAS1%Absorber_ID       = ODAS_Tmp%Absorber_ID
    ODAS1%Alpha             = ODAS_Tmp%Alpha
    ODAS1%Alpha_C1          = ODAS_Tmp%Alpha_C1
    ODAS1%Alpha_C2          = ODAS_Tmp%Alpha_C2


    ! Concatenate the channel array data
    ! ----------------------------------
    ! The first part...
    l1 = 1
    l2 = ODAS_Tmp%n_Channels
    ODAS1%Sensor_Channel(l1:l2)      = ODAS_Tmp%Sensor_Channel
    ODAS1%Order_Index(:,:,l1:l2)     = ODAS_Tmp%Order_Index
    ODAS1%Predictor_Index(:,:,l1:l2) = ODAS_Tmp%Predictor_Index
    ODAS1%C(:,:,:,l1:l2)             = ODAS_Tmp%C
    ! ...and the second part
    l1 = l2 + 1
    l2 = n_Channels
    ODAS1%Sensor_Channel(l1:l2)      = ODAS2%Sensor_Channel
    ODAS1%Order_Index(:,:,l1:l2)     = ODAS2%Order_Index
    ODAS1%Predictor_Index(:,:,l1:l2) = ODAS2%Predictor_Index
    ODAS1%C(:,:,:,l1:l2)             = ODAS2%C


    ! Destroy the temporary structure
    ! -------------------------------
    Destroy_Status = Destroy_ODAS( ODAS_Tmp, &
                                   Message_Log=Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ODAS_Tmp structure.', &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Concatenate_Channel_ODAS


!------------------------------------------------------------------------------
!
! NAME:
!       Concatenate_Absorber_ODAS
!
! PURPOSE:
!       Function to concatenate two valid ODAS structures along
!       the absorber dimension.
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_Absorber_ODAS( ODAS1                  , &  ! Input/Output
!                                                 ODAS2                  , &  ! Input
!                                                 RCS_Id     = RCS_Id    , &  ! Revision control
!                                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODAS1:         First ODAS structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       ODAS2:         Second ODAS structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODAS1:         The concatenated ODAS structure. The order of
!                      concatenation is ODAS1,ODAS2 along the 
!                      absorber dimension.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure concatenation was successful
!                         == FAILURE an error occurred.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The input ODAS1 argument contains the concatenated structure
!       data (in character-speak: ODAS1//ODAS2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input ODAS1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
!------------------------------------------------------------------------------

  FUNCTION Concatenate_Absorber_ODAS( ODAS1      , &  ! Input/Output
                                      ODAS2      , &  ! Input
                                      RCS_Id     , &  ! Revision control
                                      Message_Log) &  ! Error messaging
                                    RESULT( Error_Status )
    ! Arguments
    TYPE(ODAS_type)       , INTENT(IN OUT) :: ODAS1
    TYPE(ODAS_type)       , INTENT(IN)     :: ODAS2
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Concatenate_Absorber_ODAS'
    ! Local variables
    INTEGER :: Destroy_Status
    INTEGER :: n_Absorbers, j1, j2
    TYPE(ODAS_type) :: ODAS_Tmp

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check structures
    IF ( .NOT. Associated_ODAS( ODAS1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODAS1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_ODAS( ODAS2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODAS2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Compare structure release/version
    IF ( ODAS1%Release /= ODAS2%Release ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODAS Release values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( ODAS1%Version /= ODAS2%Version ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODAS Version values are different.', &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

    ! Check the non-absorber dimensions
    IF ( ODAS1%n_Orders     /= ODAS2%n_Orders     .OR. &
         ODAS1%n_Predictors /= ODAS2%n_Predictors .OR. &
         ODAS1%n_Channels   /= ODAS2%n_Channels        ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-absorber ODAS dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the sensor ID values
    IF ( ODAS1%Sensor_ID        /= ODAS2%Sensor_ID        .OR. &
         ODAS1%WMO_Satellite_ID /= ODAS2%WMO_Satellite_ID .OR. &
         ODAS1%WMO_Sensor_ID    /= ODAS2%WMO_Sensor_ID         ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODAS sensor ID values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the sensor type value
    IF ( ODAS1%Sensor_Type /= ODAS2%Sensor_Type  ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODAS sensor type values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the channels
    IF ( ANY( ( ODAS1%Sensor_Channel - ODAS2%Sensor_Channel ) /= 0 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ODAS channel values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Reallocate the first structure
    ! ------------------------------
    ! Copy it...
    Error_Status = Assign_ODAS( ODAS1, ODAS_Tmp, &
                                Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying ODAS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
   
    ! ... now destroy it ...
    Error_Status = Destroy_ODAS( ODAS1, &
                                 Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ODAS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! ... and now re-allocate it for all absorbers
    n_Absorbers = ODAS_Tmp%n_Absorbers + ODAS2%n_Absorbers
    Error_Status = Allocate_ODAS( ODAS_Tmp%n_Orders, &
                                  ODAS_Tmp%n_Predictors, &
                                  n_Absorbers, &
                                  ODAS_Tmp%n_Channels, &
                                  ODAS1, &
                                  Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating ODAS1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the non-absorber array data
    ! ----------------------------------
    ODAS1%Version           = MAX( ODAS_Tmp%Version, ODAS2%Version )
    ODAS1%Sensor_ID         = ODAS_Tmp%Sensor_ID
    ODAS1%WMO_Satellite_ID  = ODAS_Tmp%WMO_Satellite_ID
    ODAS1%WMO_Sensor_ID     = ODAS_Tmp%WMO_Sensor_ID
    ODAS1%Sensor_Type       = ODAS_Tmp%Sensor_Type
    ODAS1%Sensor_Channel    = ODAS_Tmp%Sensor_Channel


    ! Concatenate absorber array data
    ! -------------------------------
    ! The first part...
    j1 = 1
    j2 = ODAS_Tmp%n_Absorbers
    ODAS1%Absorber_ID(j1:j2)         = ODAS_Tmp%Absorber_ID
    ODAS1%Alpha(j1:j2)               = ODAS_Tmp%Alpha
    ODAS1%Alpha_C1(j1:j2)            = ODAS_Tmp%Alpha_C1
    ODAS1%Alpha_C2(j1:j2)            = ODAS_Tmp%Alpha_C2
    ODAS1%Order_Index(:,j1:j2,:)     = ODAS_Tmp%Order_Index
    ODAS1%Predictor_Index(:,j1:j2,:) = ODAS_Tmp%Predictor_Index
    ODAS1%C(:,:,j1:j2,:)             = ODAS_Tmp%C

    ! ...the second part
    j1 = j2 + 1
    j2 = n_Absorbers
    ODAS1%Absorber_ID(j1:j2)         = ODAS2%Absorber_ID
    ODAS1%Alpha(j1:j2)               = ODAS2%Alpha
    ODAS1%Alpha_C1(j1:j2)            = ODAS2%Alpha_C1
    ODAS1%Alpha_C2(j1:j2)            = ODAS2%Alpha_C2
    ODAS1%Order_Index(:,j1:j2,:)     = ODAS2%Order_Index
    ODAS1%Predictor_Index(:,j1:j2,:) = ODAS2%Predictor_Index
    ODAS1%C(:,:,j1:j2,:)             = ODAS2%C


    ! Destroy the temporary structure
    ! -------------------------------
    Destroy_Status = Destroy_ODAS( ODAS_Tmp, &
                                 Message_Log=Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ODAS_Tmp structure.', &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Concatenate_Absorber_ODAS


!------------------------------------------------------------------------------
!
! NAME:
!       Equal_ODAS
!
! PURPOSE:
!       Function to test if two ODAS structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_ODAS( ODAS_LHS               , &  ! Input
!                                  ODAS_RHS               , &  ! Input
!                                  ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                  Check_All  =Check_All  , &  ! Optional input
!                                  RCS_Id     =RCS_Id     , &  ! Revision control
!                                  Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODAS_LHS:      ODAS structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( ODAS_LHS == ODAS_RHS ).
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       ODAS_RHS:      ODAS structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( ODAS_LHS == ODAS_RHS ).
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
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
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Check_All:     Set this argument to check ALL the *floating point*
!                      channel data of the ODAS structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in ODAS structures.
!                      If == 0, Return with FAILURE status as soon as
!                               ANY difference is found  *DEFAULT*
!                         == 1, Set FAILURE status if ANY difference is
!                               found, but continue to check ALL data.
!                      Note: Setting this argument has no effect if, for
!                            example, the structure dimensions are different,
!                            or the sensor ids/channels are different, or the
!                            absorber ids are different, etc. 
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
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
! COMMENTS:
!       Congruency of the structure data is a prerequisite of equality.
!       That is, the *order* of the data is important. For example, if
!       two structures contain the same absorber information, but in a
!       different order, the structures are not considered equal. 
! 
!------------------------------------------------------------------------------

  FUNCTION Equal_ODAS( ODAS_LHS   , &  ! Input
                       ODAS_RHS   , &  ! Input
                       ULP_Scale  , &  ! Optional input
                       Check_All  , &  ! Optional input
                       RCS_Id     , &  ! Revision control
                       Message_Log) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    TYPE(ODAS_type)       , INTENT(IN)  :: ODAS_LHS
    TYPE(ODAS_type)       , INTENT(IN)  :: ODAS_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,      OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_ODAS'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: io, ip, j, l

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
      IF ( Check_All == 1 ) Check_Once = .FALSE.
    END IF

    ! Check the structure association status
    IF ( .NOT. Associated_ODAS( ODAS_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ODAS_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_ODAS( ODAS_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ODAS_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check structure Release/Version
    ! -------------------------------
    IF ( ( ODAS_LHS%Release /= ODAS_RHS%Release ) .OR. &
         ( ODAS_LHS%Version /= ODAS_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      ODAS_LHS%Release, ODAS_LHS%Version, &
                      ODAS_RHS%Release, ODAS_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( ODAS_LHS%n_Orders     /= ODAS_RHS%n_Orders     .OR. &
         ODAS_LHS%n_Predictors /= ODAS_RHS%n_Predictors .OR. &
         ODAS_LHS%n_Absorbers  /= ODAS_RHS%n_Absorbers  .OR. &
         ODAS_LHS%n_Channels   /= ODAS_RHS%n_Channels        ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Compare the values
    ! ------------------
    ! The Sensor_ID
    IF ( ODAS_LHS%Sensor_Id /= ODAS_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Sensor_ID values are different, ", &
                        &a, " vs. ", a )' ) &
                      TRIM( ODAS_LHS%Sensor_Id), &
                      TRIM( ODAS_RHS%Sensor_Id)
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    ! The Sensor_Type
    IF ( ODAS_LHS%Sensor_Type /= ODAS_RHS%Sensor_Type ) THEN
      WRITE( Message,'("Sensor types are different, ", &
                       &i0,"(",a,") vs. ", i0,"(",a,")")' ) &
                      ODAS_LHS%Sensor_Type, &
                      TRIM(SENSOR_TYPE_NAME(ODAS_LHS%Sensor_Type)), &
                      ODAS_RHS%Sensor_Type, &
                      TRIM(SENSOR_TYPE_NAME(ODAS_RHS%Sensor_Type))
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The WMO Satellite ID
    IF ( ODAS_LHS%WMO_Satellite_ID /= ODAS_RHS%WMO_Satellite_ID ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("WMO_Satellite_ID values are different, ",i0,&
                      &" vs. ",i0 )' ) &
                      ODAS_LHS%WMO_Satellite_ID, &
                      ODAS_RHS%WMO_Satellite_ID
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The WMO Sensor ID
    IF ( ODAS_LHS%WMO_Sensor_ID /= ODAS_RHS%WMO_Sensor_ID ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("WMO_Sensor_ID values are different, ",i0,&
                      &" vs. ",i0)' ) &
                      ODAS_LHS%WMO_Sensor_ID, &
                      ODAS_RHS%WMO_Sensor_ID
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The Sensor_Channel
    DO l = 1, ODAS_RHS%n_Channels
      IF ( ODAS_LHS%Sensor_Channel(l) /= ODAS_RHS%Sensor_Channel(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Sensor_Channel values are different, ",i0,&
                        &" vs. ",i0,", for channel index # ",i0)' ) &
                        ODAS_LHS%Sensor_Channel(l), &
                        ODAS_RHS%Sensor_Channel(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Absorber_ID
    DO j = 1, ODAS_RHS%n_Absorbers
      IF ( ODAS_LHS%Absorber_ID(j) /= ODAS_RHS%Absorber_ID(j) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Absorber_ID values are different, ",i0,&
                        &" vs. ",i0,", for absorber index # ",i0)' ) &
                        ODAS_LHS%Absorber_ID(j), &
                        ODAS_RHS%Absorber_ID(j), &
                        j
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Alpha value
    DO j = 1, ODAS_RHS%n_Absorbers
      IF ( .NOT. ( Compare_Float( ODAS_LHS%Alpha(j), &
                                  ODAS_RHS%Alpha(j), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Alpha values are different, ",es13.6,&
                        &" vs. ",es13.6,", for absorber index # ",i0)' ) &
                        ODAS_LHS%Alpha(j), &
                        ODAS_RHS%Alpha(j), &
                        j
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Alpha_C1 value
    DO j = 1, ODAS_RHS%n_Absorbers
      IF ( .NOT. ( Compare_Float( ODAS_LHS%Alpha_C1(j), &
                                  ODAS_RHS%Alpha_C1(j), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Alpha_C1 values are different, ",es13.6,&
                        &" vs. ",es13.6,", for absorber index # ",i0)' ) &
                        ODAS_LHS%Alpha_C1(j), &
                        ODAS_RHS%Alpha_C1(j), &
                        j
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Alpha_C2 value
    DO j = 1, ODAS_RHS%n_Absorbers
      IF ( .NOT. ( Compare_Float( ODAS_LHS%Alpha_C2(j), &
                                  ODAS_RHS%Alpha_C2(j), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Alpha_C2 values are different, ",es13.6,&
                        &" vs. ",es13.6,", for absorber index # ",i0)' ) &
                        ODAS_LHS%Alpha_C2(j), &
                        ODAS_RHS%Alpha_C2(j), &
                        j
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Order_Index
    DO l = 1, ODAS_RHS%n_Channels
      DO j = 1, ODAS_RHS%n_Absorbers
        DO ip = 0, ODAS_RHS%n_Predictors
          IF ( ODAS_LHS%Order_Index(ip,j,l) /= ODAS_RHS%Order_Index(ip,j,l) ) THEN
            Error_Status = FAILURE
            WRITE( Message,'("Order_Index values are different, ",i0,&
                            &" vs. ",i0,", for index (",i0,1x,i0,1x,i0,")")' ) &
                            ODAS_LHS%Order_Index(ip,j,l), &
                            ODAS_RHS%Order_Index(ip,j,l), &
                            ip,j,l
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM(Message), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        END DO
      END DO
    END DO

    ! The Predictor_Index
    DO l = 1, ODAS_RHS%n_Channels
      DO j = 1, ODAS_RHS%n_Absorbers
        DO ip = 0, ODAS_RHS%n_Predictors
          IF ( ODAS_LHS%Predictor_Index(ip,j,l) /= ODAS_RHS%Predictor_Index(ip,j,l) ) THEN
            Error_Status = FAILURE
            WRITE( Message,'("Predictor_Index values are different, ",i0,&
                            &" vs. ",i0,", for index (",i0,1x,i0,1x,i0,")")' ) &
                            ODAS_LHS%Predictor_Index(ip,j,l), &
                            ODAS_RHS%Predictor_Index(ip,j,l), &
                            ip,j,l
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM(Message), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        END DO
      END DO
    END DO

    ! The Coefficients
    DO l = 1, ODAS_RHS%n_Channels
      DO j = 1, ODAS_RHS%n_Absorbers
        DO ip = 0, ODAS_RHS%n_Predictors
          DO io = 0, ODAS_RHS%n_Orders
            IF ( ODAS_LHS%C(io,ip,j,l) /= ODAS_RHS%C(io,ip,j,l) ) THEN
              Error_Status = FAILURE
              WRITE( Message,'("C values are different, ",i0,&
                              &" vs. ",i0,", for index (",i0,1x,i0,1x,i0,1x,i0,")")' ) &
                              ODAS_LHS%C(io,ip,j,l), &
                              ODAS_RHS%C(io,ip,j,l), &
                              io,ip,j,l
              CALL Display_Message( ROUTINE_NAME, &
                                    TRIM(Message), &
                                    Error_Status, &
                                    Message_Log=Message_Log )
              IF ( Check_Once ) RETURN
            END IF
          END DO
        END DO
      END DO
    END DO

  END FUNCTION Equal_ODAS


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckRelease_ODAS
!
! PURPOSE:
!       Function to check the ODAS Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_ODASe( ODAS                   , &  ! Input
!                                          RCS_Id     = RCS_Id    , &  ! Revision control
!                                          Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODAS:          ODAS structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
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
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
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

  FUNCTION CheckRelease_ODAS( ODAS       , &  ! Input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(ODAS_type)       , INTENT(IN)  :: ODAS
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_ODAS'
    ! Local variables
    CHARACTER(ML) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Check the release
    ! -----------------
    ! Check that release is not too old
    IF ( ODAS%Release < ODAS_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODAS data update is needed. ", &
                        &"ODAS release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODAS%Release, ODAS_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that release is not too new
    IF ( ODAS%Release > ODAS_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODAS software update is needed. ", &
                        &"ODAS release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODAS%Release, ODAS_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_ODAS


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckAlgorithm_ODAS
!
! PURPOSE:
!       Function to check the ODAS Algorithm value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckAlgorithm_ODAS( ODAS                   , &  ! Input
!                                           RCS_Id     = RCS_Id    , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODAS:          ODAS structure for which the Algorithm member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
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
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure Algorithm value is valid.
!                         == FAILURE the structure Algorithm value is NOT valid.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION CheckAlgorithm_ODAS( ODAS       , &  ! Input
                                RCS_Id     , &  ! Revision control
                                Message_Log) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(ODAS_type)       , INTENT(IN)  :: ODAS
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckAlgorithm_ODAS'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Check the algorithm ID
    ! ----------------------
    IF ( ODAS%Algorithm /= ODAS_ALGORITHM ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'The ODAS Algorithm ID check failed. '//&
                            'The data structure is not an ODAS structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckAlgorithm_ODAS


!------------------------------------------------------------------------------
!
! NAME:
!       Info_ODAS
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the ODAS data structure.
!
! CALLING SEQUENCE:
!       CALL Info_ODAS( ODAS         , &  ! Input
!                       Info         , &  ! Output
!                       RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       ODAS:          Filled ODAS structure.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed ODAS data structure.
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
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Info_ODAS( ODAS  , &  ! Input
                        Info  , &  ! Output
                        RCS_Id  )  ! Revision control
    ! Arguments
    TYPE(ODAS_type)       , INTENT(IN)  :: ODAS
    CHARACTER(*),           INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Set up
    ! ------
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required data to the local string
    ! -------------------------------------------
    WRITE( LongString,'( a,3x,"ODAS RELEASE.VERSION: ",i2,".",i2.2,2x,&
                      &"N_ORDERS=",i2,2x,&
                      &"N_PREDICTORS=",i2,2x,&
                      &"N_ABSORBERS=",i2,2x,&
                      &"N_CHANNELS=",i0)' ) &
                      ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                      ODAS%Release, ODAS%Version, &
                      ODAS%n_Orders, &
                      ODAS%n_Predictors, &
                      ODAS%n_Absorbers, &
                      ODAS%n_Channels

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_ODAS


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
!       Clear_ODAS
!
! PURPOSE:
!       Subroutine to clear the scalar members of a ODAS structure.
!
! CALLING SEQUENCE:
!       CALL Clear_ODAS( ODAS ) ! Output
!
! OUTPUT ARGUMENTS:
!       ODAS:    ODAS structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       ODAS_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output ODAS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_ODAS( ODAS )
    TYPE(ODAS_type), INTENT(IN OUT) :: ODAS
    ODAS%Release   = ODAS_RELEASE
    ODAS%Version   = ODAS_VERSION
    ODAS%Algorithm = ODAS_ALGORITHM
    ODAS%Sensor_Id        = ' '
    ODAS%Sensor_Type      = INVALID_SENSOR
    ODAS%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    ODAS%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE Clear_ODAS

END MODULE ODAS_Define
