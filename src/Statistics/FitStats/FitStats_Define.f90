!
! FitStats_Define
!
! Module defining the CompactOPTRAN FitStats data structure and containing
! routines to manipulate it.
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, CIMSS/SSEC, 14-Jan-2008 
!                    paul.vandelst@ssec.wisc.edu
!

MODULE FitStats_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE SensorInfo_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: MAX_PREDICTORS
  ! Data structure definition
  PUBLIC :: FitStats_type
  ! Structure procedures
  PUBLIC :: Associated_FitStats
  PUBLIC :: Destroy_FitStats
  PUBLIC :: Allocate_FitStats
  PUBLIC :: Assign_FitStats
  PUBLIC :: Equal_FitStats
  PUBLIC :: Info_FitStats
  PUBLIC :: CheckRelease_FitStats
    

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! Message string length
  INTEGER, PARAMETER :: ML = 512
  ! Sensor Id string length
  INTEGER, PARAMETER :: SL = 20
  ! Maximum number of predictors
  INTEGER, PARAMETER :: MAX_PREDICTORS = 6
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: FITSTATS_RELEASE = 1
  INTEGER, PARAMETER :: FITSTATS_VERSION = 1

  ! -----------------------
  ! Derived type definition
  ! -----------------------
  !:tdoc+:
  TYPE :: FitStats_type
    INTEGER :: n_Allocates=0
    ! Release and version information
    INTEGER(Long) :: Release = FITSTATS_RELEASE
    INTEGER(Long) :: Version = FITSTATS_VERSION
    ! "Parameters"
    INTEGER(Long) :: Max_n_Predictors = MAX_PREDICTORS
    ! Dimensions
    INTEGER(Long) :: n_Channels = 0  ! L
    ! Scalars
    CHARACTER(SL) :: Sensor_Id        = ' '                     
    INTEGER(Long) :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    ! Arrays
    INTEGER(Long), POINTER :: Sensor_Channel(:)  => NULL()  ! L
    INTEGER(Long), POINTER :: Order(:)           => NULL()  ! L
    INTEGER(Long), POINTER :: n_Predictors(:)    => NULL()  ! L
    INTEGER(Long), POINTER :: Predictor_Idx(:,:) => NULL()  ! I x L
    REAL(Double) , POINTER :: Frequency(:)       => NULL()  ! L
    REAL(Double) , POINTER :: Fit_Residual(:)    => NULL()  ! L
    REAL(Double) , POINTER :: Tb_BIAS(:)         => NULL()  ! L
    REAL(Double) , POINTER :: Tb_SDEV(:)         => NULL()  ! L
    REAL(Double) , POINTER :: Tb_RMS(:)          => NULL()  ! L
    REAL(Double) , POINTER :: Tb_MAX(:)          => NULL()  ! L
    REAL(Double) , POINTER :: Tau_BIAS(:)        => NULL()  ! L
    REAL(Double) , POINTER :: Tau_SDEV(:)        => NULL()  ! L
    REAL(Double) , POINTER :: Tau_RMS(:)         => NULL()  ! L
    REAL(Double) , POINTER :: Tau_MAX(:)         => NULL()  ! L
    REAL(Double) , POINTER :: Tau_Max_BIAS(:)    => NULL()  ! L
    REAL(Double) , POINTER :: Tau_Max_SDEV(:)    => NULL()  ! L
    REAL(Double) , POINTER :: Tau_Max_RMS(:)     => NULL()  ! L
    REAL(Double) , POINTER :: Max_Pred_Term(:)   => NULL()  ! L
  END TYPE FitStats_type
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
!       Associated_FitStats
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       FitStats structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_FitStats( FitStats         , &  ! Input
!                                                 ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       FitStats:            FitStats structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       FitStats_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            FitStats structure pointer members are associated.
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
!                            association status of the FitStats pointer members.
!                            .TRUE.  - if ALL the FitStats pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the FitStats pointer
!                                      members are associated.
!                            .FALSE. - some or all of the FitStats pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Associated_FitStats( FitStats, &  ! Input          
                                ANY_Test) &  ! Optional input 
                              RESULT(Association_Status)      
    ! Arguments
    TYPE(FitStats_type), INTENT(IN) :: FitStats
    INTEGER, OPTIONAL  , INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test
    
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == 1 ) ALL_Test = .FALSE.
    END IF

    ! Test the structure associations    
    Association_Status = .FALSE.
    IF (ALL_Test) THEN
      IF ( ASSOCIATED(FitStats%Sensor_Channel) .AND. &
           ASSOCIATED(FitStats%Order         ) .AND. &
           ASSOCIATED(FitStats%n_Predictors  ) .AND. &
           ASSOCIATED(FitStats%Predictor_Idx ) .AND. &
           ASSOCIATED(FitStats%Frequency     ) .AND. &
           ASSOCIATED(FitStats%Fit_Residual  ) .AND. &
           ASSOCIATED(FitStats%Tb_BIAS       ) .AND. &
           ASSOCIATED(FitStats%Tb_SDEV       ) .AND. &
           ASSOCIATED(FitStats%Tb_RMS        ) .AND. &
           ASSOCIATED(FitStats%Tb_MAX        ) .AND. &
           ASSOCIATED(FitStats%Tau_BIAS      ) .AND. &
           ASSOCIATED(FitStats%Tau_SDEV      ) .AND. &
           ASSOCIATED(FitStats%Tau_RMS       ) .AND. &
           ASSOCIATED(FitStats%Tau_MAX       ) .AND. &
           ASSOCIATED(FitStats%Tau_Max_BIAS  ) .AND. &
           ASSOCIATED(FitStats%Tau_Max_SDEV  ) .AND. &
           ASSOCIATED(FitStats%Tau_Max_RMS   ) .AND. &
           ASSOCIATED(FitStats%Max_Pred_Term ) ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED(FitStats%Sensor_Channel) .OR. &
           ASSOCIATED(FitStats%Order         ) .OR. &
           ASSOCIATED(FitStats%n_Predictors  ) .OR. &
           ASSOCIATED(FitStats%Predictor_Idx ) .OR. &
           ASSOCIATED(FitStats%Frequency     ) .OR. &
           ASSOCIATED(FitStats%Fit_Residual  ) .OR. &
           ASSOCIATED(FitStats%Tb_BIAS       ) .OR. &
           ASSOCIATED(FitStats%Tb_SDEV       ) .OR. &
           ASSOCIATED(FitStats%Tb_RMS        ) .OR. &
           ASSOCIATED(FitStats%Tb_MAX        ) .OR. &
           ASSOCIATED(FitStats%Tau_BIAS      ) .OR. &
           ASSOCIATED(FitStats%Tau_SDEV      ) .OR. &
           ASSOCIATED(FitStats%Tau_RMS       ) .OR. &
           ASSOCIATED(FitStats%Tau_MAX       ) .OR. &
           ASSOCIATED(FitStats%Tau_Max_BIAS  ) .OR. &
           ASSOCIATED(FitStats%Tau_Max_SDEV  ) .OR. &
           ASSOCIATED(FitStats%Tau_Max_RMS   ) .OR. &
           ASSOCIATED(FitStats%Max_Pred_Term ) ) THEN
        Association_Status = .TRUE.
      END IF
    END IF
  END FUNCTION Associated_FitStats


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Destroy_FitStats
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of FitStats
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_FitStats( FitStats               , &  ! Output
!                                        RCS_Id     =RCS_Id     , &  ! Revision control
!                                        Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       FitStats:     Re-initialized FitStats structure.
!                     UNITS:      N/A
!                     TYPE:       FitStats_type
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
!       Note the INTENT on the output FitStats argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Destroy_FitStats( FitStats   , &  ! Output
                             No_Clear   , &  ! Optional input
                             RCS_Id     , &  ! Revision control
                             Message_Log) &  ! Error messaging
                           RESULT(Error_Status)
    ! Arguments
    TYPE(FitStats_type)   , INTENT(IN OUT) :: FitStats
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_FitStats'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Reset the dimension indicators
    FitStats%n_Channels = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == 1 ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_FitStats(FitStats)
    
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_FitStats(FitStats) ) RETURN
    
    
    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( FitStats%Sensor_Channel, &
                FitStats%Order         , &
                FitStats%n_Predictors  , &
                FitStats%Predictor_Idx , &
                FitStats%Frequency     , &
                FitStats%Fit_Residual  , &
                FitStats%Tb_BIAS       , &
                FitStats%Tb_SDEV       , &
                FitStats%Tb_RMS        , &
                FitStats%Tb_MAX        , &
                FitStats%Tau_BIAS      , &
                FitStats%Tau_SDEV      , &
                FitStats%Tau_RMS       , &
                FitStats%Tau_MAX       , &
                FitStats%Tau_Max_BIAS  , &
                FitStats%Tau_Max_SDEV  , &
                FitStats%Tau_Max_RMS   , &
                FitStats%Max_Pred_Term , &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '("Error deallocating FitStats. STAT = ",i0)') &
                      Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    FitStats%n_Allocates = FitStats%n_Allocates - 1
    IF ( FitStats%n_Allocates /= 0 ) THEN
      WRITE( Message, '("Allocation counter /= 0, Value = ",i0)') &
                      FitStats%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Destroy_FitStats


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Allocate_FitStats
! 
! PURPOSE:
!       Function to allocate the pointer members of a FitStats data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_FitStats( n_Channels             , &  ! Input
!                                         FitStats               , &  ! Output
!                                         RCS_Id     =RCS_Id     , &  ! Revision control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:             Number of sensor channels.
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       FitStats:               FitStats structure with allocated pointer
!                               members
!                               UNITS:      N/A
!                               TYPE:       FitStats_type
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:            Character string specifying a filename in
!                               which any messages will be logged. If not
!                               specified, or if an error occurs opening the
!                               log file, the default action is to output
!                               messages to standard output.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:                 Character string containing the Revision
!                               Control System Id field for the module.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:           The return value is an integer defining the
!                               error status. The error codes are defined in
!                               the Message_Handler module.
!                               If == SUCCESS the structure pointer allocations
!                                             were successful
!                                  == FAILURE - an error occurred, or
!                                             - the structure internal allocation
!                                               counter is not equal to one (1)
!                                               upon exiting this function. This
!                                               value is incremented and decre-
!                                               mented for every structure
!                                               allocation and deallocation
!                                               respectively.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output FitStats argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Allocate_FitStats( n_Channels , &  ! Input            
                              FitStats   , &  ! Output           
                              RCS_Id     , &  ! Revision control 
                              Message_Log) &  ! Error messaging  
                            RESULT( Error_Status )               
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Channels
    TYPE(FitStats_type)   , INTENT(IN OUT) :: FitStats
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_FitStats'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Check dimensions
    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input FitStats dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_FitStats( FitStats, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_FitStats( FitStats, &               
                                      No_Clear=SET, &            
                                      Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating FitStats prior to allocation.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    
    ! Perform the pointer allocation
    ! ------------------------------
    ALLOCATE( FitStats%Sensor_Channel(1:n_Channels), &
              FitStats%Order(1:n_Channels)         , &
              FitStats%n_Predictors(1:n_Channels)  , &
              FitStats%Predictor_Idx(MAX_PREDICTORS,1:n_Channels), &
              FitStats%Frequency(1:n_Channels)     , &
              FitStats%Fit_Residual (1:n_Channels) , &
              FitStats%Tb_BIAS(1:n_Channels)       , &
              FitStats%Tb_SDEV(1:n_Channels)       , &
              FitStats%Tb_RMS(1:n_Channels)        , &
              FitStats%Tb_MAX(1:n_Channels)        , &
              FitStats%Tau_BIAS(1:n_Channels)      , &
              FitStats%Tau_SDEV(1:n_Channels)      , &
              FitStats%Tau_RMS(1:n_Channels)       , &
              FitStats%Tau_MAX(1:n_Channels)       , &
              FitStats%Tau_Max_BIAS(1:n_Channels)  , &
              FitStats%Tau_Max_SDEV(1:n_Channels)  , &
              FitStats%Tau_Max_RMS(1:n_Channels)   , &
              FitStats%Max_Pred_Term(1:n_Channels) , &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE(Message,'("Error allocating FitStats data arrays. STAT = ",i0)') &
                    Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    

    ! Assign the dimensions
    ! ---------------------
    FitStats%Max_n_Predictors = MAX_PREDICTORS
    FitStats%n_Channels       = n_Channels


    ! Initialise the arrays
    ! ---------------------
    FitStats%Sensor_Channel = 0
    FitStats%Order          = 0
    FitStats%n_Predictors   = 0
    FitStats%Predictor_Idx  = 0
    FitStats%Frequency      = ZERO
    FitStats%Fit_Residual   = ZERO
    FitStats%Tb_BIAS        = ZERO
    FitStats%Tb_SDEV        = ZERO
    FitStats%Tb_RMS         = ZERO
    FitStats%Tb_MAX         = ZERO
    FitStats%Tau_BIAS       = ZERO
    FitStats%Tau_SDEV       = ZERO
    FitStats%Tau_RMS        = ZERO
    FitStats%Tau_MAX        = ZERO
    FitStats%Tau_Max_BIAS   = ZERO
    FitStats%Tau_Max_SDEV   = ZERO
    FitStats%Tau_Max_RMS    = ZERO
    FitStats%Max_Pred_Term  = ZERO


    ! Increment and test the allocation counter
    ! -----------------------------------------
    FitStats%n_Allocates = FitStats%n_Allocates + 1
    IF ( FitStats%n_Allocates /= 1 ) THEN
      WRITE( Message, '("Allocation counter /= 1, Value = ",i0)') &
                      FitStats%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Allocate_FitStats
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Assign_FitStats
!
! PURPOSE:
!       Function to copy valid FitStats structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_FitStats( FitStats_in            , &  ! Input
!                                       FitStats_out           , &  ! Output
!                                       RCS_Id     =RCS_Id     , &  ! Revision control
!                                       Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       FitStats_in:   FitStats structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       FitStats_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       FitStats_out:  Copy of the input structure, FitStats_in.
!                      UNITS:      N/A
!                      TYPE:       Same as FitStats_in
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
!                      If == SUCCESS the structure assignment was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output FitStats argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Assign_FitStats( FitStats_in , &  ! Input
                            FitStats_out, &  ! Output
                            RCS_Id      , &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    TYPE(FitStats_type)   , INTENT(IN)     :: FitStats_in
    TYPE(FitStats_type)   , INTENT(IN OUT) :: FitStats_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_FitStats'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_FitStats( FitStats_in ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT FitStats_in pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Allocate data arrays
    ! --------------------
    Error_Status = Allocate_FitStats( FitStats_in%n_Channels, &
                                      FitStats_out, &
                                      Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    
    ! Assign non-dimension scalar members
    ! -----------------------------------
    FitStats_out%Release          = FitStats_in%Release
    FitStats_out%Version          = FitStats_in%Version
    FitStats_out%Max_n_Predictors = MAX_PREDICTORS
    FitStats_out%Sensor_Id        = FitStats_in%Sensor_Id
    FitStats_out%WMO_Satellite_Id = FitStats_in%WMO_Satellite_Id
    FitStats_out%WMO_Sensor_Id    = FitStats_in%WMO_Sensor_Id
    
    ! Copy array data
    ! ---------------
    FitStats_out%Sensor_Channel = FitStats_in%Sensor_Channel
    FitStats_out%Order          = FitStats_in%Order        
    FitStats_out%n_Predictors   = FitStats_in%n_Predictors 
    FitStats_out%Predictor_Idx  = FitStats_in%Predictor_Idx
    FitStats_out%Frequency      = FitStats_in%Frequency
    FitStats_out%Fit_Residual   = FitStats_in%Fit_Residual 
    FitStats_out%Tb_BIAS        = FitStats_in%Tb_BIAS      
    FitStats_out%Tb_SDEV        = FitStats_in%Tb_SDEV      
    FitStats_out%Tb_RMS         = FitStats_in%Tb_RMS       
    FitStats_out%Tb_MAX         = FitStats_in%Tb_MAX       
    FitStats_out%Tau_BIAS       = FitStats_in%Tau_BIAS     
    FitStats_out%Tau_SDEV       = FitStats_in%Tau_SDEV     
    FitStats_out%Tau_RMS        = FitStats_in%Tau_RMS      
    FitStats_out%Tau_MAX        = FitStats_in%Tau_MAX      
    FitStats_out%Tau_Max_BIAS   = FitStats_in%Tau_Max_BIAS 
    FitStats_out%Tau_Max_SDEV   = FitStats_in%Tau_Max_SDEV 
    FitStats_out%Tau_Max_RMS    = FitStats_in%Tau_Max_RMS  
    FitStats_out%Max_Pred_Term  = FitStats_in%Max_Pred_Term
    
  END FUNCTION Assign_FitStats


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Equal_FitStats
!
! PURPOSE:
!       Function to test if two FitStats structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_FitStats( FitStats_LHS           , &  ! Input
!                                      FitStats_RHS           , &  ! Input
!                                      ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                      Check_All  =Check_All  , &  ! Optional input
!                                      RCS_Id     =RCS_Id     , &  ! Revision control
!                                      Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       FitStats_LHS:  FitStats structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( FitStats_LHS == FitStats_RHS ).
!                      UNITS:      N/A
!                      TYPE:       FitStats_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       FitStats_RHS:  FitStats structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( FitStats_LHS == FitStats_RHS ).
!                      UNITS:      N/A
!                      TYPE:       Same as FitStats_LHS
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
!                      channel data of the FitStats structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in FitStats structures.
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
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Equal_FitStats( FitStats_LHS, &  ! Input
                           FitStats_RHS, &  ! Input
                           ULP_Scale   , &  ! Optional input
                           Check_All   , &  ! Optional input
                           RCS_Id      , &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(FitStats_type)   , INTENT(IN)  :: FitStats_LHS
    TYPE(FitStats_type)   , INTENT(IN)  :: FitStats_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER     , OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_FitStats'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: i, l

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
    IF ( .NOT. Associated_FitStats( FitStats_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT FitStats_LHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_FitStats( FitStats_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT FitStats_RHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check dimensions
    IF ( FitStats_LHS%Max_n_Predictors /= MAX_PREDICTORS .OR. &
         FitStats_RHS%Max_n_Predictors /= MAX_PREDICTORS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid Max_n_Predictors value.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    IF ( FitStats_LHS%n_Channels /= FitStats_RHS%n_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the scalar components
    ! ---------------------------
    IF ( FitStats_LHS%Sensor_Id /= FitStats_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'FitStats Sensor_Id values are different; >'//&
                            TRIM(FitStats_LHS%Sensor_Id)//'< vs >'//&
                            TRIM(FitStats_RHS%Sensor_Id)//'<', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( FitStats_LHS%WMO_Satellite_Id /= FitStats_RHS%WMO_Satellite_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'FitStats scalar component WMO_Satellite_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( FitStats_LHS%WMO_Sensor_Id /= FitStats_RHS%WMO_Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'FitStats scalar component WMO_Sensor_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    
    ! Check the array components
    ! --------------------------
    DO l = 1, FitStats_LHS%n_Channels
      IF ( FitStats_LHS%Sensor_Channel(l) /= FitStats_RHS%Sensor_Channel(l) ) THEN
        WRITE(Message,'("FitStats array component Sensor_Channel values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( FitStats_LHS%Order(l) /= FitStats_RHS%Order(l) ) THEN
        WRITE(Message,'("FitStats array component Order values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( FitStats_LHS%n_Predictors(l) /= FitStats_RHS%n_Predictors(l) ) THEN
        WRITE(Message,'("FitStats array component n_Predictors values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      DO i = 1, FitStats_LHS%n_Predictors(l)
        IF ( FitStats_LHS%Predictor_Idx(i,l) /= FitStats_RHS%Predictor_Idx(i,l) ) THEN
          WRITE(Message,'("FitStats array component Predictor_Idx values ",&
                          &"are different at indices (",2(1x,i0),")")') &
                          i,l
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM(Message), &
                                Error_Status, &
                                Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Frequency(l), &
                                FitStats_RHS%Frequency(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Frequency values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Fit_Residual(l), &
                                FitStats_RHS%Fit_Residual(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Fit_Residual values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tb_BIAS(l), &
                                FitStats_RHS%Tb_BIAS(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tb_BIAS values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tb_SDEV(l), &
                                FitStats_RHS%Tb_SDEV(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tb_SDEV values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tb_RMS(l), &
                                FitStats_RHS%Tb_RMS(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tb_RMS values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tb_MAX(l), &
                                FitStats_RHS%Tb_MAX(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tb_MAX values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tau_BIAS(l), &
                                FitStats_RHS%Tau_BIAS(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tau_BIAS values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tau_SDEV(l), &
                                FitStats_RHS%Tau_SDEV(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tau_SDEV values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tau_RMS(l), &
                                FitStats_RHS%Tau_RMS(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tau_RMS values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tau_MAX(l), &
                                FitStats_RHS%Tau_MAX(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tau_MAX values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tau_Max_BIAS(l), &
                                FitStats_RHS%Tau_Max_BIAS(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tau_Max_BIAS values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tau_Max_SDEV(l), &
                                FitStats_RHS%Tau_Max_SDEV(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tau_Max_SDEV values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Tau_Max_RMS(l), &
                                FitStats_RHS%Tau_Max_RMS(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Tau_Max_RMS values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, FitStats_LHS%n_Channels
      IF ( .NOT. Compare_Float( FitStats_LHS%Max_Pred_Term(l), &
                                FitStats_RHS%Max_Pred_Term(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("FitStats array component Max_Pred_Term values ",&
                        &"are different at indices (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
  END FUNCTION Equal_FitStats


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CheckRelease_FitStats
!
! PURPOSE:
!       Function to check the FitStats Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_FitStats( FitStats               , &  ! Input
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       FitStats:      FitStats structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       FitStats_type
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
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION CheckRelease_FitStats( FitStats   , &  ! Input
                                  RCS_Id     , &  ! Revision control
                                  Message_Log) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    TYPE(FitStats_type)   , INTENT(IN)  :: FitStats
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_FitStats'
    ! Local variables
    CHARACTER(ML) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check release is not too old
    ! ----------------------------
    IF ( FitStats%Release < FITSTATS_RELEASE ) THEN
      WRITE( Message,'("A FitStats data update is needed. ",&
                      &"FitStats release is ",i0,&
                      &". Valid release is ",i0,".")' ) &
                      FitStats%Release, FitStats_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check release is not too new
    ! ----------------------------
    IF ( FitStats%Release > FITSTATS_RELEASE ) THEN
      WRITE( Message,'("A FitStats software update is needed. ",&
                      &"FitStats release is ",i0,&
                      &". Valid release is ",i0,"." )' ) &
                      FitStats%Release, FITSTATS_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_FitStats


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Info_FitStats
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the FitStats data structure.
!
! CALLING SEQUENCE:
!       CALL Info_FitStats( FitStats     , &  ! Input
!                           Info         , &  ! Output
!                           RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       FitStats:      Filled FitStats structure.
!                      UNITS:      N/A
!                      TYPE:       FitStats_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed FitStats data structure.
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
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Info_FitStats( FitStats, &  ! Input
                            Info    , &  ! Output
                            RCS_Id    )  ! Revision control
    ! Arguments
    TYPE(FitStats_type)   , INTENT(IN)  :: FitStats
    CHARACTER(*)          , INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(256) :: FmtString
    CHARACTER(512) :: LongString

    ! Set up
    ! ------
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Write the required info to the local string
    ! -------------------------------------------
    FmtString='(a,1x,a,1x,"FitStats RELEASE.VERSION: ",i2,".",i2.2,2x,&
               &"N_CHANNELS=",i0)'
    WRITE(LongString, FMT=FmtString) &
          ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
          TRIM(FitStats%Sensor_ID), &
          FitStats%Release, FitStats%Version, &
          FitStats%n_Channels

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_FitStats


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  SUBROUTINE Clear_FitStats(FitStats)
    TYPE(FitStats_type), INTENT(IN OUT) :: FitStats
    FitStats%Release          = FITSTATS_RELEASE
    FitStats%Version          = FITSTATS_VERSION
    FitStats%Max_n_Predictors = MAX_PREDICTORS
    FitStats%Sensor_Id        = ' '
    FitStats%WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    FitStats%WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE Clear_FitStats

END MODULE FitStats_Define
