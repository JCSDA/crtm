!
! ZTauCoeff_Define
!
! Module defining the ZTauCoeff data structure and containing routines to 
! manipulate it.
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, CIMSS/SSEC, 17-Jul-2007 
!                    paul.vandelst@noaa.gov
!                    Yong Han, NESDIS/STAR
!                    yong.han@noaa.gov
!

MODULE ZTauCoeff_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data structure definition
  PUBLIC :: ZTauCoeff_type
  ! Structure procedures
  PUBLIC :: Associated_ZTauCoeff
  PUBLIC :: Destroy_ZTauCoeff
  PUBLIC :: Allocate_ZTauCoeff
  PUBLIC :: Assign_ZTauCoeff
  PUBLIC :: Equal_ZTauCoeff
  PUBLIC :: Info_ZTauCoeff
  PUBLIC :: CheckRelease_ZTauCoeff
    

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! String length
  INTEGER, PARAMETER :: SL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ZTAUCOEFF_RELEASE = 1
  INTEGER, PARAMETER :: ZTAUCOEFF_VERSION = 1
  ! Sensor Id default values
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047

  ! -----------------------
  ! Derived type definition
  ! -----------------------
  TYPE :: ZTauCoeff_type
    INTEGER :: n_Allocates=0
    ! Release and version information
    INTEGER(Long) :: Release = ZTAUCOEFF_RELEASE
    INTEGER(Long) :: Version = ZTAUCOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Predictors = 0 ! Iuse
    INTEGER(Long) :: n_Layers     = 0 ! K
    INTEGER(Long) :: n_Channels   = 0 ! L
    ! Scalars
    CHARACTER(SL) :: Sensor_Id        = ' '                     
    INTEGER(Long) :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    ! The actual channel numbers
    INTEGER(Long), POINTER :: Sensor_Channel(:)   => NULL() ! L
    ! Reference pressure and altitude                                        
    REAL(Double),  POINTER :: Level_Altitude(:)   => NULL() ! 0:K
    REAL(Double),  POINTER :: Level_Pressure(:)   => NULL() ! 0:K
    REAL(Double),  POINTER :: Pressure(:)         => NULL() ! K
    ! Indexing arrays
    INTEGER(Long), POINTER :: ChannelIndex(:)     => NULL() ! L
    INTEGER(Long), POINTER :: PredictorIndex(:,:) => NULL() ! 0:Iuse x L
    ! Zenith angles at wgt.fn. peak levels  
    REAL(Double),  POINTER :: Secant_Zenith(:)    => NULL() ! L
    ! Coefficient array
    REAL(Double),  POINTER :: C(:,:,:)            => NULL() ! 0:Iuse x K x L
  END TYPE ZTauCoeff_type


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
!       Associated_ZTauCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       ZTauCoeff structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_ZTauCoeff( ZTauCoeff        , &  ! Input
!                                                  ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       ZTauCoeff:           ZTauCoeff structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       ZTauCoeff_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            ZTauCoeff structure pointer members are associated.
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
!                            association status of the ZTauCoeff pointer members.
!                            .TRUE.  - if ALL the ZTauCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the ZTauCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the ZTauCoeff pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_ZTauCoeff( ZTauCoeff, &  ! Input          
                                 ANY_Test ) &  ! Optional input 
                               RESULT(Association_Status)      
    ! Arguments
    TYPE(ZTauCoeff_type), INTENT(IN) :: ZTauCoeff
    INTEGER,    OPTIONAL, INTENT(IN) :: ANY_Test
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
    
    ! Test the structure associations    
    Association_Status = .FALSE.
    IF (ALL_Test) THEN
      IF (ASSOCIATED(ZTauCoeff%Sensor_Channel) .AND. &
          ASSOCIATED(ZTauCoeff%Level_Altitude) .AND. &
          ASSOCIATED(ZTauCoeff%Level_Pressure) .AND. &
          ASSOCIATED(ZTauCoeff%Pressure      ) .AND. &
          ASSOCIATED(ZTauCoeff%ChannelIndex  ) .AND. &
          ASSOCIATED(ZTauCoeff%PredictorIndex) .AND. &
          ASSOCIATED(ZTauCoeff%Secant_Zenith ) .AND. &
          ASSOCIATED(ZTauCoeff%C             )) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF (ASSOCIATED(ZTauCoeff%Sensor_Channel) .OR. &
          ASSOCIATED(ZTauCoeff%Level_Altitude) .OR. &
          ASSOCIATED(ZTauCoeff%Level_Pressure) .OR. &
          ASSOCIATED(ZTauCoeff%Pressure      ) .OR. &
          ASSOCIATED(ZTauCoeff%ChannelIndex  ) .OR. &
          ASSOCIATED(ZTauCoeff%PredictorIndex) .OR. &
          ASSOCIATED(ZTauCoeff%Secant_Zenith ) .OR. &
          ASSOCIATED(ZTauCoeff%C             )) THEN
        Association_Status = .TRUE.
      END IF
    END IF
  END FUNCTION Associated_ZTauCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Destroy_ZTauCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of ZTauCoeff
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_ZTauCoeff( ZTauCoeff              , &  ! Output
!                                         RCS_Id     =RCS_Id     , &  ! Revision control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       ZTauCoeff:    Re-initialized ZTauCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       ZTauCoeff_type
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
!       Note the INTENT on the output ZTauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_ZTauCoeff( ZTauCoeff  , &  ! Output
                              No_Clear   , &  ! Optional input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT(Error_Status)
    ! Arguments
    TYPE(ZTauCoeff_type)  , INTENT(IN OUT) :: ZTauCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_ZTauCoeff'
    ! Local variables
    CHARACTER(256)  :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Reset the dimension indicators
    ZTauCoeff%n_Predictors = 0
    ZTauCoeff%n_Layers     = 0
    ZTauCoeff%n_Channels   = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_ZTauCoeff(ZTauCoeff)
    
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_ZTauCoeff(ZTauCoeff) ) RETURN
    
    
    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( ZTauCoeff%Sensor_Channel, &
                ZTauCoeff%Level_Altitude, &
                ZTauCoeff%Level_Pressure, &
                ZTauCoeff%Pressure      , &
                ZTauCoeff%ChannelIndex  , &
                ZTauCoeff%PredictorIndex, &
                ZTauCoeff%Secant_Zenith , &
                ZTauCoeff%C             , &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '("Error deallocating ZTauCoeff. STAT = ",i0)') &
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
    ZTauCoeff%n_Allocates = ZTauCoeff%n_Allocates - 1
    IF ( ZTauCoeff%n_Allocates /= 0 ) THEN
      WRITE( Message, '("Allocation counter /= 0, Value = ",i0)') &
                      ZTauCoeff%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Destroy_ZTauCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Allocate_ZTauCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of a ZTauCoeff data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_ZTauCoeff( n_Predictors           , &  ! Input
!                                          n_Layers               , &  ! Input
!                                          n_Channels             , &  ! Input
!                                          ZTauCoeff              , &  ! Output
!                                          RCS_Id     =RCS_Id     , &  ! Revision control
!                                          Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Predictors:           Maximum number of predictors.
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
!       n_Layers:               Number of atmospheric layers.
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
!       n_Channels:             Number of sensor channels.
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ZTauCoeff:              ZTauCoeff structure with allocated pointer
!                               members
!                               UNITS:      N/A
!                               TYPE:       ZTauCoeff_type
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
!       Note the INTENT on the output ZTauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_ZTauCoeff( n_Predictors, &  ! Input            
                               n_Layers    , &  ! Input            
                               n_Channels  , &  ! Input            
                               ZTauCoeff   , &  ! Output           
                               RCS_Id      , &  ! Revision control 
                               Message_Log ) &  ! Error messaging  
                             RESULT( Error_Status )               
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Predictors
    INTEGER               , INTENT(IN)     :: n_Layers    
    INTEGER               , INTENT(IN)     :: n_Channels  
    TYPE(ZTauCoeff_type)  , INTENT(IN OUT) :: ZTauCoeff
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_ZTauCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Check dimensions
    IF (n_Predictors < 1 .OR. &
        n_Layers     < 1 .OR. &
        n_Channels   < 1) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ZTauCoeff dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_ZTauCoeff( ZTauCoeff, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_ZTauCoeff( ZTauCoeff, &               
                                        No_Clear=SET, &            
                                        Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating ZTauCoeff prior to allocation.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    
    ! Perform the pointer allocation
    ! ------------------------------
    ALLOCATE( ZTauCoeff%Sensor_Channel(1:n_Channels), &
              ZTauCoeff%Level_Altitude(0:n_Layers), &
              ZTauCoeff%Level_Pressure(0:n_Layers), &
              ZTauCoeff%Pressure(1:n_Layers), &
              ZTauCoeff%ChannelIndex(1:n_Channels), &
              ZTauCoeff%PredictorIndex(0:n_Predictors,1:n_Channels), &
              ZTauCoeff%Secant_Zenith(1:n_Channels), &
              ZTauCoeff%C(0:n_Predictors,1:n_Layers,1:n_Channels), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE(Message,'("Error allocating ZTauCoeff data arrays. STAT = ",i0)') &
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
    ZTauCoeff%n_Predictors = n_Predictors
    ZTauCoeff%n_Layers     = n_Layers    
    ZTauCoeff%n_Channels   = n_Channels  


    ! Initialise the arrays
    ! ---------------------
    ZTauCoeff%Sensor_Channel = 0
    ZTauCoeff%Level_Altitude = ZERO
    ZTauCoeff%Level_Pressure = ZERO
    ZTauCoeff%Pressure       = ZERO
    ZTauCoeff%ChannelIndex   = 0
    ZTauCoeff%PredictorIndex = 0
    ZTauCoeff%Secant_Zenith  = -ONE
    ZTauCoeff%C              = ZERO


    ! Increment and test the allocation counter
    ! -----------------------------------------
    ZTauCoeff%n_Allocates = ZTauCoeff%n_Allocates + 1
    IF ( ZTauCoeff%n_Allocates /= 1 ) THEN
      WRITE( Message, '("Allocation counter /= 1, Value = ",i0)') &
                      ZTauCoeff%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Allocate_ZTauCoeff
  
  
!--------------------------------------------------------------------------------
!
! NAME:
!       Assign_ZTauCoeff
!
! PURPOSE:
!       Function to copy valid ZTauCoeff structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_ZTauCoeff( ZTauCoeff_in           , &  ! Input
!                                        ZTauCoeff_out          , &  ! Output
!                                        RCS_Id     =RCS_Id     , &  ! Revision control
!                                        Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ZTauCoeff_in:  ZTauCoeff structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       ZTauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ZTauCoeff_out: Copy of the input structure, ZTauCoeff_in.
!                      UNITS:      N/A
!                      TYPE:       Same as ZTauCoeff_in
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
!       Note the INTENT on the output ZTauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Assign_ZTauCoeff( ZTauCoeff_in , &  ! Input
                             ZTauCoeff_out, &  ! Output
                             RCS_Id       , &  ! Revision control
                             Message_Log  ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    TYPE(ZTauCoeff_type)  , INTENT(IN)     :: ZTauCoeff_in
    TYPE(ZTauCoeff_type)  , INTENT(IN OUT) :: ZTauCoeff_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_ZTauCoeff'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_ZTauCoeff( ZTauCoeff_in ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ZTauCoeff_in pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Allocate data arrays
    ! --------------------
    Error_Status = Allocate_ZTauCoeff( ZTauCoeff_in%n_Predictors, &
                                       ZTauCoeff_in%n_Layers    , &
                                       ZTauCoeff_in%n_Channels  , &
                                       ZTauCoeff_out, &
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
    ZTauCoeff_out%Release          = ZTauCoeff_in%Release
    ZTauCoeff_out%Version          = ZTauCoeff_in%Version
    ZTauCoeff_out%Sensor_Id        = ZTauCoeff_in%Sensor_Id
    ZTauCoeff_out%WMO_Satellite_Id = ZTauCoeff_in%WMO_Satellite_Id
    ZTauCoeff_out%WMO_Sensor_Id    = ZTauCoeff_in%WMO_Sensor_Id
    
    ! Copy array data
    ! ---------------
    ZTauCoeff_out%Sensor_Channel = ZTauCoeff_in%Sensor_Channel
    ZTauCoeff_out%Level_Altitude = ZTauCoeff_in%Level_Altitude
    ZTauCoeff_out%Level_Pressure = ZTauCoeff_in%Level_Pressure
    ZTauCoeff_out%Pressure       = ZTauCoeff_in%Pressure      
    ZTauCoeff_out%ChannelIndex   = ZTauCoeff_in%ChannelIndex  
    ZTauCoeff_out%PredictorIndex = ZTauCoeff_in%PredictorIndex
    ZTauCoeff_out%Secant_Zenith  = ZTauCoeff_in%Secant_Zenith 
    ZTauCoeff_out%C              = ZTauCoeff_in%C             
    
  END FUNCTION Assign_ZTauCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Equal_ZTauCoeff
!
! PURPOSE:
!       Function to test if two ZTauCoeff structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_ZTauCoeff( ZTauCoeff_LHS          , &  ! Input
!                                       ZTauCoeff_RHS          , &  ! Input
!                                       ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                       Check_All  =Check_All  , &  ! Optional input
!                                       RCS_Id     =RCS_Id     , &  ! Revision control
!                                       Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ZTauCoeff_LHS: ZTauCoeff structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( ZTauCoeff_LHS == ZTauCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       ZTauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       ZTauCoeff_RHS: ZTauCoeff structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( ZTauCoeff_LHS == ZTauCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       Same as ZTauCoeff_LHS
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
!                      channel data of the ZTauCoeff structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in ZTauCoeff structures.
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

  FUNCTION Equal_ZTauCoeff( ZTauCoeff_LHS, &  ! Input
                            ZTauCoeff_RHS, &  ! Input
                            ULP_Scale    , &  ! Optional input
                            Check_All    , &  ! Optional input
                            RCS_Id       , &  ! Revision control
                            Message_Log  ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    TYPE(ZTauCoeff_type)  , INTENT(IN)  :: ZTauCoeff_LHS
    TYPE(ZTauCoeff_type)  , INTENT(IN)  :: ZTauCoeff_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER     , OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_ZTauCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: i, k, l

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
    IF ( .NOT. Associated_ZTauCoeff( ZTauCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ZTauCoeff_LHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_ZTauCoeff( ZTauCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ZTauCoeff_RHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check dimensions
    IF (ZTauCoeff_LHS%n_Predictors /= ZTauCoeff_RHS%n_Predictors .OR. &
        ZTauCoeff_LHS%n_Layers     /= ZTauCoeff_RHS%n_Layers     .OR. &
        ZTauCoeff_LHS%n_Channels   /= ZTauCoeff_RHS%n_Channels        ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check the scalar components
    ! ---------------------------
    IF ( ZTauCoeff_LHS%Sensor_Id /= ZTauCoeff_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ZTauCoeff Sensor_Id values are different; >'//&
                            TRIM(ZTauCoeff_LHS%Sensor_Id)//'< vs >'//&
                            TRIM(ZTauCoeff_RHS%Sensor_Id)//'<', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ZTauCoeff_LHS%WMO_Satellite_Id /= ZTauCoeff_RHS%WMO_Satellite_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ZTauCoeff scalar component WMO_Satellite_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ZTauCoeff_LHS%WMO_Sensor_Id /= ZTauCoeff_RHS%WMO_Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ZTauCoeff scalar component WMO_Sensor_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    
    ! Check the array components
    ! --------------------------
    DO l = 1, ZTauCoeff_LHS%n_Channels
      IF ( ZTauCoeff_LHS%Sensor_Channel(l) /= ZTauCoeff_RHS%Sensor_Channel(l) ) THEN
        WRITE(Message,'("ZTauCoeff array component Sensor_Channel values ",&
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
    DO k = 0, ZTauCoeff_LHS%n_Layers
      IF ( .NOT. Compare_Float( ZTauCoeff_LHS%Level_Altitude(k), &
                                ZTauCoeff_RHS%Level_Altitude(k), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("ZTauCoeff array component Level_Altitude values ",&
                       &"are different at index (",1(1x,i0),"): ",&
                       &2(1x,es13.6))') &
                       k, ZTauCoeff_LHS%Level_Altitude(k), ZTauCoeff_RHS%Level_Altitude(k)
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO k = 0, ZTauCoeff_LHS%n_Layers
      IF ( .NOT. Compare_Float( ZTauCoeff_LHS%Level_Pressure(k), &
                                ZTauCoeff_RHS%Level_Pressure(k), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("ZTauCoeff array component Level_Pressure values ",&
                       &"are different at index (",1(1x,i0),"): ",&
                       &2(1x,es13.6))') &
                       k, ZTauCoeff_LHS%Level_Pressure(k), ZTauCoeff_RHS%Level_Pressure(k)
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO k = 1, ZTauCoeff_LHS%n_Layers
      IF ( .NOT. Compare_Float( ZTauCoeff_LHS%Pressure(k), &
                                ZTauCoeff_RHS%Pressure(k), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("ZTauCoeff array component Pressure values ",&
                       &"are different at index (",1(1x,i0),"): ",&
                       &2(1x,es13.6))') &
                       k, ZTauCoeff_LHS%Pressure(k), ZTauCoeff_RHS%Pressure(k)
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, ZTauCoeff_LHS%n_Channels
      IF ( ZTauCoeff_LHS%ChannelIndex(l) /= ZTauCoeff_RHS%ChannelIndex(l) ) THEN
        WRITE(Message,'("ZTauCoeff array component ChannelIndex values ",&
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
    DO l = 1, ZTauCoeff_LHS%n_Channels
      DO i = 0, ZTauCoeff_LHS%n_Predictors
        IF ( ZTauCoeff_LHS%PredictorIndex(i,l) /= ZTauCoeff_RHS%PredictorIndex(i,l) ) THEN
          WRITE(Message,'("ZTauCoeff array component PredictorIndex values ",&
                         &"are different at index (",2(1x,i0),")")') &
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
    DO l = 1, ZTauCoeff_LHS%n_Channels
      IF ( .NOT. Compare_Float( ZTauCoeff_LHS%Secant_Zenith(l), &
                                ZTauCoeff_RHS%Secant_Zenith(l), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("ZTauCoeff array component Secant_Zenith values ",&
                       &"are different at index (",1(1x,i0),"): ",&
                       &2(1x,es13.6))') &
                       l, ZTauCoeff_LHS%Secant_Zenith(l), ZTauCoeff_RHS%Secant_Zenith(l)
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, ZTauCoeff_LHS%n_Channels
      DO k = 1, ZTauCoeff_LHS%n_Layers
        DO i = 0, ZTauCoeff_LHS%n_Predictors
          IF ( .NOT. Compare_Float( ZTauCoeff_LHS%C(i,k,l), &
                                    ZTauCoeff_RHS%C(i,k,l), &
                                    ULP=ULP ) ) THEN
            WRITE(Message,'("ZTauCoeff array component C values ",&
                           &"are different at index (",3(1x,i0),"): ",&
                           &2(1x,es13.6))') &
                           i,k,l, ZTauCoeff_LHS%C(i,k,l), ZTauCoeff_RHS%C(i,k,l)
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM(Message), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        END DO
      END DO
    END DO
    
  END FUNCTION Equal_ZTauCoeff


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckRelease_ZTauCoeff
!
! PURPOSE:
!       Function to check the ZTauCoeff Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_ZTauCoeff( ZTauCoeff              , &  ! Input
!                                              RCS_Id     =RCS_Id     , &  ! Revision control
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ZTauCoeff:     ZTauCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ZTauCoeff_type
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

  FUNCTION CheckRelease_ZTauCoeff( ZTauCoeff  , &  ! Input
                                   RCS_Id     , &  ! Revision control
                                   Message_Log) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    TYPE(ZTauCoeff_type)  , INTENT(IN)  :: ZTauCoeff
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_ZTauCoeff'
    ! Local variables
    CHARACTER(256) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check release is not too old
    ! ----------------------------
    IF ( ZTauCoeff%Release < ZTAUCOEFF_RELEASE ) THEN
      WRITE( Message,'("An ZTauCoeff data update is needed. ",&
                      &"ZTauCoeff release is ",i0,&
                      &". Valid release is ",i0,"." )' ) &
                      ZTauCoeff%Release, ZTAUCOEFF_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check release is not too new
    ! ----------------------------
    IF ( ZTauCoeff%Release > ZTAUCOEFF_RELEASE ) THEN
      WRITE( Message,'("An ZTauCoeff software update is needed. ",&
                      &"ZTauCoeff release is ",i0,&
                      &". Valid release is ",i0,"." )' ) &
                      ZTauCoeff%Release, ZTAUCOEFF_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_ZTauCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Info_ZTauCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the ZTauCoeff data structure.
!
! CALLING SEQUENCE:
!       CALL Info_ZTauCoeff( ZTauCoeff    , &  ! Input
!                            Info         , &  ! Output
!                            RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       ZTauCoeff:     Filled ZTauCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       ZTauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed ZTauCoeff data structure.
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

  SUBROUTINE Info_ZTauCoeff( ZTauCoeff, &  ! Input
                             Info   , &  ! Output
                             RCS_Id   )  ! Revision control
    ! Arguments
    TYPE(ZTauCoeff_type)  , INTENT(IN)  :: ZTauCoeff
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
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required info to the local string
    ! -------------------------------------------
    FmtString='(a,1x,a,1x,"ZTauCoeff RELEASE.VERSION: ",i2,".",i2.2,2x,&
               &"N_PREDICTORS=",i0,2x,&
               &"N_LAYERS=",i0,2x,&
               &"N_CHANNELS=",i0)'
    WRITE(LongString, FMT=FmtString) &
          ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
          TRIM(ZTauCoeff%Sensor_ID), &
          ZTauCoeff%Release, ZTauCoeff%Version, &
          ZTauCoeff%n_Predictors, &
          ZTauCoeff%n_Layers    , &
          ZTauCoeff%n_Channels  

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_ZTauCoeff


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  SUBROUTINE Clear_ZTauCoeff(ZTauCoeff)
    TYPE(ZTauCoeff_type), INTENT(IN OUT) :: ZTauCoeff
    ZTauCoeff%Release = ZTAUCOEFF_RELEASE
    ZTauCoeff%Version = ZTAUCOEFF_VERSION
    ZTauCoeff%Sensor_Id        = ' '
    ZTauCoeff%WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    ZTauCoeff%WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE Clear_ZTauCoeff

END MODULE ZTauCoeff_Define
