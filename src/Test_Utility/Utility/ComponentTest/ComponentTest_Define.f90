!
! ComponentTest_Define
!
! Module defining the structures to hold CRTM component model
! (e.g. Forward/Tangent-Linear, Tangent-Linear/Adjoint, Adjoint/K-matrix)
! test results and containing routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 02-Mar-2006
!                       paul.vandelst@noaa.gov
!

MODULE ComponentTest_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
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
  ! Data structure definition
  PUBLIC :: ComponentTest_type
  ! Structure parameters
  PUBLIC :: COMPONENTTEST_TESTTYPE
  PUBLIC :: COMPONENTTEST_FWDTL_TESTTYPE
  PUBLIC :: COMPONENTTEST_TLAD_TESTTYPE
  PUBLIC :: COMPONENTTEST_ADK_TESTTYPE
  PUBLIC :: COMPONENTTEST_DATATYPE
  PUBLIC :: COMPONENTTEST_POLY_DATATYPE
  PUBLIC :: COMPONENTTEST_MONO_DATATYPE
  ! Structure procedures
  PUBLIC :: Associated_ComponentTest
  PUBLIC :: Destroy_ComponentTest
  PUBLIC :: Allocate_ComponentTest
  PUBLIC :: Assign_ComponentTest
  PUBLIC :: Equal_ComponentTest
  PUBLIC :: Info_ComponentTest
  PUBLIC :: CheckRelease_ComponentTest


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! Message string length
  INTEGER, PARAMETER :: ML = 512
  ! String lengths
  INTEGER, PARAMETER :: SL  = 20  ! Sensor id string length
  INTEGER, PARAMETER :: VSL = 64  ! Variable name string length
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: COMPONENTTEST_RELEASE = 1
  INTEGER, PARAMETER :: COMPONENTTEST_VERSION = 1

  ! Initialisation values for pointer components
  INTEGER,         PARAMETER :: IP_INIT = -1
  REAL(fp), PARAMETER :: FP_INIT = -999.0_fp
  ! The test types
  INTEGER, PARAMETER :: N_COMPONENTTEST_TESTTYPE = 3
  INTEGER, PARAMETER :: COMPONENTTEST_INVALID_TESTTYPE = 0  ! Invalid value
  INTEGER, PARAMETER :: COMPONENTTEST_FWDTL_TESTTYPE   = 1  ! FWD/TL model test
  INTEGER, PARAMETER :: COMPONENTTEST_TLAD_TESTTYPE    = 2  ! TL/AD model test
  INTEGER, PARAMETER :: COMPONENTTEST_ADK_TESTTYPE     = 3  ! AD/K model test
  INTEGER, PARAMETER :: COMPONENTTEST_TESTTYPE( 0:N_COMPONENTTEST_TESTTYPE ) = &
    (/ COMPONENTTEST_INVALID_TESTTYPE, &
       COMPONENTTEST_FWDTL_TESTTYPE,   &
       COMPONENTTEST_TLAD_TESTTYPE,    &
       COMPONENTTEST_ADK_TESTTYPE     /)
  ! The data types
  INTEGER, PARAMETER :: N_COMPONENTTEST_DATATYPE = 2
  INTEGER, PARAMETER :: COMPONENTTEST_INVALID_DATATYPE = 0  ! Invalid value
  INTEGER, PARAMETER :: COMPONENTTEST_POLY_DATATYPE    = 1  ! Polychromatic data
  INTEGER, PARAMETER :: COMPONENTTEST_MONO_DATATYPE    = 2  ! Monochromatic data
  INTEGER, PARAMETER :: COMPONENTTEST_DATATYPE( 0:N_COMPONENTTEST_DATATYPE ) = &
    (/ COMPONENTTEST_INVALID_DATATYPE, &
       COMPONENTTEST_POLY_DATATYPE,    &
       COMPONENTTEST_MONO_DATATYPE    /)


  ! -----------------------------------
  ! ComponentTest data type definitions
  ! -----------------------------------
  !:tdoc+:
  TYPE :: ComponentTest_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER :: Release = COMPONENTTEST_RELEASE
    INTEGER :: Version = COMPONENTTEST_VERSION
    ! Dimensions
    INTEGER :: nK  = 0  ! Layer dimension - For surface code test, this should be set to 1
    INTEGER :: nL  = 0  ! Spectral dimension
    INTEGER :: nP  = 0  ! Perturbation dimension - For non-FWD/TL tests, this should be set to 1
    INTEGER :: nIV = 0  ! Input variable dimension
    INTEGER :: nOV = 0  ! Output variable dimension
    ! Sensor id info
    CHARACTER(SL) :: Sensor_Id        = ' '                     
    INTEGER       :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    ! The instance for a given atmospheric profile (dataset) and its identifier
    INTEGER :: nM = 0
    CHARACTER(VSL) :: nM_Name = ' '
    ! The component test type (FWD/TL, TL/AD, or AD/K)
    INTEGER :: TestType = COMPONENTTEST_INVALID_TESTTYPE
    ! The data type (Sensor or Spectral)
    INTEGER :: DataType = COMPONENTTEST_INVALID_DATATYPE
    ! Pressure, spectral dimension, and perturbation amount identifiers.
    ! For POLY data type, Spectral == Instrument channel number
    !     MONO data type, Spectral == Frequency (either cm^-1 or GHz)
    REAL(fp), POINTER :: Pressure(:)     => NULL()  ! nK
    REAL(fp), POINTER :: Spectral(:)     => NULL()  ! nL
    REAL(fp), POINTER :: Perturbation(:) => NULL()  ! nP
    ! The input variable names and units
    CHARACTER(VSL), POINTER :: Input_Variable_Name(:)  => NULL()  ! nIV
    CHARACTER(VSL), POINTER :: Input_Variable_Units(:) => NULL()  ! nIV
    ! The output variable names and units
    CHARACTER(VSL), POINTER :: Output_Variable_Name(:)  => NULL()  ! nOV
    CHARACTER(VSL), POINTER :: Output_Variable_Units(:) => NULL()  ! nOV
    ! The test results
    REAL(fp), POINTER :: d1( :,:,:,:,: ) => NULL()  ! K x L x nP x nIV x nOV
    REAL(fp), POINTER :: d2( :,:,:,:,: ) => NULL()  ! K x L x nP x nIV x nOV
  END TYPE ComponentTest_type
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
!       Associated_ComponentTest
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       ComponentTest structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_ComponentTest( ComponentTest    , &
!                                                      ANY_Test=Any_Test  )
!
! INPUT ARGUMENTS:
!       ComponentTest:       ComponentTest structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       ComponentTest_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            ComponentTest structure pointer members are associated.
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
!                            association status of the ComponentTest pointer members.
!                            .TRUE.  - if ALL the ComponentTest pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the ComponentTest pointer
!                                      members are associated.
!                            .FALSE. - some or all of the ComponentTest pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Associated_ComponentTest( ComponentTest, & ! Input
                                     ANY_Test     ) & ! Optional input
                                   RESULT(Association_Status)
    ! Arguments
    TYPE(ComponentTest_type), INTENT(IN) :: ComponentTest
    INTEGER,        OPTIONAL, INTENT(IN) :: ANY_Test
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
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED(ComponentTest%Pressure             ) .AND. &
           ASSOCIATED(ComponentTest%Spectral             ) .AND. &
           ASSOCIATED(ComponentTest%Perturbation         ) .AND. &
           ASSOCIATED(ComponentTest%Input_Variable_Name  ) .AND. &
           ASSOCIATED(ComponentTest%Input_Variable_Units ) .AND. &
           ASSOCIATED(ComponentTest%Output_Variable_Name ) .AND. &
           ASSOCIATED(ComponentTest%Output_Variable_Units) .AND. &
           ASSOCIATED(ComponentTest%d1                   ) .AND. &
           ASSOCIATED(ComponentTest%d2                   )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED(ComponentTest%Pressure             ) .OR. &
           ASSOCIATED(ComponentTest%Spectral             ) .OR. &
           ASSOCIATED(ComponentTest%Perturbation         ) .OR. &
           ASSOCIATED(ComponentTest%Input_Variable_Name  ) .OR. &
           ASSOCIATED(ComponentTest%Input_Variable_Units ) .OR. &
           ASSOCIATED(ComponentTest%Output_Variable_Name ) .OR. &
           ASSOCIATED(ComponentTest%Output_Variable_Units) .OR. &
           ASSOCIATED(ComponentTest%d1                   ) .OR. &
           ASSOCIATED(ComponentTest%d2                   )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF
  END FUNCTION Associated_ComponentTest


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Destroy_ComponentTest
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of ComponentTest
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_ComponentTest( ComponentTest          , &
!                                             RCS_Id     =RCS_Id     , &
!                                             Message_Log=Message_Log  )
!
! OUTPUT ARGUMENTS:
!       ComponentTest:  Re-initialized ComponentTest structure.
!                       UNITS:      N/A
!                       TYPE:       ComponentTest_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output ComponentTest argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Destroy_ComponentTest( ComponentTest, &  ! Output
                                  No_Clear     , &  ! Optional input
                                  RCS_Id       , &  ! Revision control
                                  Message_Log  ) &  ! Error messaging
                                RESULT(Error_Status)
    ! Arguments
    TYPE(ComponentTest_type), INTENT(IN OUT) :: ComponentTest
    INTEGER,        OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),   OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_ComponentTest'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Reset the dimension indicators
    ComponentTest%nK  = 0
    ComponentTest%nL  = 0
    ComponentTest%nP  = 0
    ComponentTest%nIV = 0
    ComponentTest%nOV = 0

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == 1 ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_ComponentTest(ComponentTest)
    
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_ComponentTest(ComponentTest) ) RETURN

    
    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( ComponentTest%Pressure             , &
                ComponentTest%Spectral             , &
                ComponentTest%Perturbation         , &
                ComponentTest%Input_Variable_Name  , &
                ComponentTest%Input_Variable_Units , &
                ComponentTest%Output_Variable_Name , &
                ComponentTest%Output_Variable_Units, &
                ComponentTest%d1                   , &
                ComponentTest%d2                   , &
                STAT=Allocate_Status                 )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error deallocating ComponentTest. STAT = ",i0)') Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    ComponentTest%n_Allocates = ComponentTest%n_Allocates - 1
    IF ( ComponentTest%n_Allocates /= 0 ) THEN
      WRITE( Message, '("Allocation counter /= 0, Value = ",i0)') &
                      ComponentTest%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Destroy_ComponentTest


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Allocate_ComponentTest
! 
! PURPOSE:
!       Function to allocate the pointer members of the ComponentTest
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_ComponentTest( nK, nL, nP, nIV, nOv,  , &
!                                              ComponentTest          , &
!                                              RCS_Id     =RCS_Id     , &
!                                              Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       nK:                   The number of layers dimension of the
!                             ComponentTest data. For surface component tests,
!                             should be set to 1.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT(IN)
!
!       nL:                   The spectral dimension (channels/frequencies) of
!                             the ComponentTest data.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT(IN)
!
!       nP:                   The number of perturbations dimension of the
!                             ComponentTest data. For non-FWD/TL component tests,
!                             should be set to 1.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT(IN)
!
!       nIV:                  The number of input variables dimension of the
!                             ComponentTest data.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT(IN)
!
!       nOV:                  The number of output variables dimension of the
!                             ComponentTest data.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ComponentTest:        ComponentTest structure with allocated pointer members
!                             UNITS:      N/A
!                             TYPE:       ComponentTest_type
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:          Character string specifying a filename in which any
!                             messages will be logged. If not specified, or if an
!                             error occurs opening the log file, the default action
!                             is to output messages to standard output.
!                             UNITS:      None
!                             TYPE:       CHARACTER(*)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:               Character string containing the Revision Control
!                             System Id field for the module.
!                             UNITS:      None
!                             TYPE:       CHARACTER(*)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:         The return value is an integer defining the error status.
!                             The error codes are defined in the ERROR_HANDLER module.
!                             If == SUCCESS the structure pointer allocations were
!                                           successful
!                                == FAILURE - an error occurred, or
!                                           - the structure internal allocation counter
!                                             is not equal to one (1) upon exiting this
!                                             function. This value is incremented and
!                                             decremented for every structure allocation
!                                             and deallocation respectively.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output ComponentTest argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Allocate_ComponentTest( nK           , &  ! Input
                                   nL           , &  ! Input
                                   nP           , &  ! Input
                                   nIV          , &  ! Input
                                   nOV          , &  ! Input
                                   ComponentTest, &  ! Output
                                   RCS_Id       , &  ! Revision control
                                   Message_Log  ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    INTEGER,                    INTENT(IN)     :: nK           
    INTEGER,                    INTENT(IN)     :: nL         
    INTEGER,                    INTENT(IN)     :: nP    
    INTEGER,                    INTENT(IN)     :: nIV  
    INTEGER,                    INTENT(IN)     :: nOV  
    TYPE(ComponentTest_type), INTENT(IN OUT) :: ComponentTest
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),   OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_ComponentTest'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Check dimensions
    IF ( nK  < 1 .OR. &
         nL  < 1 .OR. &
         nP  < 1 .OR. &
         nIV < 1 .OR. &
         nOV < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_ComponentTest( ComponentTest, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_ComponentTest( ComponentTest, &               
                                            No_Clear=SET, &            
                                            Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating ComponentTest prior to allocation.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Perform the pointer allocation
    ! ------------------------------
    ALLOCATE( ComponentTest%Pressure(1:nK)                , &
              ComponentTest%Spectral(1:nL)                , &
              ComponentTest%Perturbation(1:nP)            , &
              ComponentTest%Input_Variable_Name(1:nIV)    , &
              ComponentTest%Input_Variable_Units(1:nIV)   , &
              ComponentTest%Output_Variable_Name(1:nOV)   , &
              ComponentTest%Output_Variable_Units(1:nOV)  , &
              ComponentTest%d1(1:nK,1:nL,1:nP,1:nIV,1:nOV), &
              ComponentTest%d2(1:nK,1:nL,1:nP,1:nIV,1:nOV), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error allocating ComponentTest. STAT = ",i0)') Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the dimensions
    ! ---------------------
    ComponentTest%nK  = nK
    ComponentTest%nL  = nL
    ComponentTest%nP  = nP
    ComponentTest%nIV = nIV
    ComponentTest%nOV = nOV


    ! Initialise the arrays
    ! ---------------------
    ComponentTest%Pressure              = ZERO
    ComponentTest%Spectral              = ZERO
    ComponentTest%Perturbation          = ZERO
    ComponentTest%Input_Variable_Name   = ' '
    ComponentTest%Input_Variable_Units  = ' '
    ComponentTest%Output_Variable_Name  = ' '
    ComponentTest%Output_Variable_Units = ' '
    ComponentTest%d1                    = ZERO
    ComponentTest%d2                    = ZERO


    ! Increment and test the allocation counter
    ! -----------------------------------------
    ComponentTest%n_Allocates = ComponentTest%n_Allocates + 1
    IF ( ComponentTest%n_Allocates /= 1 ) THEN
      WRITE( Message, '("Allocation counter /= 1, Value = ",i0)') &
                      ComponentTest%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_ComponentTest
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Assign_ComponentTest
!
! PURPOSE:
!       Function to copy valid ComponentTest structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_ComponentTest( ComponentTest_in       , &
!                                            ComponentTest_out      , &
!                                            RCS_Id     =RCS_Id     , &
!                                            Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       ComponentTest_in:   ComponentTest structure which is to be copied.
!                           UNITS:      N/A
!                           TYPE:       ComponentTest_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ComponentTest_out:  Copy of the input structure, ComponentTest_in.
!                           UNITS:      N/A
!                           TYPE:       ComponentTest_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
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
!                           If == SUCCESS the structure assignment was successful
!                              == FAILURE an error occurred
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output ComponentTest argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Assign_ComponentTest( ComponentTest_in , &  ! Input
                                 ComponentTest_out, &  ! Output
                                 RCS_Id           , &  ! Revision control
                                 Message_Log      ) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    TYPE(ComponentTest_type), INTENT(IN)     :: ComponentTest_in
    TYPE(ComponentTest_type), INTENT(IN OUT) :: ComponentTest_out
    CHARACTER(*), OPTIONAL  , INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL  , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_ComponentTest'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_ComponentTest( ComponentTest_in ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ComponentTest_in pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Allocate data arrays
    ! --------------------
    Error_Status = Allocate_ComponentTest( ComponentTest_in%nK , &
                                           ComponentTest_in%nL , &
                                           ComponentTest_in%nP , &
                                           ComponentTest_in%nIV, &
                                           ComponentTest_in%nOV, &
                                           ComponentTest_out, &
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
    ComponentTest_out%Release          = ComponentTest_in%Release
    ComponentTest_out%Version          = ComponentTest_in%Version
    ComponentTest_out%Sensor_Id        = ComponentTest_in%Sensor_Id
    ComponentTest_out%WMO_Satellite_Id = ComponentTest_in%WMO_Satellite_Id
    ComponentTest_out%WMO_Sensor_Id    = ComponentTest_in%WMO_Sensor_Id
    ComponentTest_out%nM               = ComponentTest_in%nM      
    ComponentTest_out%nM_Name          = ComponentTest_in%nM_Name 
    ComponentTest_out%TestType         = ComponentTest_in%TestType
    ComponentTest_out%DataType         = ComponentTest_in%DataType

    
    ! Copy array data
    ! ---------------
    ComponentTest_out%Pressure              = ComponentTest_in%Pressure             
    ComponentTest_out%Spectral              = ComponentTest_in%Spectral             
    ComponentTest_out%Perturbation          = ComponentTest_in%Perturbation         
    ComponentTest_out%Input_Variable_Name   = ComponentTest_in%Input_Variable_Name  
    ComponentTest_out%Input_Variable_Units  = ComponentTest_in%Input_Variable_Units 
    ComponentTest_out%Output_Variable_Name  = ComponentTest_in%Output_Variable_Name 
    ComponentTest_out%Output_Variable_Units = ComponentTest_in%Output_Variable_Units
    ComponentTest_out%d1                    = ComponentTest_in%d1                   
    ComponentTest_out%d2                    = ComponentTest_in%d2                   
    
  END FUNCTION Assign_ComponentTest


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Equal_ComponentTest
!
! PURPOSE:
!       Function to test if two ComponentTest structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_ComponentTest( ComponentTest_LHS      , &
!                                           ComponentTest_RHS      , &
!                                           ULP_Scale  =ULP_Scale  , &
!                                           Check_All  =Check_All  , &
!                                           RCS_Id     =RCS_Id     , &
!                                           Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       ComponentTest_LHS:  ComponentTest structure to be compared; equivalent
!                           to the left-hand side of a lexical comparison, e.g.
!                             IF ( ComponentTest_LHS == ComponentTest_RHS ).
!                           UNITS:      N/A
!                           TYPE:       ComponentTest_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ComponentTest_RHS:  ComponentTest structure to be compared to; equivalent
!                           to right-hand side of a lexical comparison, e.g.
!                             IF ( ComponentTest_LHS == ComponentTest_RHS ).
!                           UNITS:      N/A
!                           TYPE:       ComponentTest_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:          Unit of data precision used to scale the floating
!                           point comparison. ULP stands for "Unit in the Last Place,"
!                           the smallest possible increment or decrement that can be
!                           made using a machine's floating point arithmetic.
!                           Value must be positive - if a negative value is supplied,
!                           the absolute value is used. If not specified, the default
!                           value is 1.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:          Set this argument to check ALL the floating point
!                           channel data of the ComponentTest structures. The default
!                           action is return with a FAILURE status as soon as
!                           any difference is found. This optional argument can
!                           be used to get a listing of ALL the differences
!                           between data in ComponentTest structures.
!                           If == 0, Return with FAILURE status as soon as
!                                    ANY difference is found  *DEFAULT*
!                              == 1, Set FAILURE status if ANY difference is
!                                    found, but continue to check ALL data.
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
!                           If == SUCCESS the structures were equal
!                              == FAILURE - an error occurred, or
!                                         - the structures were different.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Equal_ComponentTest( ComponentTest_LHS, &  ! Input
                                ComponentTest_RHS, &  ! Input
                                ULP_Scale        , &  ! Optional input
                                Check_All        , &  ! Optional input
                                RCS_Id           , &  ! Revision control
                                Message_Log      ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(ComponentTest_type), INTENT(IN)  :: ComponentTest_LHS
    TYPE(ComponentTest_type), INTENT(IN)  :: ComponentTest_RHS
    INTEGER     , OPTIONAL  , INTENT(IN)  :: ULP_Scale
    INTEGER     , OPTIONAL  , INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL  , INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL  , INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_ComponentTest'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: i, j, k, l, m

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
    IF ( .NOT. Associated_ComponentTest( ComponentTest_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ComponentTest_LHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_ComponentTest( ComponentTest_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ComponentTest_RHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Check dimensions
    ! ----------------
    IF ( ComponentTest_LHS%nK  /= ComponentTest_RHS%nK  .OR. &
         ComponentTest_LHS%nL  /= ComponentTest_RHS%nL  .OR. &
         ComponentTest_LHS%nP  /= ComponentTest_RHS%nP  .OR. &
         ComponentTest_LHS%nIV /= ComponentTest_RHS%nIV .OR. &
         ComponentTest_LHS%nOV /= ComponentTest_RHS%nOV      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the scalar components
    ! ---------------------------
    IF ( ComponentTest_LHS%Sensor_Id /= ComponentTest_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ComponentTest Sensor_Id values are different; >'//&
                            TRIM(ComponentTest_LHS%Sensor_Id)//'< vs >'//&
                            TRIM(ComponentTest_RHS%Sensor_Id)//'<', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ComponentTest_LHS%WMO_Satellite_Id /= ComponentTest_RHS%WMO_Satellite_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ComponentTest scalar component WMO_Satellite_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ComponentTest_LHS%WMO_Sensor_Id /= ComponentTest_RHS%WMO_Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ComponentTest scalar component WMO_Sensor_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ComponentTest_LHS%nM /= ComponentTest_RHS%nM ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ComponentTest scalar component nM values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ComponentTest_LHS%nM_Name /= ComponentTest_RHS%nM_Name ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ComponentTest nM_Name values are different; >'//&
                            TRIM(ComponentTest_LHS%nM_Name)//'< vs >'//&
                            TRIM(ComponentTest_RHS%nM_Name)//'<', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ComponentTest_LHS%TestType /= ComponentTest_RHS%TestType ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ComponentTest scalar component TestType values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( ComponentTest_LHS%DataType /= ComponentTest_RHS%DataType ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ComponentTest scalar component DataType values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    
    ! Check the array components
    ! --------------------------
    DO i = 1, ComponentTest_LHS%nK
      IF ( .NOT. Compare_Float( ComponentTest_LHS%Pressure(i), &
                                ComponentTest_RHS%Pressure(i), &
                                ULP=ULP ) ) THEN
        WRITE( Message,'("ComponentTest array component Pressure values ",&
                        &"are different at indices (",1(1x,i0),")")') i
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO i = 1, ComponentTest_LHS%nL
      IF ( .NOT. Compare_Float( ComponentTest_LHS%Spectral(i), &
                                ComponentTest_RHS%Spectral(i), &
                                ULP=ULP ) ) THEN
        WRITE( Message,'("ComponentTest array component Spectral values ",&
                        &"are different at indices (",1(1x,i0),")")') i
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO i = 1, ComponentTest_LHS%nP
      IF ( .NOT. Compare_Float( ComponentTest_LHS%Perturbation(i), &
                                ComponentTest_RHS%Perturbation(i), &
                                ULP=ULP ) ) THEN
        WRITE( Message,'("ComponentTest array component Perturbation values ",&
                        &"are different at indices (",1(1x,i0),")")') i
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO i = 1, ComponentTest_LHS%nIV
      IF ( ComponentTest_LHS%Input_Variable_Name(i) /= &
           ComponentTest_RHS%Input_Variable_Name(i)    ) THEN
        WRITE(Message,'("ComponentTest array component Input_Variable_Name values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO i = 1, ComponentTest_LHS%nIV
      IF ( ComponentTest_LHS%Input_Variable_Units(i) /= &
           ComponentTest_RHS%Input_Variable_Units(i)    ) THEN
        WRITE(Message,'("ComponentTest array component Input_Variable_Units values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO i = 1, ComponentTest_LHS%nOV
      IF ( ComponentTest_LHS%Output_Variable_Name(i) /= &
           ComponentTest_RHS%Output_Variable_Name(i)    ) THEN
        WRITE(Message,'("ComponentTest array component Output_Variable_Name values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO i = 1, ComponentTest_LHS%nOV
      IF ( ComponentTest_LHS%Output_Variable_Units(i) /= &
           ComponentTest_RHS%Output_Variable_Units(i)    ) THEN
        WRITE(Message,'("ComponentTest array component Output_Variable_Units values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        l
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO m = 1, ComponentTest_LHS%nOV
      DO l = 1, ComponentTest_LHS%nIV
        DO k = 1, ComponentTest_LHS%nP
          DO j = 1, ComponentTest_LHS%nL
            DO i = 1, ComponentTest_LHS%nK
              IF ( .NOT. Compare_Float( ComponentTest_LHS%d1(i,j,k,l,m), &
                                        ComponentTest_RHS%d1(i,j,k,l,m), &
                                        ULP=ULP ) ) THEN
                WRITE( Message,'("ComponentTest array component d1 values ",&
                                &"are different at indices (",5(1x,i0),")")') i,j,k,l,m
                Error_Status = FAILURE
                CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
                IF ( Check_Once ) RETURN
              END IF
            END DO
          END DO
        END DO
      END DO
    END DO
    DO m = 1, ComponentTest_LHS%nOV
      DO l = 1, ComponentTest_LHS%nIV
        DO k = 1, ComponentTest_LHS%nP
          DO j = 1, ComponentTest_LHS%nL
            DO i = 1, ComponentTest_LHS%nK
              IF ( .NOT. Compare_Float( ComponentTest_LHS%d2(i,j,k,l,m), &
                                        ComponentTest_RHS%d2(i,j,k,l,m), &
                                        ULP=ULP ) ) THEN
                WRITE( Message,'("ComponentTest array component d2 values ",&
                                &"are different at indices (",5(1x,i0),")")') i,j,k,l,m
                Error_Status = FAILURE
                CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
                IF ( Check_Once ) RETURN
              END IF
            END DO
          END DO
        END DO
      END DO
    END DO
    
  END FUNCTION Equal_ComponentTest


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CheckRelease_ComponentTest
!
! PURPOSE:
!       Function to check the ComponentTest Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_ComponentTest( ComponentTest          , &
!                                                  RCS_Id     =RCS_Id     , &
!                                                  Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       ComponentTest: ComponentTest structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ComponentTest_type
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
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION CheckRelease_ComponentTest( ComponentTest, &  ! Input
                                       RCS_Id       , &  ! Revision control
                                       Message_Log  ) &  ! Error messaging
                                     RESULT( Error_Status )
    ! Arguments
    TYPE(ComponentTest_type), INTENT(IN)  :: ComponentTest
    CHARACTER(*), OPTIONAL  , INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL  , INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_ComponentTest'
    ! Local variables
    CHARACTER(ML) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check release is not too old
    ! ----------------------------
    IF ( ComponentTest%Release < COMPONENTTEST_RELEASE ) THEN
      WRITE( Message, '( "A ComponentTest data update is needed. ", &
                        &"ComponentTest release is ", i0, &
                        &". Valid release is ",i0,"." )' ) &
                      ComponentTest%Release, COMPONENTTEST_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

    ! Check release is not too new
    ! ----------------------------
    IF ( ComponentTest%Release > COMPONENTTEST_RELEASE ) THEN
      WRITE( Message, '( "A ComponentTest software update is needed. ", &
                        &"ComponentTest release is ", i0, &
                        &". Valid release is ",i0,"." )' ) &
                      ComponentTest%Release, COMPONENTTEST_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_ComponentTest


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Info_ComponentTest
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the ComponentTest data structure.
!
! CALLING SEQUENCE:
!       CALL Info_ComponentTest( ComponentTest, &
!                                Info         , &
!                                RCS_Id=RCS_Id  )
!
! INPUT ARGUMENTS:
!       ComponentTest:      Filled ComponentTest structure.
!                      UNITS:      N/A
!                      TYPE:       ComponentTest_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed ComponentTest data structure.
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
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Info_ComponentTest( ComponentTest, &  ! Input
                                 Info         , &  ! Output
                                 RCS_Id         )  ! Revision control
    ! Arguments
    TYPE(ComponentTest_type), INTENT(IN)  :: ComponentTest
    CHARACTER(*)            , INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL  , INTENT(OUT) :: RCS_Id
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
    FmtString='(a,1x,a,1x,"ComponentTest RELEASE.VERSION: ",i2,".",i2.2,2x,&
               &"NK=",i0," NL=",i0," NP=",i0," NIV=",i0," NOV=",i0)'
    WRITE( LongString, FMT=FmtString ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           TRIM(ComponentTest%Sensor_ID), &
           ComponentTest%Release, ComponentTest%Version, &
           ComponentTest%nK , &
           ComponentTest%nL , &
           ComponentTest%nP , &
           ComponentTest%nIV, &
           ComponentTest%nOV
                                           

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_ComponentTest
  

!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  SUBROUTINE Clear_ComponentTest( ComponentTest )
    TYPE(ComponentTest_type), INTENT(IN OUT) :: ComponentTest
    ComponentTest%Release          = COMPONENTTEST_RELEASE
    ComponentTest%Version          = COMPONENTTEST_VERSION
    ComponentTest%Sensor_Id        = ' '
    ComponentTest%WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    ComponentTest%WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    ComponentTest%nM               = 0
    ComponentTest%nM_Name          = ' '
    ComponentTest%TestType         = COMPONENTTEST_INVALID_TESTTYPE
    ComponentTest%DataType         = COMPONENTTEST_INVALID_DATATYPE
  END SUBROUTINE Clear_ComponentTest

END MODULE ComponentTest_Define
