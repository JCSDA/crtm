!
! TauCoeff_Define
!
! Module defining the TauCoeff data structure and containing routines to 
! manipulate it.
!       
!
! *!IMPORTANT!*
! -------------
! Note that the TauCoeff_type is PUBLIC and its members are not
! encapsulated; that is, they can be fully accessed outside the
! scope of this module. This makes it possible to manipulate
! the structure and its data directly rather than, for e.g., via
! get() and set() functions. This was done to eliminate the
! overhead of the get/set type of structure access in using the
! structure. *But*, it is recommended that the user initialize,
! destroy, allocate, assign, and concatenate the structure
! using only the routines in this module where possible to
! eliminate -- or at least minimise -- the possibility of 
! memory leakage since most of the structure members are
! pointers.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!
!       Updated by:     David Groff, EMC/SAIC Oct-2009
!                       david.groff@noaa.gov
!

MODULE TauCoeff_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE Sort_Utility,          ONLY: InsertionSort
  USE ODAS_Define,           ONLY: ODAS_type
  USE ODPS_Define,           ONLY: ODPS_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: N_TAUCOEFF_ITEMS
  PUBLIC :: TAUCOEFF_DATA_TYPE
  PUBLIC :: TAUCOEFF_DATA_NAME
  ! Datatypes
  PUBLIC :: TauCoeff_type
  ! Procedures
  PUBLIC :: Associated_TauCoeff
!  PUBLIC :: Destroy_TauCoeff
  PUBLIC :: Allocate_TauCoeff
!  PUBLIC :: Assign_TauCoeff
!  PUBLIC :: Concatenate_Channel_TauCoeff
!  PUBLIC :: Concatenate_Absorber_TauCoeff
!  PUBLIC :: Equal_TauCoeff
!  PUBLIC :: Check_TauCoeff_Release
!  PUBLIC :: Count_TauCoeff_Sensors
!  PUBLIC :: Info_TauCoeff


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! TauCoeff init values
  REAL(Double), PARAMETER :: FP_INIT = 0.0_Double
  INTEGER,      PARAMETER :: IP_INIT = -1
  ! Sensor descriptor component string length
  INTEGER, PARAMETER :: DL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: TAUCOEFF_RELEASE = 5  ! This determines structure and file formats.
  INTEGER, PARAMETER :: TAUCOEFF_VERSION = 4  ! This is just the data version.
  ! Number of TauCoeff data items
  INTEGER(Long), PARAMETER :: N_TAUCOEFF_ITEMS = 12_Long
  ! Internal data type descriptors for the TauCoeff data
  !    5 = Double (i.e. 8-byte float)
  !    4 = Single (i.e. 4-byte float)
  !    3 = Long   (i.e. 4-byte integer)
  INTEGER(Long), PARAMETER, DIMENSION( N_TAUCOEFF_ITEMS ) :: &
    TAUCOEFF_DATA_TYPE = (/ 7_Long, &  ! Sensor_Descriptor
                            3_Long, &  ! NCEP_Sensor_ID
                            3_Long, &  ! WMO_Satellite_ID
                            3_Long, &  ! WMO_Sensor_ID
                            3_Long, &  ! Sensor_Channel
                            3_Long, &  ! Absorber_ID
                            5_Long, &  ! Alpha
                            5_Long, &  ! Alpha_C1
                            5_Long, &  ! Alpha_C2
                            3_Long, &  ! Order_Index
                            3_Long, &  ! Predictor_Index
                            5_Long /)  ! C
  ! Names of the data items (for error processing)
  CHARACTER(*), PARAMETER, DIMENSION( N_TAUCOEFF_ITEMS ) :: &
    TAUCOEFF_DATA_NAME = (/ 'Sensor_Descriptor', &
                            'NCEP_Sensor_ID   ', &
                            'WMO_Satellite_ID ', &
                            'WMO_Sensor_ID    ', &
                            'Sensor_Channel   ', &
                            'Absorber_ID      ', &
                            'Alpha            ', &
                            'Alpha_C1         ', &
                            'Alpha_C2         ', &
                            'Order_Index      ', &
                            'Predictor_Index  ', &
                            'Tau_Coefficients ' /)
  
  ! -----------------------
  ! Derived type definition  
  ! -----------------------
  TYPE :: TauCoeff_type
    INTEGER :: n_Allocates = 0
    ! -- Array dimensions
    INTEGER( Long ) :: n_Sensors = 0       ! n
    INTEGER( Long ) :: n_ODAS    = 0       ! I1
    INTEGER( Long ) :: n_ODPS    = 0       ! I2
!    ### More dimension variables for additional algorithms ###

    ! Algorithm_ID:   Algorithm ID
    ! Sensor_Index:   Global sensor index
    ! Sensor_LoIndex: Local sensor index for a collection of sensor using 
    !                 the same algorithm
    INTEGER, POINTER :: Algorithm_ID(:)   =>NULL()  ! n
    INTEGER, POINTER :: Sensor_Index(:)   =>NULL()  ! n
    INTEGER, POINTER :: Sensor_LoIndex(:) =>NULL()  ! n

    TYPE( ODAS_type ), POINTER  :: ODAS(:)  =>NULL()  ! I1
    TYPE( ODPS_type ), POINTER  :: ODPS(:)  =>NULL()  ! I2
!    ### More dstructure variables for additional algorithms ###
  
  END TYPE TauCoeff_type


  ! ------------------------------
  ! TauCoeff data type definition
  ! ------------------------------
!  TYPE :: TauCoeff_type
!    INTEGER :: n_Allocates = 0
!    ! Release and version information
!    INTEGER(Long) :: Release = TAUCOEFF_RELEASE
!    INTEGER(Long) :: Version = TAUCOEFF_VERSION
!    ! Array dimensions
!    INTEGER(Long) :: n_Orders     = 0    ! Iorder
!    INTEGER(Long) :: n_Predictors = 0    ! Iuse
!    INTEGER(Long) :: n_Absorbers  = 0    ! J
!    INTEGER(Long) :: n_Channels   = 0    ! L
!    INTEGER(Long) :: StrLen       = DL
!    ! Number of different satellite/sensor combinations
!    INTEGER(Long) :: n_Sensors = 0
!    ! The satellite and sensor IDs
!    CHARACTER(DL), POINTER, DIMENSION(:) :: Sensor_Descriptor => NULL() ! L
!    INTEGER(Long), POINTER, DIMENSION(:) :: NCEP_Sensor_ID    => NULL() ! L
!    INTEGER(Long), POINTER, DIMENSION(:) :: WMO_Satellite_ID  => NULL() ! L
!    INTEGER(Long), POINTER, DIMENSION(:) :: WMO_Sensor_ID     => NULL() ! L
!    ! The actual sensor channel numbers
!    INTEGER(Long), POINTER, DIMENSION(:) :: Sensor_Channel => NULL()    ! L
!    ! The absorber ID
!    INTEGER(Long), POINTER, DIMENSION(:) :: Absorber_ID => NULL()    ! J
!    ! The absorber space function values
!    REAL(Double),  POINTER, DIMENSION(:) :: Alpha    => NULL()       ! J
!    REAL(Double),  POINTER, DIMENSION(:) :: Alpha_C1 => NULL()       ! J
!    REAL(Double),  POINTER, DIMENSION(:) :: Alpha_C2 => NULL()       ! J
!    ! The polynomial order index array.
!    ! This array identifies the order of the polynomial used to
!    ! reconstruct the regression coefficients. For each predictor
!    ! (Iuse), each absorber (J) and each channel (L) a different
!    ! order of polynomial can be specified.
!    INTEGER(Long), POINTER, DIMENSION(:,:,:) :: Order_Index => NULL()  ! 0:Iuse x J x L
!    ! The predictor index array.
!    ! This array identifies which subset (Iuse) of the total number
!    ! number of predictors are used to compute the absorption coefficient
!    ! for absorber (J) and each channel (L). If Predictor_Index(0,:,:) is
!    ! less than 0, this is an indication that there is NO absorption for
!    ! the selected absorber in the current channel.
!    INTEGER(Long), POINTER, DIMENSION(:,:,:)    :: Predictor_Index => NULL()  ! 0:Iuse x J x L
!    ! The array of coefficients
!    REAL(Double),  POINTER, DIMENSION(:,:,:,:) :: C => NULL() ! 0:Iorder x 0:Iuse x J x L
!  END TYPE TauCoeff_type


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
!       Clear_TauCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of a TauCoeff structure.
!
! CALLING SEQUENCE:
!       CALL Clear_TauCoeff( TauCoeff ) ! Output
!
! OUTPUT ARGUMENTS:
!       TauCoeff:    TauCoeff structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       TauCoeff_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output TauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

!  SUBROUTINE Clear_TauCoeff( TauCoeff )
!    TYPE(TauCoeff_type), INTENT(IN OUT) :: TauCoeff
!    TauCoeff%n_Orders     = 0
!    TauCoeff%n_Predictors = 0
!    TauCoeff%n_Absorbers  = 0
!    TauCoeff%n_Channels   = 0
!    TauCoeff%StrLen       = DL
!    TauCoeff%n_Sensors = 0
!  END SUBROUTINE Clear_TauCoeff

!--------------------------------------------------------------------------------
!
! NAME:
!       Associated_TauCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       TauCoeff structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_TauCoeff( TauCoeff,           &  ! Input
!                                                 ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       TauCoeff:    TauCoeff structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       TauCoeff_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:       
!       ANY_Test:    Set this argument to test if ANY of the
!                    TauCoeff structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    .TRUE. - Test if any of the pointer members are 
!                             associated 
!                    .FALSE. - Test all pointer members that should
!                              be associated according to TauCoeff's
!                              dimensions
!                    UNITS:     N/A
!                    TYPE:      LOGICAL
!                    DIMENSION: Scalar
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the TauCoeff pointer members.
!                            .TRUE.  - if ALL the TauCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the TauCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the TauCoeff pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------
  FUNCTION Associated_TauCoeff( TauCoeff,  & ! Input
                                ANY_Test ) & ! Optional input
                              RESULT( Association_Status )
    ! Arguments
    TYPE(TauCoeff_type), INTENT(IN) :: TauCoeff
    LOGICAL,   OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test
       
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test ) ALL_Test = .FALSE.
    END IF

    ! Test the structure associations
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( TauCoeff%Algorithm_ID    ) .AND. &
           ASSOCIATED( TauCoeff%Sensor_Index    ) .AND. &
           ASSOCIATED( TauCoeff%Sensor_LoIndex  ) .AND. &
           ASSOCIATED( TauCoeff%ODAS            ) .AND. &
           ASSOCIATED( TauCoeff%ODPS            )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( TauCoeff%Algorithm_ID    ) .OR. &
           ASSOCIATED( TauCoeff%Sensor_Index    ) .OR. &
           ASSOCIATED( TauCoeff%Sensor_LoIndex  ) .OR. &
           ASSOCIATED( TauCoeff%ODAS            ) .OR. &
           ASSOCIATED( TauCoeff%ODPS            )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF
    
  END FUNCTION Associated_TauCoeff
!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_TauCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of TauCoeff
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_TauCoeff( TauCoeff,                 &  ! Output
!                                        RCS_Id = RCS_Id,          &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
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
! OUTPUT ARGUMENTS:
!       TauCoeff:     Re-initialized TauCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output TauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------
!  FUNCTION Destroy_TauCoeff( TauCoeff,     &  ! Output
!                             No_Clear,     &  ! Optional input
!                             RCS_Id,       &  ! Revision control
!                             Message_Log ) &  ! Error messaging
!                            RESULT( Error_Status )
!    ! Arguments
!    TYPE(TauCoeff_type),    INTENT(IN OUT) :: TauCoeff
!    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
!    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_TauCoeff'
!    ! Local variables
!    CHARACTER(256)  :: Message
!    LOGICAL :: Clear
!    INTEGER :: Allocate_Status
!
!    ! Set up
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! Default is to clear scalar members...
!    Clear = .TRUE.
!    ! ....unless the No_Clear argument is set
!    IF ( PRESENT( No_Clear ) ) THEN
!      IF ( No_Clear == 1 ) Clear = .FALSE.
!    END IF
!
!    ! Initialise the scalar members
!    IF ( Clear ) CALL Clear_TauCoeff( TauCoeff )
!
!    ! If ALL pointer members are NOT associated, do nothing
!    IF ( .NOT. Associated_TauCoeff(TauCoeff) ) RETURN
!
!    ! Deallocate the pointer members
!    DEALLOCATE( TauCoeff%Algorithm_ID     , &
!                TauCoeff%Sensor_Index     , &
!                TauCoeff%Sensor_LoIndex   , &
!                TauCoeff%ODAS             , &
!                TauCoeff%ODPS             , &
!                STAT = Allocate_Status )
!    IF ( Allocate_Status /= 0 ) THEN
!      Error_Status = FAILURE
!      WRITE( Message, '( "Error deallocating TauCoeff. STAT = ", i5 )' ) &
!                      Allocate_Status
!      CALL Display_Message( ROUTINE_NAME,    &
!                            TRIM( Message ), &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!    END IF
!
!    ! Decrement and test allocation counter
!    TauCoeff%n_Allocates = TauCoeff%n_Allocates - 1
!    IF ( TauCoeff%n_Allocates /= 0 ) THEN
!      Error_Status = FAILURE
!      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
!                      TauCoeff%n_Allocates
!      CALL Display_Message( ROUTINE_NAME,    &
!                            TRIM( Message ), &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!    END IF
!
!  END FUNCTION Destroy_TauCoeff
!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_TauCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the TauCoeff
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_TauCoeff( n_ODAS,                    &  ! Input
!                                         n_ODPS,                    &  ! Input
!                                         TauCoeff,                  &  ! Output
!                                         Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_ODAS:       Number of ODAS structures
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n_ODPS:       Number of ODPS structures
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       TauCoeff:     TauCoeff structure with allocated
!                     pointer members
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
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
!       Note the INTENT on the output TauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_TauCoeff( n_ODAS,       &  ! Input
                              n_ODPS,       &  ! Input
                              TauCoeff,     &  ! Output
                              Message_Log ) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_ODAS
    INTEGER,                INTENT(IN)     :: n_ODPS
    TYPE(TauCoeff_type),    INTENT(IN OUT) :: TauCoeff
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_TauCoeff'
    ! Local variables
    CHARACTER(256)  :: Message
    INTEGER :: Allocate_Status
    INTEGER :: n_Sensors

    ! Set up
    Error_Status = SUCCESS
    
    ! Check and set dimensions 
    IF ( n_ODAS       < 0 .OR. &
         n_ODPS       < 0      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff n_ODAS and n_ODPS dimensions must be >= 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
     
    n_Sensors = n_ODAS + n_ODPS
  
    IF ( n_Sensors    < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauCoeff n_Sensors dimension must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
!    IF ( Associated_TauCoeff( TauCoeff, ANY_Test=.TRUE. ) ) THEN
!      Error_Status = Destroy_TauCoeff( TauCoeff, &
!                                       No_Clear=1, &
!                                       Message_Log = Message_Log )
!      IF ( Error_Status /= SUCCESS ) THEN
!        CALL Display_Message( ROUTINE_NAME,    &
!                              'Error deallocating TauCoeff prior to allocation.', &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!        RETURN
!      END IF
!    END IF

    ! Perform the pointer allocation
    ALLOCATE( TauCoeff%Algorithm_ID( n_Sensors ), &
              TauCoeff%Sensor_Index( n_Sensors ), &
              TauCoeff%Sensor_LoIndex( n_Sensors ), &
              TauCoeff%ODAS( n_ODAS ), &
              TauCoeff%ODPS( n_ODPS ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating TauCoeff data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign the dimensions
    TauCoeff%n_Sensors = n_Sensors
    TauCoeff%n_ODAS    = n_ODAS
    TauCoeff%n_ODPS    = n_ODPS

    ! Initialise the arrays
    TauCoeff%Algorithm_ID   = IP_INIT
    TauCoeff%Sensor_Index   = IP_INIT
    TauCoeff%Sensor_LoIndex = IP_INIT

    ! Increment and test the allocation counter
    TauCoeff%n_Allocates = TauCoeff%n_Allocates + 1
    IF ( TauCoeff%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      TauCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_TauCoeff
!------------------------------------------------------------------------------
!
! NAME:
!       Assign_TauCoeff
!
! PURPOSE:
!       Function to copy valid TauCoeff structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_TauCoeff( TauCoeff_in,              &  ! Input
!                                       TauCoeff_out,             &  ! Output
!                                       RCS_Id      = RCS_Id,     &  ! Revision control
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff_in:   TauCoeff structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       TauCoeff_out:  Copy of the input structure, TauCoeff_in.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output TauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

!  FUNCTION Assign_TauCoeff( TauCoeff_in,   &  ! Input
!                            TauCoeff_out,  &  ! Output
!                            RCS_Id,        &  ! Revision control
!                            Message_Log )  &  ! Error messaging
!                          RESULT( Error_Status )
!    ! Arguments
!    TYPE(TauCoeff_type),    INTENT(IN)     :: TauCoeff_in
!    TYPE(TauCoeff_type),    INTENT(IN OUT) :: TauCoeff_out
!    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_TauCoeff'
!
!    ! Set up
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! ALL *input* pointers must be associated
!    IF ( .NOT. Associated_TauCoeff(TauCoeff_In) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME,    &
!                            'Some or all INPUT TauCoeff pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Allocate data arrays
!    Error_Status = Allocate_TauCoeff( TauCoeff_in%n_Orders    , &
!                                      TauCoeff_in%n_Predictors, &
!                                      TauCoeff_in%n_Absorbers , &
!                                      TauCoeff_in%n_Channels , &
!                                      TauCoeff_out, &
!                                      Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME,    &
!                            'Error allocating output TauCoeff arrays.', &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Assign non-dimension scalar members
!    TauCoeff_out%Release   = TauCoeff_in%Release
!    TauCoeff_out%Version   = TauCoeff_in%Version
!    TauCoeff_out%n_Sensors = TauCoeff_in%n_Sensors
!
!    ! Copy array data
!    TauCoeff_out%Sensor_Descriptor = TauCoeff_in%Sensor_Descriptor
!    TauCoeff_out%NCEP_Sensor_ID    = TauCoeff_in%NCEP_Sensor_ID
!    TauCoeff_out%WMO_Satellite_ID  = TauCoeff_in%WMO_Satellite_ID
!    TauCoeff_out%WMO_Sensor_ID     = TauCoeff_in%WMO_Sensor_ID
!    TauCoeff_out%Sensor_Channel    = TauCoeff_in%Sensor_Channel
!    TauCoeff_out%Absorber_ID       = TauCoeff_in%Absorber_ID
!    TauCoeff_out%Alpha             = TauCoeff_in%Alpha
!    TauCoeff_out%Alpha_C1          = TauCoeff_in%Alpha_C1
!    TauCoeff_out%Alpha_C2          = TauCoeff_in%Alpha_C2
!    TauCoeff_out%Order_Index       = TauCoeff_in%Order_Index
!    TauCoeff_out%Predictor_Index   = TauCoeff_in%Predictor_Index
!    TauCoeff_out%C                 = TauCoeff_in%C
!
!  END FUNCTION Assign_TauCoeff


!------------------------------------------------------------------------------
!
! NAME:
!       Concatenate_Channel_TauCoeff
!
! PURPOSE:
!       Function to concatenate two valid TauCoeff structures along
!       the channel dimension.
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_Channel_TauCoeff( TauCoeff1,                &  ! Input/Output
!                                                    TauCoeff2,                &  ! Input
!                                                    RCS_Id      = RCS_Id,     &  ! Revision control
!                                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff1:     First TauCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       TauCoeff2:     Second TauCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       TauCoeff1:     The concatenated TauCoeff structure. The order of
!                      concatenation is TauCoeff1,TauCoeff2 along the 
!                      channel dimension.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
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
!                         == FAILURE an error occurred, or
!                         == WARNING - the version numbers of the TauCoeff structure
!                                      data are different.
!                                    - the destruction of a temporary, local TauCoeff
!                                      structure failed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The input TauCoeff1 argument contains the concatenated structure
!       data (in character-speak: TauCoeff1//TauCoeff2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input TauCoeff1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
!------------------------------------------------------------------------------

!  FUNCTION Concatenate_Channel_TauCoeff( TauCoeff1,     &  ! Input/Output
!                                         TauCoeff2,     &  ! Input
!                                         RCS_Id,        &  ! Revision control
!                                         Message_Log )  &  ! Error messaging
!                                       RESULT( Error_Status )
!    ! Arguments
!    TYPE(TauCoeff_type),    INTENT(IN OUT)  :: TauCoeff1
!    TYPE(TauCoeff_type),    INTENT(IN)      :: TauCoeff2
!    CHARACTER(*), OPTIONAL, INTENT(OUT)     :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)      :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Concatenate_Channel_TauCoeff'
!    ! Local variables
!    INTEGER :: n_Channels, l1, l2
!    TYPE(TauCoeff_type) :: TauCoeff_Tmp
!
!    ! Set up
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! Check structures
!    IF ( .NOT. Associated_TauCoeff( TauCoeff1 ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME,    &
!                            'Some or all INPUT TauCoeff1 pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!    IF ( .NOT. Associated_TauCoeff( TauCoeff2 ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME,    &
!                            'Some or all INPUT TauCoeff2 pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Compare structure release/version
!    IF ( TauCoeff1%Release /= TauCoeff2%Release ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Input TauCoeff Release values are different.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!    IF ( TauCoeff1%Version /= TauCoeff2%Version ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Input TauCoeff Version values are different.', &
!                            WARNING, &
!                            Message_Log = Message_Log )
!
!    END IF
!
!    ! Check non-channel dimensions
!    IF ( TauCoeff1%n_Orders     /= TauCoeff2%n_Orders     .OR. &
!         TauCoeff1%n_Predictors /= TauCoeff2%n_Predictors .OR. &
!         TauCoeff1%n_Absorbers  /= TauCoeff2%n_Absorbers       ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Non-channel TauCoeff dimensions are different.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Copy the first structure...
!    Error_Status = Assign_TauCoeff( TauCoeff1, TauCoeff_Tmp, &
!                                    Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error copying TauCoeff1 structure.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! ... now destroy it ...
!    Error_Status = Destroy_TauCoeff( TauCoeff1, &
!                                     Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error destroying TauCoeff1 structure.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! ... and now re-allocate it for all channels
!    n_Channels = TauCoeff_Tmp%n_Channels + TauCoeff2%n_Channels
!    Error_Status = Allocate_TauCoeff( TauCoeff_Tmp%n_Orders, &
!                                      TauCoeff_Tmp%n_Predictors, &
!                                      TauCoeff_Tmp%n_Absorbers, &
!                                      n_Channels, &
!                                      TauCoeff1, &
!                                      Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error reallocating TauCoeff1 structure.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Assign the non-channel array data
!    TauCoeff1%Version     = MAX( TauCoeff_Tmp%Version, TauCoeff2%Version )
!    TauCoeff1%Absorber_ID = TauCoeff_Tmp%Absorber_ID
!    TauCoeff1%Alpha       = TauCoeff_Tmp%Alpha
!    TauCoeff1%Alpha_C1    = TauCoeff_Tmp%Alpha_C1
!    TauCoeff1%Alpha_C2    = TauCoeff_Tmp%Alpha_C2
!
!    ! Concatenate channel array data...
!    ! ...the first part
!    l1 = 1
!    l2 = TauCoeff_Tmp%n_Channels
!    TauCoeff1%Sensor_Descriptor(l1:l2)   = TauCoeff_Tmp%Sensor_Descriptor
!    TauCoeff1%NCEP_Sensor_ID(l1:l2)      = TauCoeff_Tmp%NCEP_Sensor_ID
!    TauCoeff1%WMO_Satellite_ID(l1:l2)    = TauCoeff_Tmp%WMO_Satellite_ID
!    TauCoeff1%WMO_Sensor_ID(l1:l2)       = TauCoeff_Tmp%WMO_Sensor_ID
!    TauCoeff1%Sensor_Channel(l1:l2)      = TauCoeff_Tmp%Sensor_Channel
!    TauCoeff1%Order_Index(:,:,l1:l2)     = TauCoeff_Tmp%Order_Index
!    TauCoeff1%Predictor_Index(:,:,l1:l2) = TauCoeff_Tmp%Predictor_Index
!    TauCoeff1%C(:,:,:,l1:l2)             = TauCoeff_Tmp%C
!    ! ...the second part
!    l1 = l2 + 1
!    l2 = n_Channels
!    TauCoeff1%Sensor_Descriptor(l1:l2)   = TauCoeff2%Sensor_Descriptor
!    TauCoeff1%NCEP_Sensor_ID(l1:l2)      = TauCoeff2%NCEP_Sensor_ID
!    TauCoeff1%WMO_Satellite_ID(l1:l2)    = TauCoeff2%WMO_Satellite_ID
!    TauCoeff1%WMO_Sensor_ID(l1:l2)       = TauCoeff2%WMO_Sensor_ID
!    TauCoeff1%Sensor_Channel(l1:l2)      = TauCoeff2%Sensor_Channel
!    TauCoeff1%Order_Index(:,:,l1:l2)     = TauCoeff2%Order_Index
!    TauCoeff1%Predictor_Index(:,:,l1:l2) = TauCoeff2%Predictor_Index
!    TauCoeff1%C(:,:,:,l1:l2)             = TauCoeff2%C
!
!    ! Count the number of sensors
!    CALL Count_TauCoeff_Sensors( TauCoeff1 )
!
!    ! Destroy the temporary structure
!    Error_Status = Destroy_TauCoeff( TauCoeff_Tmp, &
!                                     Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      Error_Status = WARNING
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error destroying TauCoeff_Tmp structure.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!    END IF
!
!  END FUNCTION Concatenate_Channel_TauCoeff


!------------------------------------------------------------------------------
!
! NAME:
!       Concatenate_Absorber_TauCoeff
!
! PURPOSE:
!       Function to concatenate two valid TauCoeff structures along
!       the absorber dimension.
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_Absorber_TauCoeff( TauCoeff1,                &  ! Input/Output
!                                                     TauCoeff2,                &  ! Input
!                                                     RCS_Id      = RCS_Id,     &  ! Revision control
!                                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff1:     First TauCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       TauCoeff2:     Second TauCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       TauCoeff1:     The concatenated TauCoeff structure. The order of
!                      concatenation is TauCoeff1,TauCoeff2 along the 
!                      absorber dimension.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
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
!                         == FAILURE an error occurred, or
!                         == WARNING - the version numbers of the TauCoeff structure
!                                      data are different.
!                                    - the destruction of a temporary, local TauCoeff
!                                      structure failed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The input TauCoeff1 argument contains the concatenated structure
!       data (in character-speak: TauCoeff1//TauCoeff2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input TauCoeff1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
!------------------------------------------------------------------------------

!  FUNCTION Concatenate_Absorber_TauCoeff( TauCoeff1,     &  ! Input/Output
!                                          TauCoeff2,     &  ! Input
!                                          RCS_Id,        &  ! Revision control
!                                          Message_Log )  &  ! Error messaging
!                                        RESULT( Error_Status )
!    ! Arguments
!    TYPE(TauCoeff_type),    INTENT(IN OUT)  :: TauCoeff1
!    TYPE(TauCoeff_type),    INTENT(IN)      :: TauCoeff2
!    CHARACTER(*), OPTIONAL, INTENT(OUT)     :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)      :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Concatenate_Absorber_TauCoeff'
!    ! Local variables
!    INTEGER :: n_Absorbers, j1, j2
!    TYPE(TauCoeff_type) :: TauCoeff_Tmp
!
!    ! Set up
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! Check structures
!    IF ( .NOT. Associated_TauCoeff( TauCoeff1 ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME,    &
!                            'Some or all INPUT TauCoeff1 pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!    IF ( .NOT. Associated_TauCoeff( TauCoeff2 ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME,    &
!                            'Some or all INPUT TauCoeff2 pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Compare structure release/version
!    IF ( TauCoeff1%Release /= TauCoeff2%Release ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Input TauCoeff Release values are different.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!    IF ( TauCoeff1%Version /= TauCoeff2%Version ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Input TauCoeff Version values are different.', &
!                            WARNING, &
!                            Message_Log = Message_Log )
!    END IF
!
!    ! Check the non-absorber dimensions
!    IF ( TauCoeff1%n_Orders     /= TauCoeff2%n_Orders     .OR. &
!         TauCoeff1%n_Predictors /= TauCoeff2%n_Predictors .OR. &
!         TauCoeff1%n_Channels   /= TauCoeff2%n_Channels        ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Non-absorber TauCoeff dimensions are different.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Check the ID and channel values
!    IF ( ANY( ( TauCoeff1%NCEP_Sensor_ID   - TauCoeff2%NCEP_Sensor_ID   ) /= 0 ) .OR. &
!         ANY( ( TauCoeff1%WMO_Satellite_ID - TauCoeff2%WMO_Satellite_ID ) /= 0 ) .OR. &
!         ANY( ( TauCoeff1%WMO_Sensor_ID    - TauCoeff2%WMO_Sensor_ID    ) /= 0 ) .OR. &
!         ANY( ( TauCoeff1%Sensor_Channel   - TauCoeff2%Sensor_Channel   ) /= 0 )      ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'TauCoeff sensor ID and channel values are different.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Copy the first structure...
!    Error_Status = Assign_TauCoeff( TauCoeff1, TauCoeff_Tmp, &
!                                    Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error copying TauCoeff1 structure.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!   
!    ! ... now destroy it ...
!    Error_Status = Destroy_TauCoeff( TauCoeff1, &
!                                     Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error destroying TauCoeff1 structure.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! ... and now re-allocate it for all absorbers
!    n_Absorbers = TauCoeff_Tmp%n_Absorbers + TauCoeff2%n_Absorbers
!    Error_Status = Allocate_TauCoeff( TauCoeff_Tmp%n_Orders, &
!                                      TauCoeff_Tmp%n_Predictors, &
!                                      n_Absorbers, &
!                                      TauCoeff_Tmp%n_Channels, &
!                                      TauCoeff1, &
!                                      Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error reallocating TauCoeff1 structure.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Assign the non-absorber array data
!    TauCoeff1%Version           = MAX( TauCoeff_Tmp%Version, TauCoeff2%Version )
!    TauCoeff1%Sensor_Descriptor = TauCoeff_Tmp%Sensor_Descriptor
!    TauCoeff1%NCEP_Sensor_ID    = TauCoeff_Tmp%NCEP_Sensor_ID
!    TauCoeff1%WMO_Satellite_ID  = TauCoeff_Tmp%WMO_Satellite_ID
!    TauCoeff1%WMO_Sensor_ID     = TauCoeff_Tmp%WMO_Sensor_ID
!    TauCoeff1%Sensor_Channel    = TauCoeff_Tmp%Sensor_Channel
!
!    ! Concatenate absorber array data...
!    ! ...the first part
!    j1 = 1
!    j2 = TauCoeff_Tmp%n_Absorbers
!    TauCoeff1%Absorber_ID(j1:j2)         = TauCoeff_Tmp%Absorber_ID
!    TauCoeff1%Alpha(j1:j2)               = TauCoeff_Tmp%Alpha
!    TauCoeff1%Alpha_C1(j1:j2)            = TauCoeff_Tmp%Alpha_C1
!    TauCoeff1%Alpha_C2(j1:j2)            = TauCoeff_Tmp%Alpha_C2
!    TauCoeff1%Order_Index(:,j1:j2,:)     = TauCoeff_Tmp%Order_Index
!    TauCoeff1%Predictor_Index(:,j1:j2,:) = TauCoeff_Tmp%Predictor_Index
!    TauCoeff1%C(:,:,j1:j2,:)             = TauCoeff_Tmp%C
!
!    ! ...the second part
!    j1 = j2 + 1
!    j2 = n_Absorbers
!    TauCoeff1%Absorber_ID(j1:j2)         = TauCoeff2%Absorber_ID
!    TauCoeff1%Alpha(j1:j2)               = TauCoeff2%Alpha
!    TauCoeff1%Alpha_C1(j1:j2)            = TauCoeff2%Alpha_C1
!    TauCoeff1%Alpha_C2(j1:j2)            = TauCoeff2%Alpha_C2
!    TauCoeff1%Order_Index(:,j1:j2,:)     = TauCoeff2%Order_Index
!    TauCoeff1%Predictor_Index(:,j1:j2,:) = TauCoeff2%Predictor_Index
!    TauCoeff1%C(:,:,j1:j2,:)             = TauCoeff2%C
!
!    ! Destroy the temporary structure
!    Error_Status = Destroy_TauCoeff( TauCoeff_Tmp, &
!                                     Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      Error_Status = WARNING
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error destroying TauCoeff_Tmp structure.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!    END IF
!
!  END FUNCTION Concatenate_Absorber_TauCoeff


!------------------------------------------------------------------------------
!
! NAME:
!       Equal_TauCoeff
!
! PURPOSE:
!       Function to test if two TauCoeff structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_TauCoeff( TauCoeff_LHS,             &  ! Input
!                                      TauCoeff_RHS,             &  ! Input
!                                      ULP_Scale   = ULP_Scale,  &  ! Optional input
!                                      Check_All   = Check_All,  &  ! Optional input
!                                      RCS_Id      = RCS_Id,     &  ! Optional output
!                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff_LHS:  TauCoeff structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( TauCoeff_LHS == TauCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       TauCoeff_RHS:  TauCoeff structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( TauCoeff_LHS == TauCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
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
!                      channel data of the TauCoeff structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in TauCoeff structures.
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

!  FUNCTION Equal_TauCoeff( TauCoeff_LHS, &  ! Input
!                           TauCoeff_RHS, &  ! Input
!                           ULP_Scale,    &  ! Optional input
!                           Check_All,    &  ! Optional input
!                           RCS_Id,       &  ! Revision control
!                           Message_Log ) &  ! Error messaging
!                         RESULT( Error_Status )
!    ! Arguments
!    TYPE(TauCoeff_type),    INTENT(IN)  :: TauCoeff_LHS
!    TYPE(TauCoeff_type),    INTENT(IN)  :: TauCoeff_RHS
!    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
!    INTEGER,      OPTIONAL, INTENT(IN)  :: Check_All
!    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_TauCoeff'
!    ! Local variables
!    CHARACTER(256)  :: message
!    INTEGER :: ULP
!    LOGICAL :: Check_Once
!    INTEGER :: iorder, iuse, j, l
!
!    ! Set up
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! Default precision is a single unit in last place
!    ULP = 1
!    ! ... unless the ULP_Scale argument is set and positive
!    IF ( PRESENT( ULP_Scale ) ) THEN
!      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
!    END IF
!
!    ! Default action is to return on ANY difference...
!    Check_Once = .TRUE.
!    ! ...unless the Check_All argument is set
!    IF ( PRESENT( Check_All ) ) THEN
!      IF ( Check_All == 1 ) Check_Once = .FALSE.
!    END IF
!
!    ! Check the structure association status
!    IF ( .NOT. Associated_TauCoeff( TauCoeff_LHS ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Some or all INPUT TauCoeff_LHS pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!    IF ( .NOT. Associated_TauCoeff( TauCoeff_RHS ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME,    &
!                            'Some or all INPUT TauCoeff_RHS pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Check structure Release/Version
!    IF ( ( TauCoeff_LHS%Release /= TauCoeff_RHS%Release ) .OR. &
!         ( TauCoeff_LHS%Version /= TauCoeff_RHS%Version )      ) THEN
!      Error_Status = FAILURE
!      WRITE( Message, '( "Release/Version numbers are different : ", &
!                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
!                      TauCoeff_LHS%Release, TauCoeff_LHS%Version, &
!                      TauCoeff_RHS%Release, TauCoeff_RHS%Version
!      CALL Display_Message( ROUTINE_NAME, &
!                            TRIM( Message ), &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      IF ( Check_Once ) RETURN
!    END IF
!
!    ! Check dimensions
!    IF ( TauCoeff_LHS%n_Orders     /= TauCoeff_RHS%n_Orders     .OR. &
!         TauCoeff_LHS%n_Predictors /= TauCoeff_RHS%n_Predictors .OR. &
!         TauCoeff_LHS%n_Absorbers  /= TauCoeff_RHS%n_Absorbers  .OR. &
!         TauCoeff_LHS%n_Channels   /= TauCoeff_RHS%n_Channels        ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Structure dimensions are different', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!
!    ! Check the represented sensors
!    IF ( TauCoeff_LHS%n_Sensors /= TauCoeff_RHS%n_Sensors ) THEN
!      Error_Status = FAILURE
!      WRITE( Message, '( "n_Sensors values are different : ", &
!                        &i4, " vs. ", i4 )' ) &
!                      TauCoeff_LHS%n_Sensors, TauCoeff_RHS%n_Sensors
!      CALL Display_Message( ROUTINE_NAME, &
!                            TRIM( Message ), &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Check ID pointers by channel
!    !
!    ! Each structure member is tested separately. It's a bit of a brain dead
!    ! way to do it, but easiest to implement since the data types differ.
!    ! Also, each channel is tested explicitly, rather than using the ANY
!    ! or ALL intrinsic functions, since I wanted to highlight the actual
!    ! channel index where any difference occured so it would be very easy to
!    ! track down the location of the difference.
!    l_Channel_Loop: DO l = 1, TauCoeff_RHS%n_Channels
!
!      ! The Sensor Descriptor
!      IF ( TauCoeff_LHS%Sensor_Descriptor(l) /= TauCoeff_RHS%Sensor_Descriptor(l) ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Descriptor values are different, ", &
!                          &a, " vs. ", a, ",  for channel index # ", i4 )' ) &
!                        TRIM( TauCoeff_LHS%Sensor_Descriptor(l) ), &
!                        TRIM( TauCoeff_RHS%Sensor_Descriptor(l) ), &
!                        l
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                             Message_Log = Message_Log )
!        RETURN
!      END IF
!
!      ! The NCEP sensor ID
!      IF ( TauCoeff_LHS%NCEP_Sensor_ID(l) /= TauCoeff_RHS%NCEP_Sensor_ID(l) ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "NCEP_Sensor_ID values are different, ", &
!                          &i4, " vs. ", i4, ",  for channel # ", i4 )' ) &
!                        TauCoeff_LHS%NCEP_Sensor_ID(l), &
!                        TauCoeff_RHS%NCEP_Sensor_ID(l), &
!                        l
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                             Message_Log = Message_Log )
!        RETURN
!      END IF
!
!      ! The WMO Satellite ID
!      IF ( TauCoeff_LHS%WMO_Satellite_ID(l) /= TauCoeff_RHS%WMO_Satellite_ID(l) ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "WMO_Satellite_ID values are different, ", &
!                          &i4, " vs. ", i4, ",  for channel # ", i4 )' ) &
!                        TauCoeff_LHS%WMO_Satellite_ID(l), &
!                        TauCoeff_RHS%WMO_Satellite_ID(l), &
!                        l
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                             Message_Log = Message_Log )
!        RETURN
!      END IF
!
!      ! The WMO Sensor ID
!      IF ( TauCoeff_LHS%WMO_Sensor_ID(l) /= TauCoeff_RHS%WMO_Sensor_ID(l) ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "WMO_Sensor_ID values are different, ", &
!                          &i4, " vs. ", i4, ",  for channel # ", i4 )' ) &
!                        TauCoeff_LHS%WMO_Sensor_ID(l), &
!                        TauCoeff_RHS%WMO_Sensor_ID(l), &
!                        l
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                             Message_Log = Message_Log )
!        RETURN
!      END IF
!
!      ! The sensor channel numbers
!      IF ( TauCoeff_LHS%Sensor_Channel(l) /= TauCoeff_RHS%Sensor_Channel(l) ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Sensor_Channel values are different, ", &
!                          &i4, " vs. ", i4, ",  for channel # ", i4 )' ) &
!                        TauCoeff_LHS%Sensor_Channel(l), &
!                        TauCoeff_RHS%Sensor_Channel(l), &
!                        l
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                             Message_Log = Message_Log )
!        RETURN
!      END IF
!    END DO l_Channel_Loop
!
!    ! Check absorber dimensioned pointer members
!    j_Absorber_Loop: DO j = 1, TauCoeff_RHS%n_Absorbers
!
!      ! The Absorber_ID value
!      IF ( TauCoeff_LHS%Absorber_ID(j) /= TauCoeff_RHS%Absorber_ID(j) ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Absorber_ID values are different, ", &
!                          &i3, " vs. ", i3, ",  for absorber # ", i4 )' ) &
!                        TauCoeff_LHS%Absorber_ID(j), &
!                        TauCoeff_RHS%Absorber_ID(j), &
!                        j
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                              Message_Log = Message_Log )
!        RETURN
!      END IF
!
!      ! The Alpha value
!      IF ( .NOT. Compare_Float( TauCoeff_LHS%Alpha(j), &
!                                TauCoeff_RHS%Alpha(j), &
!                                ULP = ULP              ) ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Alpha values are different, ", &
!                          &es13.6, " vs. ", es13.6, ",  for absorber # ", i4 )' ) &
!                        TauCoeff_LHS%Alpha(j), &
!                        TauCoeff_RHS%Alpha(j), &
!                        j
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                              Message_Log = Message_Log )
!        IF ( Check_Once ) RETURN
!      END IF
!
!      ! The Alpha_C1 value
!      IF ( .NOT. Compare_Float( TauCoeff_LHS%Alpha_C1(j), &
!                                TauCoeff_RHS%Alpha_C1(j), &
!                                ULP = ULP                 ) ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Alpha_C1 values are different, ", &
!                          &es13.6, " vs. ", es13.6, ",  for absorber # ", i4 )' ) &
!                        TauCoeff_LHS%Alpha_C1(j), &
!                        TauCoeff_RHS%Alpha_C1(j), &
!                        j
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                              Message_Log = Message_Log )
!        IF ( Check_Once ) RETURN
!      END IF
!
!      ! The Alpha_C2 value
!      IF ( .NOT. Compare_Float( TauCoeff_LHS%Alpha_C2(j), &
!                                TauCoeff_RHS%Alpha_C2(j), &
!                                ULP = ULP                 ) ) THEN
!        Error_Status = FAILURE
!        WRITE( Message, '( "Alpha_C2 values are different, ", &
!                          &es13.6, " vs. ", es13.6, ",  for absorber # ", i4 )' ) &
!                        TauCoeff_LHS%Alpha_C2(j), &
!                        TauCoeff_RHS%Alpha_C2(j), &
!                        j
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                              Message_Log = Message_Log )
!        IF ( Check_Once ) RETURN
!      END IF
!    END DO j_Absorber_Loop
!
!    ! Check index and coefficient pointer members by channel/absorber
!    Channel_Loop: DO l = 1, TauCoeff_RHS%n_Channels
!      Absorber_Loop: DO j = 1, TauCoeff_RHS%n_Absorbers
!
!        ! The order indices
!        DO iuse = 0, TauCoeff_RHS%n_Predictors
!          IF ( TauCoeff_LHS%Order_Index(iuse,j,l) /= TauCoeff_RHS%Order_Index(iuse,j,l) ) THEN
!            Error_Status = FAILURE
!            WRITE( Message,'("Order_Index values are different for index ",3(i0,1x))' ) iuse,j,l
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM(Message), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!            IF ( Check_Once ) RETURN
!          END IF
!        END DO
!
!        ! The predictor indices
!        DO iuse = 0, TauCoeff_RHS%n_Predictors
!          IF ( TauCoeff_LHS%Predictor_Index(iuse,j,l) /= TauCoeff_RHS%Predictor_Index(iuse,j,l) ) THEN
!            Error_Status = FAILURE
!            WRITE( Message,'("Predictor_Index values are different for index ",3(i0,1x))' ) iuse,j,l
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM(Message), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!            IF ( Check_Once ) RETURN
!          END IF
!        END DO
!
!        ! The gas absorption coefficients
!        DO iuse = 0, TauCoeff_RHS%n_Predictors
!          DO iorder = 0, TauCoeff_RHS%n_Orders
!            IF ( .NOT. Compare_Float( TauCoeff_LHS%C(iorder,iuse,j,l), &
!                                      TauCoeff_RHS%C(iorder,iuse,j,l), &
!                                      ULP = ULP                        ) ) THEN
!              Error_Status = FAILURE
!              WRITE( Message,'("Gas absorption coefficient values are different for index ",&
!                             &4(i0,1x))' ) iorder,iuse,j,l
!              CALL Display_Message( ROUTINE_NAME, &
!                                    TRIM( Message ), &
!                                    Error_Status, &
!                                    Message_Log = Message_Log )
!              IF ( Check_Once ) RETURN
!            END IF
!          END DO
!        END DO
!      END DO Absorber_Loop
!    END DO Channel_Loop
!
!  END FUNCTION Equal_TauCoeff


!----------------------------------------------------------------------------------
!
! NAME:
!       Check_TauCoeff_Release
!
! PURPOSE:
!       Function to check the TauCoeff Release value.
!
! CALLING SEQUENCE:
!       Error_Status = Check_TauCoeff_Release( TauCoeff,                 &  ! Input
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff:      TauCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
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

!  FUNCTION Check_TauCoeff_Release( TauCoeff,     &  ! Input
!                                   RCS_Id,       &  ! Revision control
!                                   Message_Log ) &  ! Error messaging
!                                 RESULT( Error_Status )
!    ! Arguments
!    TYPE(TauCoeff_type),    INTENT(IN)  :: TauCoeff
!    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Check_TauCoeff_Release'
!    ! Local variables
!    CHARACTER(256) :: Message
!
!    ! Set up
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! Check release is not too old
!    IF ( TauCoeff%Release < TAUCOEFF_RELEASE ) THEN
!      Error_Status = FAILURE
!      WRITE( Message, '( "A TauCoeff data update is needed. ", &
!                        &"TauCoeff release is ", i2, &
!                        &". Valid release is ",i2,"." )' ) &
!                      TauCoeff%Release, TAUCOEFF_RELEASE
!      CALL Display_Message( ROUTINE_NAME, &
!                            TRIM( Message ), &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Check release is not too new
!    IF ( TauCoeff%Release > TAUCOEFF_RELEASE ) THEN
!      Error_Status = FAILURE
!      WRITE( Message, '( "A TauCoeff software update is needed. ", &
!                        &"TauCoeff release is ", i2, &
!                        &". Valid release is ",i2,"." )' ) &
!                      TauCoeff%Release, TAUCOEFF_RELEASE
!      CALL Display_Message( ROUTINE_NAME, &
!                            TRIM( Message ), &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!  END FUNCTION Check_TauCoeff_Release


!------------------------------------------------------------------------------
!
! NAME:
!       Count_TauCoeff_Sensors
!
! PURPOSE:
!       Subroutine to count the number of different satellite/sensors in the
!       TauCoeff structure and set the n_Sensors field. The Sensor_Descriptor
!       field in the TauCoeff structure is used for counting.
!
! CALLING SEQUENCE:
!       CALL Count_TauCoeff_Sensors( TauCoeff,       &  ! In/Output
!                                    RCS_Id = RCS_Id )  ! Optional output
!
! INPUT ARGUMENTS:
!       TauCoeff_in:   Filled TauCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! SIDE EFFECTS:
!       The N_SENSORS field of the input TauCoeff structure is modified.
!
!------------------------------------------------------------------------------

!  SUBROUTINE Count_TauCoeff_Sensors( TauCoeff, &  ! In/Output
!                                     RCS_Id    )  ! Revision control
!    ! Arguments
!    TYPE(TauCoeff_type),    INTENT(IN OUT) :: TauCoeff
!    ! Revision control
!    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
!    ! Local variables
!    INTEGER, DIMENSION(TauCoeff%n_Channels) :: Idx
!
!    ! Set up
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! Sort the structure sensor descriptors
!    CALL InsertionSort(TauCoeff%Sensor_Descriptor, Idx)
!
!    ! Count the unique sensors
!    TauCoeff%n_Sensors = MAX( COUNT(TauCoeff%Sensor_Descriptor(Idx) /= &
!                              CSHIFT(TauCoeff%Sensor_Descriptor(Idx),1)), 1)
!
!  END SUBROUTINE Count_TauCoeff_Sensors


!------------------------------------------------------------------------------
!
! NAME:
!       Info_TauCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the TauCoeff data structure.
!
! CALLING SEQUENCE:
!       CALL Info_TauCoeff( TauCoeff,       &  ! Input
!                           Info,           &  ! Output
!                           RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       TauCoeff:      Filled TauCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed TauCoeff data structure.
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

!  SUBROUTINE Info_TauCoeff( TauCoeff, &  ! Input
!                            Info,     &  ! Output
!                            RCS_Id    )  ! Revision control
!    ! Arguments
!    TYPE(TauCoeff_type),    INTENT(IN)  :: TauCoeff
!    CHARACTER(*),           INTENT(OUT) :: Info
!    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
!    ! Parameters
!    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
!    INTEGER, PARAMETER :: LINEFEED = 10
!    ! Local variables
!    CHARACTER(512) :: Long_String
!
!    ! Set up
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! Write the required info to the local string
!    WRITE( Long_String, '( a,1x,"TauCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
!                           &"N_ORDERS=",i2,2x,&
!                           &"N_PREDICTORS=",i2,2x,&
!                           &"N_ABSORBERS=",i2,2x,&
!                           &"N_CHANNELS=",i4,2x,&
!                           &"N_SENSORS=",i2 )' ) &
!                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
!                         TauCoeff%Release, TauCoeff%Version, &
!                         TauCoeff%n_Orders, &
!                         TauCoeff%n_Predictors, &
!                         TauCoeff%n_Absorbers, &
!                         TauCoeff%n_Channels, &
!                         TauCoeff%n_Sensors
!
!    ! Trim the output based on the
!    ! dummy argument string length
!    Info = Long_String(1:MIN( LEN(Info), LEN_TRIM( Long_String ) ))
!
!  END SUBROUTINE Info_TauCoeff

END MODULE TauCoeff_Define
