
! Module defining the RadDiag_Stats structure
! and containing routines to manipulate it.

MODULE RadDiag_Stats_Define

  ! ------------
  ! Module usage
  ! ------------
  USE Type_Kinds,      ONLY: sp=>Single
  USE Message_Handler, ONLY: FAILURE, SUCCESS, WARNING, &
                             Display_Message

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
  IMPLICIT NONE


  ! ---------------------
  ! Explicit visibilities
  ! ---------------------
  PRIVATE
  ! Module derived type definitions
  PUBLIC :: RadDiag_Stats_type
  ! Module parameters
  PUBLIC :: invalidFOV
  PUBLIC :: nVariables
  PUBLIC :: iBC   
  PUBLIC :: iNBC  
  PUBLIC :: iScan 
  PUBLIC :: iConst
  PUBLIC :: iAngle
  PUBLIC :: iLpsR 
  PUBLIC :: iLpsR2
  PUBLIC :: iCLW
  ! Module subprograms
  PUBLIC :: Associated_RadDiag_Stats
  PUBLIC :: Destroy_RadDiag_Stats
  PUBLIC :: Allocate_RadDiag_Stats
  PUBLIC :: Assign_RadDiag_Stats


  ! -----------------
  ! Module parameters
  ! -----------------
  INTEGER, PARAMETER :: invalidFOV = -1
  INTEGER, PARAMETER :: nVariables = 8
  INTEGER, PARAMETER :: iBC    = 1
  INTEGER, PARAMETER :: iNBC   = 2
  INTEGER, PARAMETER :: iScan  = 3
  INTEGER, PARAMETER :: iConst = 4
  INTEGER, PARAMETER :: iAngle = 5
  INTEGER, PARAMETER :: iLpsR  = 6
  INTEGER, PARAMETER :: iLpsR2 = 7
  INTEGER, PARAMETER :: iCLW   = 8
  CHARACTER(*), PARAMETER :: VariableNames(nVariables) = (/ 'Calc-Obs dTb [Bias Corrected]          ', &
                                                            'Calc-Obs dTb [NOT Bias Corrected]      ', &
                                                            'SatBias Angle term                     ', &
                                                            'SatBias AirMass Constant term          ', &
                                                            'SatBias AirMass Angle term             ', &
                                                            'SatBias AirMass Lapse Rate term        ', &
                                                            'SatBias AirMass (Lapse Late)^2 term    ', &
                                                            'SatBias AirMass Cloud Liquid Water term' /)

  INTEGER, PARAMETER :: SL = 80
  INTEGER, PARAMETER :: SET = 1
  REAL,    PARAMETER :: ZERO = 0.0_sp
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


  ! --------------------
  ! Structure definition
  ! --------------------
  TYPE :: RadDiag_Stats_type
    ! Allocation counter
    INTEGER :: nAllocates = 0
    ! Dimensions
    INTEGER :: nPredictors = 0  ! I
    INTEGER :: nChannels   = 0  ! L
    INTEGER :: nFOVs       = 0  ! Is
    INTEGER :: nTimes      = 0  ! It
    INTEGER :: nVariables  = 0  ! N
    INTEGER :: StrLen      = SL
    ! The Air Mass bias correction coefficients
    REAL(sp), DIMENSION(:,:,:),  POINTER :: AirMassCoefficients => NULL() ! I x L x It
    ! The variable names
    CHARACTER(SL), DIMENSION(:), POINTER :: VariableNames       => NULL() ! N
    ! The channel numbers
    INTEGER,  DIMENSION(:),      POINTER :: Channel             => NULL() ! L
    ! Scan position statistics
    INTEGER,  DIMENSION(:),      POINTER :: FOV                 => NULL() ! Is
    REAL(sp), DIMENSION(:,:,:),  POINTER :: scan_Data           => NULL() ! N x L x Is
    INTEGER,  DIMENSION(:,:),    POINTER :: scan_nSamples       => NULL() ! L x Is
    ! Time series statistics
    INTEGER,  DIMENSION(:),      POINTER :: DateTime            => NULL() ! It
    REAL(sp), DIMENSION(:,:,:),  POINTER :: time_Data           => NULL() ! N x L x It
    INTEGER,  DIMENSION(:,:),    POINTER :: time_nSamples       => NULL() ! L x It
  END TYPE RadDiag_Stats_type


CONTAINS


  ! Subroutine to clear the scalar elements of
  ! the radiance statistics structure
  SUBROUTINE Clear_RadDiag_Stats( RadDiag_Stats )
    TYPE( RadDiag_Stats_type ), INTENT( IN OUT ) :: RadDiag_Stats
    RadDiag_Stats%StrLen = SL
  END SUBROUTINE Clear_RadDiag_Stats



  ! Function to test the association status of the RadDiag_Stats structure
  !
  ! CALLING SEQUENCE:
  !   Association_Status = Associated_RadDiag_Stats( &
  !                          RadDiag_Stats,  &  ! Input
  !                          ANY_Test = Any_Test )  ! Optional input
  !
  ! INPUT ARGUMENTS:
  !   RadDiag_Stats:   RadDiag_Stats structure which is to have its
  !                        pointer member's association status tested.
  !                        UNITS:      N/A
  !                        TYPE:       RadDiag_Stats_type
  !                        DIMENSION:  Scalar
  !                        ATTRIBUTES: INTENT( IN )
  !
  ! OPTIONAL INPUT ARGUMENTS:
  !   ANY_Test:            Set this argument to test if ANY of the
  !                        RadDiag_Stats structure pointer members are
  !                        associated.
  !                        The default is to test if ALL the pointer members
  !                        are associated.
  !                        If ANY_Test = 0, test if ALL the pointer members
  !                                         are associated.  (DEFAULT)
  !                           ANY_Test = 1, test if ANY of the pointer members
  !                                         are associated.
  !                        UNITS:      N/A
  !                        TYPE:       INTEGER
  !                        DIMENSION:  Scalar
  !                        ATTRIBUTES: INTENT( IN ), OPTIONAL
  !
  ! FUNCTION RESULT:
  !   Association_Status:  The return value is a logical value indicating
  !                        the association status of the RadDiag_Stats
  !                        pointer members.
  !                        .TRUE.  - if ALL the RadDiag_Stats pointer
  !                                  members are associated, or if the
  !                                  ANY_Test argument is set and ANY of the
  !                                  RadDiag_Stats pointer members are
  !                                  associated.
  !                        .FALSE. - some or all of the RadDiag_Stats
  !                                  pointer members are NOT associated.
  !                        UNITS:      N/A
  !                        TYPE:       LOGICAL
  !                        DIMENSION:  Scalar
  !
  ! CREATION HISTORY:
  !   Written by:     Paul van Delst, CIMSS/SSEC 28-Mar-2006
  !                   paul.vandelst@ssec.wisc.edu

  FUNCTION Associated_RadDiag_Stats( &
    RadDiag_Stats, &  ! Output
    ANY_Test ) & ! Optional input
    RESULT( Association_Status )


    ! -----------------
    ! Type declarations
    ! -----------------

    ! Arguments
    TYPE( RadDiag_Stats_type ), INTENT( IN ) :: RadDiag_Stats
    INTEGER, OPTIONAL, INTENT( IN ) :: ANY_Test

    ! Function result
    LOGICAL :: Association_Status

    ! Local variables
    LOGICAL :: ALL_Test


    ! ------
    ! Set up
    ! ------

    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! ---------------------------------------------
    ! Test the structure pointer member association
    ! ---------------------------------------------

    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( RadDiag_Stats%AirMassCoefficients ) .AND. &
           ASSOCIATED( RadDiag_Stats%VariableNames       ) .AND. &
           ASSOCIATED( RadDiag_Stats%Channel             ) .AND. &
           ASSOCIATED( RadDiag_Stats%FOV                 ) .AND. &
           ASSOCIATED( RadDiag_Stats%scan_Data           ) .AND. &
           ASSOCIATED( RadDiag_Stats%scan_nSamples       ) .AND. &
           ASSOCIATED( RadDiag_Stats%DateTime            ) .AND. &
           ASSOCIATED( RadDiag_Stats%time_Data           ) .AND. &
           ASSOCIATED( RadDiag_Stats%time_nSamples       )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( RadDiag_Stats%AirMassCoefficients ) .OR. &
           ASSOCIATED( RadDiag_Stats%VariableNames       ) .OR. &
           ASSOCIATED( RadDiag_Stats%Channel             ) .OR. &
           ASSOCIATED( RadDiag_Stats%FOV                 ) .OR. &
           ASSOCIATED( RadDiag_Stats%scan_Data           ) .OR. &
           ASSOCIATED( RadDiag_Stats%scan_nSamples       ) .OR. &
           ASSOCIATED( RadDiag_Stats%DateTime            ) .OR. &
           ASSOCIATED( RadDiag_Stats%time_Data           ) .OR. &
           ASSOCIATED( RadDiag_Stats%time_nSamples       )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_RadDiag_Stats


  ! Function to re-initialize the scalar and pointer members of
  ! a RadDiag_Stats data structure.
  !
  ! CALLING SEQUENCE:
  !   Error_Status = Destroy_RadDiag_Stats( RadDiag_Stats,            &  ! Output
  !                                         RCS_Id = RCS_Id,          &  ! Revision control
  !                                         Message_Log = Message_Log )  ! Error messaging
  !
  ! OPTIONAL INPUT ARGUMENTS:
  !   Message_Log:        Character string specifying a filename in which any
  !                       messages will be logged. If not specified, or if an
  !                       error occurs opening the log file, the default action
  !                       is to output messages to standard output.
  !                       UNITS:      N/A
  !                       TYPE:       CHARACTER(*)
  !                       DIMENSION:  Scalar
  !                       ATTRIBUTES: INTENT( IN ), OPTIONAL
  !
  ! OUTPUT ARGUMENTS:
  !   RadDiag_Stats:  Re-initialized RadDiag_Stats structure.
  !                       UNITS:      N/A
  !                       TYPE:       RadDiag_Stats_type
  !                       DIMENSION:  Scalar
  !                       ATTRIBUTES: INTENT( IN OUT )
  !
  ! OPTIONAL OUTPUT ARGUMENTS:
  !   RCS_Id:             Character string containing the Revision Control
  !                       System Id field for the module.
  !                       UNITS:      N/A
  !                       TYPE:       CHARACTER(*)
  !                       DIMENSION:  Scalar
  !                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
  !
  ! FUNCTION RESULT:
  !   Error_Status:       The return value is an integer defining the error status.
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
  !   Note the INTENT on the output RadDiag_Stats argument is IN OUT 
  !   rather than just OUT. This is necessary because the argument may be
  !   defined upon input. To prevent memory leaks, the IN OUT INTENT is
  !   a must.
  !
  ! CREATION HISTORY:
  !       Written by:     Paul van Delst, CIMSS/SSEC 28-Mar-2006
  !                       paul.vandelst@ssec.wisc.edu

  FUNCTION Destroy_RadDiag_Stats( RadDiag_Stats, &  ! Output
                                  No_Clear,      &  ! Optional input
                                  RCS_Id,        &  ! Revision control
                                  Message_Log )  &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    TYPE(RadDiag_Stats_type), INTENT(IN OUT) :: RadDiag_Stats
    INTEGER,      OPTIONAL, INTENT(IN)  :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_RadDiag_Stats'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! ------
    ! Set up
    ! ------

    Error_Status = SUCCESS

    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    ! ------------------------
    ! Perform reinitialisation
    ! ------------------------

    ! Initialise the scalar members
    IF ( Clear ) CALL Clear_RadDiag_Stats( RadDiag_Stats )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_RadDiag_Stats( RadDiag_Stats ) ) RETURN

    ! -- Deallocate the RadDiag_Stats AirMassCoefficients member
    IF ( ASSOCIATED( RadDiag_Stats%AirMassCoefficients ) ) THEN
      DEALLOCATE( RadDiag_Stats%AirMassCoefficients, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Stats AirMassCoefficients ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the RadDiag_Stats VariableNames member
    IF ( ASSOCIATED( RadDiag_Stats%VariableNames ) ) THEN
      DEALLOCATE( RadDiag_Stats%VariableNames, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Stats VariableNames ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the RadDiag_Stats Channel member
    IF ( ASSOCIATED( RadDiag_Stats%Channel ) ) THEN
      DEALLOCATE( RadDiag_Stats%Channel, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Stats Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the RadDiag_Stats FOV member
    IF ( ASSOCIATED( RadDiag_Stats%FOV ) ) THEN
      DEALLOCATE( RadDiag_Stats%FOV, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Stats FOV ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the RadDiag_Stats scan_Data member
    IF ( ASSOCIATED( RadDiag_Stats%scan_Data ) ) THEN
      DEALLOCATE( RadDiag_Stats%scan_Data, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Stats scan_Data ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the RadDiag_Stats scan_nSamples member
    IF ( ASSOCIATED( RadDiag_Stats%scan_nSamples ) ) THEN
      DEALLOCATE( RadDiag_Stats%scan_nSamples, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Stats scan_nSamples ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the RadDiag_Stats DateTime member
    IF ( ASSOCIATED( RadDiag_Stats%DateTime ) ) THEN
      DEALLOCATE( RadDiag_Stats%DateTime, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Stats DateTime ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the RadDiag_Stats time_Data member
    IF ( ASSOCIATED( RadDiag_Stats%time_Data ) ) THEN
      DEALLOCATE( RadDiag_Stats%time_Data, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Stats time_Data ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the RadDiag_Stats time_nSamples member
    IF ( ASSOCIATED( RadDiag_Stats%time_nSamples ) ) THEN
      DEALLOCATE( RadDiag_Stats%time_nSamples, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Stats time_nSamples ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -------------------------------------
    ! Decrement and test allocation counter
    ! -------------------------------------

    RadDiag_Stats%nAllocates = RadDiag_Stats%nAllocates - 1

    IF ( RadDiag_Stats%nAllocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      RadDiag_Stats%nAllocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_RadDiag_Stats


  ! Function to allocate the pointer members of the RadDiag_Stats
  ! data structure.
  !
  ! CALLING SEQUENCE:
  !   Error_Status = Allocate_RadDiag_Stats( nPredictors,              &  ! Input
  !                                          nChannels,                &  ! Input
  !                                          nFOVs,                    &  ! Input
  !                                          nTimes,                   &  ! Input
  !                                          nVariables,               &  ! Input
  !                                          RadDiag_Stats,            &  ! Output
  !                                          RCS_Id = RCS_Id,          &  ! Revision control
  !                                          Message_Log = Message_Log )  ! Error messaging
  !
  ! INPUT ARGUMENTS:
  !   nPredictors:     Predictor dimension of RadDiag_Stats structure pointer
  !                    members.
  !                    Must be > 0.
  !                    UNITS:      N/A
  !                    TYPE:       INTEGER
  !                    DIMENSION:  Scalar
  !                    ATTRIBUTES: INTENT( IN )
  !
  !   nChannels:       Channel dimension of RadDiag_Stats structure pointer
  !                    members.
  !                    Must be > 0.
  !                    UNITS:      N/A
  !                    TYPE:       INTEGER
  !                    DIMENSION:  Scalar
  !                    ATTRIBUTES: INTENT( IN )
  !
  !   nFOVs:           Field-of-view dimension of RadDiag_Stats structure pointer
  !                    members.
  !                    Must be > 0.
  !                    UNITS:      N/A
  !                    TYPE:       INTEGER
  !                    DIMENSION:  Scalar
  !                    ATTRIBUTES: INTENT( IN )
  !
  !   nTimes:          Time dimension of RadDiag_Stats structure pointer
  !                    members.
  !                    Must be > 0.
  !                    UNITS:      N/A
  !                    TYPE:       INTEGER
  !                    DIMENSION:  Scalar
  !                    ATTRIBUTES: INTENT( IN )
  !
  !   nVariables:      The number of data terms dimension of
  !                    RadDiag_Stats structure pointer members.
  !                    Must be > 0.
  !                    UNITS:      N/A
  !                    TYPE:       INTEGER
  !                    DIMENSION:  Scalar
  !                    ATTRIBUTES: INTENT( IN )
  !
  ! OPTIONAL INPUT ARGUMENTS:
  !   Message_Log:     Character string specifying a filename in
  !                    which any messages will be logged. If not
  !                    specified, or if an error occurs opening the
  !                    log file, the default action is to output
  !                    messages to standard output.
  !                    UNITS:      N/A
  !                    TYPE:       CHARACTER(*)
  !                    DIMENSION:  Scalar
  !                    ATTRIBUTES: INTENT( IN ), OPTIONAL
  !
  ! OUTPUT ARGUMENTS:
  !   RadDiag_Stats:   RadDiag_Stats structure with allocated pointer
  !                    members
  !                    UNITS:      N/A
  !                    TYPE:       RadDiag_Stats_type
  !                    DIMENSION:  Scalar
  !                    ATTRIBUTES: INTENT( IN OUT )
  !
  ! OPTIONAL OUTPUT ARGUMENTS:
  !   RCS_Id:          Character string containing the Revision
  !                    Control System Id field for the module.
  !                    UNITS:      N/A
  !                    TYPE:       CHARACTER(*)
  !                    DIMENSION:  Scalar
  !                    ATTRIBUTES: INTENT( OUT ), OPTIONAL
  !
  ! FUNCTION RESULT:
  !   Error_Status:    The return value is an integer defining the
  !                    error status. The error codes are defined in
  !                    the ERROR_HANDLER module.
  !                    If == SUCCESS the structure pointer allocations
  !                                  were successful
  !                       == FAILURE - an error occurred, or
  !                                  - the structure internal allocation
  !                                    counter is not equal to one (1)
  !                                    upon exiting this function. This
  !                                    value is incremented and decre-
  !                                    mented for every structure
  !                                    allocation and deallocation
  !                                    respectively.
  !                    UNITS:      N/A
  !                    TYPE:       INTEGER
  !                    DIMENSION:  Scalar
  !
  ! COMMENTS:
  !   Note the INTENT on the output RadDiag_Stats argument is IN OUT 
  !   rather than just OUT. This is necessary because the argument may be
  !   defined upon input. To prevent memory leaks, the IN OUT INTENT is
  !   a must.
  !
  ! CREATION HISTORY:
  !   Written by:     Paul van Delst, CIMSS/SSEC 28-Mar-2006
  !                   paul.vandelst@ssec.wisc.edu

  FUNCTION Allocate_RadDiag_Stats( nPredictors,   &  ! Input
                                   nChannels,     &  ! Input
                                   nFOVs,         &  ! Input
                                   nTimes,        &  ! Input
                                   nVariables,    &  ! Input
                                   RadDiag_Stats, &  ! Output
                                   RCS_Id,        &  ! Revision control
                                   Message_Log )  &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    INTEGER,                  INTENT(IN)     :: nPredictors
    INTEGER,                  INTENT(IN)     :: nChannels
    INTEGER,                  INTENT(IN)     :: nFOVs
    INTEGER,                  INTENT(IN)     :: nTimes
    INTEGER,                  INTENT(IN)     :: nVariables
    TYPE(RadDiag_Stats_type), INTENT(IN OUT) :: RadDiag_Stats
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),   OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_RadDiag_Stats'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! ------
    ! Set up
    ! ------

    Error_Status = SUCCESS

    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimensions
    IF ( nPredictors < 1 .OR. &
         nChannels   < 1 .OR. &
         nFOVs       < 1 .OR. &
         nTimes      < 1 .OR. &
         nVariables  < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input dimensions must ALL be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_RadDiag_Stats( RadDiag_Stats, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_RadDiag_Stats( RadDiag_Stats, &
                                            No_Clear=SET, &
                                            Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating RadDiag_Stats pointer members.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! Perform the allocation
    ! ----------------------

    ALLOCATE( &
      RadDiag_Stats%AirMassCoefficients(nPredictors,nChannels,nTimes), &
      RadDiag_Stats%VariableNames(nVariables), &
      RadDiag_Stats%Channel(nChannels), &
      RadDiag_Stats%FOV(nFOVs), &
      RadDiag_Stats%scan_Data(nVariables,nChannels,nFOVs), &
      RadDiag_Stats%scan_nSamples(nChannels,nFOVs), &
      RadDiag_Stats%DateTime(nTimes), &
      RadDiag_Stats%time_Data(nVariables,nChannels,nTimes), &
      RadDiag_Stats%time_nSamples(nChannels,nTimes), &
      STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating RadDiag_Stats data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------------
    ! Assign the dimensions and intialise arrays
    ! ------------------------------------------

    RadDiag_Stats%nPredictors = nPredictors
    RadDiag_Stats%nChannels   = nChannels
    RadDiag_Stats%nFOVs       = nFOVs
    RadDiag_Stats%nTimes      = nTimes
    RadDiag_Stats%nVariables  = nVariables

    RadDiag_Stats%AirMassCoefficients = ZERO
    RadDiag_Stats%VariableNames       = VariableNames
    RadDiag_Stats%Channel             = -1
    RadDiag_Stats%FOV                 = invalidFOV
    RadDiag_Stats%scan_Data           = ZERO
    RadDiag_Stats%scan_nSamples       = ZERO
    RadDiag_Stats%DateTime            = ZERO
    RadDiag_Stats%time_Data           = ZERO
    RadDiag_Stats%time_nSamples       = ZERO


    ! -----------------------------------------
    ! Increment and test the allocation counter
    ! -----------------------------------------

    RadDiag_Stats%nAllocates = RadDiag_Stats%nAllocates + 1
    IF ( RadDiag_Stats%nAllocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      RadDiag_Stats%nAllocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_RadDiag_Stats


  ! Function to copy valid RadDiag_Stats structures.
  !
  ! CALLING SEQUENCE:
  !   Error_Status = Assign_RadDiag_Stats( RadDiag_Stats_in,         &  ! Input
  !                                        RadDiag_Stats_out,        &  ! Output
  !                                        RCS_Id = RCS_Id,          &  ! Revision control
  !                                        Message_Log = Message_Log )  ! Error messaging
  !
  ! INPUT ARGUMENTS:
  !   RadDiag_Stats_in:  RadDiag_Stats structure which is to be copied.
  !                      UNITS:      N/A
  !                      TYPE:       RadDiag_Stats_type
  !                      DIMENSION:  Scalar
  !                      ATTRIBUTES: INTENT( IN )
  !
  ! OPTIONAL INPUT ARGUMENTS:
  !   Message_Log:       Character string specifying a filename in which any
  !                      messages will be logged. If not specified, or if an
  !                      error occurs opening the log file, the default action
  !                      is to output messages to standard output.
  !                      UNITS:      N/A
  !                      TYPE:       CHARACTER(*)
  !                      DIMENSION:  Scalar
  !                      ATTRIBUTES: INTENT( IN ), OPTIONAL
  !
  ! OUTPUT ARGUMENTS:
  !   RadDiag_Stats_out: Copy of the input structure, RadDiag_Stats_in.
  !                      UNITS:      N/A
  !                      TYPE:       Same as RadDiag_Stats_in
  !                      DIMENSION:  Scalar
  !                      ATTRIBUTES: INTENT( IN OUT )
  !
  ! OPTIONAL OUTPUT ARGUMENTS:
  !   RCS_Id:            Character string containing the Revision Control
  !                      System Id field for the module.
  !                      UNITS:      N/A
  !                      TYPE:       CHARACTER(*)
  !                      DIMENSION:  Scalar
  !                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
  !
  ! FUNCTION RESULT:
  !   Error_Status:      The return value is an integer defining the error status.
  !                      The error codes are defined in the ERROR_HANDLER module.
  !                      If == SUCCESS the structure assignment was successful
  !                         == FAILURE an error occurred
  !                      UNITS:      N/A
  !                      TYPE:       INTEGER
  !                      DIMENSION:  Scalar
  !
  ! COMMENTS:
  !   Note the INTENT on the output RadDiag_Stats argument is IN OUT 
  !   rather than just OUT. This is necessary because the argument may be
  !   defined upon input. To prevent memory leaks, the IN OUT INTENT is
  !   a must.
  !
  ! CREATION HISTORY:
  !   Written by:     Paul van Delst, CIMSS/SSEC 28-Mar-2006
  !                   paul.vandelst@ssec.wisc.edu

  FUNCTION Assign_RadDiag_Stats( RadDiag_Stats_in,  &  ! Input
                                 RadDiag_Stats_out, &  ! Output
                                 RCS_Id,            &  ! Revision control
                                 Message_Log )      &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    TYPE(RadDiag_Stats_type), INTENT(IN)     :: RadDiag_Stats_in
    TYPE(RadDiag_Stats_type), INTENT(IN OUT) :: RadDiag_Stats_out
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),   OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_RadDiag_Stats'
    ! Local variables
    INTEGER :: i, l, is, it, n


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. Associated_RadDiag_Stats( RadDiag_Stats_In ) ) THEN
      Error_Status = Destroy_RadDiag_Stats( RadDiag_Stats_Out, &
                                                Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating output RadDiag_Stats_out pointer members.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
      RETURN
    END IF

    ! Allocate the structure
    Error_Status = Allocate_RadDiag_Stats( RadDiag_Stats_in%nPredictors, &
                                           RadDiag_Stats_in%nChannels, &
                                           RadDiag_Stats_in%nFOVs, &
                                           RadDiag_Stats_in%nTimes, &
                                           RadDiag_Stats_in%nVariables, &
                                           RadDiag_Stats_out, &
                                           Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output RadDiag_Stats arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign non-dimension scalar members
    RadDiag_Stats_out%StrLen = RadDiag_Stats_in%StrLen

    ! Assign array data
    i  = RadDiag_Stats_in%nPredictors
    l  = RadDiag_Stats_in%nChannels
    is = RadDiag_Stats_in%nFOVs
    it = RadDiag_Stats_in%nTimes
    n  = RadDiag_Stats_in%nVariables

    RadDiag_Stats_out%AirMassCoefficients = RadDiag_Stats_in%AirMassCoefficients(:i,:l,:it)
    RadDiag_Stats_out%VariableNames       = RadDiag_Stats_in%VariableNames(:n)
    RadDiag_Stats_out%Channel             = RadDiag_Stats_in%Channel(:l)
    RadDiag_Stats_out%FOV                 = RadDiag_Stats_in%FOV(:is)
    RadDiag_Stats_out%scan_Data           = RadDiag_Stats_in%scan_Data(:n,:l,:is)
    RadDiag_Stats_out%scan_nSamples       = RadDiag_Stats_in%scan_nSamples(:l,:is)
    RadDiag_Stats_out%DateTime            = RadDiag_Stats_in%DateTime(:it)
    RadDiag_Stats_out%time_Data           = RadDiag_Stats_in%time_Data(:n,:l,:it)
    RadDiag_Stats_out%time_nSamples       = RadDiag_Stats_in%time_nSamples(:l,:it)

  END FUNCTION Assign_RadDiag_Stats

END MODULE RadDiag_Stats_Define
