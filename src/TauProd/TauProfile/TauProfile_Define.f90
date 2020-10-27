!
! TauProfile_Define
!
! Module defining the TauProfile data structure and containing
! routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu

MODULE TauProfile_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data structure definition
  PUBLIC :: TauProfile_type
  ! Structure procedures
  PUBLIC :: Associated_TauProfile
  PUBLIC :: Destroy_TauProfile
  PUBLIC :: Allocate_TauProfile
  PUBLIC :: Assign_TauProfile
  PUBLIC :: Equal_TauProfile
  PUBLIC :: Concatenate_TauProfile
  PUBLIC :: CheckRelease_TauProfile
  PUBLIC :: Info_TauProfile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Destroy_TauProfile
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE Destroy_TauProfile


  ! -------------------------
  ! Module parameters
  ! -------------------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Keyword set value
  INTEGER,  PARAMETER :: SET = 1
  ! Sensor id string length
  INTEGER,  PARAMETER :: SL = 20
  ! Invalid values
  INTEGER,  PARAMETER :: IP_INVALID = -1
  REAL(fp), PARAMETER :: FP_INVALID = -1.0_fp
  ! Current valid release and version numbers
  INTEGER,  PARAMETER :: TAUPROFILE_RELEASE = 2
  INTEGER,  PARAMETER :: TAUPROFILE_VERSION = 1
  ! Sensor Id default values
  INTEGER,  PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER,  PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047


  ! -------------------------------
  ! TauProfile data type definition
  ! -------------------------------
  TYPE :: TauProfile_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER :: Release = TAUPROFILE_RELEASE
    INTEGER :: Version = TAUPROFILE_VERSION
    ! Dimensions
    INTEGER :: n_Layers        = 0 ! == K
    INTEGER :: n_Channels      = 0 ! == L
    INTEGER :: n_Angles        = 0 ! == I
    INTEGER :: n_Profiles      = 0 ! == M
    INTEGER :: n_Molecule_Sets = 0 ! == J
    ! Sensor Ids
    CHARACTER(SL) :: Sensor_ID        = ' '
    INTEGER       :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
    ! Dimension arrays
    REAL(fp), POINTER :: Level_Pressure(:) => NULL() ! 0:K
    INTEGER , POINTER :: Channel(:)        => NULL() ! L
    REAL(fp), POINTER :: Angle(:)          => NULL() ! I
    INTEGER , POINTER :: Profile(:)        => NULL() ! M
    INTEGER , POINTER :: Molecule_Set(:)   => NULL() ! J
    ! Geometric angle arrays
    REAL(fp), POINTER :: Geometric_Angle(:,:,:) => NULL() ! K x I x M
    ! Transmittance profiles
    REAL(fp), POINTER :: Tau(:,:,:,:,:)    => NULL() ! K x L x I x M x J
  END TYPE TauProfile_type


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
!       Associated_TauProfile
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       TauProfile structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_TauProfile( TauProfile       , &  ! Input
!                                                   ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       TauProfile:  TauProfile structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       TauProfile_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    TauProfile structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the TauProfile pointer
!                            members.
!                            .TRUE.  - if ALL the TauProfile pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the TauProfile pointer
!                                      members are associated.
!                            .FALSE. - some or all of the TauProfile pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! RESTRICTIONS:
!       This function tests the association status of the TauProfile
!       structure pointer members. Therefore this function must only
!       be called after the input TauProfile structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_TauProfile( TauProfile, & ! Input
                                  ANY_Test )  & ! Optional input
                                RESULT( Association_Status )
    ! Arguments
    TYPE(TauProfile_type), INTENT(IN) :: TauProfile
    INTEGER,       OPTIONAL, INTENT(IN) :: ANY_Test
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

    ! Tes thet structure pointer member association
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( TauProfile%Level_Pressure ) .AND. &
           ASSOCIATED( TauProfile%Channel        ) .AND. &
           ASSOCIATED( TauProfile%Angle          ) .AND. &
           ASSOCIATED( TauProfile%Profile        ) .AND. &
           ASSOCIATED( TauProfile%Molecule_Set   ) .AND. &
           ASSOCIATED( TauProfile%Geometric_Angle) .AND. &
           ASSOCIATED( TauProfile%Tau            )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( TauProfile%Level_Pressure ) .OR. &
           ASSOCIATED( TauProfile%Channel        ) .OR. &
           ASSOCIATED( TauProfile%Angle          ) .OR. &
           ASSOCIATED( TauProfile%Profile        ) .OR. &
           ASSOCIATED( TauProfile%Molecule_Set   ) .OR. &
           ASSOCIATED( TauProfile%Geometric_Angle) .OR. &
           ASSOCIATED( TauProfile%Tau            )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF
  END FUNCTION Associated_TauProfile


!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_TauProfile
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of TauProfile
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_TauProfile( TauProfile             , &  ! Output
!                                          RCS_Id     =RCS_Id     , &  ! Revision control
!                                          Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       TauProfile:   Re-initialised TauProfile structure.
!                     UNITS:      N/A
!                     TYPE:       TauProfile_type
!                     DIMENSION:  Scalar or Rank-1 array
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
!                     UNITS:      None
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
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( TauProfile , &  ! Output
                           No_Clear   , &  ! Optional input
                           RCS_Id     , &  ! Revision control
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(TauProfile_type) , INTENT(IN OUT) :: TauProfile
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_TauProfile(scalar)'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Reset the dimension indicators
    TauProfile%n_Layers        = 0
    TauProfile%n_Channels      = 0
    TauProfile%n_Angles        = 0
    TauProfile%n_Profiles      = 0
    TauProfile%n_Molecule_Sets = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_TauProfile( TauProfile )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_TauProfile( TauProfile ) ) RETURN


    ! Deallocate the TauProfile pointer members
    ! -----------------------------------------
    DEALLOCATE( TauProfile%Level_Pressure, &
                TauProfile%Channel       , &
                TauProfile%Angle         , &
                TauProfile%Profile       , &
                TauProfile%Molecule_Set  , &
                TauProfile%Geometric_Angle, &
                TauProfile%Tau           , &
                STAT = Allocate_Status     )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating TauProfile. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    TauProfile%n_Allocates = TauProfile%n_Allocates - 1
    IF ( TauProfile%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      TauProfile%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( TauProfile , &  ! Output
                          No_Clear   , &  ! Optional input
                          RCS_Id     , &  ! Revision control
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(TauProfile_type) , INTENT(IN OUT) :: TauProfile(:)
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_TauProfile(rank-1)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Loop over elements
    ! ------------------
    DO i = 1, SIZE( TauProfile )

      ! Clear the current structure array element
      Scalar_Status = Destroy_Scalar( TauProfile(i)          , &
                                      No_Clear   =No_Clear   , &
                                      Message_Log=Message_Log  )

      ! If it failed, set the return error status, but
      ! continue to attempt to destroy structure array
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '("Error destroying TauProfile structure array element ",i0)') i
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
!       Allocate_TauProfile
! 
! PURPOSE:
!       Function to allocate the pointer members of the TauProfile
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_TauProfile( n_Layers               , &  ! Input
!                                           n_Channels             , &  ! Input
!                                           n_Angles               , &  ! Input
!                                           n_Profiles             , &  ! Input
!                                           n_Molecule_Sets        , &  ! Input
!                                           TauProfile             , &  ! Output
!                                           RCS_Id     =RCS_Id     , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:         Number of atmospheric layers dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Channels:       Number of spectral channels dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Angles:         Number of view angles dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Profiles:       Number of atmospheric profiles dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Molecule_Sets:  Number of molecular species/sets dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       TauProfile:       TauProfile structure with allocated
!                         pointer members
!                         UNITS:      N/A
!                         TYPE:       TauProfile_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      None
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the ERROR_HANDLER module.
!                         If == SUCCESS the structure pointer allocations were successful
!                            == FAILURE - an error occurred, or
!                                       - the structure internal allocation counter
!                                         is not equal to one (1) upon exiting this
!                                         function. This value is incremented and
!                                         decremented for every structure allocation
!                                         and deallocation respectively.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_TauProfile( n_Layers       , &  ! Input
                                n_Channels     , &  ! Input
                                n_Angles       , &  ! Input
                                n_Profiles     , &  ! Input
                                n_Molecule_Sets, &  ! Input
                                TauProfile     , &  ! Output
                                RCS_Id         , &  ! Revision control
                                Message_Log    ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Layers
    INTEGER               , INTENT(IN)     :: n_Channels
    INTEGER               , INTENT(IN)     :: n_Angles
    INTEGER               , INTENT(IN)     :: n_Profiles
    INTEGER               , INTENT(IN)     :: n_Molecule_Sets
    TYPE(TauProfile_type) , INTENT(IN OUT) :: TauProfile
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_TauProfile'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimensions
    IF ( n_Layers        < 1 .OR. &
         n_Channels      < 1 .OR. &
         n_Angles        < 1 .OR. &
         n_Profiles      < 1 .OR. &
         n_Molecule_Sets < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauProfile dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_TauProfile( TauProfile, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_TauProfile( TauProfile             , &
                                         No_Clear   =SET        , &
                                         Message_Log=Message_Log  )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating TauProfile pointer members.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Perform the pointer allocation
    ! ------------------------------
    ALLOCATE( TauProfile%Level_Pressure( 0:n_Layers ), &
              TauProfile%Channel( n_Channels ), &     
              TauProfile%Angle( n_Angles ), &
              TauProfile%Profile( n_Profiles ), &
              TauProfile%Molecule_Set( n_Molecule_Sets ), &
              TauProfile%Geometric_Angle( n_Layers, &
                                          n_Angles, &
                                          n_Profiles ), &
              TauProfile%Tau( n_Layers, &
                              n_Channels, &
                              n_Angles, &
                              n_Profiles, &
                              n_Molecule_Sets ), & 
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating TauProfile data arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the dimensions
    ! ---------------------
    TauProfile%n_Layers        = n_Layers
    TauProfile%n_Channels      = n_Channels
    TauProfile%n_Angles        = n_Angles
    TauProfile%n_Profiles      = n_Profiles
    TauProfile%n_Molecule_Sets = n_Molecule_Sets


    ! Initialise the arrays
    ! ---------------------
    TauProfile%Level_Pressure = FP_INVALID
    TauProfile%Channel        = IP_INVALID
    TauProfile%Angle          = FP_INVALID
    TauProfile%Profile        = IP_INVALID
    TauProfile%Molecule_Set   = IP_INVALID
    TauProfile%Geometric_Angle= FP_INVALID
    TauProfile%Tau            = FP_INVALID


    ! Increment and test the allocation counter
    ! -----------------------------------------
    TauProfile%n_Allocates = TauProfile%n_Allocates + 1
    IF ( TauProfile%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      TauProfile%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_TauProfile


!------------------------------------------------------------------------------
!
! NAME:
!       Assign_TauProfile
!
! PURPOSE:
!       Function to copy valid TauProfile structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_TauProfile( TauProfile_in          , &  ! Input
!                                         TauProfile_out         , &  ! Output
!                                         RCS_Id     =RCS_Id     , &  ! Revision control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauProfile_in:  TauProfile structure which is to be copied.
!                       UNITS:      N/A
!                       TYPE:       TauProfile_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       TauProfile_out: Copy of the input structure, TauProfile_in.
!                       UNITS:      N/A
!                       TYPE:       TauProfile_type
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
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the structure assignment was successful
!                          == FAILURE an error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Assign_TauProfile( TauProfile_in , &  ! Input
                              TauProfile_out, &  ! Output
                              RCS_Id        , &  ! Revision control
                              Message_Log   ) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(TauProfile_type) , INTENT(IN)     :: TauProfile_in
    TYPE(TauProfile_type) , INTENT(IN OUT) :: TauProfile_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_TauProfile'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_TauProfile( TauProfile_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauProfile pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Allocate data arrays
    ! --------------------
    Error_Status = Allocate_TauProfile( TauProfile_in%n_Layers       , &
                                        TauProfile_in%n_Channels     , &
                                        TauProfile_in%n_Angles       , &
                                        TauProfile_in%n_Profiles     , &
                                        TauProfile_in%n_Molecule_Sets, &
                                        TauProfile_out               , &
                                        Message_Log=Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output TauProfile structure.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign non-dimension scalar members
    ! -----------------------------------
    TauProfile_out%Sensor_ID        = TauProfile_in%Sensor_ID
    TauProfile_out%WMO_Satellite_ID = TauProfile_in%WMO_Satellite_ID
    TauProfile_out%WMO_Sensor_ID    = TauProfile_in%WMO_Sensor_ID

    ! Copy array data
    ! ---------------
    TauProfile_out%Level_Pressure = TauProfile_in%Level_Pressure
    TauProfile_out%Channel        = TauProfile_in%Channel
    TauProfile_out%Angle          = TauProfile_in%Angle
    TauProfile_out%Profile        = TauProfile_in%Profile
    TauProfile_out%Molecule_Set   = TauProfile_in%Molecule_Set
    TauProfile_out%Geometric_Angle= TauProfile_in%Geometric_Angle
    TauProfile_out%Tau            = TauProfile_in%Tau

  END FUNCTION Assign_TauProfile


!------------------------------------------------------------------------------
!
! NAME:
!       Concatenate_TauProfile
!
! PURPOSE:
!       Function to concatenate two valid TauProfile structures.
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_TauProfile( TauProfile1            , &  ! Input/Output
!                                              TauProfile2            , &  ! Input
!                                              By_Profile =By_Profile , &  ! Optional input
!                                              RCS_Id     =RCS_Id     , &  ! Revision control
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauProfile1:   First TauProfile structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       TauProfile2:   Second TauProfile structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       TauProfile1:   The concatenated TauProfile structure. The order of
!                      concatenation is TauProfile1,TauProfile2 along the 
!                      absorber dimension.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       By_Profile:    Set this optional argument to indicate that the 
!                      structure concatenation is to be performed along
!                      the PROFILE dimension. If not specified, the default
!                      action is to concatenate the structures along the 
!                      MOLECULE_SET dimension.
!                      If == 0, or unspecified, concatenation is along
!                               the molecule set dimension **DEFAULT**
!                         == 1, concatenation along profile dimension.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      None
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      None
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure concatenation was successful
!                         == FAILURE an error occurred
!                         == WARNING the destruction of a temporary, local
!                                    TauProfile structure failed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The input TauProfile1 argument contains the concatenated structure
!       data (in character-speak: TauProfile1//TauProfile2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input TauProfile1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
!------------------------------------------------------------------------------

  FUNCTION Concatenate_TauProfile( TauProfile1 , &  ! Input/Output
                                   TauProfile2 , &  ! Input
                                   By_Profile  , &  ! Optional Input
                                   RCS_Id      , &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    TYPE(TauProfile_type) , INTENT(IN OUT) :: TauProfile1
    TYPE(TauProfile_type) , INTENT(IN)     :: TauProfile2
    INTEGER     , OPTIONAL, INTENT(IN)     :: By_Profile
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Concatenate_TauProfile'
    ! Local variables
    LOGICAL :: By_Molecule_Set
    INTEGER :: Destroy_Status
    INTEGER :: n_Profiles,      m1, m2
    INTEGER :: n_Molecule_Sets, j1, j2
    TYPE(TauProfile_type) :: TauProfile_Tmp

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Concatentation is along the molecule
    ! set dimension by default...
    By_Molecule_Set = .TRUE.
    ! ...unless the BY_PROFILE optional
    ! argument is set
    IF ( PRESENT( By_Profile ) ) THEN
      IF ( By_Profile == SET ) By_Molecule_Set = .FALSE.
    END IF

    ! ALL structure pointers must be associated
    IF ( .NOT. Associated_TauProfile( TauProfile1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauProfile1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_TauProfile( TauProfile2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauProfile2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the non-concatenation dimensions
    IF ( TauProfile1%n_Layers   /= TauProfile2%n_Layers   .OR. &
         TauProfile1%n_Channels /= TauProfile2%n_Channels .OR. &
         TauProfile1%n_Angles   /= TauProfile2%n_Angles        ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'n_Layers, n_Channels, or n_Angles TauProfile dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the concatenation dimensions
    IF ( By_Molecule_Set ) THEN
      IF ( TauProfile1%n_Profiles /= TauProfile2%n_Profiles ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'n_Profiles TauProfile dimensions are different.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    ELSE
      IF ( TauProfile1%n_Molecule_Sets /= TauProfile2%n_Molecule_Sets ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'n_Molecule_Sets TauProfile dimensions are different.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Check the IDs
    IF ( TauProfile1%Sensor_ID        /= TauProfile2%Sensor_ID        .OR. &
         TauProfile1%WMO_Satellite_ID /= TauProfile2%WMO_Satellite_ID .OR. &
         TauProfile1%WMO_Sensor_ID    /= TauProfile2%WMO_Sensor_ID         ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile sensor ID values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the level pressure, channel, or angle values
    IF ( ANY( .NOT. Compare_Float( TauProfile1%Level_Pressure, &
                                   TauProfile2%Level_Pressure  ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile level pressure values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( ANY( ( TauProfile1%Channel - TauProfile2%Channel ) /= 0 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile channel values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( ANY( .NOT. Compare_Float( TauProfile1%Angle, &
                                   TauProfile2%Angle  ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile angle values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the profile or molecule set values
    IF ( By_Molecule_Set ) THEN
      IF ( ANY( ( TauProfile1%Profile - TauProfile2%Profile ) /= 0 ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'TauProfile profile numbers are different.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    ELSE
      IF ( ANY( ( TauProfile1%Molecule_Set - TauProfile2%Molecule_Set ) /= 0 ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'TauProfile molecule set IDs are different.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Copy the first input TauProfile structure
    ! -----------------------------------------
    Error_Status = Assign_TauProfile( TauProfile1, TauProfile_Tmp, &
                                      Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying TauProfile1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

   
    ! Reallocate the first input TauProfile structure
    ! -----------------------------------------------
    ! Destroy it
    Error_Status = Destroy_TauProfile( TauProfile1, &
                                       Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauProfile1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Re-Allocate it
    Reallocate_TauProfile1: IF ( By_Molecule_Set ) THEN
    
      ! Set the total number of molecule sets
      n_Molecule_Sets = TauProfile_Tmp%n_Molecule_Sets + TauProfile2%n_Molecule_Sets
      
      ! Perform the allocation
      Error_Status = Allocate_TauProfile( TauProfile_Tmp%n_Layers  , &
                                          TauProfile_Tmp%n_Channels, &
                                          TauProfile_Tmp%n_Angles  , &
                                          TauProfile_Tmp%n_Profiles, &
                                          n_Molecule_Sets          , &
                                          TauProfile1              , &
                                          Message_Log=Message_Log    )

    ELSE

      ! Set the total number of profiles
      n_Profiles = TauProfile_Tmp%n_Profiles + TauProfile2%n_Profiles
      
      ! Perform the allocation
      Error_Status = Allocate_TauProfile( TauProfile_Tmp%n_Layers, &
                                          TauProfile_Tmp%n_Channels, &
                                          TauProfile_Tmp%n_Angles, &
                                          n_Profiles, &
                                          TauProfile_Tmp%n_Molecule_Sets, &
                                          TauProfile1, &
                                          Message_Log=Message_Log )

    END IF Reallocate_TauProfile1

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating TauProfile1 structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the non-concatenation data
    ! ---------------------------------
    TauProfile1%Sensor_ID        = TauProfile_Tmp%Sensor_ID
    TauProfile1%WMO_Satellite_ID = TauProfile_Tmp%WMO_Satellite_ID
    TauProfile1%WMO_Sensor_ID    = TauProfile_Tmp%WMO_Sensor_ID
    TauProfile1%Level_Pressure = TauProfile_Tmp%Level_Pressure
    TauProfile1%Channel        = TauProfile_Tmp%Channel
    TauProfile1%Angle          = TauProfile_Tmp%Angle


    ! Concatenate the required bits
    ! -----------------------------
    Concatenate_TauProfile1: IF ( By_Molecule_Set ) THEN

      ! Concatenate the molecule sets
      ! -----------------------------
      ! Assign the profile numbers
      TauProfile1%Profile = TauProfile_Tmp%Profile
      ! The first part
      j1 = 1
      j2 = TauProfile_Tmp%n_Molecule_Sets
      TauProfile1%Molecule_Set(j1:j2) = TauProfile_Tmp%Molecule_Set
      TauProfile1%Tau(:,:,:,:,j1:j2)  = TauProfile_Tmp%Tau
      ! The second part
      j1 = j2 + 1
      j2 = n_Molecule_Sets
      TauProfile1%Molecule_Set(j1:j2) = TauProfile2%Molecule_Set
      TauProfile1%Tau(:,:,:,:,j1:j2)  = TauProfile2%Tau

    ELSE

      ! Concatenate the profiles
      ! ------------------------
      ! Assign the molecule set ID values
      TauProfile1%Molecule_Set = TauProfile_Tmp%Molecule_Set
      ! The first part
      m1 = 1
      m2 = TauProfile_Tmp%n_Profiles
      TauProfile1%Profile(m1:m2)     = TauProfile_Tmp%Profile
      TauProfile1%Geometric_Angle(:,:,m1:m2) = TauProfile_Tmp%Geometric_Angle
      TauProfile1%Tau(:,:,:,m1:m2,:) = TauProfile_Tmp%Tau
      ! The second part
      m1 = m2 + 1
      m2 = n_Profiles
      TauProfile1%Profile(m1:m2)     = TauProfile2%Profile
      TauProfile1%Geometric_Angle(:,:,m1:m2) = TauProfile2%Geometric_Angle
      TauProfile1%Tau(:,:,:,m1:m2,:) = TauProfile2%Tau

    END IF Concatenate_TauProfile1


    ! Clean up
    ! --------
    Destroy_Status = Destroy_TauProfile( TauProfile_Tmp, &
                                         Message_Log=Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauProfile_Tmp structure.', &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Concatenate_TauProfile


!--------------------------------------------------------------------------------
!
! NAME:
!       Equal_TauProfile
!
! PURPOSE:
!       Function to test if two TauProfile structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_TauProfile( TauProfile_LHS         , &  ! Input
!                                        TauProfile_RHS         , &  ! Input
!                                        ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                        Check_All  =Check_All  , &  ! Optional input
!                                        RCS_Id     =RCS_Id     , &  ! Revision control
!                                        Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauProfile_LHS:  TauProfile structure to be compared; equivalent to the
!                        left-hand side of a lexical comparison, e.g.
!                          IF ( TauProfile_LHS == TauProfile_RHS ).
!                        UNITS:      N/A
!                        TYPE:       TauProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       TauProfile_RHS:  TauProfile structure to be compared to; equivalent to
!                        right-hand side of a lexical comparison, e.g.
!                          IF ( TauProfile_LHS == TauProfile_RHS ).
!                        UNITS:      N/A
!                        TYPE:       Same as TauProfile_LHS
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:       Unit of data precision used to scale the floating
!                        point comparison. ULP stands for "Unit in the Last Place,"
!                        the smallest possible increment or decrement that can be
!                        made using a machine's floating point arithmetic.
!                        Value must be positive - if a negative value is supplied,
!                        the absolute value is used. If not specified, the default
!                        value is 1.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:       Set this argument to check ALL the floating point
!                        channel data of the TauProfile structures. The default
!                        action is return with a FAILURE status as soon as
!                        any difference is found. This optional argument can
!                        be used to get a listing of ALL the differences
!                        between data in TauProfile structures.
!                        If == 0, Return with FAILURE status as soon as
!                                 ANY difference is found  *DEFAULT*
!                           == 1, Set FAILURE status if ANY difference is
!                                 found, but continue to check ALL data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
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
!                        If == SUCCESS the structures were equal
!                           == FAILURE - an error occurred, or
!                                      - the structures were different.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Equal_TauProfile( TauProfile_LHS, &  ! Input
                             TauProfile_RHS, &  ! Input
                             ULP_Scale     , &  ! Optional input
                             Check_All     , &  ! Optional input
                             RCS_Id        , &  ! Revision control
                             Message_Log   ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    TYPE(TauProfile_type) , INTENT(IN)  :: TauProfile_LHS
    TYPE(TauProfile_type) , INTENT(IN)  :: TauProfile_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER     , OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_TauProfile'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: k, l, i, m, j

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
    IF ( .NOT. Associated_TauProfile( TauProfile_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT TauProfile_LHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_TauProfile( TauProfile_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT TauProfile_RHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check dimensions
    IF (TauProfile_LHS%n_Layers        /= TauProfile_RHS%n_Layers        .OR. &
        TauProfile_LHS%n_Channels      /= TauProfile_RHS%n_Channels      .OR. &
        TauProfile_LHS%n_Angles        /= TauProfile_RHS%n_Angles        .OR. &
        TauProfile_LHS%n_Profiles      /= TauProfile_RHS%n_Profiles      .OR. &
        TauProfile_LHS%n_Molecule_Sets /= TauProfile_RHS%n_Molecule_Sets      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check the scalar components
    ! ---------------------------
    IF ( TauProfile_LHS%Sensor_Id /= TauProfile_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile Sensor_Id values are different; >'//&
                            TRIM(TauProfile_LHS%Sensor_Id)//'< vs >'//&
                            TRIM(TauProfile_RHS%Sensor_Id)//'<', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( TauProfile_LHS%WMO_Satellite_Id /= TauProfile_RHS%WMO_Satellite_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile scalar component WMO_Satellite_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( TauProfile_LHS%WMO_Sensor_Id /= TauProfile_RHS%WMO_Sensor_Id ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile scalar component WMO_Sensor_Id values are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    
    ! Check the array components
    ! --------------------------
    DO k = 0, TauProfile_LHS%n_Layers
      IF ( .NOT. Compare_Float( TauProfile_LHS%Level_Pressure(k), &
                                TauProfile_RHS%Level_Pressure(k), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("TauProfile array component Level_Pressure values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        k
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO l = 1, TauProfile_LHS%n_Channels
      IF ( TauProfile_LHS%Channel(l) /= TauProfile_RHS%Channel(l) ) THEN
        WRITE(Message,'("TauProfile array component Channel values ",&
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
    DO i = 1, TauProfile_LHS%n_Angles
      IF ( .NOT. Compare_Float( TauProfile_LHS%Angle(i), &
                                TauProfile_RHS%Angle(i), &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("TauProfile array component Angle values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        i
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO m = 1, TauProfile_LHS%n_Profiles
      IF ( TauProfile_LHS%Profile(m) /= TauProfile_RHS%Profile(m) ) THEN
        WRITE(Message,'("TauProfile array component Profile values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        m
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO j = 1, TauProfile_LHS%n_Molecule_Sets
      IF ( TauProfile_LHS%Molecule_Set(j) /= TauProfile_RHS%Molecule_Set(j) ) THEN
        WRITE(Message,'("TauProfile array component Molecule_Set values ",&
                        &"are different at index (",1(1x,i0),")")') &
                        j
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO m = 1, TauProfile_LHS%n_Profiles                                               
      DO i = 1, TauProfile_LHS%n_Angles                                               
         DO k = 1, TauProfile_LHS%n_Layers                                            
           IF ( .NOT. Compare_Float( TauProfile_LHS%Geometric_Angle(k,i,m), &         
                                     TauProfile_RHS%Geometric_Angle(k,i,m), &         
                                     ULP=ULP ) ) THEN                                 
             WRITE(Message,'("TauProfile array component Geometric_Angle values ",&   
                             &"are different at indices (",3(1x,i0),")")') &          
                             k,i,m                                                    
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
     
    DO j = 1, TauProfile_LHS%n_Molecule_Sets
      DO m = 1, TauProfile_LHS%n_Profiles
        DO i = 1, TauProfile_LHS%n_Angles
          DO l = 1, TauProfile_LHS%n_Channels
            DO k = 1, TauProfile_LHS%n_Layers
              IF ( .NOT. Compare_Float( TauProfile_LHS%Tau(k,l,i,m,j), &
                                        TauProfile_RHS%Tau(k,l,i,m,j), &
                                        ULP=ULP ) ) THEN
                WRITE(Message,'("TauProfile array component Tau values ",&
                                &"are different at indices (",5(1x,i0),")")') &
                                k,l,i,m,j
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
      END DO
    END DO
  END FUNCTION Equal_TauProfile


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckRelease_TauProfile
!
! PURPOSE:
!       Function to check the TauProfile Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_TauProfile( TauProfile             , &  ! Input
!                                               RCS_Id     =RCS_Id     , &  ! Revision control
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauProfile:    TauProfile structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
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
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure Release value is valid.
!                         == FAILURE the structure Release value is NOT valid
!                                    and either a data file file or software
!                                    update is required.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION CheckRelease_TauProfile( TauProfile    , &  ! Input
                                    RCS_Id     , &  ! Revision control
                                    Message_Log) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    TYPE(TauProfile_type) , INTENT(IN)  :: TauProfile
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_TauProfile'
    ! Local variables
    CHARACTER(256) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Check release is not too old
    ! ----------------------------
    IF ( TauProfile%Release < TAUPROFILE_RELEASE ) THEN
      WRITE( Message, '( "A TauProfile data update is needed. ", &
                        &"TauProfile release is ", i0, &
                        &". Valid release is ",i0,"." )' ) &
                      TauProfile%Release, TAUPROFILE_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check release is not too new
    ! ----------------------------
    IF ( TauProfile%Release > TAUPROFILE_RELEASE ) THEN
      WRITE( Message, '( "A TauProfile software update is needed. ", &
                        &"TauProfile release is ", i0, &
                        &". Valid release is ",i0,"." )' ) &
                      TauProfile%Release, TAUPROFILE_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_TauProfile


!--------------------------------------------------------------------------------
!
! NAME:
!       Info_TauProfile
!
! PURPOSE:
!       Subroutine to return a string containing information about the
!       TauProfile data structure.
!
! CALLING SEQUENCE:
!       CALL Info_TauProfile( TauProfile   , &  ! Input
!                             Info         , &  ! Output
!                             RCS_Id=RCS_Id  )  ! Revision control
! 
! INPUT ARGUMENTS:
!       TauProfile:    Filled TauProfile structure.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing information about the passed
!                      TauProfile data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      None
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!--------------------------------------------------------------------------------

  SUBROUTINE Info_TauProfile( TauProfile, &  ! Input
                              Info      , &  ! Output
                              RCS_Id      )  ! Revision control
    ! Arguments
    TYPE(TauProfile_type) , INTENT(IN)  :: TauProfile
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

    ! Write the required data to the local string
    ! -------------------------------------------
    FmtString = '( a,1x,a,1x,"TauProfile RELEASE.VERSION: ",i2,".",i2.2,2x,&
                   &"N_LAYERS=",i3,2x,&
                   &"N_CHANNELS=",i4,2x,&
                   &"N_ANGLES=",i1,2x,&
                   &"N_PROFILES=",i3,2x,&
                   &"N_MOLECULE_SETS=",i2 )'
    WRITE( LongString, FMT=FmtString) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           TRIM(TauProfile%Sensor_ID), &
           TauProfile%Release        , &
           TauProfile%Version        , &
           TauProfile%n_Layers       , &
           TauProfile%n_Channels     , &
           TauProfile%n_Angles       , &
           TauProfile%n_Profiles     , &
           TauProfile%n_Molecule_Sets

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_TauProfile



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
!       Clear_TauProfile
!
! PURPOSE:
!       Subroutine to clear the scalar members of a TauProfile structure.
!
! CALLING SEQUENCE:
!       CALL Clear_TauProfile( TauProfile ) ! Output
!
! OUTPUT ARGUMENTS:
!       TauProfile:  TauProfile structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       TauProfile_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_TauProfile( TauProfile )
    TYPE(TauProfile_type), INTENT(IN OUT) :: TauProfile
    TauProfile%Release = TAUPROFILE_RELEASE
    TauProfile%Version = TAUPROFILE_VERSION
    TauProfile%Sensor_ID        = ' '
    TauProfile%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    TauProfile%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
  END SUBROUTINE Clear_TauProfile

END MODULE TauProfile_Define
