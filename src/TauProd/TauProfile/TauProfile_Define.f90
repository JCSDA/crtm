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
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, &
                                   Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Associated_TauProfile
  PUBLIC :: Destroy_TauProfile
  PUBLIC :: Allocate_TauProfile
  PUBLIC :: Assign_TauProfile
  PUBLIC :: Concatenate_TauProfile
  PUBLIC :: Information_TauProfile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Destroy_TauProfile
    MODULE PROCEDURE Destroy_TauProfile_scalar
    MODULE PROCEDURE Destroy_TauProfile_rank1
  END INTERFACE Destroy_TauProfile


  ! -------------------------
  ! Module parameters
  ! -------------------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: TauProfile_Define.f90,v 1.11 2006/06/30 16:47:16 dgroff Exp $'
  ! Sensor id string length
  INTEGER , PARAMETER :: SL = 20
  ! Invalid values
  INTEGER , PARAMETER :: IP_INVALID = -1
  REAL(fp), PARAMETER :: FP_INVALID = REAL(IP_INVALID,fp)
  ! Invalid sensor ids
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1


  ! -------------------------------
  ! TauProfile data type definition
  ! -------------------------------
  TYPE, PUBLIC :: TauProfile_type
    INTEGER :: n_Allocates = 0
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
    REAL(fp), DIMENSION(:), POINTER :: Level_Pressure => NULL() ! K+1
    INTEGER , DIMENSION(:), POINTER :: Channel        => NULL() ! L
    REAL(fp), DIMENSION(:), POINTER :: Angle          => NULL() ! I
    INTEGER , DIMENSION(:), POINTER :: Profile        => NULL() ! M
    INTEGER , DIMENSION(:), POINTER :: Molecule_Set   => NULL() ! J
    ! Transmittance profiles
    REAL(fp), DIMENSION(:,:,:,:,:), POINTER :: Tau => NULL() ! K x L x I x M x J
  END TYPE TauProfile_type


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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Jun-2002
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_TauProfile( TauProfile )
    TYPE(TauProfile_type), INTENT(IN OUT) :: TauProfile
    TauProfile%n_Layers        = 0
    TauProfile%n_Channels      = 0
    TauProfile%n_Angles        = 0
    TauProfile%n_Profiles      = 0
    TauProfile%n_Molecule_Sets = 0
    TauProfile%Sensor_ID        = ' '
    TauProfile%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    TauProfile%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
  END SUBROUTINE Clear_TauProfile


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
           ASSOCIATED( TauProfile%Tau            )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( TauProfile%Level_Pressure ) .OR. &
           ASSOCIATED( TauProfile%Channel        ) .OR. &
           ASSOCIATED( TauProfile%Angle          ) .OR. &
           ASSOCIATED( TauProfile%Profile        ) .OR. &
           ASSOCIATED( TauProfile%Molecule_Set   ) .OR. &
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
! OUTPUT ARGUMENTS:
!       TauProfile:   Re-initialised TauProfile structure.
!                     UNITS:      N/A
!                     TYPE:       TauProfile_type
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN OUT)
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_TauProfile_scalar( TauProfile,   &  ! Output
                                      No_Clear,     &  ! Optional input
                                      RCS_Id,       &  ! Revision control
                                      Message_Log ) &  ! Error messaging
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF

    ! Initialise the scalar members
    IF ( Clear ) CALL Clear_TauProfile( TauProfile )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_TauProfile( TauProfile ) ) RETURN

    ! Deallocate the TauProfile pointer members
    DEALLOCATE( TauProfile%Level_Pressure, &
                TauProfile%Channel       , &
                TauProfile%Angle         , &
                TauProfile%Profile       , &
                TauProfile%Molecule_Set  , &
                TauProfile%Tau           , &
                STAT = Allocate_Status     )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating TauProfile. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

    ! Decrement and test allocation counter
    TauProfile%n_Allocates = TauProfile%n_Allocates - 1
    IF ( TauProfile%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      TauProfile%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_TauProfile_scalar


  FUNCTION Destroy_TauProfile_rank1( TauProfile,   &  ! Output
                                     No_Clear,     &  ! Optional input
                                     RCS_Id,       &  ! Revision control
                                     Message_Log ) &  ! Error messaging
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Perform reinitialisation
    DO i = 1, SIZE( TauProfile )

      ! Clear the current structure array element
      Scalar_Status = Destroy_TauProfile_scalar( TauProfile(i)          , &
                                                 No_Clear   =No_Clear   , &
                                                 Message_Log=Message_Log  )

      ! If it failed, set the return error status, but
      ! continue to attempt to destroy structure array
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '("Error destroying TauProfile structure array element ",i0)') i
        CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      END IF
    END DO

  END FUNCTION Destroy_TauProfile_rank1


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
! OUTPUT ARGUMENTS:
!       TauProfile:       TauProfile structure with allocated
!                         pointer members
!                         UNITS:      N/A
!                         TYPE:       TauProfile_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure pointer allocations were successful
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
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
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
                            Message_Log = Message_Log )
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

    ! Perform the allocation
    ALLOCATE( TauProfile%Level_Pressure( n_Layers+1 ), &     
              TauProfile%Channel( n_Channels ), &     
              TauProfile%Angle( n_Angles ), &
              TauProfile%Profile( n_Profiles ), &
              TauProfile%Molecule_Set( n_Molecule_Sets ), &
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
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the dimensions
    TauProfile%n_Layers        = n_Layers
    TauProfile%n_Channels      = n_Channels
    TauProfile%n_Angles        = n_Angles
    TauProfile%n_Profiles      = n_Profiles
    TauProfile%n_Molecule_Sets = n_Molecule_Sets

    ! Initialise the arrays
    TauProfile%Level_Pressure = FP_INVALID
    TauProfile%Channel        = IP_INVALID
    TauProfile%Angle          = FP_INVALID
    TauProfile%Profile        = IP_INVALID
    TauProfile%Molecule_Set   = IP_INVALID
    TauProfile%Tau            = FP_INVALID

    ! Increment and test the allocation counter
    TauProfile%n_Allocates = TauProfile%n_Allocates + 1
    IF ( TauProfile%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      TauProfile%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
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
! OUTPUT ARGUMENTS:
!       TauProfile_out: Copy of the input structure, TauProfile_in.
!                       UNITS:      N/A
!                       TYPE:       TauProfile_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_TauProfile( TauProfile_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauProfile pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Allocate data arrays
    Error_Status = Allocate_TauProfile( TauProfile_in%n_Layers       , &
                                        TauProfile_in%n_Channels     , &
                                        TauProfile_in%n_Angles       , &
                                        TauProfile_in%n_Profiles     , &
                                        TauProfile_in%n_Molecule_Sets, &
                                        TauProfile_out               , &
                                        Message_Log=Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output TauProfile arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign scalar members
    TauProfile_out%Sensor_ID        = TauProfile_in%Sensor_ID
    TauProfile_out%WMO_Satellite_ID = TauProfile_in%WMO_Satellite_ID
    TauProfile_out%WMO_Sensor_ID    = TauProfile_in%WMO_Sensor_ID

    ! Assign array data
    TauProfile_out%Level_Pressure = TauProfile_in%Level_Pressure
    TauProfile_out%Channel        = TauProfile_in%Channel
    TauProfile_out%Angle          = TauProfile_in%Angle
    TauProfile_out%Profile        = TauProfile_in%Profile
    TauProfile_out%Molecule_Set   = TauProfile_in%Molecule_Set
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
! OUTPUT ARGUMENTS:
!       TauProfile1:   The concatenated TauProfile structure. The order of
!                      concatenation is TauProfile1,TauProfile2 along the 
!                      absorber dimension.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Concatenate_TauProfile( TauProfile1 , &  ! Input/Output
                                   TauProfile2 , &  ! Input
                                   By_Profile  , &  ! Optional Input
                                   RCS_Id      , &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    TYPE(TauProfile_type) , INTENT(IN OUT)  :: TauProfile1
    TYPE(TauProfile_type) , INTENT(IN)      :: TauProfile2
    INTEGER     , OPTIONAL, INTENT(IN)      :: By_Profile
    CHARACTER(*), OPTIONAL, INTENT(OUT)     :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)      :: Message_Log
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
    !
    ! The first structure
    IF ( .NOT. Associated_TauProfile( TauProfile1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauProfile1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! The second structure
    IF ( .NOT. Associated_TauProfile( TauProfile2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauProfile2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
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
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check the concatenation dimensions
    IF ( By_Molecule_Set ) THEN
      IF ( TauProfile1%n_Profiles /= TauProfile2%n_Profiles ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'n_Profiles TauProfile dimensions are different.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    ELSE
      IF ( TauProfile1%n_Molecule_Sets /= TauProfile2%n_Molecule_Sets ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'n_Molecule_Sets TauProfile dimensions are different.', &
                              Error_Status, &
                              Message_Log = Message_Log )
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
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Check the level pressure, channel, or angle values
    !
    ! All the pressures must be the same
    IF ( ANY( .NOT. Compare_Float( TauProfile1%Level_Pressure, &
                                   TauProfile2%Level_Pressure  ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile level pressure values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    ! All the channels numbers must be the same
    IF ( ANY( ( TauProfile1%Channel - TauProfile2%Channel ) /= 0 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile channel values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    ! All the angle values must be the same
    IF ( ANY( .NOT. Compare_Float( TauProfile1%Angle, &
                                   TauProfile2%Angle  ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile angle values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check the profile or molecule set values
    IF ( By_Molecule_Set ) THEN
      ! All the molecule set IDs must be the same
      IF ( ANY( ( TauProfile1%Molecule_Set - TauProfile2%Molecule_Set ) /= 0 ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'TauProfile molecule set IDs are different.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    ELSE
      ! All the profile number values must be the same
      IF ( ANY( ( TauProfile1%Profile - TauProfile2%Profile ) /= 0 ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'TauProfile profile numbers are different.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! Copy the first input TauProfile structure
    Error_Status = Assign_TauProfile( TauProfile1, TauProfile_Tmp, &
                                      Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying TauProfile1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

   
    ! Reallocate the first input TauProfile structure
    !
    ! Destroy it
    Error_Status = Destroy_TauProfile( TauProfile1, &
                                       Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauProfile1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
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

    ELSE ! Reallocate_TauProfile1

      ! Set the total number of profiles
      n_Profiles = TauProfile_Tmp%n_Profiles + TauProfile2%n_Profiles
      ! Perform the allocation
      Error_Status = Allocate_TauProfile( TauProfile_Tmp%n_Layers, &
                                          TauProfile_Tmp%n_Channels, &
                                          TauProfile_Tmp%n_Angles, &
                                          n_Profiles, &
                                          TauProfile_Tmp%n_Molecule_Sets, &
                                          TauProfile1, &
                                          Message_Log = Message_Log )

    END IF Reallocate_TauProfile1

    ! Check for errors
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating TauProfile1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign the non-concatenation data
    TauProfile1%Sensor_ID        = TauProfile_Tmp%Sensor_ID
    TauProfile1%WMO_Satellite_ID = TauProfile_Tmp%WMO_Satellite_ID
    TauProfile1%WMO_Sensor_ID    = TauProfile_Tmp%WMO_Sensor_ID
    TauProfile1%Level_Pressure = TauProfile_Tmp%Level_Pressure
    TauProfile1%Channel        = TauProfile_Tmp%Channel
    TauProfile1%Angle          = TauProfile_Tmp%Angle


    ! Concatenate the required bits
    Concatenate_TauProfile1: IF ( By_Molecule_Set ) THEN

      ! Concatenate the molecule sets
      !
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

    ELSE ! Concatenate_TauProfile1

      ! Concatenate the profiles
      !
      ! Assign the molecule set ID values
      TauProfile1%Molecule_Set = TauProfile_Tmp%Molecule_Set
      ! The first part
      m1 = 1
      m2 = TauProfile_Tmp%n_Profiles
      TauProfile1%Profile(m1:m2)     = TauProfile_Tmp%Molecule_Set
      TauProfile1%Tau(:,:,:,m1:m2,:) = TauProfile_Tmp%Tau
      ! The second part
      m1 = m2 + 1
      m2 = n_Molecule_Sets
      TauProfile1%Profile(m1:m2)     = TauProfile2%Molecule_Set
      TauProfile1%Tau(:,:,:,m1:m2,:) = TauProfile2%Tau

    END IF Concatenate_TauProfile1


    ! Clean up
    Destroy_Status = Destroy_TauProfile( TauProfile_Tmp, &
                                       Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauProfile_Tmp structure.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Concatenate_TauProfile


!--------------------------------------------------------------------------------
!
! NAME:
!       Information_TauProfile
!
! PURPOSE:
!       Subroutine to return a string containing information about the
!       TauProfile data structure.
!
! CALLING SEQUENCE:
!       CALL Information_TauProfile( TauProfile   , &  ! Input
!                                    Information  , &  ! Output
!                                    RCS_Id=RCS_Id  )  ! Revision control
! 
! INPUT ARGUMENTS:
!       TauProfile:    Filled TauProfile structure.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Information:   String containing information about the passed
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  SUBROUTINE Information_TauProfile( TauProfile , &  ! Input
                                     Information, &  ! Output
                                     RCS_Id       )  ! Revision control
    ! Arguments
    TYPE(TauProfile_type) , INTENT(IN)  :: TauProfile
    CHARACTER(*),           INTENT(OUT) :: Information
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(5000) :: Long_String

    ! Set up
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required data to the local string
    WRITE( Long_String, '( a,1x,a,1x,"TauProfile: ", &
                           &"N_LAYERS=",i3,2x,&
                           &"N_CHANNELS=",i4,2x,&
                           &"N_ANGLES=",i1,2x,&
                           &"N_PROFILES=",i3,2x,&
                           &"N_MOLECULE_SETS=",i2 )' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         TRIM(TauProfile%Sensor_ID), &
                         TauProfile%n_Layers       , &
                         TauProfile%n_Channels     , &
                         TauProfile%n_Angles       , &
                         TauProfile%n_Profiles     , &
                         TauProfile%n_Molecule_Sets

    ! Trim the output based on the
    ! dummy argument string length
    Information = Long_String(1:MIN( LEN(Information), LEN_TRIM(Long_String) ))

  END SUBROUTINE Information_TauProfile

END MODULE TauProfile_Define
