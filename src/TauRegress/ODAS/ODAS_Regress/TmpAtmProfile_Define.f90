!
! AtmProfile_Define
!
! Module defining the AtmProfile data structure and containing routines
! to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jul-2002
!                       paul.vandelst@noaa.gov
!

MODULE AtmProfile_Define

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
  ! Parameters
  PUBLIC :: ATMPROFILE_N_ABSORBERS
  PUBLIC :: ATMPROFILE_N_ABSORBER_UNITS
  PUBLIC :: ATMPROFILE_ABSORBER_UNITS_ID
  PUBLIC :: ATMPROFILE_ABSORBER_UNITS_NAME
  PUBLIC :: ATMPROFILE_ABSORBER_UNITS_CHAR
  PUBLIC :: ATMPROFILE_FP_INVALID
  ! Data structure definition
  PUBLIC :: AtmProfileDateTime_type
  PUBLIC :: AtmProfileLocation_type
  PUBLIC :: AtmProfile_type
  ! Structure procedures
  PUBLIC :: Associated_AtmProfile
  PUBLIC :: Destroy_AtmProfile
  PUBLIC :: Allocate_AtmProfile
  PUBLIC :: Assign_AtmProfile
  PUBLIC :: Equal_AtmProfile
  PUBLIC :: Info_AtmProfile
  PUBLIC :: CheckRelease_AtmProfile


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! String lengths
  INTEGER, PARAMETER :: ML = 256  ! Message
  INTEGER, PARAMETER :: PL = 512  ! Profile description
  INTEGER, PARAMETER :: AL = 32   ! Absorber unit name
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ATMPROFILE_RELEASE = 1
  INTEGER, PARAMETER :: ATMPROFILE_VERSION = 1
  ! Maximum number of absorbers
  INTEGER, PARAMETER :: ATMPROFILE_N_ABSORBERS = 32
  ! Absorber units parameters
  INTEGER, PARAMETER :: ATMPROFILE_N_ABSORBER_UNITS = 8
  INTEGER :: i
  INTEGER, PARAMETER :: ATMPROFILE_ABSORBER_UNITS_ID(0:ATMPROFILE_N_ABSORBER_UNITS) = &
    (/(i,i=0,ATMPROFILE_N_ABSORBER_UNITS)/)
  CHARACTER(*), PARAMETER :: ATMPROFILE_ABSORBER_UNITS_NAME(0:ATMPROFILE_N_ABSORBER_UNITS) = &
    (/ 'Invalid', &
       'ppmv   ', &
       'cm^-3  ', &
       'g/kg   ', &
       'g.m^-3 ', &
       'hPa    ', &
       'DP, K  ', &  ! [H2O only]
       'DP, C  ', &  ! [H2O only]
       'RH, %  ' /)  ! [H2O only]
  CHARACTER(*), PARAMETER :: ATMPROFILE_ABSORBER_UNITS_CHAR(0:ATMPROFILE_N_ABSORBER_UNITS) = &
    (/ 'X', &  ! Invalid
       'A', &  ! Volume mixing ratio (ppmv)
       'B', &  ! Number density (cm^-3)
       'C', &  ! Mass mixing ratio (g/kg)
       'D', &  ! Mass density (g.m^-3)
       'E', &  ! Partial pressure (hPa)
       'F', &  ! Dew point (Kelvin) [H2O only]
       'G', &  ! Dew point (Celsius) [H2O only]
       'H' /)  ! Relative humidity (%) [H2O only]
  ! Component invalid values
  REAL(Double),  PARAMETER :: ATMPROFILE_FP_INVALID = -999.0_Double


  ! ------------------------
  ! Derived type definitions
  ! ------------------------
  ! Date, time, and location
  TYPE :: AtmProfileDateTime_type
    INTEGER(Long) :: Year  = 0
    INTEGER(Long) :: Month = 0
    INTEGER(Long) :: Day   = 0
    INTEGER(Long) :: Hour  = 0
  END TYPE AtmProfileDateTime_type
  TYPE :: AtmProfileLocation_type
    REAL(Double) :: Latitude         = ATMPROFILE_FP_INVALID
    REAL(Double) :: Longitude        = ATMPROFILE_FP_INVALID
    REAL(Double) :: Surface_Altitude = ATMPROFILE_FP_INVALID
  END TYPE AtmProfileLocation_type

  ! AtmProfile
  TYPE :: AtmProfile_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER(Long) :: Release = ATMPROFILE_RELEASE
    INTEGER(Long) :: Version = ATMPROFILE_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Levels    = 0 ! K+1
    INTEGER(Long) :: n_Layers    = 0 ! K
    INTEGER(Long) :: n_Absorbers = 0 ! J
    INTEGER(Long) :: n_Profiles  = 0 ! M
    ! Absorber information
    INTEGER(Long), POINTER :: Absorber_ID(:)           => NULL() ! Dimension J
    INTEGER(Long), POINTER :: Absorber_Units_ID(:)     => NULL() ! Dimension J
    CHARACTER(AL), POINTER :: Absorber_Units_Name(:)   => NULL() ! Dimension J
    CHARACTER( 1), POINTER :: Absorber_Units_LBLRTM(:) => NULL() ! Dimension J
    ! Profile metadata    
    CHARACTER(PL),                 POINTER :: Description(:)       => NULL()  ! Dimension M
    INTEGER(Long),                 POINTER :: Climatology_Model(:) => NULL()  ! Dimension M
    TYPE(AtmProfileDateTime_type), POINTER :: DateTime(:)          => NULL()  ! Dimension M
    TYPE(AtmProfileLocation_type), POINTER :: Location(:)          => NULL()  ! Dimension M
    ! Profile LEVEL data
    REAL(Double), POINTER :: Level_Pressure(:,:)    => NULL() ! Dimension K+1 x M
    REAL(Double), POINTER :: Level_Temperature(:,:) => NULL() ! Dimension K+1 x M
    REAL(Double), POINTER :: Level_Absorber(:,:,:)  => NULL() ! Dimension K+1 x J x M
    REAL(Double), POINTER :: Level_Altitude(:,:)    => NULL() ! Dimension K+1 x M
    ! Profile LAYER data
    REAL(Double), POINTER :: Layer_Pressure(:,:)    => NULL()  ! Dimension K x M
    REAL(Double), POINTER :: Layer_Temperature(:,:) => NULL()  ! Dimension K x M
    REAL(Double), POINTER :: Layer_Absorber(:,:,:)  => NULL()  ! Dimension K x J x M
    REAL(Double), POINTER :: Layer_Delta_Z(:,:)     => NULL()  ! Dimension K x M
  END TYPE AtmProfile_type


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
!       Associated_AtmProfile
!
! PURPOSE:
!       Function to test the association status of the pointer members of an
!       AtmProfile structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_AtmProfile( AtmProfile       , &  ! Input
!                                                   ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       AtmProfile:          AtmProfile structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       AtmProfile_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            AtmProfile structure pointer members are associated.
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
!                            association status of the AtmProfile pointer
!                            members.
!                            .TRUE.  - if ALL the AtmProfile pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the AtmProfile pointer
!                                      members are associated.
!                            .FALSE. - some or all of the AtmProfile pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Associated_AtmProfile( AtmProfile, &  ! Input          
                                ANY_Test) &  ! Optional input 
                              RESULT(Association_Status)      
    ! Arguments
    TYPE(AtmProfile_type), INTENT(IN) :: AtmProfile
    INTEGER, OPTIONAL    , INTENT(IN) :: ANY_Test
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
      IF ( ASSOCIATED(AtmProfile%Absorber_ID          ) .AND. &
           ASSOCIATED(AtmProfile%Absorber_Units_ID    ) .AND. &
           ASSOCIATED(AtmProfile%Absorber_Units_Name  ) .AND. &
           ASSOCIATED(AtmProfile%Absorber_Units_LBLRTM) .AND. &
           ASSOCIATED(AtmProfile%Level_Pressure       ) .AND. &
           ASSOCIATED(AtmProfile%Layer_Pressure       ) .AND. &
           ASSOCIATED(AtmProfile%Description          ) .AND. &
           ASSOCIATED(AtmProfile%Climatology_Model    ) .AND. &
           ASSOCIATED(AtmProfile%DateTime             ) .AND. &
           ASSOCIATED(AtmProfile%Location             ) .AND. &
           ASSOCIATED(AtmProfile%Level_Temperature    ) .AND. &
           ASSOCIATED(AtmProfile%Level_Absorber       ) .AND. &
           ASSOCIATED(AtmProfile%Level_Altitude       ) .AND. &
           ASSOCIATED(AtmProfile%Layer_Temperature    ) .AND. &
           ASSOCIATED(AtmProfile%Layer_Absorber       ) .AND. &
           ASSOCIATED(AtmProfile%Layer_Delta_Z        )      ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED(AtmProfile%Absorber_ID          ) .OR. &
           ASSOCIATED(AtmProfile%Absorber_Units_ID    ) .OR. &
           ASSOCIATED(AtmProfile%Absorber_Units_Name  ) .OR. &
           ASSOCIATED(AtmProfile%Absorber_Units_LBLRTM) .OR. &
           ASSOCIATED(AtmProfile%Level_Pressure       ) .OR. &
           ASSOCIATED(AtmProfile%Layer_Pressure       ) .OR. &
           ASSOCIATED(AtmProfile%Description          ) .OR. &
           ASSOCIATED(AtmProfile%Climatology_Model    ) .OR. &
           ASSOCIATED(AtmProfile%DateTime             ) .OR. &
           ASSOCIATED(AtmProfile%Location             ) .OR. &
           ASSOCIATED(AtmProfile%Level_Temperature    ) .OR. &
           ASSOCIATED(AtmProfile%Level_Absorber       ) .OR. &
           ASSOCIATED(AtmProfile%Level_Altitude       ) .OR. &
           ASSOCIATED(AtmProfile%Layer_Pressure       ) .OR. &
           ASSOCIATED(AtmProfile%Layer_Temperature    ) .OR. &
           ASSOCIATED(AtmProfile%Layer_Absorber       ) .OR. &
           ASSOCIATED(AtmProfile%Layer_Delta_Z        )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF
  END FUNCTION Associated_AtmProfile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Destroy_AtmProfile
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of AtmProfile
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_AtmProfile( AtmProfile             , &  ! Output
!                                          RCS_Id     =RCS_Id     , &  ! Revision control
!                                          Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       AtmProfile:   Re-initialised AtmProfile structure.
!                     UNITS:      N/A
!                     TYPE:       AtmProfile_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
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
!       Note the INTENT on the output AtmProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Destroy_AtmProfile( AtmProfile   , &  ! Output
                             No_Clear   , &  ! Optional input
                             RCS_Id     , &  ! Revision control
                             Message_Log) &  ! Error messaging
                           RESULT(Error_Status)
    ! Arguments
    TYPE(AtmProfile_type)   , INTENT(IN OUT) :: AtmProfile
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_AtmProfile'
    ! Local variables
    CHARACTER(ML)  :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Reset the dimension indicators
    AtmProfile%n_Levels    = 0
    AtmProfile%n_Layers    = 0
    AtmProfile%n_Absorbers = 0
    AtmProfile%n_Profiles  = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == 1 ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_AtmProfile(AtmProfile)
    
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_AtmProfile(AtmProfile) ) RETURN
    
    
    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( AtmProfile%Absorber_ID          , &
                AtmProfile%Absorber_Units_ID    , &
                AtmProfile%Absorber_Units_Name  , &
                AtmProfile%Absorber_Units_LBLRTM, &
                AtmProfile%Description          , &
                AtmProfile%Climatology_Model    , &
                AtmProfile%DateTime             , &
                AtmProfile%Location             , &
                AtmProfile%Level_Pressure       , &
                AtmProfile%Level_Temperature    , &
                AtmProfile%Level_Absorber       , &
                AtmProfile%Level_Altitude       , &
                AtmProfile%Layer_Pressure       , &
                AtmProfile%Layer_Temperature    , &
                AtmProfile%Layer_Absorber       , &
                AtmProfile%Layer_Delta_Z        , &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '("Error deallocating AtmProfile. STAT = ",i0)') &
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
    AtmProfile%n_Allocates = AtmProfile%n_Allocates - 1
    IF ( AtmProfile%n_Allocates /= 0 ) THEN
      WRITE( Message, '("Allocation counter /= 0, Value = ",i0)') &
                      AtmProfile%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Destroy_AtmProfile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Allocate_AtmProfile
! 
! PURPOSE:
!       Function to allocate the pointer members of an AtmProfile data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_AtmProfile( n_Layers               , &  ! Input
!                                           n_Absorbers            , &  ! Input
!                                           n_Profiles             , &  ! Input
!                                           AtmProfile             , &  ! Output
!                                           RCS_Id     =RCS_Id     , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     Number of atmospheric profile layers.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:  Number of gaseous absorber species.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Profiles:   Number of atmospheric profiles.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AtmProfile:   AtmProfile structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       AtmProfile_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output AtmProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Allocate_AtmProfile( n_Layers   , &  ! Input
                                n_Absorbers, &  ! Input
                                n_Profiles , &  ! Input
                                AtmProfile , &  ! Output
                                RCS_Id     , &  ! Revision control
                                Message_Log) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Layers
    INTEGER,                INTENT(IN)     :: n_Absorbers
    INTEGER,                INTENT(IN)     :: n_Profiles
    TYPE(AtmProfile_type) , INTENT(IN OUT) :: AtmProfile
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_AtmProfile'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status
    INTEGER :: n_Levels
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Check dimensions
    IF ( n_Layers    < 1 .OR. &
         n_Absorbers < 1 .OR. &
         n_Profiles  < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input AtmProfile dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    n_Levels = n_Layers + 1

    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_AtmProfile( AtmProfile, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_AtmProfile( AtmProfile, &               
                                         No_Clear=SET, &            
                                         Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating AtmProfile prior to allocation.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    
    ! Perform the pointer allocation
    ! ------------------------------
    ALLOCATE( AtmProfile%Absorber_ID(1:n_Absorbers)                           , &
              AtmProfile%Absorber_Units_ID(1:n_Absorbers)                     , &
              AtmProfile%Absorber_Units_Name(1:n_Absorbers)                   , &
              AtmProfile%Absorber_Units_LBLRTM(1:n_Absorbers)                 , &
              AtmProfile%Description(1:n_Profiles)                            , &
              AtmProfile%Climatology_Model(1:n_Profiles)                      , &
              AtmProfile%DateTime(1:n_Profiles)                               , &
              AtmProfile%Location(1:n_Profiles)                               , &
              AtmProfile%Level_Pressure(1:n_Levels,1:n_Profiles)              , &
              AtmProfile%Level_Temperature(1:n_Levels,1:n_Profiles)           , &
              AtmProfile%Level_Absorber(1:n_Levels,1:n_Absorbers,1:n_Profiles), &
              AtmProfile%Level_Altitude(1:n_Levels,1:n_Profiles)              , &
              AtmProfile%Layer_Pressure(1:n_Layers,1:n_Profiles)              , &
              AtmProfile%Layer_Temperature(1:n_Layers,1:n_Profiles)           , &
              AtmProfile%Layer_Absorber(1:n_Layers,1:n_Absorbers,1:n_Profiles), &
              AtmProfile%Layer_Delta_Z(1:n_Layers,1:n_Profiles)               , &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE(Message,'("Error allocating AtmProfile data arrays. STAT = ",i0)') &
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
    AtmProfile%n_Levels    = n_Levels
    AtmProfile%n_Layers    = n_Layers
    AtmProfile%n_Absorbers = n_Absorbers
    AtmProfile%n_Profiles  = n_Profiles


    ! Initialise the arrays
    ! ---------------------
    AtmProfile%Absorber_ID           = 0
    AtmProfile%Absorber_Units_ID     = 0
    AtmProfile%Absorber_Units_Name   = ATMPROFILE_ABSORBER_UNITS_NAME(0)
    AtmProfile%Absorber_Units_LBLRTM = ATMPROFILE_ABSORBER_UNITS_CHAR(0)
    AtmProfile%Description       = ' '
    AtmProfile%Climatology_Model = 0
    AtmProfile%DateTime = AtmProfileDateTime_type(0,0,0,0)
    AtmProfile%Location = AtmProfileLocation_type(ATMPROFILE_FP_INVALID, &
                                                  ATMPROFILE_FP_INVALID, &
                                                  ATMPROFILE_FP_INVALID  )
    AtmProfile%Level_Pressure    = ATMPROFILE_FP_INVALID
    AtmProfile%Level_Temperature = ATMPROFILE_FP_INVALID
    AtmProfile%Level_Absorber    = ATMPROFILE_FP_INVALID
    AtmProfile%Level_Altitude    = ATMPROFILE_FP_INVALID
    AtmProfile%Layer_Pressure    = ATMPROFILE_FP_INVALID
    AtmProfile%Layer_Temperature = ATMPROFILE_FP_INVALID
    AtmProfile%Layer_Absorber    = ATMPROFILE_FP_INVALID
    AtmProfile%Layer_Delta_Z     = ATMPROFILE_FP_INVALID


    ! Increment and test the allocation counter
    ! -----------------------------------------
    AtmProfile%n_Allocates = AtmProfile%n_Allocates + 1
    IF ( AtmProfile%n_Allocates /= 1 ) THEN
      WRITE( Message, '("Allocation counter /= 1, Value = ",i0)') &
                      AtmProfile%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_AtmProfile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Assign_AtmProfile
!
! PURPOSE:
!       Function to copy valid AtmProfile structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_AtmProfile( AtmProfile_in          , &  ! Input
!                                         AtmProfile_out         , &  ! Output
!                                         RCS_Id     =RCS_Id     , &  ! Revision control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AtmProfile_in:  AtmProfile structure which is to be copied.
!                       UNITS:      N/A
!                       TYPE:       AtmProfile_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AtmProfile_out: Copy of the input structure, AtmProfile_in.
!                       UNITS:      N/A
!                       TYPE:       AtmProfile_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       Messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output Messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
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
!       Note the INTENT on the output AtmProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Assign_AtmProfile( AtmProfile_in , &  ! Input
                              AtmProfile_out, &  ! Output
                              RCS_Id        , &  ! Revision control
                              Message_Log   ) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(AtmProfile_type) , INTENT(IN)     :: AtmProfile_in
    TYPE(AtmProfile_type) , INTENT(IN OUT) :: AtmProfile_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_AtmProfile'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_AtmProfile( AtmProfile_in ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AtmProfile_in pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Allocate data arrays
    ! --------------------
    Error_Status = Allocate_AtmProfile( AtmProfile_in%n_Layers, &
                                        AtmProfile_in%n_Absorbers, &
                                        AtmProfile_in%n_Profiles, &
                                        AtmProfile_out, &
                                        Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign non-dimension scalar members
    ! -----------------------------------
    AtmProfile_out%Release = AtmProfile_in%Release
    AtmProfile_out%Version = AtmProfile_in%Version

    ! Copy array data
    ! -----------------
    ! Absorber info
    AtmProfile_out%Absorber_ID           = AtmProfile_in%Absorber_ID
    AtmProfile_out%Absorber_Units_ID     = AtmProfile_in%Absorber_Units_ID
    AtmProfile_out%Absorber_Units_Name   = AtmProfile_in%Absorber_Units_Name
    AtmProfile_out%Absorber_Units_LBLRTM = AtmProfile_in%Absorber_Units_LBLRTM
    ! Profile independent pressures
    AtmProfile_out%Level_Pressure    = AtmProfile_in%Level_Pressure
    AtmProfile_out%Layer_Pressure    = AtmProfile_in%Layer_Pressure
    ! Profile dependent information
    AtmProfile_out%Description       = AtmProfile_in%Description
    AtmProfile_out%Climatology_Model = AtmProfile_in%Climatology_Model
    AtmProfile_out%DateTime          = AtmProfile_in%DateTime
    AtmProfile_out%Location          = AtmProfile_in%Location
    ! Profile dependent LEVEL data
    AtmProfile_out%Level_Temperature = AtmProfile_in%Level_Temperature
    AtmProfile_out%Level_Absorber    = AtmProfile_in%Level_Absorber
    AtmProfile_out%Level_Altitude    = AtmProfile_in%Level_Altitude
    ! Profile dependent LAYER data
    AtmProfile_out%Layer_Temperature = AtmProfile_in%Layer_Temperature
    AtmProfile_out%Layer_Absorber    = AtmProfile_in%Layer_Absorber
    AtmProfile_out%Layer_Delta_Z     = AtmProfile_in%Layer_Delta_Z

  END FUNCTION Assign_AtmProfile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Equal_AtmProfile
!
! PURPOSE:
!       Function to test if two AtmProfile structures are equal.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Equal_AtmProfile( AtmProfile_LHS         , &  ! Input
!                                        AtmProfile_RHS         , &  ! Input
!                                        ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                        Check_All  =Check_All  , &  ! Optional input
!                                        RCS_Id     =RCS_Id     , &  ! Revision control
!                                        Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AtmProfile_LHS:  AtmProfile structure to be compared; equivalent to the
!                        left-hand side of a lexical comparison, e.g.
!                          IF ( AtmProfile_LHS == AtmProfile_RHS ).
!                        UNITS:      N/A
!                        TYPE:       AtmProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       AtmProfile_RHS:  AtmProfile structure to be compared to; equivalent to
!                        right-hand side of a lexical comparison, e.g.
!                          IF ( AtmProfile_LHS == AtmProfile_RHS ).
!                        UNITS:      N/A
!                        TYPE:       AtmProfile_type
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
!                        channel data of the AtmProfile structures. The default
!                        action is return with a FAILURE status as soon as
!                        any difference is found. This optional argument can
!                        be used to get a listing of ALL the differences
!                        between data in AtmProfile structures.
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
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      None
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
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Equal_AtmProfile( AtmProfile_LHS, &  ! Input
                             AtmProfile_RHS, &  ! Input
                             ULP_Scale     , &  ! Optional input
                             Check_All     , &  ! Optional input
                             RCS_Id        , &  ! Revision control
                             Message_Log   ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    TYPE(AtmProfile_type) , INTENT(IN)  :: AtmProfile_LHS
    TYPE(AtmProfile_type) , INTENT(IN)  :: AtmProfile_RHS
    INTEGER     , OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER     , OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_AtmProfile'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: j, k, m

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
    IF ( .NOT. Associated_AtmProfile( AtmProfile_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AtmProfile_LHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_AtmProfile( AtmProfile_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AtmProfile_RHS pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check dimensions
    IF ( AtmProfile_LHS%n_Levels    /= AtmProfile_RHS%n_Levels    .OR. &
         AtmProfile_LHS%n_Layers    /= AtmProfile_RHS%n_Layers    .OR. &
         AtmProfile_LHS%n_Absorbers /= AtmProfile_RHS%n_Absorbers .OR. &
         AtmProfile_LHS%n_Profiles  /= AtmProfile_RHS%n_Profiles       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check the array components
    ! --------------------------
    ! The absorber Ids
    DO j = 1, AtmProfile_LHS%n_Absorbers
      IF ( AtmProfile_LHS%Absorber_ID(j) /= AtmProfile_RHS%Absorber_ID(j) ) THEN
        WRITE(Message,'("AtmProfile component Absorber_Id values ",&
                       &"are different at index (",1(1x,i0),")")') &
                       j
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! The absorber unit Ids
    DO j = 1, AtmProfile_LHS%n_Absorbers
      IF ( AtmProfile_LHS%Absorber_Units_ID(j) /= AtmProfile_RHS%Absorber_Units_ID(j) ) THEN
        WRITE(Message,'("AtmProfile component Absorber_Units_ID values ",&
                       &"are different at index (",1(1x,i0),")")') &
                       j
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! The absorber unit names
    DO j = 1, AtmProfile_LHS%n_Absorbers
      IF ( AtmProfile_LHS%Absorber_Units_Name(j) /= AtmProfile_RHS%Absorber_Units_Name(j) ) THEN
        WRITE(Message,'("AtmProfile component Absorber_Units_Name values ",&
                       &"are different at index (",1(1x,i0),")")') &
                       j
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! The LBLRTM absorber unit identifiers
    DO j = 1, AtmProfile_LHS%n_Absorbers
      IF ( AtmProfile_LHS%Absorber_Units_LBLRTM(j) /= AtmProfile_RHS%Absorber_Units_LBLRTM(j) ) THEN
        WRITE(Message,'("AtmProfile component Absorber_Units_LBLRTM values ",&
                       &"are different at index (",1(1x,i0),")")') &
                       j
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! Profile descriptor string
    DO m = 1, AtmProfile_LHS%n_Profiles
      IF ( AtmProfile_LHS%Description(m) /= AtmProfile_RHS%Description(m) ) THEN
        WRITE(Message,'("AtmProfile component Description values ",&
                       &"are different at index (",1(1x,i0),")")') &
                       m
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! Profile climatology model ID
    DO m = 1, AtmProfile_LHS%n_Profiles
      IF ( AtmProfile_LHS%Climatology_Model(m) /= AtmProfile_RHS%Climatology_Model(m) ) THEN
        WRITE(Message,'("AtmProfile component Climatology_Model values ",&
                       &"are different at index (",1(1x,i0),")")') &
                       m
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! Profile date and time
    DO m = 1, AtmProfile_LHS%n_Profiles
      IF ( AtmProfile_LHS%DateTime(m)%Year  /= AtmProfile_RHS%DateTime(m)%Year  .OR. &
           AtmProfile_LHS%DateTime(m)%Month /= AtmProfile_RHS%DateTime(m)%Month .OR. &
           AtmProfile_LHS%DateTime(m)%Day   /= AtmProfile_RHS%DateTime(m)%Day   .OR. &
           AtmProfile_LHS%DateTime(m)%Hour  /= AtmProfile_RHS%DateTime(m)%Hour       ) THEN
        WRITE(Message,'("AtmProfile component DateTime values ",&
                       &"are different at index (",1(1x,i0),")")') &
                       m
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! Profile location
    DO m = 1, AtmProfile_LHS%n_Profiles
      IF ( .NOT. Compare_Float( AtmProfile_LHS%Location(m)%Latitude, &
                                AtmProfile_RHS%Location(m)%Latitude, &
                                ULP=ULP ) .OR. &
           .NOT. Compare_Float( AtmProfile_LHS%Location(m)%Longitude, &
                                AtmProfile_RHS%Location(m)%Longitude, &
                                ULP=ULP ) .OR. &
           .NOT. Compare_Float( AtmProfile_LHS%Location(m)%Surface_Altitude, &
                                AtmProfile_RHS%Location(m)%Surface_Altitude, &
                                ULP=ULP ) ) THEN
        WRITE(Message,'("AtmProfile component Location values ",&
                       &"are different at index (",1(1x,i0),")")') &
                       m
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! Level pressures
    DO m = 1, AtmProfile_LHS%n_Profiles
      DO k = 1, AtmProfile_LHS%n_Levels
        IF ( .NOT. Compare_Float( AtmProfile_LHS%Level_Pressure(k,m), &
                                  AtmProfile_RHS%Level_Pressure(k,m), &
                                  ULP=ULP ) ) THEN
          WRITE(Message,'("AtmProfile array component Level_Pressure values ",&
                         &"are different at indices (",2(1x,i0),")")') &
                         k, m
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    ! Level temperatures
    DO m = 1, AtmProfile_LHS%n_Profiles
      DO k = 1, AtmProfile_LHS%n_Levels
        IF ( .NOT. Compare_Float( AtmProfile_LHS%Level_Temperature(k,m), &
                                  AtmProfile_RHS%Level_Temperature(k,m), &
                                  ULP=ULP ) ) THEN
          WRITE(Message,'("AtmProfile array component Level_Temperature values ",&
                         &"are different at indices (",2(1x,i0),")")') &
                         k, m
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    ! Level absorbers
    DO m = 1, AtmProfile_LHS%n_Profiles
      DO j = 1, AtmProfile_LHS%n_Absorbers
        DO k = 1, AtmProfile_LHS%n_Levels
          IF ( .NOT. Compare_Float( AtmProfile_LHS%Level_Absorber(k,j,m), &
                                    AtmProfile_RHS%Level_Absorber(k,j,m), &
                                    ULP=ULP ) ) THEN
            WRITE(Message,'("AtmProfile array component Level_Absorber values ",&
                           &"are different at indices (",3(1x,i0),")")') &
                           k, j, m
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        END DO
      END DO
    END DO
    ! Level altitudes
    DO m = 1, AtmProfile_LHS%n_Profiles
      DO k = 1, AtmProfile_LHS%n_Levels
        IF ( .NOT. Compare_Float( AtmProfile_LHS%Level_Altitude(k,m), &
                                  AtmProfile_RHS%Level_Altitude(k,m), &
                                  ULP=ULP ) ) THEN
          WRITE(Message,'("AtmProfile array component Level_Altitude values ",&
                         &"are different at indices (",2(1x,i0),")")') &
                         k, m
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    ! Layer pressures
    DO m = 1, AtmProfile_LHS%n_Profiles
      DO k = 1, AtmProfile_LHS%n_Layers
        IF ( .NOT. Compare_Float( AtmProfile_LHS%Layer_Pressure(k,m), &
                                  AtmProfile_RHS%Layer_Pressure(k,m), &
                                  ULP=ULP ) ) THEN
          WRITE(Message,'("AtmProfile array component Layer_Pressure values ",&
                         &"are different at indices (",2(1x,i0),")")') &
                         k, m
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    ! Layer temperatures
    DO m = 1, AtmProfile_LHS%n_Profiles
      DO k = 1, AtmProfile_LHS%n_Layers
        IF ( .NOT. Compare_Float( AtmProfile_LHS%Layer_Temperature(k,m), &
                                  AtmProfile_RHS%Layer_Temperature(k,m), &
                                  ULP=ULP ) ) THEN
          WRITE(Message,'("AtmProfile array component Layer_Temperature values ",&
                         &"are different at indices (",2(1x,i0),")")') &
                         k, m
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    ! Layer absorbers
    DO m = 1, AtmProfile_LHS%n_Profiles
      DO j = 1, AtmProfile_LHS%n_Absorbers
        DO k = 1, AtmProfile_LHS%n_Layers
          IF ( .NOT. Compare_Float( AtmProfile_LHS%Layer_Absorber(k,j,m), &
                                    AtmProfile_RHS%Layer_Absorber(k,j,m), &
                                    ULP=ULP ) ) THEN
            WRITE(Message,'("AtmProfile array component Layer_Absorber values ",&
                           &"are different at indices (",3(1x,i0),")")') &
                           k, j, m
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        END DO
      END DO
    END DO
    ! Layer thickness
    DO m = 1, AtmProfile_LHS%n_Profiles
      DO k = 1, AtmProfile_LHS%n_Layers
        IF ( .NOT. Compare_Float( AtmProfile_LHS%Layer_Delta_Z(k,m), &
                                  AtmProfile_RHS%Layer_Delta_Z(k,m), &
                                  ULP=ULP ) ) THEN
          WRITE(Message,'("AtmProfile array component Layer_Delta_Z values ",&
                         &"are different at indices (",2(1x,i0),")")') &
                         k, m
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO

  END FUNCTION Equal_AtmProfile


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CheckRelease_AtmProfile
!
! PURPOSE:
!       Function to check the AtmProfile Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_AtmProfile( AtmProfile             , &  ! Input
!                                               RCS_Id     =RCS_Id     , &  ! Revision control
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AtmProfile:    AtmProfile structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       AtmProfile_type
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

  FUNCTION CheckRelease_AtmProfile( AtmProfile , &  ! Input
                                    RCS_Id     , &  ! Revision control
                                    Message_Log) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    TYPE(AtmProfile_type) , INTENT(IN)  :: AtmProfile
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_AtmProfile'
    ! Local variables
    CHARACTER(ML) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check release is not too old
    ! ----------------------------
    IF ( AtmProfile%Release < ATMPROFILE_RELEASE ) THEN
      WRITE( Message,'("An AtmProfile data update is needed. ",&
                      &"AtmProfile release is ",i0,&
                      &". Valid release is ",i0,"." )' ) &
                      AtmProfile%Release, ATMPROFILE_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check release is not too new
    ! ----------------------------
    IF ( AtmProfile%Release > ATMPROFILE_RELEASE ) THEN
      WRITE( Message,'("An AtmProfile software update is needed. ",&
                      &"AtmProfile release is ",i0,&
                      &". Valid release is ",i0,"." )' ) &
                      AtmProfile%Release, ATMPROFILE_RELEASE
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_AtmProfile



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Info_AtmProfile
!
! PURPOSE:
!       Subroutine to return a string containing information about the
!       AtmProfile data structure.
!
! CALLING SEQUENCE:
!       CALL Inf_AtmProfile( AtmProfile   , &  ! Input
!                            Info         , &  ! Output
!                            RCS_Id=RCS_Id  )  ! Revision control
! 
! INPUT ARGUMENTS:
!       AtmProfile:    Filled AtmProfile structure.
!                      UNITS:      N/A
!                      TYPE:       AtmProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed AtmProfile data structure.
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

  SUBROUTINE Info_AtmProfile( AtmProfile, &  ! Input
                              Info      , &  ! Output
                              RCS_Id      )  ! Revision control
    ! Arguments
    TYPE(AtmProfile_type) , INTENT(IN)  :: AtmProfile
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

    ! Create the format string
    ! ------------------------
    WRITE( FmtString,'("(a,",''" AtmProfile RELEASE.VERSION: "'',",i2,",''"."'',",i2.2,2x,", &
                      &''"N_LAYERS="'',",i0,2x,", &
                      &''"N_ABSORBERS="'',",i0,2x,",&
                      &''"N_PROFILES="'',",i0, ",&
                      &"a,",''"     ABSORBER_IDs:   "'', ", ", i0, "i3,", &
                      &"a,",''"     ABSORBER_UNITS: "'', ", ", i0, "a8)")' ) &
                      AtmProfile%n_Absorbers, AtmProfile%n_Absorbers


    ! Write the required data to the local string
    ! -------------------------------------------
    WRITE( LongString,FMT=FmtString ) ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                                      AtmProfile%Release, AtmProfile%Version, &
                                      AtmProfile%n_Layers, &
                                      AtmProfile%n_Absorbers, &
                                      AtmProfile%n_Profiles, &
                                      ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                                      AtmProfile%Absorber_ID, &
                                      ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                                      AtmProfile%Absorber_Units_Name

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_AtmProfile


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  SUBROUTINE Clear_AtmProfile( AtmProfile )
    TYPE(AtmProfile_type), INTENT(IN OUT) :: AtmProfile
    AtmProfile%Release = ATMPROFILE_RELEASE
    AtmProfile%Version = ATMPROFILE_VERSION
  END SUBROUTINE Clear_AtmProfile

 END MODULE AtmProfile_Define
