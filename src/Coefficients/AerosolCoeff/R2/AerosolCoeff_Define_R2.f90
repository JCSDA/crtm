!
! AerosolCoeff_Define
!
! Module defining the AerosolCoeff data structure and containing routines
! to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 04-Feb-2005
!                       paul.vandelst@noaa.gov
!       Modified by:    Quanhua Liu, QSS Group, Inc;  quanhua.liu@noaa.gov
!                       David Groff, SAIC;            david.groff@noaa.gov
!

MODULE AerosolCoeff_Define_R2

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: AerosolCoeff_type
  ! Procedures
  PUBLIC :: Associated_AerosolCoeff
  PUBLIC :: Destroy_AerosolCoeff
  PUBLIC :: Allocate_AerosolCoeff
  PUBLIC :: Assign_AerosolCoeff
  PUBLIC :: Equal_AerosolCoeff
  PUBLIC :: Check_AerosolCoeff_Release
  PUBLIC :: Info_AerosolCoeff


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! AerosolCoeff init values
  INTEGER,      PARAMETER :: SL = 20
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: AEROSOLCOEFF_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: AEROSOLCOEFF_VERSION = 2  ! This is just the data version.


  ! ---------------------------------
  ! AerosolCoeff data type definition
  ! ---------------------------------
  TYPE :: AerosolCoeff_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER(Long) :: Release = AEROSOLCOEFF_RELEASE
    INTEGER(Long) :: Version = AEROSOLCOEFF_VERSION
    ! Character string lengths
    INTEGER(Long) :: StrLen = SL
    ! Dimensions
    INTEGER(Long) :: n_Wavelengths      = 0   ! I1 dimension
    INTEGER(Long) :: n_Radii            = 0   ! I2 dimension
    INTEGER(Long) :: n_Types            = 0   ! I3 dimension
    INTEGER(Long) :: n_RH               = 0   ! I4 dimension
    INTEGER(Long) :: Max_Legendre_Terms = 0   ! I5 dimension
    INTEGER(Long) :: n_Legendre_Terms   = 0
    INTEGER(Long) :: Max_Phase_Elements = 0   ! I6 dimension
    INTEGER(Long) :: n_Phase_Elements   = 0   
    ! LUT dimension vectors
    CHARACTER(SL), POINTER :: Type_Name(:)      => NULL()  ! I3
    INTEGER(Long), POINTER :: Type(:)           => NULL()  ! I3
    REAL(Double),  POINTER :: Wavelength(:)     => NULL()  ! I1
    REAL(Double),  POINTER :: Frequency(:)      => NULL()  ! I1
    REAL(Double),  POINTER :: Reff(:,:)         => NULL()  ! I2 x I3
    REAL(Double),  POINTER :: RH(:)             => NULL()  ! I4
    REAL(Double),  POINTER :: ke(:,:,:)         => NULL()  ! I1 x I2 x I3
    REAL(Double),  POINTER :: w(:,:,:)          => NULL()  ! I1 x I2 x I3 
    REAL(Double),  POINTER :: g(:,:,:)          => NULL()  ! I1 x I2 x I3
    REAL(Double),  POINTER :: pcoeff(:,:,:,:,:) => NULL()  ! I1 x I2 x I3 x I5 x I6
  END TYPE AerosolCoeff_type


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
!       Associated_AerosolCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       AerosolCoeff structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_AerosolCoeff( AerosolCoeff     , &  ! Input
!                                                     ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       AerosolCoeff:        AerosolCoeff structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       AerosolCoeff_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            AerosolCoeff structure pointer members are associated.
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
!                            association status of the AerosolCoeff pointer members.
!                            .TRUE.  - if ALL the AerosolCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the AerosolCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the AerosolCoeff pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_AerosolCoeff( AerosolCoeff, & ! Input
                                    ANY_Test    ) & ! Optional input
                                  RESULT( Association_Status )
    ! Arguments
    TYPE(AerosolCoeff_type), INTENT(IN) :: AerosolCoeff
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

    ! Test the structure associations
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( AerosolCoeff%Type_Name  ) .AND. &
           ASSOCIATED( AerosolCoeff%Type       ) .AND. &
           ASSOCIATED( AerosolCoeff%Wavelength ) .AND. &
           ASSOCIATED( AerosolCoeff%Frequency  ) .AND. &
           ASSOCIATED( AerosolCoeff%Reff       ) .AND. &
           ASSOCIATED( AerosolCoeff%RH         ) .AND. &
           ASSOCIATED( AerosolCoeff%ke         ) .AND. &
           ASSOCIATED( AerosolCoeff%w          ) .AND. &
           ASSOCIATED( AerosolCoeff%g          ) .AND. &
           ASSOCIATED( AerosolCoeff%pcoeff     )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( AerosolCoeff%Type_Name  ) .OR. &
           ASSOCIATED( AerosolCoeff%Type       ) .OR. &
           ASSOCIATED( AerosolCoeff%Wavelength ) .OR. &
           ASSOCIATED( AerosolCoeff%Frequency  ) .OR. &
           ASSOCIATED( AerosolCoeff%Reff       ) .OR. &
           ASSOCIATED( AerosolCoeff%RH         ) .OR. &
           ASSOCIATED( AerosolCoeff%ke         ) .OR. &
           ASSOCIATED( AerosolCoeff%w          ) .OR. &
           ASSOCIATED( AerosolCoeff%g          ) .OR. &
           ASSOCIATED( AerosolCoeff%pcoeff     )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_AerosolCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Destroy_AerosolCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of AerosolCoeff
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_AerosolCoeff( AerosolCoeff,           &  ! Output
!                                            RCS_Id=RCS_Id,          &  ! Revision control
!                                            Message_Log=Message_Log )  ! Error messaging
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
!       AerosolCoeff: Re-initialized AerosolCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       AerosolCoeff_type
!                     DIMENSION:  Scalar
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
!       Note the INTENT on the output AerosolCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_AerosolCoeff( AerosolCoeff, &  ! Output
                                 No_Clear,     &  ! Optional input
                                 RCS_Id,       &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    TYPE(AerosolCoeff_type), INTENT(IN OUT) :: AerosolCoeff
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_AerosolCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Reinitialise the dimensions
    AerosolCoeff%n_Wavelengths      = 0     
    AerosolCoeff%n_Radii            = 0
    AerosolCoeff%n_Types            = 0
    AerosolCoeff%n_RH               = 0
    AerosolCoeff%Max_Legendre_Terms = 0
    AerosolCoeff%n_Legendre_Terms   = 0
    AerosolCoeff%Max_Phase_Elements = 0
    AerosolCoeff%n_Phase_Elements   = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_AerosolCoeff( AerosolCoeff )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff ) ) RETURN


    ! Deallocate the array components
    ! -------------------------------
    DEALLOCATE( AerosolCoeff%Type_Name , &
                AerosolCoeff%Type      , &
                AerosolCoeff%Wavelength, &
                AerosolCoeff%Frequency , &
                AerosolCoeff%Reff      , &
                AerosolCoeff%RH        , &
                AerosolCoeff%ke        , &
                AerosolCoeff%w         , &
                AerosolCoeff%g         , &
                AerosolCoeff%pcoeff    , &
                STAT = Allocate_Status   )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating AerosolCoeff. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    AerosolCoeff%n_Allocates = AerosolCoeff%n_Allocates - 1
    IF ( AerosolCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i0 )' ) &
                      AerosolCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Destroy_AerosolCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Allocate_AerosolCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the AerosolCoeff
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_AerosolCoeff( n_Wavelengths          , &  ! Input 
!                                             n_Radii                , &  ! Input 
!                                             n_Types                , &  ! Input 
!                                             n_RH                   , &  ! Input 
!                                             n_Legendre_Terms       , &  ! Input 
!                                             n_Phase_Elements       , &  ! Input
!                                             AerosolCoeff           , &  ! Output
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Wavelengths:     The number of wavelengths in the look-up
!                          table (LUT) 
!                          The "I1" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Radii:           The number of discrete effective radii for
!                          scatterers in the LUT.
!                          The "I2" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Types:           The number of different aerosol types in
!                          the LUT.
!                          The "I3" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_RH:              The number of relative humidity entries in
!                          the LUT.
!                          The "I4" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:  The maximum number of Legendre polynomial
!                          terms in the LUT.
!                          The "I5" dimension. Can be = 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Phase_Elements:  The maximum number of phase elements in the LUT.
!                          The "I6" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AerosolCoeff:      AerosolCoeff structure with allocated pointer members
!                          UNITS:      N/A
!                          TYPE:       AerosolCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the structure pointer allocations were
!                                        successful
!                             == FAILURE - an error occurred, or
!                                        - the structure internal allocation counter
!                                          is not equal to one (1) upon exiting this
!                                          function. This value is incremented and
!                                          decremented for every structure allocation
!                                          and deallocation respectively.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output AerosolCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_AerosolCoeff( n_Wavelengths   , &  ! Input
                                  n_Radii         , &  ! Input
                                  n_Types         , &  ! Input
                                  n_RH            , &  ! Input
                                  n_Legendre_Terms, &  ! Input
                                  n_Phase_Elements, &  ! Input
                                  AerosolCoeff    , &  ! Output
                                  RCS_Id          , &  ! Revision control
                                  Message_Log     ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: n_Wavelengths   
    INTEGER,                 INTENT(IN)     :: n_Radii         
    INTEGER,                 INTENT(IN)     :: n_Types         
    INTEGER,                 INTENT(IN)     :: n_RH    
    INTEGER,                 INTENT(IN)     :: n_Legendre_Terms
    INTEGER,                 INTENT(IN)     :: n_Phase_Elements
    TYPE(AerosolCoeff_type), INTENT(IN OUT) :: AerosolCoeff
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_AerosolCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check the dimensions
    IF ( n_Wavelengths    < 1 .OR. &
         n_Radii          < 1 .OR. &
         n_Types          < 1 .OR. &
         n_RH             < 1 .OR. &
         n_Legendre_Terms < 0 .OR. &
         n_Phase_Elements < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid AerosolCoeff dimensions detected.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_AerosolCoeff( AerosolCoeff, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_AerosolCoeff( AerosolCoeff, &
                                           No_Clear=SET, &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating AerosolCoeff pointer members.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Perform the allocation
    ! ----------------------
    ALLOCATE( AerosolCoeff%Type_Name( n_Types ), &
              AerosolCoeff%Type( n_Types ), &
              AerosolCoeff%Wavelength( n_Wavelengths ), &
              AerosolCoeff%Frequency(  n_Wavelengths ), &
              AerosolCoeff%Reff( n_Radii, n_Types ), &
              AerosolCoeff%RH( n_RH ), &
              AerosolCoeff%ke( n_Wavelengths, n_Radii, n_Types ), &
              AerosolCoeff%w(  n_Wavelengths, n_Radii, n_Types ), &
              AerosolCoeff%g(  n_Wavelengths, n_Radii, n_Types ), &
              AerosolCoeff%pcoeff( n_Wavelengths     , &
                                   n_Radii           , &
                                   n_Types           , &
                                   0:n_Legendre_Terms, &
                                   n_Phase_Elements    ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating AerosolCoeff data arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the dimensions
    ! ---------------------
    AerosolCoeff%n_Types            = n_Types 
    AerosolCoeff%n_Wavelengths      = n_Wavelengths 
    AerosolCoeff%n_Radii            = n_Radii 
    AerosolCoeff%n_RH               = n_RH 
    AerosolCoeff%Max_Legendre_Terms = n_Legendre_Terms 
    AerosolCoeff%n_Legendre_Terms   = n_Legendre_Terms 
    AerosolCoeff%Max_Phase_Elements = n_Phase_Elements
    AerosolCoeff%n_Phase_Elements   = n_Phase_Elements 


    ! Initialise the arrays
    ! ---------------------
    AerosolCoeff%Type_Name  = ' ' 
    AerosolCoeff%Type       = 0
    AerosolCoeff%Wavelength = ZERO
    AerosolCoeff%Frequency  = ZERO
    AerosolCoeff%Reff       = ZERO
    AerosolCoeff%RH         = ZERO
    AerosolCoeff%ke         = ZERO
    AerosolCoeff%w          = ZERO
    AerosolCoeff%g          = ZERO
    AerosolCoeff%pcoeff     = ZERO


    ! Increment and test allocation counter
    ! -------------------------------------
    AerosolCoeff%n_Allocates = AerosolCoeff%n_Allocates + 1
    IF ( AerosolCoeff%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i0 )' ) &
                      AerosolCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_AerosolCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Assign_AerosolCoeff
!
! PURPOSE:
!       Function to copy valid AerosolCoeff structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_AerosolCoeff( AerosolCoeff_in        , &  ! Input
!                                           AerosolCoeff_out       , &  ! Output
!                                           RCS_Id     =RCS_Id     , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AerosolCoeff_in:   AerosolCoeff structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       AerosolCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AerosolCoeff_out:  Copy of the input structure, AerosolCoeff_in.
!                          UNITS:      N/A
!                          TYPE:       AerosolCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the structure assignment was successful
!                             == FAILURE an error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output AerosolCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Assign_AerosolCoeff( AerosolCoeff_in , &  ! Input
                                AerosolCoeff_out, &  ! Output
                                RCS_Id          , &  ! Revision control
                                Message_Log     ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(AerosolCoeff_type), INTENT(IN)     :: AerosolCoeff_in
    TYPE(AerosolCoeff_type), INTENT(IN OUT) :: AerosolCoeff_out
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_AerosolCoeff'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT AerosolCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_AerosolCoeff( AerosolCoeff_in%n_Wavelengths     , &
                                          AerosolCoeff_in%n_Radii           , &
                                          AerosolCoeff_in%n_Types           , &
                                          AerosolCoeff_in%n_RH              , &
                                          AerosolCoeff_in%Max_Legendre_Terms, &
                                          AerosolCoeff_in%Max_Phase_Elements, &
                                          AerosolCoeff_out, &
                                          Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output AerosolCoeff arrays.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign structure data
    ! ---------------------
    ! Non-dimension scalar members
    AerosolCoeff_out%Release = AerosolCoeff_in%Release
    AerosolCoeff_out%Version = AerosolCoeff_in%Version

    ! Used-dimension members
    AerosolCoeff_out%n_Legendre_Terms = AerosolCoeff_in%n_Legendre_Terms
    AerosolCoeff_out%n_Phase_Elements = AerosolCoeff_in%n_Phase_Elements
    
    ! Copy array data
    AerosolCoeff_out%Type_Name  = AerosolCoeff_in%Type_Name
    AerosolCoeff_out%Type       = AerosolCoeff_in%Type
    AerosolCoeff_out%Wavelength = AerosolCoeff_in%Wavelength
    AerosolCoeff_out%Frequency  = AerosolCoeff_in%Frequency
    AerosolCoeff_out%Reff       = AerosolCoeff_in%Reff
    AerosolCoeff_out%RH         = AerosolCoeff_in%RH        
    AerosolCoeff_out%ke         = AerosolCoeff_in%ke
    AerosolCoeff_out%w          = AerosolCoeff_in%w
    AerosolCoeff_out%g          = AerosolCoeff_in%g
    AerosolCoeff_out%pcoeff     = AerosolCoeff_in%pcoeff

  END FUNCTION Assign_AerosolCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Equal_AerosolCoeff
!
! PURPOSE:
!       Function to test if two AerosolCoeff structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_AerosolCoeff( AerosolCoeff_LHS       , &  ! Input
!                                          AerosolCoeff_RHS       , &  ! Input
!                                          ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                          Check_All  =Check_All  , &  ! Optional input
!                                          RCS_Id     =RCS_Id     , &  ! Optional output
!                                          Message_Log=Message_Log  )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!       AerosolCoeff_LHS:  AerosolCoeff structure to be compared; equivalent to the
!                          left-hand side of a lexical comparison, e.g.
!                            IF ( AerosolCoeff_LHS == AerosolCoeff_RHS ).
!                          UNITS:      N/A
!                          TYPE:       AerosolCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AerosolCoeff_RHS:  AerosolCoeff structure to be compared to; equivalent to
!                          right-hand side of a lexical comparison, e.g.
!                            IF ( AerosolCoeff_LHS == AerosolCoeff_RHS ).
!                          UNITS:      N/A
!                          TYPE:       AerosolCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:         Unit of data precision used to scale the floating
!                          point comparison. ULP stands for "Unit in the Last Place,"
!                          the smallest possible increment or decrement that can be
!                          made using a machine's floating point arithmetic.
!                          Value must be positive - if a negative value is supplied,
!                          the absolute value is used. If not specified, the default
!                          value is 1.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:         Set this argument to check ALL the floating point
!                          channel data of the AerosolCoeff structures. The default
!                          action is return with a FAILURE status as soon as
!                          any difference is found. This optional argument can
!                          be used to get a listing of ALL the differences
!                          between data in AerosolCoeff structures.
!                          If == 0, Return with FAILURE status as soon as
!                                   ANY difference is found  *DEFAULT*
!                             == 1, Set FAILURE status if ANY difference is
!                                   found, but continue to check ALL data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the structures were equal
!                             == FAILURE - an error occurred, or
!                                        - the structures were different.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Equal_AerosolCoeff( AerosolCoeff_LHS, &  ! Input
                               AerosolCoeff_RHS, &  ! Input
                               ULP_Scale       , &  ! Optional input
                               Check_All       , &  ! Optional input
                               RCS_Id          , &  ! Revision control
                               Message_Log     ) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    TYPE(AerosolCoeff_type), INTENT(IN)  :: AerosolCoeff_LHS
    TYPE(AerosolCoeff_type), INTENT(IN)  :: AerosolCoeff_RHS
    INTEGER,       OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,       OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_AerosolCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: i, j, k, m, n

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
    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AerosolCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT AerosolCoeff_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check structure Release/Version
    ! -------------------------------
    IF ( ( AerosolCoeff_LHS%Release /= AerosolCoeff_RHS%Release ) .OR. &
         ( AerosolCoeff_LHS%Version /= AerosolCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      AerosolCoeff_LHS%Release, AerosolCoeff_LHS%Version, &
                      AerosolCoeff_RHS%Release, AerosolCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( AerosolCoeff_LHS%n_Wavelengths    /= AerosolCoeff_RHS%n_Wavelengths    .OR. &
         AerosolCoeff_LHS%n_Radii          /= AerosolCoeff_RHS%n_Radii          .OR. &
         AerosolCoeff_LHS%n_Types          /= AerosolCoeff_RHS%n_Types          .OR. &
         AerosolCoeff_LHS%n_RH             /= AerosolCoeff_RHS%n_RH             .OR. &
         AerosolCoeff_LHS%n_Legendre_Terms /= AerosolCoeff_RHS%n_Legendre_Terms .OR. &
         AerosolCoeff_LHS%n_Phase_Elements /= AerosolCoeff_RHS%n_Phase_Elements      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the dimension data
    ! ------------------------
    ! The aerosol type names
    DO i = 1, AerosolCoeff_LHS%n_Types
      IF ( TRIM(ADJUSTL(AerosolCoeff_LHS%Type_Name(i))) /= &
           TRIM(ADJUSTL(AerosolCoeff_RHS%Type_Name(i)))    ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Type_Name values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    ! The aerosol type values
    DO i = 1, AerosolCoeff_LHS%n_Types
      IF ( AerosolCoeff_LHS%Type(i) /= AerosolCoeff_RHS%Type(i) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Type values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    ! Wavelengths
    DO i = 1, AerosolCoeff_LHS%n_Wavelengths
      IF ( .NOT. Compare_Float( AerosolCoeff_LHS%Wavelength(i), &
                                AerosolCoeff_RHS%Wavelength(i), &
                                ULP = ULP                       ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Wavelength values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    ! Frequencies
    DO i = 1, AerosolCoeff_LHS%n_Wavelengths
      IF ( .NOT. Compare_Float( AerosolCoeff_LHS%Frequency(i), &
                                AerosolCoeff_RHS%Frequency(i), &
                                ULP = ULP                       ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Frequency values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The effective radii
    DO j = 1, AerosolCoeff_LHS%n_Types
      DO i = 1, AerosolCoeff_LHS%n_Radii
        IF ( .NOT. Compare_Float( AerosolCoeff_LHS%Reff(i,j), &
                                  AerosolCoeff_RHS%Reff(i,j), &
                                  ULP = ULP                   ) ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Reff values are different', &
                                Error_Status, &
                                Message_Log = Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    
    ! Relative humidity
    DO i = 1, AerosolCoeff_LHS%n_RH        
      IF ( .NOT. Compare_Float( AerosolCoeff_LHS%RH(i), &
                                AerosolCoeff_RHS%RH(i), &
                                ULP = ULP               ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'RH values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The extinction coefficient, single scatter
    ! albedo, and asymmetry parameter.
    DO k = 1, AerosolCoeff_LHS%n_Types
      DO j = 1, AerosolCoeff_LHS%n_Radii
        DO i = 1, AerosolCoeff_LHS%n_Wavelengths
        
          ! Extinction coefficient
          IF ( .NOT. Compare_Float(AerosolCoeff_LHS%ke(i,j,k), &
                                   AerosolCoeff_RHS%ke(i,j,k), &
                                   ULP = ULP                   ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'ke values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
          
          ! Single scatter albedo
          IF ( .NOT. Compare_Float(AerosolCoeff_LHS%w(i,j,k), &
                                   AerosolCoeff_RHS%w(i,j,k), &
                                   ULP = ULP                 ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'w values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
          
          ! Asymmetry parameter
          IF ( .NOT. Compare_Float(AerosolCoeff_LHS%g(i,j,k), &
                                   AerosolCoeff_RHS%g(i,j,k), &
                                   ULP = ULP                  ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'g values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        
    
        END DO
      END DO
    END DO
    
    ! The phase coefficients
    DO n = 1, AerosolCoeff_LHS%n_Phase_Elements
      DO m = 0, AerosolCoeff_LHS%n_Legendre_Terms
        DO k = 1, AerosolCoeff_LHS%n_Types
          DO j = 1, AerosolCoeff_LHS%n_Radii
            DO i = 1, AerosolCoeff_LHS%n_Wavelengths
            
              IF ( .NOT. Compare_Float(AerosolCoeff_LHS%pcoeff(i,j,k,m,n), &
                                       AerosolCoeff_RHS%pcoeff(i,j,k,m,n), &
                                       ULP = ULP                         ) ) THEN
                Error_Status = FAILURE
                CALL Display_Message( ROUTINE_NAME, &
                                      'pcoeff values are different', &
                                      Error_Status, &
                                      Message_Log = Message_Log )
                IF ( Check_Once ) RETURN
              END IF
              
            END DO
          END DO
        END DO
      END DO
    END DO

  END FUNCTION Equal_AerosolCoeff


!----------------------------------------------------------------------------------
!
! NAME:
!       Check_AerosolCoeff_Release
!
! PURPOSE:
!       Function to check the AerosolCoeff Release value.
!
! CALLING SEQUENCE:
!       Error_Status = Check_AerosolCoeff_Release( AerosolCoeff           , &  ! Input
!                                                  RCS_Id     =RCS_Id     , &  ! Revision control
!                                                  Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AerosolCoeff:  AerosolCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       AerosolCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
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

  FUNCTION Check_AerosolCoeff_Release( AerosolCoeff, &  ! Input
                                       RCS_Id,       &  ! Revision control
                                       Message_Log ) &  ! Error messaging
                                     RESULT( Error_Status )
    ! Arguments
    TYPE(AerosolCoeff_type), INTENT(IN)  :: AerosolCoeff
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Check_AerosolCoeff_Release'
    ! Local variables
    CHARACTER(256) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Check release is not too old
    ! ----------------------------
    IF ( AerosolCoeff%Release < AEROSOLCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An AerosolCoeff data update is needed. ", &
                        &"AerosolCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      AerosolCoeff%Release, AEROSOLCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check release is not too new
    ! ----------------------------
    IF ( AerosolCoeff%Release > AEROSOLCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An AerosolCoeff software update is needed. ", &
                        &"AerosolCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      AerosolCoeff%Release, AEROSOLCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Check_AerosolCoeff_Release


!--------------------------------------------------------------------------------
!
! NAME:
!       Info_AerosolCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the AerosolCoeff data structure.
!
! CALLING SEQUENCE:
!       CALL Info_AerosolCoeff( AerosolCoeff , &  ! Input
!                               Info         , &  ! Output
!                               RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       AerosolCoeff:  Filled AerosolCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       AerosolCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed AerosolCoeff data structure.
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

  SUBROUTINE Info_AerosolCoeff( AerosolCoeff, &  ! Input
                                Info        , &  ! Output
                                RCS_Id        )  ! Revision control
    ! Arguments
    TYPE(AerosolCoeff_type), INTENT(IN)  :: AerosolCoeff
    CHARACTER(*),            INTENT(OUT) :: Info
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(1000) :: Long_String

    ! Set up
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required data to the local string
    WRITE( Long_String, '( a,1x,"AerosolCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
                           &"N_WAVELENGTHS=",i4,2x,&
                           &"N_RADII=",i3,2x,&
                           &"N_TYPES=",i2,2x,&
                           &"N_RH=",i3,2x,&
                           &"N_LEGENDRE_TERMS=",i2,2x,&
                           &"N_PHASE_ELEMENTS=",i2 )' ) &
                        ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                        AerosolCoeff%Release, AerosolCoeff%Version, &
                        AerosolCoeff%n_Wavelengths   , &
                        AerosolCoeff%n_Radii         , &
                        AerosolCoeff%n_Types         , &
                        AerosolCoeff%n_RH            , &
                        AerosolCoeff%n_Legendre_Terms, &
                        AerosolCoeff%n_Phase_Elements

    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN( LEN(Info), LEN_TRIM(Long_String) ))

  END SUBROUTINE Info_AerosolCoeff


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
!       Clear_AerosolCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of a AerosolCoeff structure.
!
! CALLING SEQUENCE:
!       CALL Clear_AerosolCoeff( AerosolCoeff ) ! Output
!
! OUTPUT ARGUMENTS:
!       AerosolCoeff:  AerosolCoeff structure for which the scalar members have
!                      been cleared.
!                      UNITS:      N/A
!                      TYPE:       AerosolCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output AerosolCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_AerosolCoeff( AerosolCoeff )
    TYPE(AerosolCoeff_type), INTENT(IN OUT) :: AerosolCoeff
    AerosolCoeff%Release = AEROSOLCOEFF_RELEASE
    AerosolCoeff%Version = AEROSOLCOEFF_VERSION
    AerosolCoeff%StrLen  = SL
  END SUBROUTINE Clear_AerosolCoeff

END MODULE AerosolCoeff_Define_R2
