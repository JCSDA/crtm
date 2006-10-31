!
! CloudCoeff_Define
!
! Module defining the CloudCoeff data structure and containing routines to 
! manipulate it.
!
!
! *!IMPORTANT!*
! -------------
! Note that the CloudCoeff_type is PUBLIC and its members are not
! encapsulated; that is, they can be fully accessed outside the
! scope of this module. This makes it possible to manipulate
! the structure and its data directly rather than, for e.g., via
! get() and set() functions. This was done to eliminate the
! overhead of the get/set type of structure access in using the
! structure. *But*, it is recommended that the user destroy,
! allocate, assign, and concatenate the structure using only the
! routines in this module where possible to eliminate -- or at
! least minimise -- the possibility of memory leakage since most
! of the structure members are pointers.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!

MODULE CloudCoeff_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: CloudCoeff_type
  ! Procedures
  PUBLIC :: Associated_CloudCoeff
  PUBLIC :: Destroy_CloudCoeff
  PUBLIC :: Allocate_CloudCoeff
  PUBLIC :: Assign_CloudCoeff
  PUBLIC :: Equal_CloudCoeff
  PUBLIC :: Check_CloudCoeff_Release
  PUBLIC :: Info_CloudCoeff


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CloudCoeff_Define.f90,v 1.3 2006/06/23 22:39:33 wd20pd Exp $'
  ! CloudCoeff init values
  REAL(Double), PARAMETER :: FP_INIT = 0.0_Double
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: CLOUDCOEFF_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: CLOUDCOEFF_VERSION = 2  ! This is just the data version.


  ! ------------------------------
  ! CloudCoeff data type definition, 
  !   MW:   Microwave
  !   IR:   Infrared
  !   Reff: Effective radius
  !   ext:  Extinction coefficient
  !   w:    Single scatter albedo
  !   g:    Asymmetry parameter
  !   L:    Liquid phase
  !   S:    Solid phase
  ! ------------------------------
  TYPE :: CloudCoeff_type
    INTEGER :: n_Allocates
    ! Release and version information
    INTEGER(Long) :: Release = CLOUDCOEFF_RELEASE
    INTEGER(Long) :: Version = CLOUDCOEFF_VERSION
    ! Array dimensions
    INTEGER(Long) :: n_Frequencies      = 0   ! I1 dimension 
    INTEGER(Long) :: n_Reff_MW          = 0   ! I2 dimension
    INTEGER(Long) :: n_Wavenumbers      = 0   ! I3 dimension
    INTEGER(Long) :: n_Reff_IR          = 0   ! I4 dimension
    INTEGER(Long) :: n_Temperatures     = 0   ! I5 dimension
    INTEGER(Long) :: n_Densities        = 0   ! I6 dimension
    INTEGER(Long) :: Max_Legendre_Terms = 0   ! I7 dimension
    INTEGER(Long) :: n_Legendre_Terms   = 0   
    INTEGER(Long) :: Max_Phase_Elements = 0   ! I8 dimension
    INTEGER(Long) :: n_Phase_Elements   = 0   
    ! LUT dimension vectors
    REAL(Double), POINTER, DIMENSION(:) ::  Frequency   => NULL()  ! I1
    REAL(Double), POINTER, DIMENSION(:) ::  Wavenumber  => NULL()  ! I3
    REAL(Double), POINTER, DIMENSION(:) ::  Reff_MW     => NULL()  ! I2 
    REAL(Double), POINTER, DIMENSION(:) ::  Reff_IR     => NULL()  ! I4
    REAL(Double), POINTER, DIMENSION(:) ::  Temperature => NULL()  ! I5
    REAL(Double), POINTER, DIMENSION(:) ::  Density     => NULL()  ! I6
    ! Microwave data
    REAL(Double), POINTER, DIMENSION(:,:,:) :: ext_L_MW => NULL()  ! I1 x I2 x I5
    REAL(Double), POINTER, DIMENSION(:,:,:) :: w_L_MW   => NULL()  ! I1 x I2 x I5
    REAL(Double), POINTER, DIMENSION(:,:,:) :: g_L_MW   => NULL()  ! I1 x I2 x I5
    REAL(Double), POINTER, DIMENSION(:,:,:) :: ext_S_MW => NULL()  ! I1 x I2 x I6
    REAL(Double), POINTER, DIMENSION(:,:,:) :: w_S_MW   => NULL()  ! I1 x I2 x I6
    REAL(Double), POINTER, DIMENSION(:,:,:) :: g_S_MW   => NULL()  ! I1 x I2 x I6
    REAL(Double), POINTER, DIMENSION(:,:,:,:,:) :: phase_coeff_L_MW => NULL()  ! I1 x I2 x I5 x I7 x I8
    REAL(Double), POINTER, DIMENSION(:,:,:,:,:) :: phase_coeff_S_MW => NULL()  ! I1 x I2 x I6 x I7 x I8
    ! Infrared data
    ! Note the dimensional difference between the
    ! liquid (L) and solid (S) phase components
    REAL(Double), POINTER, DIMENSION(:,:)   :: ext_L_IR => NULL()  ! I3 x I4
    REAL(Double), POINTER, DIMENSION(:,:)   :: w_L_IR   => NULL()  ! I3 x I4
    REAL(Double), POINTER, DIMENSION(:,:)   :: g_L_IR   => NULL()  ! I3 x I4
    REAL(Double), POINTER, DIMENSION(:,:,:) :: ext_S_IR => NULL()  ! I3 x I4 x I6
    REAL(Double), POINTER, DIMENSION(:,:,:) :: w_S_IR   => NULL()  ! I3 x I4 x I6
    REAL(Double), POINTER, DIMENSION(:,:,:) :: g_S_IR   => NULL()  ! I3 x I4 x I6
    REAL(Double), POINTER, DIMENSION(:,:,:)   :: phase_coeff_L_IR => NULL() ! I3 x I4 x I7
    REAL(Double), POINTER, DIMENSION(:,:,:,:) :: phase_coeff_S_IR => NULL() ! I3 x I4 x I6 x I7
  END TYPE CloudCoeff_type


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
!       Clear_CloudCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CloudCoeff structure.
!
! CALLING SEQUENCE:
!       CALL Clear_CloudCoeff( CloudCoeff ) ! Output
!
! OUTPUT ARGUMENTS:
!       CloudCoeff:    CloudCoeff structure for which the scalar members have
!                      been cleared.
!                      UNITS:      N/A
!                      TYPE:       CloudCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output CloudCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_CloudCoeff(CloudCoeff)
    TYPE(CloudCoeff_type), INTENT(IN OUT) :: CloudCoeff
    CloudCoeff%n_Frequencies      = 0
    CloudCoeff%n_Reff_MW          = 0
    CloudCoeff%n_Wavenumbers      = 0
    CloudCoeff%n_Reff_IR          = 0
    CloudCoeff%n_Temperatures     = 0
    CloudCoeff%n_Densities        = 0
    CloudCoeff%Max_Legendre_Terms = 0
    CloudCoeff%n_Legendre_Terms   = 0
    CloudCoeff%Max_Phase_Elements = 0
    CloudCoeff%n_Phase_Elements   = 0
  END SUBROUTINE Clear_CloudCoeff


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
!       Associated_CloudCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CloudCoeff structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_CloudCoeff( CloudCoeff,         &  ! Input
!                                                   ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       CloudCoeff: CloudCoeff structure which is to have its pointer
!                   member's association status tested.
!                   UNITS:      N/A
!                   TYPE:       CloudCoeff_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:   Set this argument to test if ANY of the
!                   CloudCoeff structure pointer members are associated.
!                   The default is to test if ALL the pointer members
!                   are associated.
!                   If ANY_Test = 0, test if ALL the pointer members
!                                    are associated.  (DEFAULT)
!                      ANY_Test = 1, test if ANY of the pointer members
!                                    are associated.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the CloudCoeff pointer members.
!                            .TRUE.  - if ALL the CloudCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the CloudCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the CloudCoeff pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_CloudCoeff( CloudCoeff, & ! Input
                                  ANY_Test )  & ! Optional input
                                RESULT( Association_Status )
    ! Arguments
    TYPE(CloudCoeff_type), INTENT(IN) :: CloudCoeff
    INTEGER,         OPTIONAL, INTENT(IN) :: ANY_Test
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
      IF ( ASSOCIATED( CloudCoeff%Frequency        ) .AND. &
           ASSOCIATED( CloudCoeff%Wavenumber       ) .AND. &
           ASSOCIATED( CloudCoeff%Reff_MW          ) .AND. &
           ASSOCIATED( CloudCoeff%Reff_IR          ) .AND. &
           ASSOCIATED( CloudCoeff%Temperature      ) .AND. &
           ASSOCIATED( CloudCoeff%Density          ) .AND. &
           ASSOCIATED( CloudCoeff%ext_L_MW         ) .AND. &
           ASSOCIATED( CloudCoeff%w_L_MW           ) .AND. &
           ASSOCIATED( CloudCoeff%g_L_MW           ) .AND. &
           ASSOCIATED( CloudCoeff%ext_S_MW         ) .AND. &
           ASSOCIATED( CloudCoeff%w_S_MW           ) .AND. &
           ASSOCIATED( CloudCoeff%g_S_MW           ) .AND. &
           ASSOCIATED( CloudCoeff%phase_coeff_L_MW ) .AND. &
           ASSOCIATED( CloudCoeff%phase_coeff_S_MW ) .AND. &
           ASSOCIATED( CloudCoeff%ext_L_IR         ) .AND. &
           ASSOCIATED( CloudCoeff%w_L_IR           ) .AND. &
           ASSOCIATED( CloudCoeff%g_L_IR           ) .AND. &
           ASSOCIATED( CloudCoeff%ext_S_IR         ) .AND. &
           ASSOCIATED( CloudCoeff%w_S_IR           ) .AND. &
           ASSOCIATED( CloudCoeff%g_S_IR           ) .AND. &
           ASSOCIATED( CloudCoeff%phase_coeff_L_IR ) .AND. &
           ASSOCIATED( CloudCoeff%phase_coeff_S_IR )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( CloudCoeff%Frequency        ) .OR. &
           ASSOCIATED( CloudCoeff%Wavenumber       ) .OR. &
           ASSOCIATED( CloudCoeff%Reff_MW          ) .OR. &
           ASSOCIATED( CloudCoeff%Reff_IR          ) .OR. &
           ASSOCIATED( CloudCoeff%Temperature      ) .OR. &
           ASSOCIATED( CloudCoeff%Density          ) .OR. &
           ASSOCIATED( CloudCoeff%ext_L_MW         ) .OR. &
           ASSOCIATED( CloudCoeff%w_L_MW           ) .OR. &
           ASSOCIATED( CloudCoeff%g_L_MW           ) .OR. &
           ASSOCIATED( CloudCoeff%ext_S_MW         ) .OR. &
           ASSOCIATED( CloudCoeff%w_S_MW           ) .OR. &
           ASSOCIATED( CloudCoeff%g_S_MW           ) .OR. &
           ASSOCIATED( CloudCoeff%phase_coeff_L_MW ) .OR. &
           ASSOCIATED( CloudCoeff%phase_coeff_S_MW ) .OR. &
           ASSOCIATED( CloudCoeff%ext_L_IR         ) .OR. &
           ASSOCIATED( CloudCoeff%w_L_IR           ) .OR. &
           ASSOCIATED( CloudCoeff%g_L_IR           ) .OR. &
           ASSOCIATED( CloudCoeff%ext_S_IR         ) .OR. &
           ASSOCIATED( CloudCoeff%w_S_IR           ) .OR. &
           ASSOCIATED( CloudCoeff%g_S_IR           ) .OR. &
           ASSOCIATED( CloudCoeff%phase_coeff_L_IR ) .OR. &
           ASSOCIATED( CloudCoeff%phase_coeff_S_IR )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_CloudCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Destroy_CloudCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of CloudCoeff
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_CloudCoeff( CloudCoeff,               &  ! Output
!                                          RCS_Id = RCS_Id,          &  ! Revision control
!                                          Message_Log = Message_Log )  ! Error messaging
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
! OUTPUT ARGUMENTS:
!       CloudCoeff:   Re-initialized CloudCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       CloudCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output CloudCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_CloudCoeff( CloudCoeff,   &  ! Output
                               No_Clear,     &  ! Optional input
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    TYPE(CloudCoeff_type), INTENT(IN OUT) :: CloudCoeff
    INTEGER,         OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_CloudCoeff'
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
      IF ( No_Clear == 1 ) Clear = .FALSE.
    END IF

    ! Initialise the scalar members
    IF ( Clear ) CALL Clear_CloudCoeff( CloudCoeff )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_CloudCoeff( CloudCoeff ) ) RETURN

    ! Deallocate the pointer members
    DEALLOCATE( CloudCoeff%Frequency       , &
                CloudCoeff%Wavenumber      , &
                CloudCoeff%Reff_MW         , &
                CloudCoeff%Reff_IR         , &
                CloudCoeff%Temperature     , &
                CloudCoeff%Density         , &
                CloudCoeff%ext_L_MW        , &
                CloudCoeff%w_L_MW          , &
                CloudCoeff%g_L_MW          , &
                CloudCoeff%ext_S_MW        , &
                CloudCoeff%w_S_MW          , &
                CloudCoeff%g_S_MW          , &
                CloudCoeff%phase_coeff_L_MW, &
                CloudCoeff%phase_coeff_S_MW, &
                CloudCoeff%ext_L_IR        , &
                CloudCoeff%w_L_IR          , &
                CloudCoeff%g_L_IR          , &
                CloudCoeff%ext_S_IR        , &
                CloudCoeff%w_S_IR          , &
                CloudCoeff%g_S_IR          , &
                CloudCoeff%phase_coeff_L_IR, &
                CloudCoeff%phase_coeff_S_IR, &
                STAT = Allocate_Status       )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating CloudCoeff. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

    ! Decrement and test allocation counter
    CloudCoeff%n_Allocates = CloudCoeff%n_Allocates - 1
    IF ( CloudCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      CloudCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_CloudCoeff


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_CloudCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the CloudCoeff
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_CloudCoeff( n_Frequencies,            &  ! Input
!                                           n_Reff_MW,                &  ! Input
!                                           n_Wavenumbers,            &  ! Input
!                                           n_Reff_IR,                &  ! Input
!                                           n_Temperatures,           &  ! Input
!                                           n_Densities,              &  ! Input
!                                           n_Legendre_Terms,         &  ! Input
!                                           n_Phase_Elements,         &  ! Input
!                                           CloudCoeff,               &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Frequencies:     The number of monocromatic microwave frequencies 
!                          within look-up table (LUT) 
!                          The "I1" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Reff_MW:         The number of discrete effective radii 
!                          of MW scatterers.
!                          The "I2" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Wavenumbers:     The number of monocromatic infrared wavenumbers
!                          within LUT 
!                          The "I3" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Reff_IR:         The number of discrete effective radii 
!                          of IR scatterers
!                          The "I4" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Temperatures:    The number of discrete layer temperatures
!                          within LUT 
!                          The "I5" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Densities:       The number of fixed densities for snow, graupel,
!                          and hail/ice 
!                          The "I6" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:  The maximum number of Legendre polynomial
!                          terms.
!                          The "I7" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Phase_Elements:  The maximum number of phase elements 
!                          The "I8" dimension. Must be > 0.
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
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       CloudCoeff:        CloudCoeff structure with allocated pointer members
!                          UNITS:      N/A
!                          TYPE:       CloudCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
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
!       Note the INTENT on the output CloudCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_CloudCoeff( n_Frequencies   , &  ! Input
                                n_Reff_MW       , &  ! Input
                                n_Wavenumbers   , &  ! Input
                                n_Reff_IR       , &  ! Input
                                n_Temperatures  , &  ! Input
                                n_Densities     , &  ! Input
                                n_Legendre_Terms, &  ! Input
                                n_Phase_Elements, &  ! Input
                                CloudCoeff      , &  ! Output
                                RCS_Id          , &  ! Revision control
                                Message_Log     ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Frequencies
    INTEGER,                INTENT(IN)     :: n_Reff_MW
    INTEGER,                INTENT(IN)     :: n_Wavenumbers
    INTEGER,                INTENT(IN)     :: n_Reff_IR
    INTEGER,                INTENT(IN)     :: n_Temperatures
    INTEGER,                INTENT(IN)     :: n_Densities
    INTEGER,                INTENT(IN)     :: n_Legendre_Terms
    INTEGER,                INTENT(IN)     :: n_Phase_Elements
    TYPE(CloudCoeff_type),  INTENT(IN OUT) :: CloudCoeff
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_CloudCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    IF ( n_Frequencies    < 1 .OR. &
         n_Reff_MW        < 1 .OR. &
         n_Wavenumbers    < 1 .OR. &
         n_Reff_IR        < 1 .OR. &
         n_Temperatures   < 1 .OR. &
         n_Densities      < 1 .OR. &
         n_Legendre_Terms < 1 .OR. &
         n_Phase_Elements < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input CloudCoeff dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_CloudCoeff( CloudCoeff, ANY_Test=1 ) ) THEN
      Error_Status = Destroy_CloudCoeff( CloudCoeff, &
                                         No_Clear=1, &
                                         Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CloudCoeff prior to allocation.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Perform the pointer allocation
    ALLOCATE( CloudCoeff%Frequency(n_Frequencies), &
              CloudCoeff%Wavenumber(n_Wavenumbers), &
              CloudCoeff%Reff_MW(n_Reff_MW), &
              CloudCoeff%Reff_IR(n_Reff_IR), &
              CloudCoeff%Temperature(n_Temperatures), &
              CloudCoeff%Density(n_Densities), &
              CloudCoeff%ext_L_MW(n_Frequencies,n_Reff_MW,n_Temperatures), &
              CloudCoeff%w_L_MW(n_Frequencies,n_Reff_MW,n_Temperatures), &
              CloudCoeff%g_L_MW(n_Frequencies,n_Reff_MW,n_Temperatures), &
              CloudCoeff%ext_S_MW(n_Frequencies,n_Reff_MW,n_Densities), &
              CloudCoeff%w_S_MW(n_Frequencies,n_Reff_MW,n_Densities), &
              CloudCoeff%g_S_MW(n_Frequencies,n_Reff_MW,n_Densities), &
              CloudCoeff%phase_coeff_L_MW(n_Frequencies,n_Reff_MW,&
                                          n_Temperatures,&
                                          0:n_Legendre_Terms,n_Phase_Elements), &
              CloudCoeff%phase_coeff_S_MW(n_Frequencies,n_Reff_MW,&
                                          n_Densities,&
                                          0:n_Legendre_Terms,n_Phase_Elements), &
              CloudCoeff%ext_L_IR(n_Wavenumbers,n_Reff_IR), &
              CloudCoeff%w_L_IR(n_Wavenumbers,n_Reff_IR), &
              CloudCoeff%g_L_IR(n_Wavenumbers,n_Reff_IR), &
              CloudCoeff%ext_S_IR(n_Wavenumbers,n_Reff_IR,n_Densities), &
              CloudCoeff%w_S_IR(n_Wavenumbers,n_Reff_IR,n_Densities), &
              CloudCoeff%g_S_IR(n_Wavenumbers,n_Reff_IR,n_Densities), &
              CloudCoeff%phase_coeff_L_IR(n_Wavenumbers,n_Reff_IR,&
                                          0:n_Legendre_Terms), &
              CloudCoeff%phase_coeff_S_IR(n_Wavenumbers,n_Reff_IR,n_Densities,&
                                          0:n_Legendre_Terms), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CloudCoeff data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign the dimensions
    CloudCoeff%n_Frequencies      = n_Frequencies
    CloudCoeff%n_Reff_MW          = n_Reff_MW
    CloudCoeff%n_Wavenumbers      = n_Wavenumbers
    CloudCoeff%n_Reff_IR          = n_Reff_IR
    CloudCoeff%n_Temperatures     = n_Temperatures
    CloudCoeff%n_Densities        = n_Densities
    CloudCoeff%Max_Legendre_Terms = n_Legendre_Terms
    CloudCoeff%n_Legendre_Terms   = n_Legendre_Terms 
    CloudCoeff%Max_Phase_Elements = n_Phase_Elements
    CloudCoeff%n_Phase_Elements   = n_Phase_Elements 

    ! Initialise the arrays
    CloudCoeff%Frequency        = FP_INIT
    CloudCoeff%Wavenumber       = FP_INIT
    CloudCoeff%Reff_MW          = FP_INIT
    CloudCoeff%Reff_IR          = FP_INIT
    CloudCoeff%Temperature      = FP_INIT
    CloudCoeff%Density          = FP_INIT
    CloudCoeff%ext_L_MW         = FP_INIT
    CloudCoeff%w_L_MW           = FP_INIT
    CloudCoeff%g_L_MW           = FP_INIT
    CloudCoeff%ext_S_MW         = FP_INIT
    CloudCoeff%w_S_MW           = FP_INIT
    CloudCoeff%g_S_MW           = FP_INIT
    CloudCoeff%phase_coeff_L_MW = FP_INIT
    CloudCoeff%phase_coeff_S_MW = FP_INIT
    CloudCoeff%ext_L_IR         = FP_INIT
    CloudCoeff%w_L_IR           = FP_INIT
    CloudCoeff%g_L_IR           = FP_INIT
    CloudCoeff%ext_S_IR         = FP_INIT
    CloudCoeff%w_S_IR           = FP_INIT
    CloudCoeff%g_S_IR           = FP_INIT
    CloudCoeff%phase_coeff_L_IR = FP_INIT
    CloudCoeff%phase_coeff_S_IR = FP_INIT

    ! Increment and test the allocation counter
    CloudCoeff%n_Allocates = CloudCoeff%n_Allocates + 1
    IF ( CloudCoeff%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      CloudCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_CloudCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Assign_CloudCoeff
!
! PURPOSE:
!       Function to copy valid CloudCoeff structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_CloudCoeff( CloudCoeff_in,            &  ! Input
!                                         CloudCoeff_out,           &  ! Output
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       CloudCoeff_in:     CloudCoeff structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       CloudCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       CloudCoeff_out:    Copy of the input structure, CloudCoeff_in.
!                          UNITS:      N/A
!                          TYPE:       CloudCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
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
!       Note the INTENT on the output CloudCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Assign_CloudCoeff( CloudCoeff_in,  &  ! Input
                              CloudCoeff_out, &  ! Output
                              RCS_Id,         &  ! Revision control
                              Message_Log )   &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(CloudCoeff_type),  INTENT(IN)     :: CloudCoeff_in
    TYPE(CloudCoeff_type),  INTENT(IN OUT) :: CloudCoeff_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_CloudCoeff'

    ! Set up
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    IF ( .NOT. Associated_CloudCoeff( CloudCoeff_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT CloudCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Allocate the structure
    Error_Status = Allocate_CloudCoeff( CloudCoeff_in%n_Frequencies     , &
                                        CloudCoeff_in%n_Reff_MW         , &
                                        CloudCoeff_in%n_Wavenumbers     , &
                                        CloudCoeff_in%n_Reff_IR         , &
                                        CloudCoeff_in%n_Temperatures    , &
                                        CloudCoeff_in%n_Densities       , &
                                        CloudCoeff_in%Max_Legendre_Terms, &
                                        CloudCoeff_in%Max_Phase_Elements, &
                                        CloudCoeff_out, &
                                        Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CloudCoeff arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign non-dimension scalar members
    CloudCoeff_out%Release = CloudCoeff_in%Release
    CloudCoeff_out%Version = CloudCoeff_in%Version

    ! Copy array data
    CloudCoeff_out%Frequency        = CloudCoeff_in%Frequency
    CloudCoeff_out%Wavenumber       = CloudCoeff_in%Wavenumber
    CloudCoeff_out%Reff_MW          = CloudCoeff_in%Reff_MW
    CloudCoeff_out%Reff_IR          = CloudCoeff_in%Reff_IR
    CloudCoeff_out%Temperature      = CloudCoeff_in%Temperature
    CloudCoeff_out%Density          = CloudCoeff_in%Density
    CloudCoeff_out%ext_L_MW         = CloudCoeff_in%ext_L_MW
    CloudCoeff_out%w_L_MW           = CloudCoeff_in%w_L_MW
    CloudCoeff_out%g_L_MW           = CloudCoeff_in%g_L_MW
    CloudCoeff_out%ext_S_MW         = CloudCoeff_in%ext_S_MW
    CloudCoeff_out%w_S_MW           = CloudCoeff_in%w_S_MW
    CloudCoeff_out%g_S_MW           = CloudCoeff_in%g_S_MW
    CloudCoeff_out%phase_coeff_L_MW = CloudCoeff_in%phase_coeff_L_MW
    CloudCoeff_out%phase_coeff_S_MW = CloudCoeff_in%phase_coeff_S_MW
    CloudCoeff_out%ext_L_IR         = CloudCoeff_in%ext_L_IR
    CloudCoeff_out%w_L_IR           = CloudCoeff_in%w_L_IR
    CloudCoeff_out%g_L_IR           = CloudCoeff_in%g_L_IR
    CloudCoeff_out%ext_S_IR         = CloudCoeff_in%ext_S_IR
    CloudCoeff_out%w_S_IR           = CloudCoeff_in%w_S_IR
    CloudCoeff_out%g_S_IR           = CloudCoeff_in%g_S_IR
    CloudCoeff_out%phase_coeff_L_IR = CloudCoeff_in%phase_coeff_L_IR
    CloudCoeff_out%phase_coeff_S_IR = CloudCoeff_in%phase_coeff_S_IR

  END FUNCTION Assign_CloudCoeff


!--------------------------------------------------------------------------------
!
! NAME:
!       Equal_CloudCoeff
!
! PURPOSE:
!       Function to test if two CloudCoeff structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_CloudCoeff( CloudCoeff_LHS,           &  ! Input
!                                        CloudCoeff_RHS,           &  ! Input
!                                        ULP_Scale   = ULP_Scale,  &  ! Optional input
!                                        Check_All   = Check_All,  &  ! Optional input
!                                        RCS_Id      = RCS_Id,     &  ! Optional output
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       CloudCoeff_LHS:    CloudCoeff structure to be compared; equivalent to the
!                          left-hand side of a lexical comparison, e.g.
!                            IF ( CloudCoeff_LHS == CloudCoeff_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CloudCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       CloudCoeff_RHS:    CloudCoeff structure to be compared to; equivalent to
!                          right-hand side of a lexical comparison, e.g.
!                            IF ( CloudCoeff_LHS == CloudCoeff_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CloudCoeff_type
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
!                          channel data of the CloudCoeff structures. The default
!                          action is return with a FAILURE status as soon as
!                          any difference is found. This optional argument can
!                          be used to get a listing of ALL the differences
!                          between data in CloudCoeff structures.
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
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
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

  FUNCTION Equal_CloudCoeff( CloudCoeff_LHS, &  ! Input
                             CloudCoeff_RHS, &  ! Input
                             ULP_Scale,      &  ! Optional input
                             Check_All,      &  ! Optional input
                             RCS_Id,         &  ! Revision control
                             Message_Log )   &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    TYPE(CloudCoeff_type),  INTENT(IN)  :: CloudCoeff_LHS
    TYPE(CloudCoeff_type),  INTENT(IN)  :: CloudCoeff_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,      OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_CloudCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: i, j, k, m, n

    LOGICAL, DIMENSION(CloudCoeff_LHS%n_Frequencies, &
                       CloudCoeff_LHS%n_Reff_MW,     &
                       CloudCoeff_LHS%n_Temperatures ) :: Compare_PhaseCoeff_L_MW
    LOGICAL, DIMENSION(CloudCoeff_LHS%n_Frequencies,&
                       CloudCoeff_LHS%n_Reff_MW,    &
                       CloudCoeff_LHS%n_Densities   )  :: Compare_PhaseCoeff_S_MW

    LOGICAL, DIMENSION(CloudCoeff_LHS%n_Wavenumbers, &
                       CloudCoeff_LHS%n_Reff_IR      ) :: Compare_PhaseCoeff_L_IR
    LOGICAL, DIMENSION(CloudCoeff_LHS%n_Wavenumbers,&
                       CloudCoeff_LHS%n_Reff_IR,    &
                       CloudCoeff_LHS%n_Densities   )  :: Compare_PhaseCoeff_S_IR

    ! Set up
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
    IF ( .NOT. Associated_CloudCoeff( CloudCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT CloudCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_CloudCoeff( CloudCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT CloudCoeff_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check structure Release/Version
    IF ( ( CloudCoeff_LHS%Release /= CloudCoeff_RHS%Release ) .OR. &
         ( CloudCoeff_LHS%Version /= CloudCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      CloudCoeff_LHS%Release, CloudCoeff_LHS%Version, &
                      CloudCoeff_RHS%Release, CloudCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check dimensions
    IF ( CloudCoeff_LHS%n_Frequencies      /= CloudCoeff_RHS%n_Frequencies      .OR. &
         CloudCoeff_LHS%n_Reff_MW          /= CloudCoeff_RHS%n_Reff_MW          .OR. &
         CloudCoeff_LHS%n_Wavenumbers      /= CloudCoeff_RHS%n_Wavenumbers      .OR. &
         CloudCoeff_LHS%n_Reff_IR          /= CloudCoeff_RHS%n_Reff_IR          .OR. &
         CloudCoeff_LHS%n_Temperatures     /= CloudCoeff_RHS%n_Temperatures     .OR. &
         CloudCoeff_LHS%n_Densities        /= CloudCoeff_RHS%n_Densities        .OR. &
         CloudCoeff_LHS%n_Legendre_Terms   /= CloudCoeff_RHS%n_Legendre_Terms   .OR. &
         CloudCoeff_LHS%n_Phase_Elements   /= CloudCoeff_RHS%n_Phase_Elements        ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check the spectral data
    !
    ! Frequencies
    DO i = 1, CloudCoeff_LHS%n_Frequencies
      IF ( .NOT. Compare_Float( CloudCoeff_LHS%Frequency(i), &
                                CloudCoeff_RHS%Frequency(i), &
                                ULP = ULP                    ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Frequency values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! Wavenumber
    DO j = 1, CloudCoeff_LHS%n_Wavenumbers
      IF ( .NOT. Compare_Float( CloudCoeff_LHS%Wavenumber(i), &
                                CloudCoeff_RHS%Wavenumber(i), &
                                ULP = ULP                  ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Wavenumber values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! Check the effective radii
    !
    ! Microwave
    DO j = 1, CloudCoeff_LHS%n_Reff_MW
      IF ( .NOT. Compare_Float( CloudCoeff_LHS%Reff_MW(j), &
                                CloudCoeff_RHS%Reff_MW(j), &
                                ULP = ULP              ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Reff_MW values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    ! Infrared
    DO j = 1, CloudCoeff_LHS%n_Reff_IR
      IF ( .NOT. Compare_Float( CloudCoeff_LHS%Reff_IR(j), &
                                CloudCoeff_RHS%Reff_IR(j), &
                                ULP = ULP                  ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Reff_IR values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! Check the temperature
    DO k = 1, CloudCoeff_LHS%n_Temperatures
      IF ( .NOT. Compare_Float( CloudCoeff_LHS%Temperature(k), &
                                CloudCoeff_RHS%Temperature(k), &
                                ULP = ULP                      ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Temperature values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! Check the density
    DO k = 1, CloudCoeff_LHS%n_Densities
      IF ( .NOT. Compare_Float( CloudCoeff_LHS%Density(k), &
                                CloudCoeff_RHS%Density(k), &
                                ULP = ULP                  ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Density values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! Check the microwave data
    !
    ! The liquid phase data
    DO k = 1, CloudCoeff_LHS%n_Temperatures
      DO j = 1, CloudCoeff_LHS%n_Reff_MW
        DO i = 1, CloudCoeff_LHS%n_Frequencies
          ! ext_L_MW
          IF ( .NOT. Compare_Float(CloudCoeff_LHS%ext_L_MW(i,j,k), &
                                   CloudCoeff_RHS%ext_L_MW(i,j,k), &
                                   ULP = ULP                       ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'ext_L_MW values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
          ! w_L_MW
          IF ( .NOT. Compare_Float(CloudCoeff_LHS%w_L_MW(i,j,k), &
                                   CloudCoeff_RHS%w_L_MW(i,j,k), &
                                   ULP = ULP                     ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'w_L_MW values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
          ! g_L_MW
          IF ( .NOT. Compare_Float(CloudCoeff_LHS%g_L_MW(i,j,k), &
                                   CloudCoeff_RHS%g_L_MW(i,j,k), &
                                   ULP = ULP                     ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'g_L_MW values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        END DO
      END DO
    END DO
    DO n = 1, CloudCoeff_LHS%n_Phase_Elements
      DO m = 0, CloudCoeff_LHS%n_Legendre_Terms
        Compare_PhaseCoeff_L_MW = Compare_Float(CloudCoeff_LHS%Phase_Coeff_L_MW(:,:,:,m,n), &
                                                CloudCoeff_RHS%Phase_Coeff_L_MW(:,:,:,m,n), &
                                                ULP = ULP )
        IF ( ANY( .NOT. Compare_PhaseCoeff_L_MW) ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Phase_Coeff_L_MW values are different', &
                                Error_Status, &
                                Message_Log = Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    ! The solid phase data
    DO k = 1, CloudCoeff_LHS%n_Densities
      DO j = 1, CloudCoeff_LHS%n_Reff_MW
        DO i = 1, CloudCoeff_LHS%n_Frequencies
          ! ext_S_MW
          IF ( .NOT. Compare_Float(CloudCoeff_LHS%ext_S_MW(i,j,k), &
                                   CloudCoeff_RHS%ext_S_MW(i,j,k), &
                                   ULP = ULP                       ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'ext_S_MW values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
          ! w_S_MW
          IF ( .NOT. Compare_Float(CloudCoeff_LHS%w_S_MW(i,j,k), &
                                   CloudCoeff_RHS%w_S_MW(i,j,k), &
                                   ULP = ULP                     ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'w_S_MW values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
          ! g_S_MW
          IF ( .NOT. Compare_Float(CloudCoeff_LHS%g_S_MW(i,j,k), &
                                   CloudCoeff_RHS%g_S_MW(i,j,k), &
                                   ULP = ULP                     ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'g_S_MW values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        END DO
      END DO
    END DO
    DO n = 1, CloudCoeff_LHS%n_Phase_Elements
      DO m = 0, CloudCoeff_LHS%n_Legendre_Terms
        Compare_PhaseCoeff_S_MW = Compare_Float(CloudCoeff_LHS%Phase_Coeff_S_MW(:,:,:,m,n), &
                                                CloudCoeff_RHS%Phase_Coeff_S_MW(:,:,:,m,n), &
                                                ULP = ULP )
        IF ( ANY( .NOT. Compare_PhaseCoeff_S_MW) ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Phase_Coeff_S_MW values are different', &
                                Error_Status, &
                                Message_Log = Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO

    ! Check the infrared data
    !
    ! The liquid phase data
    DO j = 1, CloudCoeff_LHS%n_Reff_IR
      DO i = 1, CloudCoeff_LHS%n_Wavenumbers
        ! ext_L_IR
        IF ( .NOT. Compare_Float(CloudCoeff_LHS%ext_L_IR(i,j), &
                                 CloudCoeff_RHS%ext_L_IR(i,j), &
                                 ULP = ULP                     ) ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'ext_L_IR values are different', &
                                Error_Status, &
                                Message_Log = Message_Log )
          IF ( Check_Once ) RETURN
        END IF
        ! w_L_IR
        IF ( .NOT. Compare_Float(CloudCoeff_LHS%w_L_IR(i,j), &
                                 CloudCoeff_RHS%w_L_IR(i,j), &
                                 ULP = ULP                   ) ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'w_L_IR values are different', &
                                Error_Status, &
                                Message_Log = Message_Log )
          IF ( Check_Once ) RETURN
        END IF
        ! g_L_IR
        IF ( .NOT. Compare_Float(CloudCoeff_LHS%g_L_IR(i,j), &
                                 CloudCoeff_RHS%g_L_IR(i,j), &
                                 ULP = ULP                   ) ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'g_L_IR values are different', &
                                Error_Status, &
                                Message_Log = Message_Log )
          IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
    DO m = 0, CloudCoeff_LHS%n_Legendre_Terms
      Compare_PhaseCoeff_L_IR = Compare_Float(CloudCoeff_LHS%Phase_Coeff_L_IR(:,:,m), &
                                              CloudCoeff_RHS%Phase_Coeff_L_IR(:,:,m), &
                                              ULP = ULP )
      IF ( ANY( .NOT. Compare_PhaseCoeff_L_IR) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Phase_Coeff_L_IR values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The solid phase data
    DO k = 1, CloudCoeff_LHS%n_Densities
      DO j = 1, CloudCoeff_LHS%n_Reff_IR
        DO i = 1, CloudCoeff_LHS%n_Wavenumbers
          ! ext_S_IR
          IF ( .NOT. Compare_Float(CloudCoeff_LHS%ext_S_IR(i,j,k), &
                                   CloudCoeff_RHS%ext_S_IR(i,j,k), &
                                   ULP = ULP                       ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'ext_S_IR values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
          ! w_S_IR
          IF ( .NOT. Compare_Float(CloudCoeff_LHS%w_S_IR(i,j,k), &
                                   CloudCoeff_RHS%w_S_IR(i,j,k), &
                                   ULP = ULP                     ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'w_S_IR values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
          ! g_S_IR
          IF ( .NOT. Compare_Float(CloudCoeff_LHS%g_S_IR(i,j,k), &
                                   CloudCoeff_RHS%g_S_IR(i,j,k), &
                                   ULP = ULP                     ) ) THEN
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'g_S_IR values are different', &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            IF ( Check_Once ) RETURN
          END IF
        END DO
      END DO
    END DO
    DO m = 0, CloudCoeff_LHS%n_Legendre_Terms
      Compare_PhaseCoeff_S_IR = Compare_Float(CloudCoeff_LHS%Phase_Coeff_S_IR(:,:,:,m), &
                                              CloudCoeff_RHS%Phase_Coeff_S_IR(:,:,:,m), &
                                              ULP = ULP )
      IF ( ANY( .NOT. Compare_PhaseCoeff_S_IR) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Phase_Coeff_S_IR values are different', &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

  END FUNCTION Equal_CloudCoeff


!----------------------------------------------------------------------------------
!
! NAME:
!       Check_CloudCoeff_Release
!
! PURPOSE:
!       Function to check the CloudCoeff Release value.
!
! CALLING SEQUENCE:
!       Error_Status = Check_CloudCoeff_Release( CloudCoeff,               &  ! Input
!                                                RCS_Id      = RCS_Id,     &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       CloudCoeff:    CloudCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       CloudCoeff_type
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

  FUNCTION Check_CloudCoeff_Release( CloudCoeff,   &  ! Input
                                     RCS_Id,       &  ! Revision control
                                     Message_Log ) &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    TYPE(CloudCoeff_type), INTENT(IN)  :: CloudCoeff
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Check_CloudCoeff_Release'
    ! Local variables
    CHARACTER(256) :: Message

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check release is not too old
    IF ( CloudCoeff%Release < CLOUDCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A CloudCoeff data update is needed. ", &
                        &"CloudCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      CloudCoeff%Release, CLOUDCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check release is not too new
    IF ( CloudCoeff%Release > CLOUDCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A CloudCoeff software update is needed. ", &
                        &"CloudCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      CloudCoeff%Release, CLOUDCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_CloudCoeff_Release


!--------------------------------------------------------------------------------
!
! NAME:
!       Info_CloudCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the CloudCoeff data structure.
!
! CALLING SEQUENCE:
!       CALL Info_CloudCoeff( CloudCoeff,     &  ! Input
!                             Info,           &  ! Output
!                             RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       CloudCoeff:    Filled CloudCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       CloudCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed CloudCoeff data structure.
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

  SUBROUTINE Info_CloudCoeff( CloudCoeff, &  ! Input
                              Info,       &  ! Output
                              RCS_Id      )  ! Revision control
    ! Arguments
    TYPE(CloudCoeff_type),  INTENT(IN)  :: CloudCoeff
    CHARACTER(*),           INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(1000) :: Long_String

    ! Set up
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required info to the local string
    WRITE( Long_String, '( a,1x,"CloudCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
                           &"N_FREQUENCIES(MW)=",i4,2x,&
                           &"N_WAVENUMBERS(IR)=",i4,2x,&
                           &"N_REFF(MW)=",i2,2x,&
                           &"N_REFF(IR)=",i2,2x,&
                           &"N_TEMPERATURES=",i2,2x,&
                           &"N_DENSITIES=",i2,2x,&
                           &"N_LEGENDRE_TERMS=",i2,2x,&
                           &"N_PHASE_ELEMENTS=",i2 )' ) &
                        ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                        CloudCoeff%Release, CloudCoeff%Version, &
                        CloudCoeff%n_Frequencies   , &
                        CloudCoeff%n_Wavenumbers   , &
                        CloudCoeff%n_Reff_MW       , &
                        CloudCoeff%n_Reff_IR       , &
                        CloudCoeff%n_Temperatures  , &
                        CloudCoeff%n_Densities     , &
                        CloudCoeff%n_Legendre_Terms, &
                        CloudCoeff%n_Phase_Elements

    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN( LEN(Info), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Info_CloudCoeff

END MODULE CloudCoeff_Define
