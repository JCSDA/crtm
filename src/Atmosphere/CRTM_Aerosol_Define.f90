!
! CRTM_Aerosol_Define
!
! Module defining the CRTM Aerosol structure and containing
! routines to manipulate it.
!       
! PUBLIC PARAMETERS:
!       1) The valid aerosol type values used in the Aerosol%Type field:
!
!              Aerosol Type      Parameter Name
!         --------------------------------------------------
!                 None           NO_AEROSOL   
!                 Dust           DUST_AEROSOL   
!             Sea salt SSAM(*)   SEASALT_SSAM_AEROSOL 
!             Sea salt SSCM(+)   SEASALT_SSCM_AEROSOL 
!           Dry organic carbon   DRY_ORGANIC_CARBON_AEROSOL
!           Wet organic carbon   WET_ORGANIC_CARBON_AEROSOL
!            Dry black carbon    DRY_BLACK_CARBON_AEROSOL
!            Wet black carbon    WET_BLACK_CARBON_AEROSOL
!                Sulfate         SULFATE_AEROSOL  
!
!          (*) SSAM == sea salt accumulation mode, Reff ~ 0.5 - 5.0 um
!          (+) SSCM == sea salt coarse mode,       Reff ~ 5.0 - 30 um
!
!       2) The number of valid aerosol types is specified by the 
!            N_VALID_AEROSOL_TYPES
!          parameter.
!
!       3) The character string array parameter
!            AEROSOL_TYPE_NAME
!          uses the above aerosol type definitions to provide a string value for
!          the type of aerosol. For example,
!            AEROSOL_TYPE_NAME( DRY_BLACK_CARBON_AEROSOL )
!          contains the string
!            'Dry black carbon'
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!                       Quanhua Liu, QSS
!                       Quanhua.Liu@noaa.gov
!

MODULE CRTM_Aerosol_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE CRTM_Parameters      , ONLY: ZERO, ONE, SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_Aerosol parameters
  PUBLIC :: N_VALID_AEROSOL_TYPES
  PUBLIC :: NO_AEROSOL
  PUBLIC :: DUST_AEROSOL
  PUBLIC :: SEASALT_SSAM_AEROSOL
  PUBLIC :: SEASALT_SSCM_AEROSOL
  PUBLIC :: DRY_ORGANIC_CARBON_AEROSOL
  PUBLIC :: WET_ORGANIC_CARBON_AEROSOL
  PUBLIC :: DRY_BLACK_CARBON_AEROSOL
  PUBLIC :: WET_BLACK_CARBON_AEROSOL
  PUBLIC :: SULFATE_AEROSOL
  PUBLIC :: AEROSOL_TYPE_NAME
  ! CRTM_Aerosol data structure definition
  PUBLIC :: CRTM_Aerosol_type
  ! CRTM_Aerosol structure routines
  PUBLIC :: CRTM_Associated_Aerosol
  PUBLIC :: CRTM_Destroy_Aerosol
  PUBLIC :: CRTM_Allocate_Aerosol
  PUBLIC :: CRTM_Assign_Aerosol
  PUBLIC :: CRTM_Equal_Aerosol
  PUBLIC :: CRTM_Sum_Aerosol
  PUBLIC :: CRTM_Zero_Aerosol
  PUBLIC :: CRTM_RCS_ID_Aerosol


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Associated_Aerosol
    MODULE PROCEDURE Associated_Scalar
    MODULE PROCEDURE Associated_Rank1
  END INTERFACE CRTM_Associated_Aerosol

  INTERFACE CRTM_Destroy_Aerosol
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE CRTM_Destroy_Aerosol

  INTERFACE CRTM_Allocate_Aerosol
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE CRTM_Allocate_Aerosol

  INTERFACE CRTM_Assign_Aerosol
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Aerosol

  INTERFACE CRTM_Equal_Aerosol
    MODULE PROCEDURE Equal_Scalar
    MODULE PROCEDURE Equal_Rank1
  END INTERFACE CRTM_Equal_Aerosol

  INTERFACE CRTM_Sum_Aerosol
    MODULE PROCEDURE Sum_Scalar
    MODULE PROCEDURE Sum_Rank1
  END INTERFACE CRTM_Sum_Aerosol

  INTERFACE CRTM_Zero_Aerosol
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE CRTM_Zero_Aerosol


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Aerosol types and names
  INTEGER, PARAMETER :: N_VALID_AEROSOL_TYPES = 8
  INTEGER, PARAMETER ::                 NO_AEROSOL = 0
  INTEGER, PARAMETER ::               DUST_AEROSOL = 1
  INTEGER, PARAMETER ::       SEASALT_SSAM_AEROSOL = 2
  INTEGER, PARAMETER ::       SEASALT_SSCM_AEROSOL = 3
  INTEGER, PARAMETER :: DRY_ORGANIC_CARBON_AEROSOL = 4
  INTEGER, PARAMETER :: WET_ORGANIC_CARBON_AEROSOL = 5
  INTEGER, PARAMETER ::   DRY_BLACK_CARBON_AEROSOL = 6
  INTEGER, PARAMETER ::   WET_BLACK_CARBON_AEROSOL = 7
  INTEGER, PARAMETER ::            SULFATE_AEROSOL = 8
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_AEROSOL_TYPES ) :: &
    AEROSOL_TYPE_NAME = (/ 'None              ', &
                           'Dust              ', &
                           'Sea salt (SSAM)   ', &
                           'Sea salt (SSCM)   ', &
                           'Dry organic carbon', &
                           'Wet organic carbon', &
                           'Dry black carbon  ', &
                           'Wet black carbon  ', &
                           'Sulfate           ' /)
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ----------------------------
  ! Aerosol data type definition
  ! ----------------------------
  TYPE :: CRTM_Aerosol_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: Max_Layers = 0  ! K dimension.
    INTEGER :: n_Layers   = 0  ! K dimension
    ! Number of added layers
    INTEGER :: n_Added_Layers = 0
    ! Aerosol type
    INTEGER :: Type = NO_AEROSOL
    ! Aerosol state variables
    REAL(fp), POINTER :: Effective_Radius(:) => NULL()  ! K. Units are microns
    REAL(fp), POINTER :: Concentration(:)    => NULL()  ! K. Units are kg/m^2  
  END TYPE CRTM_Aerosol_type


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
!       CRTM_Clear_Aerosol
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_Aerosol structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Aerosol( Aerosol ) ! Output
!
! OUTPUT ARGUMENTS:
!       Aerosol:  CRTM_Aerosol structure for which the scalar members have
!                 been cleared.
!                 UNITS:      N/A
!                 TYPE:       CRTM_Aerosol_type
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Aerosol( Aerosol )
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    Aerosol%Type = NO_AEROSOL
  END SUBROUTINE CRTM_Clear_Aerosol


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
!       CRTM_Associated_Aerosol
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_Aerosol structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Aerosol( Aerosol          , &  ! Input
!                                                     ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       Aerosol:             CRTM_Aerosol structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Aerosol_type
!                            DIMENSION:  Scalar or Rank-1
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CRTM_Aerosol structure pointer members are associated.
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
!                            association status of the CRTM_Aerosol pointer members.
!                            .TRUE.  - if ALL the CRTM_Aerosol pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the CRTM_Aerosol pointer
!                                      members are associated.
!                            .FALSE. - some or all of the CRTM_Aerosol pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_Scalar( Aerosol , & ! Input
                              ANY_Test) & ! Optional input
                            RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: Aerosol
    INTEGER,       OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test

    ! Set up
    ! ------
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT(ANY_Test) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! Test the structure pointer member association
    ! ---------------------------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED(Aerosol%Concentration   ) .AND. &
           ASSOCIATED(Aerosol%Effective_Radius) ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED(Aerosol%Concentration   ) .OR. &
           ASSOCIATED(Aerosol%Effective_Radius) ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_Scalar


  FUNCTION Associated_Rank1( Aerosol , & ! Input
                             ANY_Test) & ! Optional input
                           RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: Aerosol(:)
    INTEGER,       OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status(SIZE(Aerosol))
    ! Local variables
    INTEGER :: n

    DO n = 1, SIZE(Aerosol)
      Association_Status(n) = Associated_Scalar(Aerosol(n), ANY_Test=ANY_Test)
    END DO

  END FUNCTION Associated_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_Aerosol
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_Aerosol data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Aerosol( Aerosol                , &  ! Output
!                                            Message_Log=Message_Log  )  ! Error messaging
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
!       Aerosol:      Re-initialized CRTM_Aerosol structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar or Rank1
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Aerosol    , &  ! Output
                           No_Clear   , &  ! Optional input
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Aerosol(Scalar)'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    
    ! Reinitialise the dimensions
    Aerosol%Max_Layers = 0
    Aerosol%n_Layers   = 0
    Aerosol%n_Added_Layers = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT(No_Clear) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL CRTM_Clear_Aerosol( Aerosol )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol ) ) RETURN


    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( Aerosol%Concentration   , &
                Aerosol%Effective_Radius, &
                STAT = Allocate_Status    )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating CRTM_Aerosol pointer components.", &
                        &" STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    Aerosol%n_Allocates = Aerosol%n_Allocates - 1
    IF ( Aerosol%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i0 )' ) &
                      Aerosol%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( Aerosol    , &  ! Output
                          No_Clear   , &  ! Optional input
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol(:)
    INTEGER     ,  OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Aerosol(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Reinitialise array
    ! ------------------
    DO n = 1, SIZE(Aerosol)
      Scalar_Status = Destroy_Scalar( Aerosol(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i0, &
                          &" of Aerosol structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO

  END FUNCTION Destroy_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_Aerosol
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_Aerosol
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Aerosol( n_Layers               , &  ! Input
!                                             Aerosol                , &  ! Output
!                                             Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Layers:   Number of atmospheric layers dimension.
!                     Must be > 0
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
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
!       Aerosol:      CRTM_Aerosol structure with allocated pointer members.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Same as input n_Layers argument
!                     ATTRIBUTES: INTENT(IN OUT)
!
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
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Layers   , &  ! Input
                            Aerosol    , &  ! Output
                            Message_Log) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER                , INTENT(IN)     :: n_Layers
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Scalar)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    
    ! Dimensions
    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( CRTM_Associated_Aerosol( Aerosol, ANY_Test=SET ) ) THEN
      Error_Status = CRTM_Destroy_Aerosol( Aerosol, &
                                           No_Clear=SET, &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating Aerosol pointer members.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Perform the allocation
    ! ----------------------
    ALLOCATE( Aerosol%Effective_Radius( n_Layers ), &
              Aerosol%Concentration( n_Layers ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Aerosol components. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the dimensions and initalise arrays
    ! ------------------------------------------
    Aerosol%Max_Layers = n_Layers
    Aerosol%n_Layers   = n_Layers
    Aerosol%n_Added_Layers = 0
    
    Aerosol%Effective_Radius = ZERO
    Aerosol%Concentration    = ZERO


    ! Increment and test allocation counter
    ! -------------------------------------
    Aerosol%n_Allocates = Aerosol%n_Allocates + 1
    IF ( Aerosol%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i0 )' ) &
                      Aerosol%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank1( n_Layers   , &  ! Input
                           Aerosol    , &  ! Output
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    INTEGER                , INTENT(IN)     :: n_Layers(:)
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol(:)
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Array arguments must conform
    n = SIZE(Aerosol)
    IF ( SIZE(n_Layers) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and Aerosol arrays have different dimensions', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Perform the allocation
    ! ----------------------
    DO i = 1, n
      Scalar_Status = Allocate_Scalar( n_Layers(i), &
                                       Aerosol(i), &
                                       Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i0, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO

  END FUNCTION Allocate_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_Aerosol
!
! PURPOSE:
!       Function to copy valid CRTM_Aerosol structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Aerosol( Aerosol_in             , &  ! Input
!                                           Aerosol_out            , &  ! Output
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Aerosol_in:      CRTM_Aerosol structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Aerosol_type
!                        DIMENSION:  Scalar or Rank-1 array
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Aerosol_out:     Copy of the input structure, CRTM_Aerosol_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Aerosol_type
!                        DIMENSION:  Same as Aerosol_in argument
!                        ATTRIBUTES: INTENT(IN OUT)
!
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( Aerosol_in    , &  ! Input
                          Aerosol_out   , &  ! Output
                          n_Added_Layers, &  ! Optional input
                          Message_Log   ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN)     :: Aerosol_in
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol_out
    INTEGER,       OPTIONAL, INTENT(IN)     :: n_Added_Layers
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Aerosol(Scalar)'
    ! Local variables
    INTEGER :: na, no, nt

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! ALL *input* pointers must be associated.
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol_In ) ) THEN
      Error_Status = CRTM_Destroy_Aerosol( Aerosol_Out, &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating Aerosol_out components.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
      END IF
      RETURN
    END IF

    ! Set the number of extra layers to add
    na = 0
    IF ( PRESENT(n_Added_Layers) ) na = MAX(n_Added_Layers,na)


    ! Allocate the structure
    ! ----------------------
    IF ( CRTM_Associated_Aerosol( Aerosol_out ) .AND. &
         Aerosol_Out%Max_Layers >= Aerosol_in%n_Layers+na ) THEN
         
      ! If structure already allocated to sufficient size,
      ! then zero and set layer dimension
      CALL CRTM_Zero_Aerosol( Aerosol_Out )
      Aerosol_Out%n_Layers = Aerosol_in%n_Layers+na
      
    ELSE
    
      ! Explicitly allocate structure
      Error_Status = CRTM_Allocate_Aerosol( Aerosol_in%n_Layers+na, &
                                            Aerosol_out, &
                                            Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error allocating Aerosol_out components.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    

    ! Assign data
    ! -----------
    Aerosol_out%n_Added_Layers = Aerosol_in%n_Added_Layers+na
    Aerosol_out%Type = Aerosol_in%Type
    
    no = Aerosol_In%n_Layers
    nt = Aerosol_Out%n_Layers
    Aerosol_out%Effective_Radius(na+1:nt) = Aerosol_in%Effective_Radius(1:no)
    Aerosol_out%Concentration(na+1:nt)    = Aerosol_in%Concentration(1:no)

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Aerosol_in    , &  ! Input
                         Aerosol_out   , &  ! Output
                         n_Added_Layers, &  ! Optional input
                         Message_Log   ) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN)     :: Aerosol_in(:)
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol_out(:)
    INTEGER,       OPTIONAL, INTENT(IN)     :: n_Added_Layers
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Aerosol(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Output array must be large enough to handle input copy.
    n = SIZE(Aerosol_in)
    IF ( SIZE(Aerosol_out) < n ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Aerosol_out array not large enough (",i0,&
                      &") to hold Aerosol_in data (",i0,").")' ) &
                      SIZE(Aerosol_out), n
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Perform the assignment
    ! ----------------------
    DO i = 1, n
      Scalar_Status = Assign_Scalar( Aerosol_in(i), &
                                     Aerosol_out(i), &
                                     n_Added_Layers=n_Added_Layers, &
                                     Message_Log   =Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i0, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO

  END FUNCTION Assign_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Equal_Aerosol
!
! PURPOSE:
!       Function to test if two Aerosol structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Equal_Aerosol( Aerosol_LHS            , &  ! Input
!                                          Aerosol_RHS            , &  ! Input
!                                          ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                          Check_All  =Check_All  , &  ! Optional input
!                                          Message_Log=Message_Log  )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!       Aerosol_LHS:       Aerosol structure to be compared; equivalent to the
!                          left-hand side of a lexical comparison, e.g.
!                            IF ( Aerosol_LHS == Aerosol_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CRTM_Aerosol_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Aerosol_RHS:       Aerosol structure to be compared to; equivalent to
!                          right-hand side of a lexical comparison, e.g.
!                            IF ( Aerosol_LHS == Aerosol_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CRTM_Aerosol_type
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
!                          channel data of the Aerosol structures. The default
!                          action is return with a FAILURE status as soon as
!                          any difference is found. This optional argument can
!                          be used to get a listing of ALL the differences
!                          between data in Aerosol structures.
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

  FUNCTION Equal_Scalar( Aerosol_LHS, &  ! Input
                         Aerosol_RHS, &  ! Input
                         ULP_Scale  , &  ! Optional input
                         Check_All  , &  ! Optional input
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: Aerosol_LHS
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: Aerosol_RHS
    INTEGER,       OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,       OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_Aerosol(scalar)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: i, j, k, m, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Default precision is a single unit in last place
    ULP = 1
    ! ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT(ULP_Scale) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF

    ! Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT(Check_All) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF

    ! Check the structure association status
    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Aerosol_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Aerosol_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( Aerosol_LHS%n_Layers /= Aerosol_RHS%n_Layers ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Compare the values
    ! ------------------
    IF ( Aerosol_LHS%Type /= Aerosol_RHS%Type ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Type values are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    
    DO k = 1, Aerosol_LHS%n_Layers
      IF ( .NOT. Compare_Float( Aerosol_LHS%Effective_Radius(k), &
                                Aerosol_RHS%Effective_Radius(k), &
                                ULP = ULP ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Effective_Radius values are different', &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    
    DO k = 1, Aerosol_LHS%n_Layers
      IF ( .NOT. Compare_Float( Aerosol_LHS%Concentration(k), &
                                Aerosol_RHS%Concentration(k), &
                                ULP = ULP ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Concentration values are different', &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
  END FUNCTION Equal_Scalar


  FUNCTION Equal_Rank1( Aerosol_LHS, &  ! Input
                        Aerosol_RHS, &  ! Output
                        ULP_Scale  , &  ! Optional input
                        Check_All  , &  ! Optional input
                        Message_Log) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: Aerosol_LHS(:)
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: Aerosol_RHS(:)
    INTEGER,       OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,       OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_Aerosol(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Check_Once
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT(Check_All) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF

    ! Dimensions
    n = SIZE(Aerosol_LHS)
    IF ( SIZE(Aerosol_RHS) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Aerosol_LHS and Aerosol_RHS arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Test for equality
    ! -----------------
    DO i = 1, n
      Scalar_Status = Equal_Scalar( Aerosol_LHS(i), &
                                    Aerosol_RHS(i), &
                                    ULP_Scale  =ULP_Scale, &
                                    Check_All  =Check_All, &
                                    Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error comparing element (",i0,")", &
                          &" of rank-1 CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
  END FUNCTION Equal_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Sum_Aerosol
!
! PURPOSE:
!       Function to perform a sum of two valid CRTM_Aerosol structures. The
!       summation performed is:
!         A = A + Scale_Factor*B + Offset
!       where A and B are the CRTM_Aerosol structures, and Scale_Factor and Offset
!       are optional weighting factors.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Sum_Aerosol( A                        , &  ! In/Output
!                                        B                        , &  ! Input
!                                        Scale_Factor=Scale_Factor, &  ! Optional input
!                                        Offset      =Offset      , &  ! Optional input
!                                        Message_Log =Message_Log   )  ! Error messaging
!
! INPUT ARGUMENTS:
!       A:             Aerosol structure that is to be added to.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Scalar OR Rank-1
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       B:             Aerosol structure that is to be weighted and
!                      added to structure A.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Same as A
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Scale_Factor:  The first weighting factor used to scale the
!                      contents of the input structure, B.
!                      If not specified, Scale_Factor = 1.0.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Offset:        The second weighting factor used to offset the
!                      sum of the input structures.
!                      If not specified, Offset = 0.0.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
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
! OUTPUT ARGUMENTS:
!       A:             Structure containing the summation result,
!                        A = A + Scale_Factor*B + Offset
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Same as B
!                      ATTRIBUTES: INTENT(IN OUT)
!
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure summation was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The argument A is INTENT(IN OUT) and is modified upon output.
!
!--------------------------------------------------------------------------------

  FUNCTION Sum_Scalar( A           , &  ! Input/Output
                       B           , &  ! Input
                       Scale_Factor, &  ! Input
                       Offset      , &  ! optional input
                       Message_Log ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: A
    TYPE(CRTM_Aerosol_type), INTENT(IN)     :: B
    REAL(fp)    , OPTIONAL,  INTENT(IN)     :: Scale_Factor
    REAL(fp)    , OPTIONAL,  INTENT(IN)     :: Offset
    CHARACTER(*), OPTIONAL,  INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Sum_Aerosol(Scalar)'
    ! Local variables
    INTEGER  :: n
    REAL(fp) :: m, c

    ! Set up
    ! ------
    Error_Status = SUCCESS
    
    ! ALL *input* pointers must be associated
    IF ( .NOT. CRTM_Associated_Aerosol( A ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument A appears empty.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. CRTM_Associated_Aerosol( B ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument B appears empty.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Array arguments must conform
    n = A%n_Layers
    IF ( B%n_Layers /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure dimensions are different.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Aerosol types must be the same
    IF ( A%Type /= B%Type ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure Aerosol types are different.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the optional weights
    m = ONE; c = ZERO
    IF ( PRESENT(Scale_Factor) ) m = Scale_Factor
    IF ( PRESENT(Offset      ) ) c = Offset


    ! Perform the summation
    ! ---------------------
    A%Concentration(1:n)    = A%Concentration(1:n)    + (m*B%Concentration(1:n)   ) + c
    A%Effective_Radius(1:n) = A%Effective_Radius(1:n) + (m*B%Effective_Radius(1:n)) + c

  END FUNCTION Sum_Scalar


  FUNCTION Sum_Rank1( A           , &  ! Input/Output
                      B           , &  ! Input
                      Scale_Factor, &  ! Input
                      Offset      , &  ! optional input
                      Message_Log ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: A(:)
    TYPE(CRTM_Aerosol_type), INTENT(IN)     :: B(:)
    REAL(fp)    ,  OPTIONAL, INTENT(IN)     :: Scale_Factor
    REAL(fp)    ,  OPTIONAL, INTENT(IN)     :: Offset
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Sum_Aerosol(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Array arguments must "loosely" conform, i.e. B must contain
    ! at least enough elements to handle the summation with A.
    n = SIZE(A)
    IF ( SIZE(B) < n ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Input structure arguments have incompatible dimensions. ",&
                      &"Size of A (",i0,") must be < or = size of B (",i0,").")' ) &
                      n, SIZE(B)
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Perform the summation
    ! ---------------------
    DO i = 1, n
      Scalar_Status = Sum_Scalar( A(i), &
                                  B(i), &
                                  Scale_Factor=Scale_Factor, &
                                  Offset      =Offset, &
                                  Message_Log =Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error computing sum for element #", i0, &
                          &" of CRTM_Aerosol structure arrays." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO

  END FUNCTION Sum_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Zero_Aerosol
! 
! PURPOSE:
!       Subroutine to zero-out all members of a CRTM_Aerosol structure - both
!       scalar and pointer.
!
! CALLING SEQUENCE:
!       CALL CRTM_Zero_Aerosol( Aerosol )
!
! OUTPUT ARGUMENTS:
!       Aerosol: Zeroed out Aerosol structure.
!                UNITS:      N/A
!                TYPE:       CRTM_Aerosol_type
!                DIMENSION:  Scalar or Rank-1 array
!                ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - No checking of the input structure is performed, so there are no
!         tests for pointer member association status. This means the Aerosol
!         structure must have allocated pointer members upon entry to this
!         routine.
!
!       - The dimension components of the structure are *NOT*
!         set to zero.
!
!       - The aerosol type component is *NOT* reset.
!
!       - Note the INTENT on the output Aerosol argument is IN OUT rather than
!         just OUT. This is necessary because the argument must be defined upon
!         input.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Scalar( Aerosol )  ! Output
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    
    ! Reset the added layer count
    Aerosol%n_Added_Layers = 0
    
    ! Reset the array components
    Aerosol%Effective_Radius   = ZERO
    Aerosol%Concentration      = ZERO
  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( Aerosol )  ! Output
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol(:)
    INTEGER :: n
    DO n = 1, SIZE( Aerosol )
      CALL Zero_Scalar( Aerosol(n) )
    END DO
  END SUBROUTINE Zero_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_RCS_ID_Aerosol
!
! PURPOSE:
!       Subroutine to return the module RCS Id information.
!
! CALLING SEQUENCE:
!       CALL CRTM_RCS_Id_Aerosol( RCS_Id )
!
! OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_RCS_ID_Aerosol( RCS_Id )
    CHARACTER(*), INTENT(OUT) :: RCS_Id
    RCS_Id = MODULE_RCS_ID
  END SUBROUTINE CRTM_RCS_ID_Aerosol

END MODULE CRTM_Aerosol_Define
