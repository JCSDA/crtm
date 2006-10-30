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
!                Sea salt        SEASALT_AEROSOL  
!           Dry organic carbon   DRY_ORGANIC_CARBON_AEROSOL
!           Wet organic carbon   WET_ORGANIC_CARBON_AEROSOL
!            Dry black carbon    DRY_BLACK_CARBON_AEROSOL
!            Wet black carbon    WET_BLACK_CARBON_AEROSOL
!                Sulfate         SULFATE_AEROSOL  
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
!

MODULE CRTM_Aerosol_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,      ONLY: fp=>fp_kind
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_Aerosol parameters
  PUBLIC :: N_VALID_AEROSOL_TYPES
  PUBLIC ::                 NO_AEROSOL
  PUBLIC ::               DUST_AEROSOL
  PUBLIC ::            SEASALT_AEROSOL
  PUBLIC :: DRY_ORGANIC_CARBON_AEROSOL
  PUBLIC :: WET_ORGANIC_CARBON_AEROSOL
  PUBLIC ::   DRY_BLACK_CARBON_AEROSOL
  PUBLIC ::   WET_BLACK_CARBON_AEROSOL
  PUBLIC ::            SULFATE_AEROSOL
  PUBLIC :: AEROSOL_TYPE_NAME
  ! CRTM_Aerosol data structure definition
  PUBLIC :: CRTM_Aerosol_type
  ! CRTM_Aerosol structure routines
  PUBLIC :: CRTM_Associated_Aerosol
  PUBLIC :: CRTM_Destroy_Aerosol
  PUBLIC :: CRTM_Allocate_Aerosol
  PUBLIC :: CRTM_Assign_Aerosol
  PUBLIC :: CRTM_WeightedSum_Aerosol
  PUBLIC :: CRTM_Zero_Aerosol


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Destroy_Aerosol
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE CRTM_Destroy_Aerosol

  INTERFACE CRTM_Allocate_Aerosol
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank001
    MODULE PROCEDURE Allocate_Rank011
    MODULE PROCEDURE Allocate_Rank101
    MODULE PROCEDURE Allocate_Rank111
  END INTERFACE CRTM_Allocate_Aerosol

  INTERFACE CRTM_Assign_Aerosol
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Aerosol

  INTERFACE CRTM_WeightedSum_Aerosol
    MODULE PROCEDURE WeightedSum_Scalar
    MODULE PROCEDURE WeightedSum_Rank1
  END INTERFACE CRTM_WeightedSum_Aerosol

  INTERFACE CRTM_Zero_Aerosol
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE CRTM_Zero_Aerosol


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Aerosol types and names
  INTEGER, PARAMETER :: N_VALID_AEROSOL_TYPES = 7
  INTEGER, PARAMETER ::                 NO_AEROSOL = 0
  INTEGER, PARAMETER ::               DUST_AEROSOL = 1
  INTEGER, PARAMETER ::            SEASALT_AEROSOL = 2
  INTEGER, PARAMETER :: DRY_ORGANIC_CARBON_AEROSOL = 3
  INTEGER, PARAMETER :: WET_ORGANIC_CARBON_AEROSOL = 4
  INTEGER, PARAMETER ::   DRY_BLACK_CARBON_AEROSOL = 5
  INTEGER, PARAMETER ::   WET_BLACK_CARBON_AEROSOL = 6
  INTEGER, PARAMETER ::            SULFATE_AEROSOL = 7
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_AEROSOL_TYPES ) :: &
    AEROSOL_TYPE_NAME = (/ 'None              ', &
                           'Dust              ', &
                           'Sea salt          ', &
                           'Dry organic carbon', &
                           'Wet organic carbon', &
                           'Dry black carbon  ', &
                           'Wet black carbon  ', &
                           'Sulfate           ' /)
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Aerosol_Define.f90,v 2.7 2006/05/25 19:33:27 wd20pd Exp $'


  ! ----------------------------
  ! Aerosol data type definition
  ! ----------------------------
  TYPE :: CRTM_Aerosol_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Layers  = 0  ! K dimension
    INTEGER :: Max_Modes = 0  ! Nm dimension
    INTEGER :: n_Modes   = 0  ! NmUse dimension
    ! Aerosol type
    INTEGER :: Type = NO_AEROSOL
    ! Particle size distribution parameters
    REAL(fp), DIMENSION(:,:), POINTER :: Effective_Radius   => NULL() ! K x Nm
    REAL(fp), DIMENSION(:,:), POINTER :: Effective_Variance => NULL() ! K x Nm
    ! Aerosol state variables
    REAL(fp), DIMENSION(:,:), POINTER :: Concentration => NULL()      ! K x Nm
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
    Aerosol%n_Layers  = 0
    Aerosol%Max_Modes = 0
    Aerosol%n_Modes   = 0
    Aerosol%Type      = NO_AEROSOL
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
!       Association_Status = CRTM_Associated_Aerosol( Aerosol,            &  ! Input
!                                                     ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Aerosol:             CRTM_Aerosol structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Aerosol_type
!                            DIMENSION:  Scalar
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

  FUNCTION CRTM_Associated_Aerosol( Aerosol,   & ! Input
                                    ANY_Test ) & ! Optional input
                                  RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: Aerosol
    INTEGER,       OPTIONAL, INTENT(IN) :: ANY_Test
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
      IF ( ASSOCIATED( Aerosol%Effective_Radius   ) .AND. &
           ASSOCIATED( Aerosol%Effective_Variance ) .AND. &
           ASSOCIATED( Aerosol%Concentration      )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( Aerosol%Effective_Radius   ) .OR. &
           ASSOCIATED( Aerosol%Effective_Variance ) .OR. &
           ASSOCIATED( Aerosol%Concentration      )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION CRTM_Associated_Aerosol


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
!       Error_Status = CRTM_Destroy_Aerosol( Aerosol,                  &  ! Output
!                                            RCS_Id = RCS_Id,          &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
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
!       Aerosol:      Re-initialized CRTM_Aerosol structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
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
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Aerosol,      &  ! Output
                           No_Clear,     &  ! Optional input
                           RCS_Id,       &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Aerosol(Scalar)'
    ! Local variables
    CHARACTER( 256 ) :: Message
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


    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------
    IF ( Clear ) CALL CRTM_Clear_Aerosol( Aerosol )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------
    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------
    ! Deallocate the Effective_Radius profile
    IF ( ASSOCIATED( Aerosol%Effective_Radius ) ) THEN
      DEALLOCATE( Aerosol%Effective_Radius, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Aerosol Effective_Radius ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the Effective_Variance profile
    IF ( ASSOCIATED( Aerosol%Effective_Variance ) ) THEN
      DEALLOCATE( Aerosol%Effective_Variance, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Aerosol Effective_Variance ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the Concentration profile
    IF ( ASSOCIATED( Aerosol%Concentration ) ) THEN
      DEALLOCATE( Aerosol%Concentration, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Aerosol Concentration ", &
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
    Aerosol%n_Allocates = Aerosol%n_Allocates - 1
    IF ( Aerosol%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Aerosol%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( Aerosol,      &  ! Output
                          No_Clear,     &  ! Optional input
                          RCS_Id,       &  ! Revision control
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN OUT) :: Aerosol
    INTEGER,                 OPTIONAL,     INTENT(IN)     :: No_Clear
    CHARACTER(*),            OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Aerosol(Rank-1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise array
    DO n = 1, SIZE( Aerosol )
      Scalar_Status = Destroy_Scalar( Aerosol(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of Aerosol structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
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
!       Error_Status = CRTM_Allocate_Aerosol( n_Layers,                 &  ! Input
!                                             n_Modes,                  &  ! Input
!                                             Aerosol,                  &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Layers:   Number of atmospheric layers dimension.
!                     Must be > 0
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Aerosol argument
!                                 dimensionality chart
!                     ATTRIBUTES: INTENT(IN)
!
!         n_Modes:    Number of size distribution modes dimension.
!                     Must be > 0
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Aerosol argument
!                                 dimensionality chart
!                     ATTRIBUTES: INTENT(IN)
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
!       Aerosol:      CRTM_Aerosol structure with allocated pointer members. The
!                     following table shows the allowable dimension combinations
!                     for the calling routine, where N == number of aerosol types:
!
!                        Input       Input       Output
!                       n_Layers    n_Modes      Aerosol
!                       dimension   dimension   dimension
!                     -------------------------------------
!                        scalar      scalar       scalar
!                        scalar      scalar         N
!                        scalar        N            N
!                          N         scalar         N
!                          N           N            N
!
!                     These multiple interfaces are supplied purely for ease of
!                     use depending on what data is available.
!                     
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar OR Rank-1
!                                 See table above.
!                     ATTRIBUTES: INTENT(IN OUT)
!
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

  FUNCTION Allocate_Scalar( n_Layers,     &  ! Input
                            n_Modes,      &  ! Input
                            Aerosol,      &  ! Output
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: n_Layers
    INTEGER,                 INTENT(IN)     :: n_Modes
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Scalar)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Allocate_Status


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Modes < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Modes must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( CRTM_Associated_Aerosol( Aerosol, ANY_Test = SET ) ) THEN
      Error_Status = CRTM_Destroy_Aerosol( Aerosol, &
                                           No_Clear = SET, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_Aerosol pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! Perform the allocation
    ! ----------------------
    ALLOCATE( Aerosol%Effective_Radius( n_Layers, n_Modes ), &
              Aerosol%Effective_Variance( n_Layers, n_Modes ), &
              Aerosol%Concentration( n_Layers, n_Modes ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Aerosol data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------------
    ! Assign the dimensions and initalise arrays
    ! ------------------------------------------
    Aerosol%n_Layers  = n_Layers
    Aerosol%Max_Modes = n_Modes
    Aerosol%n_Modes   = n_Modes
    Aerosol%Effective_Radius   = ZERO
    Aerosol%Effective_Variance = ZERO
    Aerosol%Concentration      = ZERO


    ! -------------------------------------
    ! Increment and test allocation counter
    ! -------------------------------------
    Aerosol%n_Allocates = Aerosol%n_Allocates + 1
    IF ( Aerosol%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Aerosol%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank001( n_Layers,     &  ! Input
                             n_Modes,      &  ! Input
                             Aerosol,      &  ! Output
                             RCS_Id,       &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    INTEGER,                               INTENT(IN)     :: n_Layers
    INTEGER,                               INTENT(IN)     :: n_Modes
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN OUT) :: Aerosol
    CHARACTER(*),            OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Rank-001)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Perform the allocation
    DO i = 1, SIZE( Aerosol )
      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       n_Modes, &
                                       Aerosol(i), &
                                       Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO

  END FUNCTION Allocate_Rank001


  FUNCTION Allocate_Rank011( n_Layers,     &  ! Input
                             n_Modes,      &  ! Input
                             Aerosol,      &  ! Output
                             RCS_Id,       &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    INTEGER,                               INTENT(IN)     :: n_Layers
    INTEGER,                 DIMENSION(:), INTENT(IN)     :: n_Modes
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN OUT) :: Aerosol
    CHARACTER(*),            OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Rank-011)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Array arguments must conform
    n = SIZE( Aerosol )
    IF ( SIZE( n_Modes ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Modes and CRTM_Aerosol arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Perform the allocation
    DO i = 1, n
      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       n_Modes(i), &
                                       Aerosol(i), &
                                       Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO

  END FUNCTION Allocate_Rank011


  FUNCTION Allocate_Rank101( n_Layers,     &  ! Input
                             n_Modes,      &  ! Input
                             Aerosol,      &  ! Output
                             RCS_Id,       &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    INTEGER,                 DIMENSION(:), INTENT(IN)     :: n_Layers
    INTEGER,                               INTENT(IN)     :: n_Modes
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN OUT) :: Aerosol
    CHARACTER(*),            OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Rank-101)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Array arguments must conform
    n = SIZE( Aerosol )
    IF ( SIZE( n_Layers ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and CRTM_Aerosol arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Perform the allocation
    DO i = 1, n
      Scalar_Status = Allocate_Scalar( n_Layers(i), &
                                       n_Modes, &
                                       Aerosol(i), &
                                       Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO

  END FUNCTION Allocate_Rank101


  FUNCTION Allocate_Rank111( n_Layers,     &  ! Input
                             n_Modes,      &  ! Input
                             Aerosol,      &  ! Output
                             RCS_Id,       &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    INTEGER,                 DIMENSION(:), INTENT(IN)     :: n_Layers
    INTEGER,                 DIMENSION(:), INTENT(IN)     :: n_Modes
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN OUT) :: Aerosol
    CHARACTER(*),            OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Rank-111)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Array arguments must conform
    n = SIZE( Aerosol )
    IF ( SIZE( n_Layers ) /= n .OR. &
         SIZE( n_Modes  ) /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers, n_Modes and CRTM_Aerosol '//&
                            'arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Perform the allocation
    DO i = 1, n
      Scalar_Status = Allocate_Scalar( n_Layers(i), &
                                       n_Modes(i), &
                                       Aerosol(i), &
                                       Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO

  END FUNCTION Allocate_Rank111


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_Aerosol
!
! PURPOSE:
!       Function to copy valid CRTM_Aerosol structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Aerosol( Aerosol_in,               &  ! Input
!                                           Aerosol_out,              &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Aerosol_in:      CRTM_Aerosol structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Aerosol_type
!                        DIMENSION:  Scalar
!                                      OR
!                                    Rank1 array
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
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

  FUNCTION Assign_Scalar( Aerosol_in,   &  ! Input
                          Aerosol_out,  &  ! Output
                          RCS_Id,       &  ! Revision control
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN)     :: Aerosol_in
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol_out
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Aerosol(Scalar)'


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol_In ) ) THEN
      Error_Status = CRTM_Destroy_Aerosol( Aerosol_Out, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_Aerosol pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------
    Error_Status = CRTM_Allocate_Aerosol( Aerosol_in%n_Layers, &
                                          Aerosol_in%Max_Modes, &
                                          Aerosol_out, &
                                          Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CRTM_Aerosol arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------
    Aerosol_out%Type = Aerosol_in%Type


    ! -----------------
    ! Assign array data
    ! -----------------
    Aerosol_out%Effective_Radius   = Aerosol_in%Effective_Radius  
    Aerosol_out%Effective_Variance = Aerosol_in%Effective_Variance
    Aerosol_out%Concentration      = Aerosol_in%Concentration

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Aerosol_in,   &  ! Input
                         Aerosol_out,  &  ! Output
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN)     :: Aerosol_in
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN OUT) :: Aerosol_out
    CHARACTER(*),          OPTIONAL,       INTENT(OUT)    :: RCS_Id
    CHARACTER(*),          OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Aerosol(Rank-1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Array arguments must conform
    n = SIZE( Aerosol_in )
    IF ( SIZE( Aerosol_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Aerosol_in and Aerosol_out arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Perform the assignment
    DO i = 1, n
      Scalar_Status = Assign_Scalar( Aerosol_in(i), &
                                     Aerosol_out(i), &
                                     Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO

  END FUNCTION Assign_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_WeightedSum_Aerosol
!
! PURPOSE:
!       Function to perform a weighted sum of two valid CRTM_Aerosol
!       structures. The weighted summation performed is:
!         A = A + w1*B + w2
!       where A and B are the CRTM_Aerosol structures, and w1 and w2
!       are the weighting factors. Note that w2 is optional.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_WeightedSum_Aerosol( A,                        &  ! In/Output
!                                                B,                        &  ! Input
!                                                w1,                       &  ! Input
!                                                w2 = w2,                  &  ! Optional input
!                                                RCS_Id = RCS_Id,          &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
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
!       w1:            The first weighting factor used to multiply the
!                      contents of the input structure, B.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       w2:            The second weighting factor used to offset the
!                      weighted sum of the input structures.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
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
!       A:             Structure containing the weight sum result,
!                        A = A + w1*B + w2
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Same as B
!                      ATTRIBUTES: INTENT(IN OUT)
!
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
! SIDE EFFECTS:
!       The argument A is INTENT(IN OUT) and is modified upon output.
!
!--------------------------------------------------------------------------------

  FUNCTION WeightedSum_Scalar( A,              &  ! Input/Output
                               B,              &  ! Input
                               w1,             &  ! Input
                               w2,             &  ! optional input
                               RCS_Id,         &  ! Revision control
                               Message_Log )   &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: A
    TYPE(CRTM_Aerosol_type), INTENT(IN)     :: B
    REAL(fp),                INTENT(IN)     :: w1
    REAL(fp),     OPTIONAL,  INTENT(IN)     :: w2
    CHARACTER(*), OPTIONAL,  INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL,  INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Aerosol(Scalar)'
    ! Local variables
    REAL(fp) :: w2_Local


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. CRTM_Associated_Aerosol( A ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument A appears empty.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( .NOT. CRTM_Associated_Aerosol( B ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument B appears empty.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Array arguments must conform
    IF ( A%n_Layers /= B%n_Layers .OR. &
         A%n_Modes  /= B%n_Modes       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure dimensions are different.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Aerosol types must be the same
    IF ( A%Type /= B%Type ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure Aerosol types are different.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign the optional weight
    w2_Local = ZERO
    IF ( PRESENT( w2 ) ) w2_Local = w2


    ! ------------------------
    ! Perform the weighted sum
    ! ------------------------
    A%Effective_Radius   = A%Effective_Radius   + (w1*B%Effective_Radius)   + w2_Local
    A%Effective_Variance = A%Effective_Variance + (w1*B%Effective_Variance) + w2_Local
    A%Concentration      = A%Concentration      + (w1*B%Concentration)      + w2_Local

  END FUNCTION WeightedSum_Scalar


  FUNCTION WeightedSum_Rank1( A,              &  ! Input/Output
                              B,              &  ! Input
                              w1,             &  ! Input
                              w2,             &  ! optional input
                              RCS_Id,         &  ! Revision control
                              Message_Log )   &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN OUT) :: A
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN)     :: B
    REAL(fp),                              INTENT(IN)     :: w1
    REAL(fp),                OPTIONAL,     INTENT(IN)     :: w2
    CHARACTER(*),            OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Aerosol(Rank-1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Array arguments must conform
    n = SIZE( A )
    IF ( SIZE( B )  /= n  ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input structure arguments have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Perform the summation
    DO i = 1, n
      Scalar_Status = WeightedSum_Scalar( A(i), &
                                          B(i), &
                                          w1, &
                                          w2 = w2, &
                                          Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error computing weighted sum for element #", i5, &
                          &" of CRTM_Aerosol structure arrays." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO

  END FUNCTION WeightedSum_Rank1


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
!       Aerosol:      Zeroed out Aerosol structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       - The n_Modes component is set to the value of the Max_Modes
!         component.
!
!       - The aerosol type component is *NOT* reset.
!
!       - Note the INTENT on the output Aerosol argument is IN OUT rather than
!         just OUT. This is necessary because the argument must be defined upon
!         input.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Scalar( Aerosol )  ! Output
    TYPE(CRTM_Aerosol_type),  INTENT(IN OUT) :: Aerosol
    ! Reset the multi-dimensional scalar components
    Aerosol%n_Modes = Aerosol%Max_Modes
    ! Reset the array components
    Aerosol%Effective_Radius   = ZERO
    Aerosol%Effective_Variance = ZERO
    Aerosol%Concentration      = ZERO
  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( Aerosol )  ! Output
    TYPE(CRTM_Aerosol_type), DIMENSION(:), INTENT(IN OUT) :: Aerosol
    INTEGER :: n
    DO n = 1, SIZE( Aerosol )
      CALL Zero_Scalar( Aerosol(n) )
    END DO
  END SUBROUTINE Zero_Rank1

END MODULE CRTM_Aerosol_Define
