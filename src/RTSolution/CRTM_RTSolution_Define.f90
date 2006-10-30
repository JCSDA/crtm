!
! CRTM_RTSolution_Define
!
! Module defining the CRTM RTSolution structure and containing routines
! to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-June-2004
!

MODULE CRTM_RTSolution_Define


  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds,      ONLY: fp=>fp_kind
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, SET
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  ! Structure data type
  PUBLIC :: CRTM_RTSolution_type
  ! Public procedures
  PUBLIC :: CRTM_Associated_RTSolution
  PUBLIC :: CRTM_Destroy_RTSolution
  PUBLIC :: CRTM_Allocate_RTSolution
  PUBLIC :: CRTM_Assign_RTSolution


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Destroy_RTSolution
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
    MODULE PROCEDURE Destroy_Rank2
  END INTERFACE CRTM_Destroy_RTSolution

  INTERFACE CRTM_Allocate_RTSolution
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank01
    MODULE PROCEDURE Allocate_Rank02
    MODULE PROCEDURE Allocate_Rank11
    MODULE PROCEDURE Allocate_Rank12
  END INTERFACE CRTM_Allocate_RTSolution

  INTERFACE CRTM_Assign_RTSolution
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
    MODULE PROCEDURE Assign_Rank2
  END INTERFACE CRTM_Assign_RTSolution


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_RTSolution_Define.f90,v 1.17 2006/06/13 16:51:36 wd20pd Exp $'
  REAL(fp), PARAMETER :: FP_DEFAULT = ZERO
  INTEGER,  PARAMETER :: IP_DEFAULT = 0
  LOGICAL,  PARAMETER :: LP_DEFAULT = .TRUE.


  ! -------------------------------
  ! RTSolution data type definition
  ! -------------------------------
  TYPE :: CRTM_RTSolution_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Layers = 0  ! K
    ! Forward radiative transfer intermediate results for a single channel
    !    These components are not defined when they are used as TL, AD
    !    and K variables
    REAL(fp) :: Surface_Emissivity      = FP_DEFAULT
    REAL(fp) :: Up_Radiance             = FP_DEFAULT
    REAL(fp) :: Down_Radiance           = FP_DEFAULT
    REAL(fp) :: Down_Solar_Radiance     = FP_DEFAULT
    REAL(fp) :: Surface_Planck_Radiance = FP_DEFAULT
    REAL(fp), DIMENSION(:), POINTER :: Layer_Optical_Depth => NULL()  ! K
    ! Internal variables. Users do not need to worry about these.
    INTEGER :: n_Full_Streams  = IP_DEFAULT
    LOGICAL :: Scattering_Flag = LP_DEFAULT
    INTEGER :: n_Stokes        = IP_DEFAULT
    ! Radiative transfer results for a single channel/node
    REAL(fp) :: Radiance               = FP_DEFAULT
    REAL(fp) :: Brightness_Temperature = FP_DEFAULT
  END TYPE CRTM_RTSolution_type


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
!       CRTM_Clear_RTSolution
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_RTSolution structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_RTSolution( RTSolution ) ! Output
!
! OUTPUT ARGUMENTS:
!       RTSolution:  CRTM_RTSolution structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_RTSolution_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_RTSolution( RTSolution )
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: RTSolution
    RTSolution%n_Layers = 0
    RTSolution%Surface_Emissivity      = FP_DEFAULT
    RTSolution%Up_Radiance             = FP_DEFAULT
    RTSolution%Down_Radiance           = FP_DEFAULT
    RTSolution%Down_Solar_Radiance     = FP_DEFAULT
    RTSolution%Surface_Planck_Radiance = FP_DEFAULT
    RTSolution%n_Full_Streams     = IP_DEFAULT
    RTSolution%Scattering_Flag    = LP_DEFAULT
    RTSolution%n_Stokes           = IP_DEFAULT
    RTSolution%Radiance               = FP_DEFAULT
    RTSolution%Brightness_Temperature = FP_DEFAULT
  END SUBROUTINE CRTM_Clear_RTSolution





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Associated_RTSolution
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_RTSolution structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_RTSolution( RTSolution,         &  ! Input
!                                                        ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       RTSolution:          RTSolution structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_RTSolution_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CRTM_SfcOptics structure pointer members are associated.
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
!                            association status of the RTSolution pointer
!                            members.
!                            .TRUE.  - if ALL the RTSolution pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the RTSolution
!                                      pointer members are associated.
!                            .FALSE. - some or all of the RTSolution pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_RTSolution( RTSolution, & ! Input
                                       ANY_Test ) & ! Optional input
                                     RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: RTSolution
    INTEGER,         OPTIONAL, INTENT(IN) :: ANY_Test
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


    ! --------------------------------------
    ! Test the structure pointer association
    ! NOTE: The Any/All test logic is included for future changes
    !       when/if there is more than one pointer member
    ! --------------------------------------
    Association_Status = .FALSE.
!    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( RTSolution%Layer_Optical_Depth ) ) THEN  ! .AND.
        Association_Status = .TRUE.
      END IF
!    ELSE
!      IF ( ASSOCIATED( RTSolution%Layer_Optical_Depth ) ) THEN  ! .OR.
!        Association_Status = .TRUE.
!      END IF
!    END IF

  END FUNCTION CRTM_Associated_RTSolution


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_RTSolution
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a CRTM
!       RTSolution data structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_RTSolution( RTSolution,               &  ! Output
!                                               RCS_Id = RCS_Id,          &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
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
! OUTPUT ARGUMENTS:
!       RTSolution:  Re-initialized RTSolution structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar, Rank-1, or Rank-2
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
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( RTSolution,   &  ! Output
                           No_Clear,     &  ! Optional input
                           RCS_Id,       &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT)  :: RTSolution
    INTEGER,          OPTIONAL, INTENT(IN)      :: No_Clear
    CHARACTER(*),     OPTIONAL, INTENT(OUT)     :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)      :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_RTSolution(Scalar)'
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
    IF ( Clear ) CALL CRTM_Clear_RTSolution( RTSolution )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------
    IF ( .NOT. CRTM_Associated_RTSolution( RTSolution ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! Deallocate the RTSolution Layer_Optical_Depth member
    IF ( ASSOCIATED( RTSolution%Layer_Optical_Depth ) ) THEN
      DEALLOCATE( RTSolution%Layer_Optical_Depth, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_RTSolution Layer_Optical_Depth ", &
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
    RTSolution%n_Allocates = RTSolution%n_Allocates - 1
    IF ( RTSolution%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      RTSolution%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF
  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( RTSolution,   &  ! Output
                          No_Clear,     &  ! Optional input
                          RCS_Id,       &  ! Revision control
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_RTSolution_type), DIMENSION(:), INTENT(IN OUT) :: RTSolution
    INTEGER,                    OPTIONAL,     INTENT(IN)     :: No_Clear
    CHARACTER(*),               OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),               OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_RTSolution(Rank-1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! ----------------------------
    ! Loop over RTSolution entries
    ! ----------------------------
    DO n = 1, SIZE( RTSolution )
      Scalar_Status = Destroy_Scalar( RTSolution(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of rank-1 CRTM_RTSolution structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO
  END FUNCTION Destroy_Rank1


  FUNCTION Destroy_Rank2( RTSolution,   &  ! Output
                          No_Clear,     &  ! Optional input
                          RCS_Id,       &  ! Revision control
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_RTSolution_type), DIMENSION(:,:), INTENT(IN OUT) :: RTSolution
    INTEGER,                    OPTIONAL,       INTENT(IN)     :: No_Clear
    CHARACTER(*),               OPTIONAL,       INTENT(OUT)    :: RCS_Id
    CHARACTER(*),               OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_RTSolution(Rank-2)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, j


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! ----------------------------
    ! Loop over RTSolution entries
    ! ----------------------------
    DO j = 1, SIZE( RTSolution, DIM = 2 )
      DO i = 1, SIZE( RTSolution, DIM = 1 )
        Scalar_Status = Destroy_Scalar( RTSolution(i,j), &
                                        No_Clear = No_Clear, &
                                        Message_Log = Message_Log )
        IF ( Scalar_Status /= SUCCESS ) THEN
          Error_Status = Scalar_Status
          WRITE( Message, '( "Error destroying element #", i5, ",", i5, &
                            &" of rank-2 CRTM_RTSolution structure array." )' ) i,j
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
        END IF
      END DO
    END DO
  END FUNCTION Destroy_Rank2


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_RTSolution
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_RTSolution
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_RTSolution( n_Layers,                 &  ! Input
!                                                RTSolution,               &  ! Output
!                                                RCS_Id = RCS_Id,          &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     Number of atmospheric layers 
!                     Must be > 0
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output RTSolution dimensionality chart
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
!       RTSolution:   CRTM_RTSolution structure with allocated pointer members.
!                     Upon allocation, all pointer members are initialized to
!                     a value of zero.
!
!                     The following chart shows the allowable dimension
!                     combinations for the calling routine, where
!                       L == number of channels
!                       M == number of profiles
!
!                        Input                       Output
!                       n_Layers                    RTSolution
!                       dimension                   dimension
!                     ----------------------------------------------------------
!                        scalar      scalar, Rank-1 (L or M), or Rank-2 (L x M)
!                          L                 Rank-1 (L),      or Rank-2 (L x M)
!
!                     These multiple interfaces are supplied purely for ease of
!                     use depending on how it's used.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar, Rank-1, or Rank-2
!                     ATTRIBUTES: INTENT(IN OUT)
!
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
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Layers,     &  ! Input
                            RTSolution,   &  ! Output
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                      INTENT(IN)     :: n_Layers
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: RTSolution
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution(Scalar)'
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

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( CRTM_Associated_RTSolution( RTSolution ) ) THEN
      Error_Status = CRTM_Destroy_RTSolution( RTSolution, &
                                              No_Clear = SET, &
                                              Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_RTSolution pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------
    ALLOCATE( RTSolution%Layer_Optical_Depth( n_Layers ),    &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating RTSolution data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------------
    ! Assign dimensions and initialise variables
    ! ------------------------------------------
    RTSolution%n_Layers = n_Layers
    RTSolution%Layer_Optical_Depth = ZERO


    ! -----------------------------------------
    ! Increment and test the allocation counter
    ! -----------------------------------------
    RTSolution%n_Allocates = RTSolution%n_Allocates + 1
    IF ( RTSolution%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      RTSolution%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF
  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank01( n_Layers,     &  ! Input, scalar
                            RTSolution,   &  ! Output, L or M   
                            RCS_Id,       &  ! Revision control 
                            Message_Log ) &  ! Error messaging  
                          RESULT( Error_Status )                
    ! Arguments
    INTEGER,                                  INTENT(IN)     :: n_Layers
    TYPE(CRTM_RTSolution_type), DIMENSION(:), INTENT(IN OUT) :: RTSolution
    CHARACTER(*),               OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),               OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution(Rank-0,1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! ----------------------------
    ! Loop over RTSolution entries
    ! ----------------------------
    DO n = 1, SIZE( RTSolution )
      Scalar_Status = Allocate_Scalar( n_Layers,      & ! Input
                                       RTSolution(n), & ! Output
                                       Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of rank-1 CRTM_RTSolution structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO
  END FUNCTION Allocate_Rank01


  FUNCTION Allocate_Rank02( n_Layers,     &  ! Input, scalar
                            RTSolution,   &  ! Output, L x M    
                            RCS_Id,       &  ! Revision control 
                            Message_Log ) &  ! Error messaging  
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                                    INTENT(IN)     :: n_Layers
    TYPE(CRTM_RTSolution_type), DIMENSION(:,:), INTENT(IN OUT) :: RTSolution
    CHARACTER(*),               OPTIONAL,       INTENT(OUT)    :: RCS_Id
    CHARACTER(*),               OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution(Rank-0,2)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, j


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! ----------------------------
    ! Loop over RTSolution entries
    ! ----------------------------
    DO j = 1, SIZE( RTSolution, DIM = 2 )
      DO i = 1, SIZE( RTSolution, DIM = 1 )
        Scalar_Status = Allocate_Scalar( n_Layers,        & ! Input
                                         RTSolution(i,j), & ! Output
                                         Message_Log = Message_Log )
        IF ( Scalar_Status /= SUCCESS ) THEN
          Error_Status = Scalar_Status
          WRITE( Message, '( "Error allocating element #", i5, ",", i5, &
                            &" of rank-2 CRTM_RTSolution structure array." )' ) i, j
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
        END IF
      END DO
    END DO
  END FUNCTION Allocate_Rank02


  FUNCTION Allocate_Rank11( n_Layers,     &  ! Input, L
                            RTSolution,   &  ! Output, L
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                    DIMENSION(:), INTENT(IN)     :: n_Layers
    TYPE(CRTM_RTSolution_type), DIMENSION(:), INTENT(IN OUT) :: RTSolution
    CHARACTER(*),               OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),               OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution(Rank-1,1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions of n_Layers and
    ! RTSolution must be the same
    n = SIZE( n_Layers )
    IF ( SIZE( RTSolution ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and RTSolution arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------
    ! Loop over RTSolution entries
    ! ----------------------------
    DO i = 1, n
      Scalar_Status = Allocate_Scalar( n_Layers(i),   & ! Input
                                       RTSolution(i), & ! Output
                                       Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of rank-1 CRTM_RTSolution structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO
  END FUNCTION Allocate_Rank11


  FUNCTION Allocate_Rank12( n_Layers,     &  ! Input, L
                            RTSolution,   &  ! Output, L x M
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                      DIMENSION(:),   INTENT(IN)     :: n_Layers
    TYPE(CRTM_RTSolution_type), DIMENSION(:,:), INTENT(IN OUT) :: RTSolution
    CHARACTER(*),               OPTIONAL,       INTENT(OUT)    :: RCS_Id
    CHARACTER(*),               OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution(Rank-1,2)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, j, n


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions of n_layers and first dimension
    ! of RTSolution must be the same
    n = SIZE( n_Layers )
    IF ( SIZE( RTSolution, DIM = 1 ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers array and the first dimension of'//&
                            ' the RTSolution array have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------
    ! Loop over RTSolution entries
    ! ----------------------------
    DO j = 1, SIZE( RTSolution, DIM = 2 )
      DO i = 1, n
        Scalar_Status = Allocate_Scalar( n_Layers(i),     & ! Input
                                         RTSolution(i,j), & ! Output
                                         Message_Log = Message_Log )
        IF ( Scalar_Status /= SUCCESS ) THEN
          Error_Status = Scalar_Status
          WRITE( Message, '( "Error allocating element #", i5, ",", i5, &
                            &" of rank-2 CRTM_RTSolution structure array." )' ) i, j
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
        END IF
      END DO
    END DO
  END FUNCTION Allocate_Rank12


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_RTSolution
!
! PURPOSE:
!       Function to copy valid CRTM_RTSolution structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_RTSolution( RTSolution_in,            &  ! Input
!                                              RTSolution_out,           &  ! Output
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       RTSolution_in:   CRTM_RTSolution structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_RTSolution_type
!                        DIMENSION:  Scalar, Rank-1, or Rank-2
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
!       RTSolution_out:  Copy of the input structure, CRTM_RTSolution_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_RTSolution_type
!                        DIMENSION:  Same as input RTSolution_in
!                        ATTRIBUTES: INTENT(IN OUT)
!
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
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( RTSolution_in,  &  ! Input
                          RTSolution_out, &  ! Output
                          RCS_Id,         &  ! Revision control
                          Message_Log )   &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_RTSolution_type), INTENT(IN)     :: RTSolution_in
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: RTSolution_out
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_RTSolution(Scalar)'


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! ----------------------------------------------
    ! ALL *input* pointers must be associated.
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    ! ----------------------------------------------
    IF ( .NOT. CRTM_Associated_RTSolution( RTSolution_In ) ) THEN
      Error_Status = CRTM_Destroy_RTSolution( RTSolution_Out, &
                                              Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_RTSolution pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------
    Error_Status = CRTM_Allocate_RTSolution( RTSolution_in%n_Layers, &
                                             RTSolution_out, &
                                             Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output RTSolution arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------
    ! Assign scalar data
    ! ------------------
    RTSolution_out%Surface_Emissivity      = RTSolution_in%Surface_Emissivity
    RTSolution_out%Up_Radiance             = RTSolution_in%Up_Radiance
    RTSolution_out%Down_Radiance           = RTSolution_in%Down_Radiance
    RTSolution_out%Down_Solar_Radiance     = RTSolution_in%Down_Solar_Radiance
    RTSolution_out%Surface_Planck_Radiance = RTSolution_in%Surface_Planck_Radiance
    RTSolution_out%n_Full_Streams          = RTSolution_in%n_Full_Streams
    RTSolution_out%Scattering_Flag         = RTSolution_in%Scattering_Flag
    RTSolution_out%n_Stokes                = RTSolution_in%n_Stokes
    RTSolution_out%Radiance                = RTSolution_in%Radiance
    RTSolution_out%Brightness_Temperature  = RTSolution_in%Brightness_Temperature


    ! -----------------
    ! Assign array data
    ! -----------------
    RTSolution_out%Layer_Optical_Depth = RTSolution_in%Layer_Optical_Depth             
  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( RTSolution_in,  &  ! Input
                         RTSolution_out, &  ! Output
                         RCS_Id,         &  ! Revision control
                         Message_Log )   &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_RTSolution_type), DIMENSION(:), INTENT(IN)     :: RTSolution_in
    TYPE(CRTM_RTSolution_type), DIMENSION(:), INTENT(IN OUT) :: RTSolution_out
    CHARACTER(*),               OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),               OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_RTSolution(Rank-1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    n = SIZE( RTSolution_in )
    IF ( SIZE( RTSolution_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input RTSolution_in and RTSolution_out arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Perform the asignment
    ! ---------------------
    DO i = 1, n
      Scalar_Status = Assign_Scalar( RTSolution_in(i), &
                                     RTSolution_out(i), &
                                     Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of rank-1 CRTM_RTSolution structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO
  END FUNCTION Assign_Rank1


  FUNCTION Assign_Rank2( RTSolution_in,  &  ! Input
                         RTSolution_out, &  ! Output
                         RCS_Id,         &  ! Revision control
                         Message_Log )   &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_RTSolution_type), DIMENSION(:,:), INTENT(IN)     :: RTSolution_in
    TYPE(CRTM_RTSolution_type), DIMENSION(:,:), INTENT(IN OUT) :: RTSolution_out
    CHARACTER(*),               OPTIONAL,       INTENT(OUT)    :: RCS_Id
    CHARACTER(*),               OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_RTSolution(Rank-2)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, j, l, m


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    l = SIZE( RTSolution_in, DIM = 1 )
    m = SIZE( RTSolution_in, DIM = 2 )
    IF ( SIZE( RTSolution_out, DIM = 1 ) /= l .AND. &
         SIZE( RTSolution_out, DIM = 2 ) /= m       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input RTSolution_in and RTSolution_out arrays'//&
                            ' have different dimension sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Perform the asignment
    ! ---------------------
    DO j = 1, m
      DO i = 1, l
        Scalar_Status = Assign_Scalar( RTSolution_in(i,j), &
                                       RTSolution_out(i,j), &
                                       Message_Log = Message_Log )
        IF ( Scalar_Status /= SUCCESS ) THEN
          Error_Status = Scalar_Status
          WRITE( Message, '( "Error copying element #", i5, ",", i5, &
                            &" of rank-2 CRTM_RTSolution structure array." )' ) i, j
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
        END IF
      END DO
    END DO
  END FUNCTION Assign_Rank2

END MODULE CRTM_RTSolution_Define
