!
! CRTM_Cloud_Define
!
! Module defining the CRTM_Cloud structure and containing routines to 
! manipulate it.
!
! PUBLIC PARAMETERS:
!       1) The valid cloud type values used in the Cloud%Type field:
!
!           Cloud Type      Parameter Name
!         ----------------------------------
!             None          NO_CLOUD
!             Water         WATER_CLOUD
!             Ice           ICE_CLOUD
!             Rain          RAIN_CLOUD
!             Snow          SNOW_CLOUD
!             Graupel       GRAUPEL_CLOUD
!             Hail          HAIL_CLOUD
!
!       2) The number of valid cloud types is specified by the 
!            N_VALID_CLOUD_TYPES
!          parameter.
!
!       3) The character string array parameter
!            CLOUD_TYPE_NAME
!          uses the above cloud type definitions to provide a string value for
!          the type of cloud. For example,
!            CLOUD_TYPE_NAME( GRAUPEL_CLOUD )
!          contains the string
!            'Graupel'
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       20-Feb-2004
!

MODULE CRTM_Cloud_Define


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
  ! CRTM_Cloud Parameters
  PUBLIC :: N_VALID_CLOUD_TYPES
  PUBLIC ::      NO_CLOUD 
  PUBLIC ::   WATER_CLOUD 
  PUBLIC ::     ICE_CLOUD 
  PUBLIC ::    RAIN_CLOUD 
  PUBLIC ::    SNOW_CLOUD 
  PUBLIC :: GRAUPEL_CLOUD 
  PUBLIC ::    HAIL_CLOUD 
  PUBLIC :: CLOUD_TYPE_NAME
  ! CRTM_Cloud data structure definition
  PUBLIC :: CRTM_Cloud_type
  ! CRTM_Cloud structure routines
  PUBLIC :: CRTM_Associated_Cloud
  PUBLIC :: CRTM_Destroy_Cloud
  PUBLIC :: CRTM_Allocate_Cloud
  PUBLIC :: CRTM_Assign_Cloud
  PUBLIC :: CRTM_WeightedSum_Cloud
  PUBLIC :: CRTM_Zero_Cloud


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Destroy_Cloud
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE CRTM_Destroy_Cloud

  INTERFACE CRTM_Allocate_Cloud
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank01
    MODULE PROCEDURE Allocate_Rank11
  END INTERFACE CRTM_Allocate_Cloud

  INTERFACE CRTM_Assign_Cloud
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Cloud

  INTERFACE CRTM_WeightedSum_Cloud
    MODULE PROCEDURE WeightedSum_Scalar
    MODULE PROCEDURE WeightedSum_Rank1
  END INTERFACE CRTM_WeightedSum_Cloud

  INTERFACE CRTM_Zero_Cloud
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE CRTM_Zero_Cloud


  ! -----------------
  ! Module parameters
  ! -----------------
  ! The valid cloud types and names
  INTEGER, PARAMETER :: N_VALID_CLOUD_TYPES = 6
  INTEGER, PARAMETER ::      NO_CLOUD = 0
  INTEGER, PARAMETER ::   WATER_CLOUD = 1
  INTEGER, PARAMETER ::     ICE_CLOUD = 2
  INTEGER, PARAMETER ::    RAIN_CLOUD = 3
  INTEGER, PARAMETER ::    SNOW_CLOUD = 4
  INTEGER, PARAMETER :: GRAUPEL_CLOUD = 5
  INTEGER, PARAMETER ::    HAIL_CLOUD = 6
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_CLOUD_TYPES ) :: &
    CLOUD_TYPE_NAME = (/ 'None   ', &
                         'Water  ', &
                         'Ice    ', &
                         'Rain   ', &
                         'Snow   ', &
                         'Graupel', &
                         'Hail   ' /)

  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Cloud_Define.f90,v 1.17 2006/05/25 19:33:27 wd20pd Exp $'



  ! --------------------------
  ! Cloud data type definition
  ! --------------------------
  TYPE :: CRTM_Cloud_type
    INTEGER :: n_Allocates = 0
    ! Dimension values
    INTEGER :: n_Layers = 0  ! K dimension.
    ! Cloud type
    INTEGER :: Type = NO_CLOUD
    ! Particle size distribution parameters
    REAL(fp), DIMENSION(:), POINTER :: Effective_Radius   => NULL() ! K. Units are microns
    REAL(fp), DIMENSION(:), POINTER :: Effective_Variance => NULL() ! K. Units are Dimensionless
    ! Cloud state variables
    REAL(fp), DIMENSION(:), POINTER :: Water_Content => NULL()      ! K. Units are kg/m^2
  END TYPE CRTM_Cloud_type


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
!       CRTM_Clear_Cloud
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Cloud structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Cloud( Cloud ) ! Output
!
! OUTPUT ARGUMENTS:
!       Cloud:       Cloud structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_Cloud_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output Cloud argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Cloud( Cloud )
    TYPE(CRTM_Cloud_type), INTENT(IN OUT) :: Cloud
    Cloud%n_Layers = 0
    Cloud%Type     = NO_CLOUD
  END SUBROUTINE CRTM_Clear_Cloud





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
!       CRTM_Associated_Cloud
!
! PURPOSE:
!       Function to test the association status of a CRTM_Cloud structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Cloud( Cloud,              &  ! Input
!                                                   ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Cloud:               Cloud structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Cloud_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Cloud structure pointer members are associated.
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
!                            association status of the Cloud pointer members.
!                            .TRUE.  - if ALL the Cloud pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the Cloud pointer
!                                      members are associated.
!                            .FALSE. - some or all of the Cloud pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_Cloud( Cloud,     & ! Input
                                  ANY_Test ) & ! Optional input
                                RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type), INTENT(IN) :: Cloud
    INTEGER,     OPTIONAL, INTENT(IN) :: ANY_Test
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
      IF ( ASSOCIATED( Cloud%Effective_Radius   ) .AND. &
           ASSOCIATED( Cloud%Effective_Variance ) .AND. &
           ASSOCIATED( Cloud%Water_Content      )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( Cloud%Effective_Radius   ) .OR. &
           ASSOCIATED( Cloud%Effective_Variance ) .OR. &
           ASSOCIATED( Cloud%Water_Content      )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION CRTM_Associated_Cloud


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_Cloud
! 
! PURPOSE:
!       Function to re-initialize CRTM_Cloud structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Cloud( Cloud,                    &  ! Output
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
!       Cloud:        Re-initialized Cloud structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
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
!       Note the INTENT on the output Cloud argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Cloud,        &  ! Output
                           No_Clear,     &  ! Optional input
                           RCS_Id,       &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: Cloud
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Cloud(Scalar)'
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
    IF ( Clear ) CALL CRTM_Clear_Cloud( Cloud )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------
    IF ( .NOT. CRTM_Associated_Cloud( Cloud ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------
    ! Deallocate the Effective_Radius profile
    IF ( ASSOCIATED( Cloud%Effective_Radius ) ) THEN
      DEALLOCATE( Cloud%Effective_Radius, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Cloud Effective_Radius ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the Effective_Variance profile
    IF ( ASSOCIATED( Cloud%Effective_Variance ) ) THEN
      DEALLOCATE( Cloud%Effective_Variance, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Cloud Effective_Variance ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the water content profile
    IF ( ASSOCIATED( Cloud%Water_Content ) ) THEN
      DEALLOCATE( Cloud%Water_Content, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Cloud Water_Content ", &
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
    Cloud%n_Allocates = Cloud%n_Allocates - 1
    IF ( Cloud%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Cloud%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( Cloud,        &  ! Output
                          No_Clear,     &  ! Optional input
                          RCS_Id,       &  ! Revision control
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type), DIMENSION(:), INTENT(IN OUT) :: Cloud
    INTEGER,               OPTIONAL,     INTENT(IN)     :: No_Clear
    CHARACTER(*),          OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),          OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Cloud(Rank-1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise array
    DO n = 1, SIZE( Cloud )
      Scalar_Status = Destroy_Scalar( Cloud(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of Cloud structure array." )' ) n
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
!       CRTM_Allocate_Cloud
! 
! PURPOSE:
!       Function to allocate CRTM_Cloud structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Cloud( n_Layers,                 &  ! Input
!                                           Cloud,                    &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     Number of layers for which there is cloud data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Cloud dimensionality chart
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
!       Cloud:        Cloud structure with allocated pointer members. The
!                     following table shows the allowable dimension combinations
!                     for the calling routine, where N == number of clouds:
!
!                        Input       Output
!                       n_Layers      Cloud
!                       dimension   dimension
!                     ---------------------------
!                        scalar       scalar
!                        scalar         N
!                          N            N
!
!                     These multiple interfaces are supplied purely for ease of
!                     use depending on what data is available.
!                     
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar OR Rank-1
!                                 See table above.
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
!       Note the INTENT on the output Cloud argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Layers,     &  ! Input
                            Cloud,        &  ! Output
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Layers
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: Cloud
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Scalar)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Allocate_Status


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Layer dimension
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
    IF ( CRTM_Associated_Cloud( Cloud, ANY_Test = SET ) ) THEN
      Error_Status = CRTM_Destroy_Cloud( Cloud, &
                                         No_Clear = SET, &
                                         Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_Cloud pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF



    ! ----------------------
    ! Perform the allocation
    ! ----------------------
    ALLOCATE( Cloud%Effective_Radius( n_Layers ), &
              Cloud%Effective_Variance( n_Layers ), &
              Cloud%Water_Content( n_Layers ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTM_Cloud data arrays. STAT = ", i5 )' ) &
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
    Cloud%n_Layers = n_Layers
    Cloud%Effective_Radius   = ZERO
    Cloud%Effective_Variance = ZERO
    Cloud%Water_Content      = ZERO


    ! -------------------------------------
    ! Increment and test allocation counter
    ! -------------------------------------
    Cloud%n_Allocates = Cloud%n_Allocates + 1
    IF ( Cloud%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Cloud%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank01( n_Layers,     &  ! Input
                            Cloud,        &  ! Output
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                             INTENT(IN)     :: n_Layers
    TYPE(CRTM_Cloud_type), DIMENSION(:), INTENT(IN OUT) :: Cloud
    CHARACTER(*),          OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),          OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Rank-01)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Perform the allocation
    DO i = 1, SIZE( Cloud )
      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       Cloud(i), &
                                       Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Cloud structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO

  END FUNCTION Allocate_Rank01


  FUNCTION Allocate_Rank11( n_Layers,     &  ! Input
                            Cloud,        &  ! Output
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                 DIMENSION(:), INTENT(IN)     :: n_Layers
    TYPE(CRTM_Cloud_type), DIMENSION(:), INTENT(IN OUT) :: Cloud
    CHARACTER(*),          OPTIONAL,       INTENT(OUT)    :: RCS_Id
    CHARACTER(*),          OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Rank-11)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Array arguments must conform
    n = SIZE( n_Layers )
    IF ( SIZE( Cloud ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and CRTM_Cloud arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Perform the allocation
    DO i = 1, n
      Scalar_Status = Allocate_Scalar( n_Layers(i), &
                                       Cloud(i), &
                                       Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Cloud structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END DO

  END FUNCTION Allocate_Rank11


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_Cloud
!
! PURPOSE:
!       Function to copy valid CRTM_Cloud structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Cloud( Cloud_in,                 &  ! Input  
!                                         Cloud_out,                &  ! Output 
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Cloud_in:      Cloud structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Scalar
!                                    OR
!                                  Rank1 array
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
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Cloud_out:     Copy of the input structure, Cloud_in.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Same as Cloud_in argument
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
! COMMENTS:
!       Note the INTENT on the output Cloud argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( Cloud_in,     &  ! Input
                          Cloud_out,    &  ! Output
                          RCS_Id,       &  ! Revision control
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type),  INTENT(IN)     :: Cloud_in
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: Cloud_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Cloud(Scalar)'


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. CRTM_Associated_Cloud( Cloud_In ) ) THEN
      Error_Status = CRTM_Destroy_Cloud( Cloud_Out, &
                                         Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_Cloud pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------
    Error_Status = CRTM_Allocate_Cloud( Cloud_in%n_Layers, &
                                        Cloud_out, &
                                        Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CRTM_Cloud arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------
    Cloud_out%Type = Cloud_in%Type


    ! -----------------
    ! Assign array data
    ! -----------------
    Cloud_out%Effective_Radius   = Cloud_in%Effective_Radius  
    Cloud_out%Effective_Variance = Cloud_in%Effective_Variance
    Cloud_out%Water_Content      = Cloud_in%Water_Content

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Cloud_in,     &  ! Input
                         Cloud_out,    &  ! Output
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type), DIMENSION(:), INTENT(IN)     :: Cloud_in
    TYPE(CRTM_Cloud_type), DIMENSION(:), INTENT(IN OUT) :: Cloud_out
    CHARACTER(*),          OPTIONAL,       INTENT(OUT)    :: RCS_Id
    CHARACTER(*),          OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Cloud(Rank-1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Array arguments must conform
    n = SIZE( Cloud_in )
    IF ( SIZE( Cloud_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Cloud_in and Cloud_out arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Perform the assignment
    DO i = 1, n
      Scalar_Status = Assign_Scalar( Cloud_in(i), &
                                     Cloud_out(i), &
                                     Message_Log = Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of CRTM_Cloud structure array." )' ) i
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
!       CRTM_WeightedSum_Cloud
!
! PURPOSE:
!       Function to perform a weighted sum of two valid CRTM_Cloud
!       structures. The weighted summation performed is:
!         A = A + w1*B + w2
!       where A and B are the CRTM_Cloud structures, and w1 and w2
!       are the weighting factors. Note that w2 is optional.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_WeightedSum_Cloud( A,                        &  ! In/Output
!                                              B,                        &  ! Input
!                                              w1,                       &  ! Input
!                                              w2 = w2,                  &  ! Optional input
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       A:             Cloud structure that is to be added to.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Scalar OR Rank-1
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       B:             Cloud structure that is to be weighted and
!                      added to structure A.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
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
!                      TYPE:       CRTM_Cloud_type
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
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: A
    TYPE(CRTM_Cloud_type),  INTENT(IN)     :: B
    REAL(fp),               INTENT(IN)     :: w1
    REAL(fp),     OPTIONAL, INTENT(IN)     :: w2
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Cloud(Scalar)'
    ! Local variables
    REAL(fp) :: w2_Local


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. CRTM_Associated_Cloud( A ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument A appears empty.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( .NOT. CRTM_Associated_Cloud( B ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument B appears empty.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Array arguments must conform
    IF ( A%n_Layers /= B%n_Layers ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure dimensions are different.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF
         
    ! Cloud types must be the same
    IF ( A%Type /= B%Type ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure Cloud types are different.', &
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
    A%Water_Content      = A%Water_Content      + (w1*B%Water_Content)      + w2_Local

  END FUNCTION WeightedSum_Scalar


  FUNCTION WeightedSum_Rank1( A,              &  ! Input/Output
                              B,              &  ! Input
                              w1,             &  ! Input
                              w2,             &  ! optional input
                              RCS_Id,         &  ! Revision control
                              Message_Log )   &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type), DIMENSION(:), INTENT(IN OUT) :: A
    TYPE(CRTM_Cloud_type), DIMENSION(:), INTENT(IN)     :: B
    REAL(fp),                            INTENT(IN)     :: w1
    REAL(fp),              OPTIONAL,     INTENT(IN)     :: w2
    CHARACTER(*),          OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),          OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Cloud(Rank-1)'
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
                          &" of CRTM_Cloud structure arrays." )' ) i
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
!       CRTM_Zero_Cloud
! 
! PURPOSE:
!       Subroutine to zero-out all members of a CRTM_Cloud structure - both
!       scalar and pointer.
!
! CALLING SEQUENCE:
!       CALL CRTM_Zero_Cloud( Cloud )
!
! OUTPUT ARGUMENTS:
!       Cloud:        Zeroed out Cloud structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - No checking of the input structure is performed, so there are no
!         tests for pointer member association status. This means the Cloud
!         structure must have allocated pointer members upon entry to this
!         routine.
!
!       - The dimension components of the structure are *NOT*
!         set to zero.
!
!       - The cloud type component is *NOT* reset.
!
!       - Note the INTENT on the output Cloud argument is IN OUT rather than
!         just OUT. This is necessary because the argument must be defined upon
!         input.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Scalar( Cloud )  ! Output
    TYPE(CRTM_Cloud_type), INTENT(IN OUT) :: Cloud
    ! Reset the array components
    Cloud%Effective_Radius   = ZERO
    Cloud%Effective_Variance = ZERO
    Cloud%Water_Content      = ZERO
  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( Cloud )  ! Output
    TYPE(CRTM_Cloud_type), DIMENSION(:), INTENT(IN OUT) :: Cloud
    INTEGER :: n
    DO n = 1, SIZE( Cloud )
      CALL Zero_Scalar( Cloud(n) )
    END DO
  END SUBROUTINE Zero_Rank1

END MODULE CRTM_Cloud_Define
