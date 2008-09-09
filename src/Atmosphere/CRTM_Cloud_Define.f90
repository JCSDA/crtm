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
  PUBLIC :: CRTM_Equal_Cloud
  PUBLIC :: CRTM_SetLayers_Cloud
  PUBLIC :: CRTM_Sum_Cloud
  PUBLIC :: CRTM_Zero_Cloud
  PUBLIC :: CRTM_RCS_ID_Cloud


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Associated_Cloud
    MODULE PROCEDURE Associated_Scalar
    MODULE PROCEDURE Associated_Rank1
  END INTERFACE CRTM_Associated_Cloud

  INTERFACE CRTM_Destroy_Cloud
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE CRTM_Destroy_Cloud

  INTERFACE CRTM_Allocate_Cloud
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE CRTM_Allocate_Cloud

  INTERFACE CRTM_Assign_Cloud
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Cloud

  INTERFACE CRTM_Equal_Cloud
    MODULE PROCEDURE Equal_Scalar
    MODULE PROCEDURE Equal_Rank1
  END INTERFACE CRTM_Equal_Cloud

  INTERFACE CRTM_SetLayers_Cloud
    MODULE PROCEDURE SetLayers_Scalar
    MODULE PROCEDURE SetLayers_Rank1
  END INTERFACE CRTM_SetLayers_Cloud

  INTERFACE CRTM_Sum_Cloud
    MODULE PROCEDURE Sum_Scalar
    MODULE PROCEDURE Sum_Rank1
  END INTERFACE CRTM_Sum_Cloud

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
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! --------------------------
  ! Cloud data type definition
  ! --------------------------
  TYPE :: CRTM_Cloud_type
    INTEGER :: n_Allocates = 0
    ! Dimension values
    INTEGER :: Max_Layers = 0  ! K dimension.
    INTEGER :: n_Layers   = 0  ! Kuse dimension.
    ! Number of added layers
    INTEGER :: n_Added_Layers = 0
    ! Cloud type
    INTEGER :: Type = NO_CLOUD
    ! Cloud state variables
    REAL(fp), POINTER :: Effective_Radius(:)   => NULL() ! K. Units are microns
    REAL(fp), POINTER :: Effective_Variance(:) => NULL() ! K. Units are microns^2
    REAL(fp), POINTER :: Water_Content(:) => NULL()      ! K. Units are kg/m^2
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
    Cloud%Type = NO_CLOUD
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
!       Association_Status = CRTM_Associated_Cloud( Cloud            , &  ! Input
!                                                   ANY_Test=Any_Test  )  ! Optional input
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

  FUNCTION Associated_Scalar( Cloud,     & ! Input
                              ANY_Test ) & ! Optional input
                            RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type), INTENT(IN) :: Cloud
    INTEGER,     OPTIONAL, INTENT(IN) :: ANY_Test
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
      IF ( ASSOCIATED(Cloud%Effective_Radius  ) .AND. &
           ASSOCIATED(Cloud%Effective_Variance) .AND. &
           ASSOCIATED(Cloud%Water_Content     )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( Cloud%Effective_Radius  ) .OR. &
           ASSOCIATED( Cloud%Effective_Variance) .OR. &
           ASSOCIATED( Cloud%Water_Content     )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_Scalar

  FUNCTION Associated_Rank1( Cloud,     & ! Input
                             ANY_Test ) & ! Optional input
                           RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type), INTENT(IN) :: Cloud(:)
    INTEGER,     OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status(SIZE(Cloud))
    ! Local variables
    INTEGER :: n

    DO n = 1, SIZE(Cloud)
      Association_Status(n) = Associated_Scalar(Cloud(n), ANY_Test=ANY_Test)
    END DO

  END FUNCTION Associated_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_Cloud
! 
! PURPOSE:
!       Function to re-initialize CRTM_Cloud structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Cloud( Cloud                  , &  ! Output
!                                          Message_Log=Message_Log  )  ! Error messaging
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

  FUNCTION Destroy_Scalar( Cloud      , &  ! Output
                           No_Clear   , &  ! Optional input
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: Cloud
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Cloud(Scalar)'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Reinitialise the dimensions
    Cloud%Max_Layers = 0
    Cloud%n_Layers   = 0
    Cloud%n_Added_Layers = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT(No_Clear) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL CRTM_Clear_Cloud( Cloud )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. CRTM_Associated_Cloud( Cloud ) ) RETURN


    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( Cloud%Effective_Radius, &
                Cloud%Effective_Variance, &
                Cloud%Water_Content, &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating CRTM_Cloud pointer components.", &
                      &" STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    Cloud%n_Allocates = Cloud%n_Allocates - 1
    IF ( Cloud%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i0 )' ) &
                      Cloud%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( Cloud      , &  ! Output
                          No_Clear   , &  ! Optional input
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: Cloud(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Cloud(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Reinitialise array
    ! ------------------
    DO n = 1, SIZE(Cloud)
      Scalar_Status = Destroy_Scalar( Cloud(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i0, &
                          &" of Cloud structure array." )' ) n
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
!       CRTM_Allocate_Cloud
! 
! PURPOSE:
!       Function to allocate CRTM_Cloud structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Cloud( n_Layers               , &  ! Input
!                                           Cloud                  , &  ! Output
!                                           Message_Log=Message_Log  )  ! Error messaging
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
! OUTPUT ARGUMENTS:
!       Cloud:        Cloud structure with allocated pointer members.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Same as input n_Layers argument
!                     ATTRIBUTES: INTENT(IN OUT)
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

  FUNCTION Allocate_Scalar( n_Layers   , &  ! Input
                            Cloud      , &  ! Output
                            Message_Log) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Layers
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: Cloud
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Scalar)'
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
    IF ( CRTM_Associated_Cloud( Cloud, ANY_Test=SET ) ) THEN
      Error_Status = CRTM_Destroy_Cloud( Cloud, &
                                         No_Clear=SET, &
                                         Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_Cloud pointer members.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Perform the allocation
    ! ----------------------
    ALLOCATE( Cloud%Effective_Radius( n_Layers ), &
              Cloud%Effective_Variance( n_Layers ), &
              Cloud%Water_Content( n_Layers ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTM_Cloud data arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the dimensions and initalise arrays
    ! ------------------------------------------
    Cloud%Max_Layers = n_Layers
    Cloud%n_Layers   = n_Layers
    Cloud%n_Added_Layers = 0
    
    Cloud%Effective_Radius   = ZERO
    Cloud%Effective_Variance = ZERO
    Cloud%Water_Content      = ZERO


    ! Increment and test allocation counter
    ! -------------------------------------
    Cloud%n_Allocates = Cloud%n_Allocates + 1
    IF ( Cloud%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i0 )' ) &
                      Cloud%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank1( n_Layers   , &  ! Input
                           Cloud      , &  ! Output
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Layers(:)
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: Cloud(:)
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Rank-11)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Array arguments must conform
    n = SIZE(Cloud)
    IF ( SIZE(n_Layers) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and CRTM_Cloud arrays have different dimensions', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Perform the allocation
    ! ----------------------
    DO i = 1, n
      Scalar_Status = Allocate_Scalar( n_Layers(i), &
                                       Cloud(i), &
                                       Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i0, &
                          &" of CRTM_Cloud structure array." )' ) i
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
!       CRTM_Assign_Cloud
!
! PURPOSE:
!       Function to copy valid CRTM_Cloud structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Cloud( Cloud_in               , &  ! Input  
!                                         Cloud_out              , &  ! Output 
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Cloud_in:      Cloud structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Scalar or Rank-1 array
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Cloud_out:     Copy of the input structure, Cloud_in.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Same as Cloud_in argument
!                      ATTRIBUTES: INTENT(IN OUT)
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

  FUNCTION Assign_Scalar( Cloud_in      , &  ! Input
                          Cloud_out     , &  ! Output
                          n_Added_Layers, &  ! Optional input
                          Message_Log   ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type),  INTENT(IN)     :: Cloud_in
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: Cloud_out
    INTEGER,      OPTIONAL, INTENT(IN)     :: n_Added_Layers
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Cloud(Scalar)'
    ! Local variables
    INTEGER :: na, no, nt

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! ALL *input* pointers must be associated.
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. CRTM_Associated_Cloud( Cloud_In ) ) THEN
      Error_Status = CRTM_Destroy_Cloud( Cloud_Out, &
                                         Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_Cloud pointer members.', &
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
    IF ( CRTM_Associated_Cloud( Cloud_out ) .AND. &
         Cloud_Out%Max_Layers >= Cloud_in%n_Layers+na ) THEN
         
      ! If structure already allocated to sufficient size,
      ! then zero and set layer dimension
      CALL CRTM_Zero_Cloud( Cloud_Out )
      Cloud_Out%n_Layers = Cloud_in%n_Layers+na
      
    ELSE
    
      ! Explicitly allocate structure
      Error_Status = CRTM_Allocate_Cloud( Cloud_in%n_Layers+na, &
                                          Cloud_out, &
                                          Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error allocating output CRTM_Cloud arrays.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Assign data
    ! -----------
    Cloud_out%n_Added_Layers = Cloud_in%n_Added_Layers+na
    Cloud_out%Type = Cloud_in%Type
    
    no = Cloud_In%n_Layers
    nt = Cloud_Out%n_Layers
    Cloud_out%Effective_Radius(na+1:nt)   = Cloud_in%Effective_Radius(1:no)  
    Cloud_out%Effective_Variance(na+1:nt) = Cloud_in%Effective_Variance(1:no)
    Cloud_out%Water_Content(na+1:nt)      = Cloud_in%Water_Content(1:no)

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Cloud_in      , &  ! Input
                         Cloud_out     , &  ! Output
                         n_Added_Layers, &  ! Optional input
                         Message_Log   ) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type),  INTENT(IN)     :: Cloud_in(:)
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: Cloud_out(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: n_Added_Layers
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Cloud(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Output array must be large enough to handle input copy.
    n = SIZE(Cloud_in)
    IF ( SIZE(Cloud_out) < n ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Cloud_out array not large enough (",i0,&
                      &") to hold Cloud_in data (",i0,").")' ) &
                      SIZE(Cloud_out), n
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Perform the assignment
    ! ----------------------
    DO i = 1, n
      Scalar_Status = Assign_Scalar( Cloud_in(i), &
                                     Cloud_out(i), &
                                     n_Added_Layers=n_Added_Layers, &
                                     Message_Log   =Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i0, &
                          &" of CRTM_Cloud structure array." )' ) i
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
!       CRTM_Equal_Cloud
!
! PURPOSE:
!       Function to test if two Cloud structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Equal_Cloud( Cloud_LHS              , &  ! Input
!                                        Cloud_RHS              , &  ! Input
!                                        ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                        Check_All  =Check_All  , &  ! Optional input
!                                        Message_Log=Message_Log  )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!       Cloud_LHS:         Cloud structure to be compared; equivalent to the
!                          left-hand side of a lexical comparison, e.g.
!                            IF ( Cloud_LHS == Cloud_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CRTM_Cloud_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Cloud_RHS:         Cloud structure to be compared to; equivalent to
!                          right-hand side of a lexical comparison, e.g.
!                            IF ( Cloud_LHS == Cloud_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CRTM_Cloud_type
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
!                          channel data of the Cloud structures. The default
!                          action is return with a FAILURE status as soon as
!                          any difference is found. This optional argument can
!                          be used to get a listing of ALL the differences
!                          between data in Cloud structures.
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

  FUNCTION Equal_Scalar( Cloud_LHS  , &  ! Input
                         Cloud_RHS  , &  ! Input
                         ULP_Scale  , &  ! Optional input
                         Check_All  , &  ! Optional input
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: Cloud_LHS
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: Cloud_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,      OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_Cloud(scalar)'
    CHARACTER(*), PARAMETER :: FFMT = 'es22.15'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: k

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
    IF ( .NOT. CRTM_Associated_Cloud( Cloud_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Cloud_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. CRTM_Associated_Cloud( Cloud_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Cloud_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( Cloud_LHS%n_Layers /= Cloud_RHS%n_Layers ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Compare the values
    ! ------------------
    IF ( Cloud_LHS%Type /= Cloud_RHS%Type ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Type values are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    DO k = 1, Cloud_LHS%n_Layers
      IF ( .NOT. Compare_Float( Cloud_LHS%Effective_Radius(k), &
                                Cloud_RHS%Effective_Radius(k), &
                                ULP = ULP ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Effective_Radius values are different at level ",i0,&
                        &":",3(1x,'//FFMT//'))') &
                       k, Cloud_LHS%Effective_Radius(k), &
                          Cloud_RHS%Effective_Radius(k), &
                          Cloud_LHS%Effective_Radius(k)-Cloud_RHS%Effective_Radius(k)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO k = 1, Cloud_LHS%n_Layers
      IF ( .NOT. Compare_Float( Cloud_LHS%Effective_Variance(k), &
                                Cloud_RHS%Effective_Variance(k), &
                                ULP = ULP ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Effective_Variance values are different at level ",i0,&
                        &":",3(1x,'//FFMT//'))') &
                       k, Cloud_LHS%Effective_Variance(k), &
                          Cloud_RHS%Effective_Variance(k), &
                          Cloud_LHS%Effective_Variance(k)-Cloud_RHS%Effective_Variance(k)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    DO k = 1, Cloud_LHS%n_Layers
      IF ( .NOT. Compare_Float( Cloud_LHS%Water_Content(k), &
                                Cloud_RHS%Water_Content(k), &
                                ULP = ULP ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Water_Content values are different at level ",i0,&
                        &":",3(1x,'//FFMT//'))') &
                       k, Cloud_LHS%Water_Content(k), &
                          Cloud_RHS%Water_Content(k), &
                          Cloud_LHS%Water_Content(k)-Cloud_RHS%Water_Content(k)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
  END FUNCTION Equal_Scalar


  FUNCTION Equal_Rank1( Cloud_LHS  , &  ! Input
                        Cloud_RHS  , &  ! Output
                        ULP_Scale  , &  ! Optional input
                        Check_All  , &  ! Optional input
                        Message_Log) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: Cloud_LHS(:)
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: Cloud_RHS(:)
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,      OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_Cloud(Rank-1)'
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
    n = SIZE(Cloud_LHS)
    IF ( SIZE(Cloud_RHS) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Cloud_LHS and Cloud_RHS arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Test for equality
    ! -----------------
    DO i = 1, n
      Scalar_Status = Equal_Scalar( Cloud_LHS(i), &
                                    Cloud_RHS(i), &
                                    ULP_Scale  =ULP_Scale, &
                                    Check_All  =Check_All, &
                                    Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error comparing element (",i0,")", &
                          &" of rank-1 CRTM_Cloud structure array." )' ) i
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
!       CRTM_SetLayers_Cloud
! 
! PURPOSE:
!       Function to set the number of layers to use in a CRTM_Cloud
!       structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_SetLayers_Cloud( n_Layers               , &  ! Input
!                                            Cloud                  , &  ! In/Output
!                                            Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     The value to set the n_Layers component of the 
!                     Cloud structure, as well as those of any of its
!                     structure components.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Cloud:        Cloud structure in which the n_Layers dimension
!                     is to be updated.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN OUT)
! OUTPUT ARGUMENTS:
!       Cloud:        On output, the Cloud structure with the updated
!                     n_Layers dimension.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
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
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the layer reset was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The argument Cloud is INTENT(IN OUT) and is modified upon output. The
!       elements of the structureare reinitialised
!
! COMMENTS:
!       - Note that the n_Layers input is *ALWAYS* scalar. Thus, all Cloud
!         elements will be set to the same number of layers.
!
!       - If n_Layers <= Cloud%Max_Layers, then only the dimension value
!         of the structure and any sub-structures are changed.
!
!       - If n_Layers > Cloud%Max_Layers, then the entire structure is
!         reallocated to the required number of layers.
!
!--------------------------------------------------------------------------------

  FUNCTION SetLayers_Scalar( n_Layers   , &  ! Input
                             Cloud      , &  ! In/Output
                             Message_Log) &  ! Error Messaging
                           RESULT( err_stat )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Layers
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: Cloud
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SetLayers_Cloud(scalar)'

    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Set dimension or allocate based on current size
    ! -----------------------------------------------        
    IF ( n_Layers < Cloud%Max_Layers ) THEN
      Cloud%n_Layers = n_Layers
      CALL CRTM_Zero_Cloud(Cloud)
    ELSE
      ! Deallocate
      err_stat = CRTM_Destroy_Cloud( Cloud, Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, 'Error deallocating cloud structure', &
                              err_stat, Message_Log=Message_Log )
        RETURN
      END IF
      ! Reallocate
      err_stat = CRTM_Allocate_Cloud( n_Layers, Cloud, Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, 'Error reallocating cloud structure', &
                              err_stat, Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    
  END FUNCTION SetLayers_Scalar


  FUNCTION SetLayers_Rank1( n_Layers   , &  ! Input
                            Cloud      , &  ! In/Output
                            Message_Log) &  ! Error Messaging
                          RESULT( err_stat )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Layers
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: Cloud(:)
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SetLayers_Cloud(scalar)'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: m, set_stat
    
    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Loop over elements. If an error is encountered,
    ! report it but continue with the reset.
    ! -----------------------------------------------
    DO m = 1, SIZE(Cloud)
      set_stat = SetLayers_Scalar( n_Layers, Cloud(m), Message_Log=Message_Log )
      IF ( set_stat /= SUCCESS ) THEN
        err_stat = FAILURE
        WRITE( msg,'("Error resetting element ",i0," cloud array n_Layers")' ) m
        CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
      END IF
    END DO
  END FUNCTION SetLayers_Rank1
  
  
!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Sum_Cloud
!
! PURPOSE:
!       Function to perform a sum of two valid CRTM_Cloud structures. The
!       summation performed is:
!         A = A + Scale_Factor*B + Offset
!       where A and B are the CRTM_Cloud structures, and Scale_Factor and Offset
!       are optional weighting factors.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Sum_Cloud( A                        , &  ! In/Output
!                                      B                        , &  ! Input
!                                      Scale_Factor=Scale_Factor, &  ! Optional input
!                                      Offset      =Offset      , &  ! Optional input
!                                      Message_Log =Message_Log   )  ! Error messaging
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
!                      TYPE:       CRTM_Cloud_type
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
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: A
    TYPE(CRTM_Cloud_type),  INTENT(IN)     :: B
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Scale_Factor
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Offset
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Sum_Cloud(Scalar)'
    ! Local variables
    INTEGER  :: n
    REAL(fp) :: m, c

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! ALL *input* pointers must be associated
    IF ( .NOT. CRTM_Associated_Cloud( A ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument A appears empty.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. CRTM_Associated_Cloud( B ) ) THEN
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
         
    ! Cloud types must be the same
    IF ( A%Type /= B%Type ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure Cloud types are different.', &
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
    A%Effective_Radius(1:n)   = A%Effective_Radius(1:n)   + (m*B%Effective_Radius(1:n)  ) + c
    A%Effective_Variance(1:n) = A%Effective_Variance(1:n) + (m*B%Effective_Variance(1:n)) + c
    A%Water_Content(1:n)      = A%Water_Content(1:n)      + (m*B%Water_Content(1:n)     ) + c

  END FUNCTION Sum_Scalar


  FUNCTION Sum_Rank1( A           , &  ! Input/Output
                      B           , &  ! Input
                      Scale_Factor, &  ! Input
                      Offset      , &  ! optional input
                      Message_Log ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: A(:)
    TYPE(CRTM_Cloud_type) , INTENT(IN)     :: B(:)
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Scale_Factor
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Offset
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Sum_Cloud(Rank-1)'
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
      WRITE( Message,'("Input structure arguments have incompactible dimensions. ",&
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
                          &" of CRTM_Cloud structure arrays." )' ) i
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
!       CRTM_Zero_Cloud
! 
! PURPOSE:
!       Subroutine to zero-out various members of a CRTM_Cloud structure - both
!       scalar and pointer.
!
! CALLING SEQUENCE:
!       CALL CRTM_Zero_Cloud( Cloud )
!
! OUTPUT ARGUMENTS:
!       Cloud:        Zeroed out Cloud structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - No checking of the input structure is performed, so there are no
!         tests for pointer member association status. This means the Cloud
!         structure must have allocated pointer members upon entry to this
!         routine.
!
!       - The dimension components of the structure are *NOT* set to zero.
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
    
    ! Reset the added layer count
    Cloud%n_Added_Layers = 0
    
    ! Reset the array components
    Cloud%Effective_Radius   = ZERO
    Cloud%Effective_Variance = ZERO
    Cloud%Water_Content      = ZERO
  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( Cloud )  ! Output
    TYPE(CRTM_Cloud_type), INTENT(IN OUT) :: Cloud(:)
    INTEGER :: n
    DO n = 1, SIZE(Cloud)
      CALL Zero_Scalar( Cloud(n) )
    END DO
  END SUBROUTINE Zero_Rank1


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_RCS_ID_Cloud
!
! PURPOSE:
!       Subroutine to return the module RCS Id information.
!
! CALLING SEQUENCE:
!       CALL CRTM_RCS_Id_Cloud( RCS_Id )
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

  SUBROUTINE CRTM_RCS_ID_Cloud( RCS_Id )
    CHARACTER(*), INTENT(OUT) :: RCS_Id
    RCS_Id = MODULE_RCS_ID
  END SUBROUTINE CRTM_RCS_ID_Cloud

END MODULE CRTM_Cloud_Define
