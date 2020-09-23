!
! DeltaRad_TrainSet_Define
!
! Module defining the TauProfile data structure and containing
! routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Yong Han, 14-June-2010
!                      

MODULE DeltaRad_TrainSet_Define

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
  PUBLIC :: TrainSet_type
  ! Structure procedures
  PUBLIC :: TrainSet_Associated
  PUBLIC :: TrainSet_Destroy
  PUBLIC :: TrainSet_Create
  PUBLIC :: TrainSet_Concatenate


  ! index values for concatenation dimension
  INTEGER, PARAMETER :: PROFILE =1, SENSORANGLE =2, SUNANGLE=3

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
  INTEGER,  PARAMETER :: RELEASE_NUMBER = 1
  INTEGER,  PARAMETER :: RADIANCE_NUMBER= 1
  ! Sensor Id default values
  INTEGER,  PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER,  PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047

  TYPE :: TrainSet_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER :: Release = RELEASE_NUMBER
    INTEGER :: Version = RADIANCE_NUMBER
    ! Dimensions
    INTEGER :: n_Channels      = 0 ! == L
    INTEGER :: n_Sensor_Angles = 0 ! == Isen
    INTEGER :: n_Sun_Angles    = 0 ! == Isun
    INTEGER :: n_Layers        = 0 ! == K
    INTEGER :: n_Profiles      = 0 ! == M
    ! Sensor Ids
    CHARACTER(SL) :: Sensor_ID        = ' '
    INTEGER       :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
    ! Dimension arrays
    REAL(fp), ALLOCATABLE :: Radiance_nlte(:, :, :, :)  ! L x Isen x Isun x M
    REAL(fp), ALLOCATABLE ::  Radiance_lte(:, :, :)     ! L x Isen x M
    REAL(fp), ALLOCATABLE :: Sensor_Angle(:)            ! Isen
    REAL(fp), ALLOCATABLE :: Sun_Angle(:)               ! Isun
    REAL(fp), ALLOCATABLE :: Level_Pressure(:,:)        ! 0:K x M
    REAL(fp), ALLOCATABLE :: Level_Temperature(:,:)     ! 0:K x M
    REAL(fp), ALLOCATABLE :: Level_CO2(:,:)             ! 0:K x M
    INTEGER , ALLOCATABLE :: Channel(:)                 ! L
        
  END TYPE TrainSet_type

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
!       TrainSet_Associated
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       TauProfile structure.
!
! CALLING SEQUENCE:
!       Association_Status = TrainSet_Associated( TrainSet       ,   &  ! Input
!                                                 ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       TrainSet:    TrainSet structure which is to have its allocatable
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       TrainSet_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                   TrainSet structure pointer members are associated.
!                    The default is to test if ALL the allocatable members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the allocatable members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of theallocatable members
!                                     are associated.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the TauProfile pointer
!                            members.
!                            .TRUE.  - if ALL the allocatable members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the allocatable
!                                      members are associated.
!                            .FALSE. - some or all of the TrainSet allocatable
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! RESTRICTIONS:
!       This function tests the association status of the TrainSet
!       structure allocatable members. Therefore this function must only
!       be called after the input TauProfile structure has, at least,
!       had its allocatable members initialized.
!
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION TrainSet_Associated( TrainSet )   & ! Input
                                RESULT( Association_Status )
    ! Arguments
    TYPE(TrainSet_type), INTENT(IN)     :: TrainSet
    ! Function result
    LOGICAL :: Association_Status

    Association_Status = TrainSet%Is_Allocated
  END FUNCTION TrainSet_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       TrainSet_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM TrainSet objects.
!
! CALLING SEQUENCE:
!       CALL TrainSet_Destroy( TrainSet )
!
! OBJECTS:
!       TrainSet:     Re-initialized TrainSet structure.
!                     UNITS:      N/A
!                     TYPE:       TrainSet_type
!                     DIMENSION:  Scalar 
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE TrainSet_Destroy( TrainSet )
  
    ! Arguments
    TYPE(TrainSet_type)   , INTENT(IN OUT) :: TrainSet
    TrainSet%Is_Allocated = .FALSE.
  END SUBROUTINE TrainSet_Destroy


!------------------------------------------------------------------------------
!
! NAME:
!       TrainSet_Create
! 
! PURPOSE:
!       Function to allocate the allocatable members of the TrainSet
!       data structure.
!
! CALLING SEQUENCE:
!       CALL TrainSet_Create(    TrainSet       , &  ! Output
!                                n_Channels     , &  ! Input
!                                n_Sensor_Angles, &  ! Input
!                                n_Sun_Angles   , &  ! Input
!                                n_Layers       , &  ! Input
!                                n_Profiles )        ! Input
!
!
! INPUT ARGUMENTS:
!
!       n_Channels:       Number of spectral channels dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Sensor_Angles:  Number of view (sensor zenith) angles dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Sun_Angles:     Number of solar zenith angles dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Layers:         Number of atmospheric layers dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!       n_Profiles:       Number of atmospheric profiles dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OUTPUT ARGUMENTS:
!       TrainSet:         TrainSet structure with allocated
!                         allocatable members
!                         UNITS:      N/A
!                         TYPE:       TrainSet_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
!
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE TrainSet_Create( &
    TrainSet       , &  ! Output
    n_Channels     , &  ! Input
    n_Sensor_Angles, &  ! Input
    n_Sun_Angles   , &  ! Input
    n_Layers       , &  ! Input
    n_Profiles )        ! Input
    ! Arguments
    TYPE(TrainSet_type), INTENT(OUT) :: TrainSet
    INTEGER                   , INTENT(IN)  :: n_Channels    
    INTEGER                   , INTENT(IN)  :: n_Sensor_Angles 
    INTEGER                   , INTENT(IN)  :: n_Sun_Angles    
    INTEGER                   , INTENT(IN)  :: n_Layers    
    INTEGER                   , INTENT(IN)  :: n_Profiles    
    ! Local variables
    INTEGER :: alloc_stat
    ! Function result

    ! Check dimensions
    IF ( n_Channels      < 1 .OR. &
         n_Sensor_Angles < 1 .OR. &
         n_Sun_Angles    < 1 .OR. &
         n_Layers        < 1 .OR. &          
         n_Profiles      < 1      ) THEN
      RETURN
    END IF

    ! Perform the allocation
    ALLOCATE( TrainSet%Radiance_nlte( n_Channels, n_Sensor_Angles, n_Sun_Angles, n_Profiles ), &
              TrainSet%Radiance_lte( n_Channels, n_Sensor_Angles, n_Profiles ), &
              TrainSet%Sensor_Angle( n_Sensor_Angles ), &
              TrainSet%Sun_Angle( n_Sun_Angles ), &
              TrainSet%Level_Pressure( 0:n_Layers, n_Profiles ), &
              TrainSet%Level_Temperature( 0:n_Layers, n_Profiles ), &
              TrainSet%Level_CO2( 0:n_Layers, n_Profiles ), &
              TrainSet%Channel( n_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Assign the dimensions
    ! ---------------------
    TrainSet%n_Channels      = n_Channels
    TrainSet%n_Layers        = n_Layers
    TrainSet%n_Sensor_Angles = n_Sensor_Angles
    TrainSet%n_Sun_Angles    = n_Sun_Angles
    TrainSet%n_Profiles      = n_Profiles


    ! Initialise the arrays
    ! ---------------------
    TrainSet%Radiance_nlte     = FP_INVALID
    TrainSet%Radiance_lte      = FP_INVALID
    TrainSet%Sensor_Angle      = FP_INVALID
    TrainSet%Sun_Angle         = FP_INVALID
    TrainSet%Level_Pressure    = FP_INVALID
    TrainSet%Level_Temperature = FP_INVALID
    TrainSet%Level_CO2         = FP_INVALID
    TrainSet%Channel           = IP_INVALID


    ! Set allocation indicator
    TrainSet%Is_Allocated = .TRUE.

  END SUBROUTINE TrainSet_Create

!------------------------------------------------------------------------------
!
! NAME:
!       Concatenate_ TrainSet
!
! PURPOSE:
!       Function to concatenate two valid TauProfile structures.
!
! CALLING SEQUENCE:
!  Error_Status = TrainSet_Concatenate(  TrainSet1        , &  ! Input
!                                        TrainSet2        , &  ! Input
!                                        Concat_dimIndex) , &  ! Input
!                                        TrainSet_out)         ! Output
!
! INPUT ARGUMENTS:
!       TrainSet1:     First TrainSet structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TrainSet_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       TrainSet2:     Second TrainSet structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TrainSet_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!     Concat_dimIndex  Index for concatenation dimension
!                      = 1  - concatenate along profile dimension
!                      = 2  - concatenate along sensor angle dimension
!                      = 3  - concatenate along sun angle dimension
!
! OUTPUT ARGUMENTS:
!       TrainSet_out:  The concatenated TrainSet structure. The order of
!                      concatenation is TrainSet1,TrainSet2 along the dimension 
!                      indicated by Concat_dimIndex
!                      UNITS:      N/A
!                      TYPE:       TrainSet_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!
!------------------------------------------------------------------------------

  FUNCTION TrainSet_Concatenate(  TrainSet1        , &  ! Input
                                  TrainSet2        , &  ! Input
                                  Concat_dimIndex  , &  ! Input
                                  TrainSet_out)      &  ! Output
                                 RESULT( Error_Status )
    ! Arguments
    TYPE(TrainSet_type) , INTENT(IN)     :: TrainSet1
    TYPE(TrainSet_type) , INTENT(IN)     :: TrainSet2
    INTEGER             , INTENT(IN)     :: Concat_dimIndex
    TYPE(TrainSet_type) , INTENT(INOUT)  :: TrainSet_out
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Concatenate_ TrainSet'
    ! Local variables
    INTEGER :: n1, n2, n3

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! ALL structure pointers must be associated
    IF ( .NOT. TrainSet_Associated( TrainSet1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TrainSet1 '//&
                            'members are NOT associated.', &
                            Error_Status)
      RETURN
    END IF
    IF ( .NOT. TrainSet_Associated( TrainSet2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TrainSet2 '//&
                            'members are NOT associated.', &
                            Error_Status)
      RETURN
    END IF

    ! Check the IDs
    IF ( TrainSet1%Sensor_ID        /= TrainSet2%Sensor_ID        .OR. &
         TrainSet1%WMO_Satellite_ID /= TrainSet2%WMO_Satellite_ID .OR. &
         TrainSet1%WMO_Sensor_ID    /= TrainSet2%WMO_Sensor_ID         ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TrainSet sensor ID values are different.', &
                            Error_Status)
      RETURN
    END IF

    SELECT CASE( Concat_dimIndex )
     CASE(PROFILE)
       ! Check the non-concatenation dimensions
       IF ( TrainSet1%n_Channels        /= TrainSet2%n_Channels      .OR. &
            TrainSet1%n_Sensor_Angles   /= TrainSet2%n_Sensor_Angles .OR. &
            TrainSet1%n_Sun_Angles      /= TrainSet2%n_Sun_Angles    .OR. &
            TrainSet1%n_Layers          /= TrainSet2%n_Layers  ) THEN
         Error_Status = FAILURE
         CALL Display_Message( ROUTINE_NAME, &
                              'Error: one or more of the non-concatenation dimensions are different.', & 
                               Error_Status)
         RETURN
       END IF
 
       CALL TrainSet_Create( TrainSet_out, &
                            TrainSet1%n_Channels, &
                            TrainSet1%n_Sensor_Angles, &
                            TrainSet1%n_Sun_Angles, &
                            TrainSet1%n_Layers, &
                            TrainSet1%n_Profiles + TrainSet2%n_Profiles)
       ! non concatenation members
       IF( ANY(Compare_Float( TrainSet1%Sensor_Angle, TrainSet2%Sensor_Angle )).AND. &
           ANY(Compare_Float( TrainSet1%Sun_Angle,    TrainSet2%Sun_Angle ))   .AND. &
           ALL( TrainSet1%Channel ==  TrainSet2%Channel) )THEN
           
           TrainSet_out%Sensor_Angle = TrainSet1%Sensor_Angle
           TrainSet_out%Sun_Angle    = TrainSet1%Sun_Angle
           TrainSet_out%Channel      = TrainSet1%Channel
       ELSE
           Error_Status = FAILURE
           CALL Display_Message( ROUTINE_NAME, &
                                 'Error: the non concatenation array values are different.', &
                                 Error_Status)
           RETURN
       END IF
    
       n1 = TrainSet1%n_Profiles
       n2 = n1+1
       n3 = TrainSet_out%n_Profiles

       TrainSet_out%Radiance_nlte(:, :, :, 1:n1) = TrainSet1%Radiance_nlte
       TrainSet_out%Radiance_lte(:, :, 1:n1)     = TrainSet1%Radiance_lte
       TrainSet_out%Level_Pressure(:, 1:n1)      = TrainSet1%Level_Pressure
       TrainSet_out%Level_Temperature(:, 1:n1)   = TrainSet1%Level_Temperature
       TrainSet_out%Level_CO2(:, 1:n1)           = TrainSet1%Level_CO2

       TrainSet_out%Radiance_nlte(:, :, :, n2:n3) = TrainSet2%Radiance_nlte
       TrainSet_out%Radiance_lte(:, :, n2:n3)     = TrainSet2%Radiance_lte
       TrainSet_out%Level_Pressure(:, n2:n3)      = TrainSet2%Level_Pressure
       TrainSet_out%Level_Temperature(:, n2:n3)   = TrainSet2%Level_Temperature
       TrainSet_out%Level_CO2(:, n2:n3)           = TrainSet2%Level_CO2
     
     CASE(SENSORANGLE)
        ! Check the non-concatenation dimensions
       IF ( TrainSet1%n_Channels        /= TrainSet2%n_Channels      .OR. &
            TrainSet1%n_Sun_Angles      /= TrainSet2%n_Sun_Angles    .OR. &
            TrainSet1%n_Profiles        /= TrainSet2%n_Profiles      .OR. &
            TrainSet1%n_Layers          /= TrainSet2%n_Layers  ) THEN
         Error_Status = FAILURE
         CALL Display_Message( ROUTINE_NAME, &
                              'Error: one or more of the non-concatenation dimensions are different.', &
                               Error_Status)
         RETURN
       END IF
 
       CALL TrainSet_Create( TrainSet_out, &
                            TrainSet1%n_Channels, &
                            TrainSet1%n_Sensor_Angles + TrainSet2%n_Sensor_Angles, &
                            TrainSet1%n_Sun_Angles, &
                            TrainSet1%n_Layers, &
                            TrainSet1%n_Profiles)
       ! non concatenation members
       IF( ANY(Compare_Float( TrainSet1%Sun_Angle, TrainSet2%Sun_Angle ))                   .AND. &
           ANY(Compare_Float( TrainSet1%Level_Pressure, TrainSet2%Level_Pressure ))         .AND. &
           ANY(Compare_Float( TrainSet1%Level_Temperature, TrainSet2%Level_Temperature ))   .AND. &
           ANY(Compare_Float( TrainSet1%Level_CO2, TrainSet2%Level_CO2 ))                   .AND. &
           ALL( TrainSet1%Channel ==  TrainSet2%Channel) )THEN
           
           TrainSet_out%Sun_Angle           = TrainSet1%Sun_Angle
           TrainSet_out%Channel             = TrainSet1%Channel
           TrainSet_out%Level_Pressure      = TrainSet1%Level_Pressure
           TrainSet_out%Level_Temperature   = TrainSet1%Level_Temperature
           TrainSet_out%Level_CO2           = TrainSet1%Level_CO2
       ELSE
           Error_Status = FAILURE
           CALL Display_Message( ROUTINE_NAME, &
                                 'Error: the non concatenation members of the two structures are different ',&
                                 Error_Status)
           RETURN
       END IF
    
       n1 = TrainSet1%n_Sensor_Angles
       n2 = n1+1
       n3 = TrainSet_out%n_Sensor_Angles

       TrainSet_out%Radiance_nlte(:, 1:n1, :, :)  = TrainSet1%Radiance_nlte
       TrainSet_out%Radiance_lte(:, 1:n1, :)      = TrainSet1%Radiance_lte
       TrainSet_out%Sensor_Angle(1:n1)            = TrainSet1%Sensor_Angle
       TrainSet_out%Radiance_nlte(:, n2:n3, :, :) = TrainSet2%Radiance_nlte
       TrainSet_out%Radiance_lte(:, n2:n3, :)     = TrainSet2%Radiance_lte
       TrainSet_out%Sensor_Angle(n2:n3)           = TrainSet2%Sensor_Angle
     
     CASE(SUNANGLE)
      
        ! Check the non-concatenation dimensions
       IF ( TrainSet1%n_Channels        /= TrainSet2%n_Channels        .OR. &
            TrainSet1%n_Sensor_Angles   /= TrainSet2%n_Sensor_Angles   .OR. &
            TrainSet1%n_Profiles        /= TrainSet2%n_Profiles        .OR. &
            TrainSet1%n_Layers          /= TrainSet2%n_Layers  ) THEN
         Error_Status = FAILURE
         CALL Display_Message( ROUTINE_NAME, &
                              'Error: one or more of the non-concatenation dimensions are different.',& 
                               Error_Status)
         RETURN
       END IF
 
       CALL TrainSet_Create( TrainSet_out, &
                            TrainSet1%n_Channels, &
                            TrainSet1%n_Sensor_Angles, &
                            TrainSet1%n_Sun_Angles + TrainSet2%n_Sun_Angles, &
                            TrainSet1%n_Layers, &
                            TrainSet1%n_Profiles)
       ! non concatenation members
       IF( ANY(Compare_Float( TrainSet1%Radiance_lte, TrainSet2%Radiance_lte ))             .AND. &
           ANY(Compare_Float( TrainSet1%Sensor_Angle, TrainSet2%Sensor_Angle ) )            .AND. &
           ANY(Compare_Float( TrainSet1%Level_Pressure, TrainSet2%Level_Pressure ))         .AND. &
           ANY(Compare_Float( TrainSet1%Level_Temperature, TrainSet2%Level_Temperature ))   .AND. &
           ANY(Compare_Float( TrainSet1%Level_CO2, TrainSet2%Level_CO2 ))                   .AND. &
           ALL( TrainSet1%Channel ==  TrainSet2%Channel) )THEN
           
           TrainSet_out%Radiance_lte        = TrainSet1%Radiance_lte
           TrainSet_out%sensor_Angle        = TrainSet1%Sensor_Angle
           TrainSet_out%Channel             = TrainSet1%Channel
           TrainSet_out%Level_Pressure      = TrainSet1%Level_Pressure
           TrainSet_out%Level_Temperature   = TrainSet1%Level_Temperature
           TrainSet_out%Level_CO2           = TrainSet1%Level_CO2
       ELSE
           Error_Status = FAILURE
           CALL Display_Message( ROUTINE_NAME, &
                                 'Error: the non concatenation members of the two structures are different ',&
                                 Error_Status)
           RETURN
       END IF
    
       n1 = TrainSet1%n_Sun_Angles
       n2 = n1+1
       n3 = TrainSet_out%n_Sun_Angles

       TrainSet_out%Radiance_nlte(:, :, 1:n1, :)  = TrainSet1%Radiance_nlte
       TrainSet_out%Sun_Angle(1:n1)               = TrainSet1%Sun_Angle
       TrainSet_out%Radiance_nlte(:, :, n2:n3, :) = TrainSet2%Radiance_nlte
       TrainSet_out%Sun_Angle(n2:n3)              = TrainSet2%Sun_Angle

     CASE DEFAULT
           Error_Status = FAILURE
           CALL Display_Message( ROUTINE_NAME, &
                                 'Error: incorrect dimension index for cancatenation. ', &
                                 Error_Status)
           RETURN
       
     END SELECT

     TrainSet_out%Sensor_ID        = TrainSet1%Sensor_ID        
     TrainSet_out%WMO_Satellite_ID = TrainSet1%WMO_Satellite_ID 
     TrainSet_out%WMO_Sensor_ID    = TrainSet1%WMO_Sensor_ID    

  END FUNCTION TrainSet_Concatenate


END MODULE DeltaRad_TrainSet_Define
