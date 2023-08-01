!
! SensorInfo_Define
!
! Module defining the SensorInfo data structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE SensorInfo_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE SensorInfo_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID   , &
                                   N_SENSOR_TYPES          , &
                                   INVALID_SENSOR          , &
                                   MICROWAVE_SENSOR        , &
                                   INFRARED_SENSOR         , &
                                   VISIBLE_SENSOR          , &
                                   ULTRAVIOLET_SENSOR      , &
                                   SENSOR_TYPE_NAME        , &
                                   N_POLARIZATION_TYPES    , &
                                   UNPOLARIZED
                                   
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters (passed through from SensorInfo_Parameters)
  PUBLIC :: UNPOLARIZED
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: ULTRAVIOLET_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME
  PUBLIC :: N_POLARIZATION_TYPES
  ! The derived type definition
  PUBLIC :: SensorInfo_type
  ! Procedures
  PUBLIC :: Associated_SensorInfo
  PUBLIC :: Destroy_SensorInfo
  PUBLIC :: Allocate_SensorInfo
  PUBLIC :: Assign_SensorInfo


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE Destroy_SensorInfo
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE Destroy_SensorInfo


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! String lengths
  INTEGER, PARAMETER :: ML  = 256
  INTEGER, PARAMETER :: SL  = 20
  INTEGER, PARAMETER :: SL2 = 12
  ! Default values
  INTEGER, PARAMETER :: INVALID = -1


  ! -------------------------------
  ! SensorInfo data type definition
  ! -------------------------------
  TYPE :: SensorInfo_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Channels = 0  ! L
    INTEGER :: n_FOVs     = 0  ! I
    ! Descriptors
    CHARACTER(SL2) :: Sensor_Name    = ' '
    CHARACTER(SL2) :: Satellite_Name = ' '
    ! Sensor Ids
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER       :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    ! Sensor type
    INTEGER :: Sensor_Type = INVALID_SENSOR
    ! The channel data
    INTEGER , POINTER :: Sensor_Channel(:) => NULL() ! L
    INTEGER , POINTER :: Use_Flag(:)       => NULL() ! L
    REAL(fp), POINTER :: Noise(:)          => NULL() ! L
  END TYPE SensorInfo_type


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
!       Associated_SensorInfo
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       SensorInfo structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_SensorInfo( SensorInfo       , &  ! Input
!                                                   ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       SensorInfo:          SensorInfo structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       SensorInfo_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            SensorInfo structure pointer members are associated.
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
!                            association status of the SensorInfo pointer members.
!                            .TRUE.  - if ALL the SensorInfo pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the SensorInfo pointer
!                                      members are associated.
!                            .FALSE. - some or all of the SensorInfo pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_SensorInfo( SensorInfo, & ! Input
                                  ANY_Test  ) & ! Optional input
                                RESULT( Association_Status )
    ! Arguments
    TYPE(SensorInfo_type), INTENT(IN) :: SensorInfo
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
      IF ( ASSOCIATED(SensorInfo%Sensor_Channel) .AND. &
           ASSOCIATED(SensorInfo%Use_Flag      ) .AND. &
           ASSOCIATED(SensorInfo%Noise         )) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED(SensorInfo%Sensor_Channel) .OR. &
           ASSOCIATED(SensorInfo%Use_Flag      ) .OR. &
           ASSOCIATED(SensorInfo%Noise         )) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_SensorInfo


!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_SensorInfo
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of SensorInfo
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_SensorInfo( SensorInfo )
!
! OUTPUT ARGUMENTS:
!       SensorInfo:   Re-initialized SensorInfo structure.
!                     UNITS:      N/A
!                     TYPE:       SensorInfo_type
!                     DIMENSION:  Scalar or Rank-1
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
!       Note the INTENT on the output SensorInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( SensorInfo , &  ! Output
                           No_Clear   ) &  ! Optional input
                         RESULT( Error_Status )
    ! Arguments
    TYPE(SensorInfo_type) , INTENT(IN OUT) :: SensorInfo
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_Clear
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_SensorInfo'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    
    ! Reset the dimension indicators
    SensorInfo%n_Channels = 0
    SensorInfo%n_FOVs     = 0

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == 1 ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_SensorInfo(SensorInfo)
    
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_SensorInfo(SensorInfo) ) RETURN


    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( SensorInfo%Sensor_Channel, &
                SensorInfo%Use_Flag      , &
                SensorInfo%Noise         , &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '("Error deallocating SensorInfo. STAT = ",i0)') &
                      Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status )
      RETURN
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    SensorInfo%n_Allocates = SensorInfo%n_Allocates - 1
    IF ( SensorInfo%n_Allocates /= 0 ) THEN
      WRITE( Message, '("Allocation counter /= 0, Value = ",i0)') &
                      SensorInfo%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status )
      RETURN
    END IF
    
  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( SensorInfo , &  ! Output
                          No_Clear   ) &  ! Optional input
                        RESULT( Error_Status )
    ! Arguments
    TYPE(SensorInfo_type) , INTENT(IN OUT) :: SensorInfo(:)
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_Clear
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_SensorInfo(rank1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n

    ! Set up
    ! ------
    Error_Status = SUCCESS
    

    ! Perform the reinitialisation
    ! ----------------------------
    DO n = 1, SIZE(SensorInfo)

      ! Call the scalar function
      Scalar_Status = Destroy_Scalar( SensorInfo(n), &
                                      No_Clear = No_Clear )

      ! Check the result, but do not halt so deallocation
      ! continues even if an error is encountered.
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message,'("Error destroying SensorInfo structure array element ",i0)' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status )
      END IF
    END DO

  END FUNCTION Destroy_Rank1


!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_SensorInfo
! 
! PURPOSE:
!       Function to allocate the pointer members of the SensorInfo
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_SensorInfo( n_Channels, &  ! Input
!                                           SensorInfo  )  ! Output
!
!
! INPUT ARGUMENTS:
!       n_Channels:   The number of channels in the SensorInfo structure.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SensorInfo:   SensorInfo structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       SensorInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure pointer allocations were
!                                   successful
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
!       Note the INTENT on the output SensorInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_SensorInfo( n_Channels , &  ! Input            
                                SensorInfo ) &  ! Output           
                              RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Channels
    TYPE(SensorInfo_type) , INTENT(IN OUT) :: SensorInfo
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_SensorInfo'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    
    ! Check dimensions
    IF (n_Channels < 1) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SensorInfo dimensions must all be > 0.', &
                            Error_Status )
      RETURN
    END IF
    
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_SensorInfo( SensorInfo, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_SensorInfo( SensorInfo, &               
                                         No_Clear=SET )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating SensorInfo prior to allocation.', &
                              Error_Status )
        RETURN
      END IF
    END IF

    
    ! Perform the pointer allocation
    ! ------------------------------
    ALLOCATE( SensorInfo%Sensor_Channel( n_Channels ), &
              SensorInfo%Use_Flag( n_Channels ), &
              SensorInfo%Noise( n_Channels ), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating SensorInfo data arrays. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status )
      RETURN
    END IF


    ! Assign the dimensions
    ! ---------------------
    SensorInfo%n_Channels = n_Channels


    ! Initialise the arrays
    ! ---------------------
    SensorInfo%Sensor_Channel = 0
    SensorInfo%Use_Flag       = 0
    SensorInfo%Noise          = ZERO


    ! Increment and test the allocation counter
    ! -----------------------------------------
    SensorInfo%n_Allocates = SensorInfo%n_Allocates + 1
    IF ( SensorInfo%n_Allocates /= 1 ) THEN
      WRITE( Message, '("Allocation counter /= 1, Value = ",i0)') &
                      SensorInfo%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status )
      RETURN
    END IF

  END FUNCTION Allocate_SensorInfo


!------------------------------------------------------------------------------
!
! NAME:
!       Assign_SensorInfo
!
! PURPOSE:
!       Function to copy valid SensorInfo structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_SensorInfo( SensorInfo_in , &  ! Input
!                                         SensorInfo_out  )  ! Output
!
! INPUT ARGUMENTS:
!       SensorInfo_in:   SensorInfo structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SensorInfo_out:  Copy of the input structure, SensorInfo_in.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output SensorInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Assign_SensorInfo( SensorInfo_in , &  ! Input
                              SensorInfo_out) &  ! Output
                            RESULT( Error_Status )
    ! Arguments
    TYPE(SensorInfo_type) , INTENT(IN)     :: SensorInfo_in
    TYPE(SensorInfo_type) , INTENT(IN OUT) :: SensorInfo_out
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_SensorInfo'

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_SensorInfo( SensorInfo_in ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SensorInfo_in pointer members are NOT associated.', &
                            Error_Status )
      RETURN
    END IF
    
    
    ! Allocate data arrays
    ! --------------------
    Error_Status = Allocate_SensorInfo( SensorInfo_in%n_Channels, &
                                        SensorInfo_out )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output structure.', &
                            Error_Status )
      RETURN
    END IF


    ! Assign non-dimension scalar members
    ! -----------------------------------
    SensorInfo_out%n_FOVs           = SensorInfo_in%n_FOVs
    SensorInfo_out%Sensor_Name      = SensorInfo_in%Sensor_Name
    SensorInfo_out%Satellite_Name   = SensorInfo_in%Satellite_Name
    SensorInfo_out%Sensor_Id        = SensorInfo_in%Sensor_Id
    SensorInfo_out%WMO_Satellite_Id = SensorInfo_in%WMO_Satellite_Id
    SensorInfo_out%WMO_Sensor_Id    = SensorInfo_in%WMO_Sensor_Id
    SensorInfo_out%Sensor_Type      = SensorInfo_in%Sensor_Type

    ! Copy array data
    ! ---------------
    SensorInfo_out%Sensor_Channel = SensorInfo_in%Sensor_Channel
    SensorInfo_out%Use_Flag       = SensorInfo_in%Use_Flag
    SensorInfo_out%Noise          = SensorInfo_in%Noise

  END FUNCTION Assign_SensorInfo


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
!       Clear_SensorInfo
!
! PURPOSE:
!       Subroutine to clear the scalar members of a SensorInfo structure.
!
! CALLING SEQUENCE:
!       CALL Clear_SensorInfo( SensorInfo) ! Output
!
! OUTPUT ARGUMENTS:
!       SensorInfo:  SensorInfo structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       SensorInfo_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output SensorInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined (at least
!       its components may be) upon input. To prevent memory leaks, the IN OUT
!       INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_SensorInfo( SensorInfo )
    TYPE(SensorInfo_type), INTENT(IN OUT) :: SensorInfo
    SensorInfo%Sensor_Name    = ' '
    SensorInfo%Satellite_Name = ' '
    SensorInfo%Sensor_Id        = ' '                      
    SensorInfo%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    SensorInfo%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    SensorInfo%Sensor_Type      = INVALID_SENSOR
  END SUBROUTINE Clear_SensorInfo

END MODULE SensorInfo_Define
