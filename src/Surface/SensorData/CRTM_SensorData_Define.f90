!
! CRTM_SensorData_Define
!
! Module defining the CRTM SensorData structure and containing
! routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Jul-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_SensorData_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE CRTM_Parameters      , ONLY: ZERO, ONE, SET, STRLEN, &
                                   INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID   
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  ! Datatypes
  PUBLIC :: CRTM_SensorData_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(+)
  ! Procedures
  PUBLIC :: CRTM_SensorData_Associated
  PUBLIC :: CRTM_SensorData_Destroy
  PUBLIC :: CRTM_SensorData_Create
  PUBLIC :: CRTM_SensorData_Zero
  PUBLIC :: CRTM_SensorData_IsValid
  PUBLIC :: CRTM_SensorData_Inspect
  PUBLIC :: CRTM_SensorData_DefineVersion
  PUBLIC :: CRTM_SensorData_Compare


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_SensorData_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_SensorData_Add
  END INTERFACE OPERATOR(+)

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! -------------------------------
  ! SensorData structure definition
  ! -------------------------------
  !:tdoc+:
  TYPE :: CRTM_SensorData_type
    ! Dimension values
    INTEGER :: n_Channels = 0  ! L
    ! The WMO sensor ID of the sensor for which the data is to be used
    INTEGER :: Select_WMO_Sensor_Id = INVALID_WMO_SENSOR_ID
    ! The data sensor IDs and channels
    CHARACTER(STRLEN), ALLOCATABLE :: Sensor_Id(:)        ! L
    INTEGER,           ALLOCATABLE :: WMO_Satellite_ID(:) ! L
    INTEGER,           ALLOCATABLE :: WMO_Sensor_ID(:)    ! L
    INTEGER,           ALLOCATABLE :: Sensor_Channel(:)   ! L
    ! The sensor brightness temperatures
    REAL(fp), ALLOCATABLE :: Tb(:) ! L
  END TYPE CRTM_SensorData_type
  !:tdoc-:


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
!       CRTM_SensorData_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM SensorData object.
!
! CALLING SEQUENCE:
!       Status = CRTM_SensorData_Associated( SensorData )
!
! OBJECTS:
!       SensorData:  SensorData structure which is to have its member's
!                    status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_SensorData_type
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:      The return value is a logical value indicating the
!                    status of the SensorData members.
!                    .TRUE.  - if ANY of the SensorData allocatable or
!                              pointer members are in use.
!                    .FALSE. - if ALL of the SensorData allocatable or
!                              pointer members are not in use.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input SensorData argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SensorData_Associated( SensorData ) RESULT( Status )
    ! Arguments
    TYPE(CRTM_SensorData_type), INTENT(IN) :: SensorData
    ! Function result
    LOGICAL :: Status

    ! Test the structure members
    Status = &
      ALLOCATED(SensorData%Sensor_Id       ) .OR. &
      ALLOCATED(SensorData%WMO_Satellite_ID) .OR. &
      ALLOCATED(SensorData%WMO_Sensor_ID   ) .OR. &
      ALLOCATED(SensorData%Sensor_Channel  ) .OR. &
      ALLOCATED(SensorData%Tb              )

  END FUNCTION CRTM_SensorData_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM SensorData objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_Destroy( SensorData )
!
! OBJECTS:
!       SensorData:   Re-initialized SensorData structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SensorData_type
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SensorData_Destroy( SensorData )
    TYPE(CRTM_SensorData_type), INTENT(OUT) :: SensorData
  END SUBROUTINE CRTM_SensorData_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM SensorData object.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_Create( SensorData, n_Channels )
!
! OBJECTS:
!       SensorData:   SensorData structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SensorData_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:   Number of sensor channels.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as SensorData object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SensorData_Create( SensorData, n_Channels )
    ! Arguments
    TYPE(CRTM_SensorData_type), INTENT(OUT) :: SensorData
    INTEGER,                    INTENT(IN)  :: n_Channels
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Channels < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( SensorData%Sensor_Id( n_Channels ), &
              SensorData%WMO_Satellite_ID( n_Channels ), &
              SensorData%WMO_Sensor_ID( n_Channels ), &
              SensorData%Sensor_Channel( n_Channels ), &
              SensorData%Tb( n_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    SensorData%n_Channels = n_Channels
    ! ...Arrays
    SensorData%Select_WMO_Sensor_Id  = INVALID_WMO_SENSOR_ID
    SensorData%Sensor_Id        = ' '
    SensorData%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    SensorData%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    SensorData%Sensor_Channel   = 0
    SensorData%Tb               = ZERO
    
  END SUBROUTINE CRTM_SensorData_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_Zero
!
! PURPOSE:
!       Elemental subroutine to zero out the data arrays in a
!       CRTM SensorData object.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_Zero( SensorData )
!
! OBJECTS:
!       SensorData:    CRTM SensorData structure in which the data arrays are
!                      to be zeroed out.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - The dimension components of the structure are *NOT* set to zero.
!       - The SensorData sensor id and channel components are *NOT* reset.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SensorData_Zero( SensorData )
    TYPE(CRTM_SensorData_type), INTENT(IN OUT) :: SensorData
    ! Do nothing if structure is unused
    IF ( .NOT. CRTM_SensorData_Associated(SensorData) ) RETURN
    ! Only zero out the data arrays
    SensorData%Tb = ZERO
  END SUBROUTINE CRTM_SensorData_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       CRTM SensorData object. 
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = CRTM_SensorData_IsValid( SensorData )
!
!         or
!
!       IF ( CRTM_SensorData_IsValid( SensorData ) ) THEN....
!
! OBJECTS:
!       SensorData:    CRTM SensorData object which is to have its
!                      contents checked.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical variable indicating whether or not the input
!                      passed the check.
!                      If == .FALSE., SensorData object is unused or contains
!                                     invalid data.
!                         == .TRUE.,  SensorData object can be used in CRTM.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_SensorData_IsValid( SensorData ) RESULT( IsValid )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: SensorData
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SensorData_IsValid'
    CHARACTER(ML) :: msg
    
    ! Setup
    IsValid = .FALSE.
    ! ...Check if structure is used
    IF ( .NOT. CRTM_SensorData_Associated(SensorData) ) THEN
      msg = 'SensorData structure not allocated'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      RETURN
    ENDIF
    IF ( SensorData%n_channels < 1 ) THEN
      msg = 'SensorData structure dimension invalid'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      RETURN
    ENDIF
    
    ! Check data
    ! ...Change default so all entries can be checked
    IsValid = .TRUE.
    ! ...The WMO Sensor id
    IF ( SensorData%Select_WMO_Sensor_Id == INVALID_WMO_SENSOR_ID ) THEN
      msg = 'Invalid Select WMO Sensor Id'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...Data sensor ids
    IF ( ANY(LEN_TRIM(SensorData%Sensor_Id) == 0) ) THEN
      msg = 'Invalid Sensor Id found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( ANY(SensorData%WMO_Satellite_Id == INVALID_WMO_SATELLITE_ID) ) THEN
      msg = 'Invalid WMO Satellite Id found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( ANY(SensorData%WMO_Sensor_Id == INVALID_WMO_SENSOR_ID) ) THEN
      msg = 'Invalid WMO Sensor Id'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( ANY(SensorData%Sensor_Channel < 1) ) THEN
      msg = 'Invalid Sensor Channel found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...Data
    IF ( ANY(SensorData%Tb < ZERO ) ) THEN
      msg = 'Negative SensorData brightness temperatures found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF

  END FUNCTION CRTM_SensorData_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM SensorData object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_Inspect( SensorData )
!
! INPUTS:
!       SensorData:    CRTM SensorData object to display.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_SensorData_Inspect( SensorData )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: SensorData
    INTEGER :: l
    ! Display components
    WRITE(*, '(5x,"SensorData n_Channels:",1x,i0)') SensorData%n_Channels
    WRITE(*, '(5x,"SensorData Select_WMO_Sensor_Id:",1x,i0)') SensorData%Select_WMO_Sensor_Id
    IF ( .NOT. CRTM_SensorData_Associated(SensorData) ) RETURN
    DO l = 1, SensorData%n_Channels
      WRITE(*, '(5x,a," channel ",i0," Tb:",es13.6)') &
            TRIM(SensorData%Sensor_Id(l)), SensorData%Sensor_Channel(l), SensorData%Tb(l)
    END DO
  END SUBROUTINE CRTM_SensorData_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SensorData_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_SensorData_DefineVersion( Id )
!
! OUTPUT ARGUMENTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_SensorData_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_SensorData_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_SensorData_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_SensorData objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_SensorData_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM SensorData objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_SigFig:      Number of significant figure to compare floating point
!                      components.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or same as input
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SensorData_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: x, y
    INTEGER,          OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: l, n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF
    
    ! Check the structure association status
    IF ( (.NOT. CRTM_SensorData_Associated(x)) .OR. &
         (.NOT. CRTM_SensorData_Associated(y)) ) RETURN

    ! Check scalars
    IF ( (x%n_Channels           /= y%n_Channels) .OR. &
         (x%Select_WMO_Sensor_Id /= y%Select_WMO_Sensor_Id) ) RETURN

    ! Check arrays
    l = x%n_Channels
    IF ( ANY(x%Sensor_Id(1:l)        /= y%Sensor_Id(1:l)       ) .OR. &
         ANY(x%WMO_Satellite_ID(1:l) /= y%WMO_Satellite_ID(1:l)) .OR. &
         ANY(x%WMO_Sensor_ID(1:l)    /= y%WMO_Sensor_ID(1:l)   ) .OR. &
         ANY(x%Sensor_Channel(1:l)   /= y%Sensor_Channel(1:l)  ) ) RETURN
    IF ( .NOT. ALL(Compares_Within_Tolerance(x%Tb(1:l),y%Tb(1:l),n)) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.

  END FUNCTION CRTM_SensorData_Compare


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_SensorData_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_SensorData objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_SensorData_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM SensorData objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SensorData_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SensorData_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_SensorData_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    ! Variables
    INTEGER :: n

    ! Set up
    is_equal = .FALSE.
    
    ! Check the structure association status
    IF ( (.NOT. CRTM_SensorData_Associated(x)) .OR. &
         (.NOT. CRTM_SensorData_Associated(y))      ) RETURN

    ! Check contents
    ! ...Scalars
    IF ( (x%n_Channels /= y%n_Channels) .OR. &
         (x%Select_WMO_Sensor_Id /= y%Select_WMO_Sensor_Id) ) RETURN
    ! ...Arrays
    n = x%n_Channels
    IF ( ALL(x%Sensor_Id(1:n)        == y%Sensor_Id(1:n)       ) .AND. &
         ALL(x%WMO_Satellite_ID(1:n) == y%WMO_Satellite_ID(1:n)) .AND. &
         ALL(x%WMO_Sensor_ID(1:n)    == y%WMO_Sensor_ID(1:n)   ) .AND. &
         ALL(x%Sensor_Channel(1:n)   == y%Sensor_Channel(1:n)  ) .AND. &
         ALL(x%Tb(1:n) .EqualTo. y%Tb(1:n)) ) &
      is_equal = .TRUE.

  END FUNCTION CRTM_SensorData_Equal


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_SensorData_Add
!
! PURPOSE:
!       Pure function to add two CRTM SensorData objects.
!       Used in OPERATOR(+) interface block.
!
! CALLING SEQUENCE:
!       sDatasum = CRTM_SensorData_Add( sData1, sData2 )
!
!         or
!
!       sDatasum = sData1 + sData2
!
!
! INPUTS:
!       sData1, sData2: The SensorData objects to add.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SensorData_type
!                       DIMENSION:  Scalar or any rank
!                       ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       sDatasum:       SensorData structure containing the added components.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SensorData_type
!                       DIMENSION:  Same as input
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SensorData_Add( sData1, sData2 ) RESULT( sDatasum )
    TYPE(CRTM_SensorData_type), INTENT(IN) :: sData1, sData2
    TYPE(CRTM_SensorData_type) :: sDatasum
    ! Variables
    INTEGER :: n

    ! Check input
    ! ...If input structures not used, do nothing
    IF ( .NOT. CRTM_SensorData_Associated( sData1 ) .OR. &
         .NOT. CRTM_SensorData_Associated( sData2 )      ) RETURN
    ! ...If input structure for different sensors, or sizes, do nothing
    IF ( sData1%n_Channels           /= sData2%n_Channels            .OR. &
         sData1%Select_WMO_Sensor_Id /= sData2%Select_WMO_Sensor_Id  .OR. &
         ANY(sData1%Sensor_Id        /= sData2%Sensor_Id       )     .OR. &
         ANY(sData1%WMO_Satellite_ID /= sData2%WMO_Satellite_ID)     .OR. &
         ANY(sData1%WMO_Sensor_ID    /= sData2%WMO_Sensor_ID   )     .OR. &
         ANY(sData1%Sensor_Channel   /= sData2%Sensor_Channel  )          ) RETURN
         
    ! Copy the first structure
    sDatasum = sData1

    ! And add its components to the second one
    n = sData1%n_Channels
    sDatasum%Tb(1:n) = sDatasum%Tb(1:n) + sData2%Tb(1:n)     

  END FUNCTION CRTM_SensorData_Add

END MODULE CRTM_SensorData_Define
