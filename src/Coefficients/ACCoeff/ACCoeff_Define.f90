!
! ACCoeff_Define
!
! Module defining the ACCoeff data structure and containing routines to 
! manipulate it.
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, 25-Jan-2011
!                    paul.vandelst@noaa.gov
!

MODULE ACCoeff_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE SensorInfo_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID   
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: ACCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: ACCoeff_Associated
  PUBLIC :: ACCoeff_Destroy
  PUBLIC :: ACCoeff_Create
  PUBLIC :: ACCoeff_Inspect
  PUBLIC :: ACCoeff_ValidRelease
  PUBLIC :: ACCoeff_Info
  PUBLIC :: ACCoeff_DefineVersion


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE ACCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512
  ! Sensor id string length
  INTEGER, PARAMETER :: SL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ACCOEFF_RELEASE = 1
  INTEGER, PARAMETER :: ACCOEFF_VERSION = 1


  ! -----------------------
  ! Derived type definition
  ! -----------------------
  TYPE :: ACCoeff_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = ACCOEFF_RELEASE
    INTEGER(Long) :: Version = ACCOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: n_FOVs     = 0  ! N
    INTEGER(Long) :: n_Channels = 0  ! L
    ! Sensor info
    CHARACTER(SL)              :: Sensor_Id        = ''                     
    INTEGER(Long)              :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER(Long)              :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    INTEGER(Long), ALLOCATABLE :: Sensor_Channel(:)  ! L
    ! Antenna correction coefficients
    REAL(Double) , ALLOCATABLE :: A_earth(:,:)     ! N x L
    REAL(Double) , ALLOCATABLE :: A_space(:,:)     ! N x L
    REAL(Double) , ALLOCATABLE :: A_platform(:,:)  ! N x L
  END TYPE ACCoeff_type


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
!       ACCoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the ACCoeff structure.
!
! CALLING SEQUENCE:
!       Status = ACCoeff_Associated( ACCoeff )
!
! OBJECTS:
!       ACCoeff:    Structure which is to have its member's
!                   status tested.
!                   UNITS:      N/A
!                   TYPE:       ACCoeff_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the ACCoeff members.
!                    .TRUE.  - if ANY of the ACCoeff allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the ACCoeff allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION ACCoeff_Associated( ACCoeff ) RESULT( Status )
    TYPE(ACCoeff_type), INTENT(IN) :: ACCoeff
    LOGICAL :: Status
    Status = ACCoeff%Is_Allocated
  END FUNCTION ACCoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize ACCoeff objects.
!
! CALLING SEQUENCE:
!       CALL ACCoeff_Destroy( ACCoeff )
!
! OBJECTS:
!       ACCoeff:      Re-initialized ACCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       ACCoeff_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ACCoeff_Destroy( ACCoeff )
    TYPE(ACCoeff_type), INTENT(OUT) :: ACCoeff
    ACCoeff%Is_Allocated = .FALSE.
    ACCoeff%n_FOVs           = 0
    ACCoeff%n_Channels       = 0
    ACCoeff%Sensor_Id        = ''
    ACCoeff%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    ACCoeff%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE ACCoeff_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of an ACCoeff object.
!
! CALLING SEQUENCE:
!       CALL ACCoeff_Create( ACCoeff   , &
!                            n_FOVs    , &
!                            n_Channels  )         
!
! OBJECTS:
!       ACCoeff:            ACCoeff object structure.
!                           UNITS:      N/A
!                           TYPE:       ACCoeff_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_FOVs:             Number of sensor fields-of-view (FOVs).
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels:         Number of sensor channels.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ACCoeff_Create( &
    ACCoeff   , &  ! Output
    n_FOVs    , &  ! Input
    n_Channels  )  ! Input
    ! Arguments
    TYPE(ACCoeff_type), INTENT(OUT) :: ACCoeff
    INTEGER           , INTENT(IN)  :: n_FOVs
    INTEGER           , INTENT(IN)  :: n_Channels
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_FOVs     < 1 .OR. &
         n_Channels < 1 ) RETURN
    
    ! Perform the allocation
    ALLOCATE( ACCoeff%Sensor_Channel( 1:n_Channels ), &
              ACCoeff%A_earth( 1:n_FOVs, 1:n_Channels ), &
              ACCoeff%A_space( 1:n_FOVs, 1:n_Channels ), &
              ACCoeff%A_platform( 1:n_FOVs, 1:n_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    ACCoeff%n_FOVs     = n_FOVs
    ACCoeff%n_Channels = n_Channels
    ! ...Arrays
    ACCoeff%Sensor_Channel = 0
    ACCoeff%A_earth        = ONE
    ACCoeff%A_space        = ZERO
    ACCoeff%A_platform     = ZERO


    ! Set allocation indicator
    ACCoeff%Is_Allocated = .TRUE.

  END SUBROUTINE ACCoeff_Create
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a ACCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL ACCoeff_Inspect( ACCoeff )
!
! OBJECTS:
!       ACCoeff:       ACCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       ACCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE ACCoeff_Inspect( ACCoeff )
    TYPE(ACCoeff_type), INTENT(IN) :: ACCoeff
    WRITE(*,'(1x,"ACCoeff OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version  :",1x,i0,".",i0)') ACCoeff%Release, ACCoeff%Version
    ! Dimensions
    WRITE(*,'(3x,"n_FOVs           :",1x,i0)') ACCoeff%n_FOVs    
    WRITE(*,'(3x,"n_Channels       :",1x,i0)') ACCoeff%n_Channels
    IF ( .NOT. ACCoeff_Associated(ACCoeff) ) RETURN
    ! Sensor info
    WRITE(*,'(3x,"Sensor_Id        :",1x,a )') TRIM(ACCoeff%Sensor_Id)
    WRITE(*,'(3x,"WMO_Satellite_ID :",1x,i0)') ACCoeff%WMO_Satellite_ID 
    WRITE(*,'(3x,"WMO_Sensor_ID    :",1x,i0)') ACCoeff%WMO_Sensor_ID
    WRITE(*,'(3x,"Sensor_Channel   :")')
    WRITE(*,'(10(1x,i5,:))') ACCoeff%Sensor_Channel
    ! Coefficient arrays
    WRITE(*,'(3x,"A_earth    :")')
    WRITE(*,'(5(1x,es13.6,:))') ACCoeff%A_earth   
    WRITE(*,'(3x,"A_space    :")')
    WRITE(*,'(5(1x,es13.6,:))') ACCoeff%A_space   
    WRITE(*,'(3x,"A_platform :")')
    WRITE(*,'(5(1x,es13.6,:))') ACCoeff%A_platform
    
  END SUBROUTINE ACCoeff_Inspect
  
  
!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the ACCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = ACCoeff_ValidRelease( ACCoeff )
!
! INPUTS:
!       ACCoeff:       ACCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ACCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION ACCoeff_ValidRelease( ACCoeff ) RESULT( IsValid )
    ! Arguments
    TYPE(ACCoeff_type), INTENT(IN) :: ACCoeff
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( ACCoeff%Release < ACCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An ACCoeff data update is needed. ", &
                  &"ACCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  ACCoeff%Release, ACCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( ACCoeff%Release > ACCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An ACCoeff software update is needed. ", &
                  &"ACCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  ACCoeff%Release, ACCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION ACCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a ACCoeff object.
!
! CALLING SEQUENCE:
!       CALL ACCoeff_Info( ACCoeff, Info )
!
! OBJECTS:
!       ACCoeff:       ACCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       ACCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the ACCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE ACCoeff_Info( ACCoeff, Info )
    ! Arguments
    TYPE(ACCoeff_type), INTENT(IN)  :: ACCoeff
    CHARACTER(*),       INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"ACCoeff RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_FOVS=",i0,2x,&
           &"N_CHANNELS=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           ACCoeff%Release, ACCoeff%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           ACCoeff%n_FOVs    , &
           ACCoeff%n_Channels
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE ACCoeff_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL ACCoeff_DefineVersion( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE ACCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE ACCoeff_DefineVersion


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
!       ACCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two ACCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = ACCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two ACCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       ACCoeff_type
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

  ELEMENTAL FUNCTION ACCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(ACCoeff_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. ACCoeff_Associated(x)) .OR. &
         (.NOT. ACCoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_FOVs     /= y%n_FOVs     ) .OR. &
         (x%n_Channels /= y%n_Channels ) ) RETURN
    ! ...Scalars
    IF ( (x%Sensor_Id        /= y%Sensor_Id       ) .OR. &
         (x%WMO_Satellite_ID /= y%WMO_Satellite_ID) .OR. &
         (x%WMO_Sensor_ID    /= y%WMO_Sensor_ID   ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Sensor_Channel     ==    y%Sensor_Channel ) .AND. &
         ALL(x%A_earth        .EqualTo. y%A_earth        ) .AND. &
         ALL(x%A_space        .EqualTo. y%A_space        ) .AND. &
         ALL(x%A_platform     .EqualTo. y%A_platform     ) ) &
      is_equal = .TRUE.

  END FUNCTION ACCoeff_Equal

END MODULE ACCoeff_Define
