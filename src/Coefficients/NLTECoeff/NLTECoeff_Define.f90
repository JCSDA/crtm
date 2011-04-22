!
! NLTECoeff_Define
!
! Module defining the NLTECoeff structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, 08-05-2010
!                       yong.han@noaa.gov
!
!       Refactored by:  Paul van Delst, 19-Jan-2011
!                       paul.vandelst@noaa.gov
!

MODULE NLTECoeff_Define

  ! -----------------
  ! Environment setup
  ! -----------------
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
  PUBLIC :: NLTECoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: NLTECoeff_Associated
  PUBLIC :: NLTECoeff_Destroy
  PUBLIC :: NLTECoeff_Create
  PUBLIC :: NLTECoeff_Inspect
  PUBLIC :: NLTECoeff_ValidRelease
  PUBLIC :: NLTECoeff_Info
  PUBLIC :: NLTECoeff_DefineVersion


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE NLTECoeff_Equal
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
  INTEGER, PARAMETER :: NLTECOEFF_RELEASE = 2   ! This determines structure and file formats.
  INTEGER, PARAMETER :: NLTECOEFF_VERSION = 1   ! This is just the data version.
  ! Number of layers for which mean temperatures are computed
  INTEGER, PARAMETER :: N_LAYERS = 2
  ! Integer flags corresponding to logical false/true
  INTEGER, PARAMETER :: FALSE = 0
  INTEGER, PARAMETER :: TRUE  = 1

  
  ! -----------------------
  ! Derived type definition
  ! -----------------------
  TYPE :: NLTECoeff_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = NLTECOEFF_RELEASE
    INTEGER(Long) :: Version = NLTECOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Predictors     = 0  ! n1 dimension
    INTEGER(Long) :: n_Sensor_Angles  = 0  ! n2 dimension
    INTEGER(Long) :: n_Solar_Angles   = 0  ! n3 dimension
    INTEGER(Long) :: n_NLTE_Channels  = 0  ! n4 dimension
    INTEGER(Long) :: n_Channels       = 0  ! n5 dimension
    ! ..."Internal" dimension
    INTEGER(Long) :: n_Layers = N_LAYERS
    ! Sensor info
    CHARACTER(SL)              :: Sensor_Id        = ''
    INTEGER(Long)              :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long)              :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER(Long), ALLOCATABLE :: Sensor_Channel(:)        ! n5  
    ! Pressure levels used for computing mean temperatures in the two layers
    REAL(Double) :: Upper_Plevel(N_LAYERS) = ZERO
    REAL(Double) :: Lower_Plevel(N_LAYERS) = ZERO
    ! Min., max. and mean layer temperatures used as the temperature predictor limits 
    REAL(Double) :: Min_Tm(N_LAYERS)  = ZERO
    REAL(Double) :: Max_Tm(N_LAYERS)  = ZERO
    REAL(Double) :: Mean_Tm(N_LAYERS) = ZERO
    ! Coefficient table dimension vectors
    REAL(Double) , ALLOCATABLE :: Secant_Sensor_Zenith(:)  ! n2  
    REAL(Double) , ALLOCATABLE :: Secant_Solar_Zenith(:)   ! n3  
    INTEGER(Long), ALLOCATABLE :: NLTE_Channel(:)          ! n4
    LOGICAL      , ALLOCATABLE :: Is_NLTE_Channel(:)       ! n5
    ! Coefficients for NLTE corrections
    INTEGER(Long), ALLOCATABLE :: C_Index(:)               ! n5
    REAL(Double) , ALLOCATABLE :: C(:,:,:,:)               ! n1 x n2 x n3 x n4 
  END TYPE NLTECoeff_type

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
!       NLTECoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the NLTECoeff structure.
!
! CALLING SEQUENCE:
!       Status = NLTECoeff_Associated( NLTECoeff )
!
! OBJECTS:
!       NLTECoeff:  Structure which is to have its member's
!                   status tested.
!                   UNITS:      N/A
!                   TYPE:       NLTECoeff_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the NLTE members.
!                    .TRUE.  - if ANY of the NLTECoeff allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the NLTECoeff allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION NLTECoeff_Associated( NLTECoeff ) RESULT( Status )
    TYPE(NLTECoeff_type), INTENT(IN) :: NLTECoeff
    LOGICAL :: Status
    Status = NLTECoeff%Is_Allocated
  END FUNCTION NLTECoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize NLTECoeff objects.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Destroy( NLTECoeff )
!
! OBJECTS:
!       NLTECoeff:    Re-initialized NLTECoeff structure.
!                     UNITS:      N/A
!                     TYPE:       NLTECoeff_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE NLTECoeff_Destroy( NLTECoeff )
    TYPE(NLTECoeff_type), INTENT(OUT) :: NLTECoeff
    NLTECoeff%Is_Allocated = .FALSE.
    NLTECoeff%n_Predictors     = 0
    NLTECoeff%n_Sensor_Angles  = 0
    NLTECoeff%n_Solar_Angles   = 0
    NLTECoeff%n_NLTE_Channels  = 0
    NLTECoeff%n_Channels       = 0
    NLTECoeff%Sensor_Id        = ''
    NLTECoeff%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    NLTECoeff%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
  END SUBROUTINE NLTECoeff_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of an NLTECoeff object.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Create( NLTECoeff       , &
!                              n_Predictors    , &      
!                              n_Sensor_Angles , &      
!                              n_Solar_Angles  , &      
!                              n_NLTE_Channels , &      
!                              n_Channels        )         
!
! OBJECTS:
!       NLTECoeff:          NLTECoeff object structure.
!                           UNITS:      N/A
!                           TYPE:       NLTECoeff_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Predictors:       Number of predictors used in NLTE correction algorithm.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as the NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Sensor_Angles:    Number of sensor zenith angles.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as the NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Solar_Angles:     Number of solar zenith angles.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as the NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_NLTE_Channels:    Number of NLTE channels for the sensor.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as the NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!        n_Channels:        Total number of channels for the sensor.
!                           Must be >= n_NLTE_Channels.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as NLTECoeff object
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE NLTECoeff_Create( &
    NLTECoeff       , &  ! Output
    n_Predictors    , &  ! Input
    n_Sensor_Angles , &  ! Input
    n_Solar_Angles  , &  ! Input
    n_NLTE_Channels , &  ! Input
    n_Channels        )  ! Input
    ! Arguments
    TYPE(NLTECoeff_type), INTENT(OUT) :: NLTECoeff
    INTEGER             , INTENT(IN)  :: n_Predictors            
    INTEGER             , INTENT(IN)  :: n_Sensor_Angles              
    INTEGER             , INTENT(IN)  :: n_Solar_Angles            
    INTEGER             , INTENT(IN)  :: n_NLTE_Channels            
    INTEGER             , INTENT(IN)  :: n_Channels              
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Predictors    < 1 .OR. &
         n_Sensor_Angles < 1 .OR. &
         n_Solar_Angles  < 1 .OR. &
         n_NLTE_Channels < 1 .OR. &
         n_Channels < n_NLTE_Channels ) RETURN
    
    ! Perform the allocation
    ALLOCATE( NLTECoeff%Sensor_Channel( n_Channels ), &
              NLTECoeff%Secant_Sensor_Zenith( n_Sensor_Angles ), &
              NLTECoeff%Secant_Solar_Zenith( n_Solar_Angles ), &
              NLTECoeff%NLTE_Channel( n_NLTE_Channels ), &
              NLTECoeff%Is_NLTE_Channel( n_Channels ), &
              NLTECoeff%C_Index( n_Channels ), &
              NLTECoeff%C( n_Predictors, n_Sensor_Angles, n_Solar_Angles, n_NLTE_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    NLTECoeff%n_Predictors    = n_Predictors
    NLTECoeff%n_Sensor_Angles = n_Sensor_Angles
    NLTECoeff%n_Solar_Angles  = n_Solar_Angles
    NLTECoeff%n_NLTE_Channels = n_NLTE_Channels
    NLTECoeff%n_Channels      = n_Channels
    ! ...Arrays
    NLTECoeff%Sensor_Channel       = 0
    NLTECoeff%Secant_Sensor_Zenith = ZERO
    NLTECoeff%Secant_Solar_Zenith  = ZERO
    NLTECoeff%NLTE_Channel         = 0
    NLTECoeff%Is_NLTE_Channel      = .FALSE.
    NLTECoeff%C_Index              = 0
    NLTECoeff%C                    = ZERO

    ! Set allocation indicator
    NLTECoeff%Is_Allocated = .TRUE.

  END SUBROUTINE NLTECoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a NLTECoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Inspect( NLTECoeff )
!
! OBJECTS:
!       NLTECoeff:     NLTECoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       NLTECoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_Inspect( NLTECoeff )
    TYPE(NLTECoeff_type), INTENT(IN) :: NLTECoeff
    INTEGER :: i
    CHARACTER(3) :: maybe
    WRITE(*,'(1x,"NLTECoeff OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version  :",1x,i0,".",i0)') NLTECoeff%Release, NLTECoeff%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Predictors     :",1x,i0)') NLTECoeff%n_Predictors    
    WRITE(*,'(3x,"n_Sensor_Angles  :",1x,i0)') NLTECoeff%n_Sensor_Angles 
    WRITE(*,'(3x,"n_Solar_Angles   :",1x,i0)') NLTECoeff%n_Solar_Angles  
    WRITE(*,'(3x,"n_NLTE_Channels  :",1x,i0)') NLTECoeff%n_NLTE_Channels 
    WRITE(*,'(3x,"n_Channels       :",1x,i0)') NLTECoeff%n_Channels
    IF ( .NOT. NLTECoeff_Associated(NLTECoeff) ) RETURN
    ! Sensor info
    WRITE(*,'(3x,"Sensor_Id        :",1x,a )') TRIM(NLTECoeff%Sensor_Id)
    WRITE(*,'(3x,"WMO_Satellite_ID :",1x,i0)') NLTECoeff%WMO_Satellite_ID 
    WRITE(*,'(3x,"WMO_Sensor_ID    :",1x,i0)') NLTECoeff%WMO_Sensor_ID
    ! Pressure arrays
    WRITE(*,'(3x,"Upper_Plevel :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Upper_Plevel
    WRITE(*,'(3x,"Lower_Plevel :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Lower_Plevel
    ! Temperature arrays
    WRITE(*,'(3x,"Min_Tm  :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Min_Tm
    WRITE(*,'(3x,"Max_Tm  :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Max_Tm
    WRITE(*,'(3x,"Mean_Tm :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Mean_Tm
    ! Coefficient table dimension vectors
    WRITE(*,'(3x,"Secant_Sensor_Zenith :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Secant_Sensor_Zenith
    WRITE(*,'(3x,"Secant_Solar_Zenith  :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%Secant_Solar_Zenith
    WRITE(*,'(3x,"NLTE_Channel         :")')
    WRITE(*,'(10(1x,i5,:))') NLTECoeff%NLTE_Channel
    ! NLTE channel flag
    WRITE(*,'(3x,"NLTE_Channel_Flag :")')
    DO i = 1, NLTECoeff%n_Channels
      IF ( MOD(i,5) == 0 .OR. i == NLTECoeff%n_Channels ) THEN
        maybe = 'yes'
      ELSE
        maybe = 'no'
      END IF
      WRITE(*,FMT='(1x,i5,":",l1,", c-index: ",i0)',ADVANCE=maybe) NLTECoeff%Sensor_Channel(i), &
                                                                   NLTECoeff%Is_NLTE_Channel(i), &
                                                                   NLTECoeff%C_Index(i)
    END DO
    ! Coefficient data
    WRITE(*,'(3x,"NLTE correction coefficients :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTECoeff%C
  END SUBROUTINE NLTECoeff_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_ValidRelease
!
! PURPOSE:
!       Function to check the NLTECoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = NLTECoeff_ValidRelease( NLTECoeff )
!
! INPUTS:
!       NLTECoeff:     NLTECoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       NLTECoeff_type
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

  FUNCTION NLTECoeff_ValidRelease( NLTECoeff ) RESULT( IsValid )
    ! Arguments
    TYPE(NLTECoeff_type), INTENT(IN) :: NLTECoeff
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( NLTECoeff%Release < NLTECOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A NLTECoeff data update is needed. ", &
                  &"NLTECoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  NLTECoeff%Release, NLTECOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( NLTECoeff%Release > NLTECOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A NLTECoeff software update is needed. ", &
                  &"NLTECoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  NLTECoeff%Release, NLTECOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION NLTECoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a NLTECoeff object.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_Info( NLTECoeff, Info )
!
! OBJECTS:
!       NLTECoeff:     NLTECoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       NLTECoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the NLTECoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_Info( NLTECoeff, Info )
    ! Arguments
    TYPE(NLTECoeff_type), INTENT(IN)  :: NLTECoeff
    CHARACTER(*),         INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"NLTECoeff RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_PREDICTORS=",i0,2x,&
           &"N_SENSOR_ANGLES=",i0,2x,&
           &"N_SOLAR_ANGLES=",i0,2x,&
           &"N_NLTE_CHANNELS=",i0,2x,&
           &"N_CHANNELS=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           NLTECoeff%Release, NLTECoeff%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           NLTECoeff%n_Predictors    , &
           NLTECoeff%n_Sensor_Angles , &
           NLTECoeff%n_Solar_Angles  , &
           NLTECoeff%n_NLTE_Channels , &
           NLTECoeff%n_Channels
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE NLTECoeff_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_DefineVersion( Id )
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

  SUBROUTINE NLTECoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE NLTECoeff_DefineVersion



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
!       NLTECoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two NLTECoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = NLTECoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two NLTECoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       NLTECoeff_type
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

  ELEMENTAL FUNCTION NLTECoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(NLTECoeff_type), INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. NLTECoeff_Associated(x)) .OR. &
         (.NOT. NLTECoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Predictors     /= y%n_Predictors    ) .OR. &
         (x%n_Sensor_Angles  /= y%n_Sensor_Angles ) .OR. &
         (x%n_Solar_Angles   /= y%n_Solar_Angles  ) .OR. &
         (x%n_NLTE_Channels  /= y%n_NLTE_Channels ) .OR. &
         (x%n_Channels       /= y%n_Channels      ) ) RETURN
    ! ...Scalars
    IF ( (x%Sensor_Id        /= y%Sensor_Id       ) .OR. &
         (x%WMO_Satellite_ID /= y%WMO_Satellite_ID) .OR. &
         (x%WMO_Sensor_ID    /= y%WMO_Sensor_ID   ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Sensor_Channel           ==    y%Sensor_Channel      ) .AND. &
         ALL(x%Upper_Plevel         .EqualTo. y%Upper_Plevel        ) .AND. &
         ALL(x%Lower_Plevel         .EqualTo. y%Lower_Plevel        ) .AND. &
         ALL(x%Min_Tm               .EqualTo. y%Min_Tm              ) .AND. &
         ALL(x%Max_Tm               .EqualTo. y%Max_Tm              ) .AND. &
         ALL(x%Mean_Tm              .EqualTo. y%Mean_Tm             ) .AND. &
         ALL(x%Secant_Sensor_Zenith .EqualTo. y%Secant_Sensor_Zenith) .AND. &
         ALL(x%Secant_Solar_Zenith  .EqualTo. y%Secant_Solar_Zenith ) .AND. &
         ALL(x%NLTE_Channel             ==    y%NLTE_Channel        ) .AND. &
         ALL(x%Is_NLTE_Channel        .EQV.   y%Is_NLTE_Channel     ) .AND. &
         ALL(x%C_Index                  ==    y%C_Index             ) .AND. &
         ALL(x%C                    .EqualTo. y%C                   ) ) &
      is_equal = .TRUE.

  END FUNCTION NLTECoeff_Equal

END MODULE NLTECoeff_Define
