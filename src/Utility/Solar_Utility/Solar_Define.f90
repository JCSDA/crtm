!
! Solar_Define
!
! Module defining the Solar data structure and containing routines to 
! manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2002
!                       paul.vandelst@noaa.gov
!

MODULE Solar_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,            ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: Solar_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: Solar_Associated
  PUBLIC :: Solar_Destroy
  PUBLIC :: Solar_Create
  PUBLIC :: Solar_Frequency
  PUBLIC :: Solar_Inspect
  PUBLIC :: Solar_ValidRelease
  PUBLIC :: Solar_Info
  PUBLIC :: Solar_DefineVersion


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE Solar_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512
  ! Sensor id string length
  INTEGER, PARAMETER :: SL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: SOLAR_RELEASE = 2
  INTEGER, PARAMETER :: SOLAR_VERSION = 1
  ! Physical constants
  ! ...Default Solar blackbody temperature in KELVIN
  REAL(Double), PARAMETER :: DEFAULT_BLACKBODY_TEMPERATURE = 5783.0_Double
  ! ...Default mean Solar radius in METRES
  REAL(Double), PARAMETER :: DEFAULT_RADIUS = 6.599e+08_Double
  ! ...Default mean Earth-Solar distance in METRES
  REAL(Double), PARAMETER :: DEFAULT_EARTH_SUN_DISTANCE = 1.495979e+11_Double


  ! --------------------------
  ! Solar data type definition
  ! --------------------------
  TYPE :: Solar_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = SOLAR_RELEASE
    INTEGER(Long) :: Version = SOLAR_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Frequencies = 0
    ! Scalar components
    REAL(Double) :: Blackbody_Temperature = DEFAULT_BLACKBODY_TEMPERATURE
    REAL(Double) :: Radius                = DEFAULT_RADIUS
    REAL(Double) :: Earth_Sun_Distance    = DEFAULT_EARTH_SUN_DISTANCE
    REAL(Double) :: f1 = ZERO
    REAL(Double) :: f2 = ZERO
    ! Array components
    REAL(Double), ALLOCATABLE :: Frequency(:)           
    REAL(Double), ALLOCATABLE :: Irradiance(:)          
    REAL(Double), ALLOCATABLE :: Blackbody_Irradiance(:)
  END TYPE Solar_type


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
!       Solar_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the Solar structure.
!
! CALLING SEQUENCE:
!       Status = Solar_Associated( Solar )
!
! OBJECTS:
!       Solar:      Structure which is to have its member's
!                   status tested.
!                   UNITS:      N/A
!                   TYPE:       Solar_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the Solar members.
!                    .TRUE.  - if ANY of the Solar allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the Solar allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Solar_Associated( Solar ) RESULT( Status )
    TYPE(Solar_type), INTENT(IN) :: Solar
    LOGICAL :: Status
    Status = Solar%Is_Allocated             
  END FUNCTION Solar_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize Solar objects.
!
! CALLING SEQUENCE:
!       CALL Solar_Destroy( Solar )
!
! OBJECTS:
!       Solar:        Re-initialized Solar structure.
!                     UNITS:      N/A
!                     TYPE:       Solar_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Solar_Destroy( Solar )
    TYPE(Solar_type), INTENT(OUT) :: Solar
    Solar%Is_Allocated = .FALSE.
    Solar%n_Frequencies = 0
    Solar%Blackbody_Temperature = DEFAULT_BLACKBODY_TEMPERATURE
    Solar%Radius                = DEFAULT_RADIUS
    Solar%Earth_Sun_Distance    = DEFAULT_EARTH_SUN_DISTANCE
    Solar%f1 = ZERO
    Solar%f2 = ZERO
  END SUBROUTINE Solar_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of an Solar object.
!
! CALLING SEQUENCE:
!       CALL Solar_Create( Solar, n_Frequencies )
!
! OBJECTS:
!       Solar:              Solar object structure.
!                           UNITS:      N/A
!                           TYPE:       Solar_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Frequencies:      Size of the solar spectral arrays.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Solar_Create( &
    Solar        , &  ! Output
    n_Frequencies  )  ! Input
    ! Arguments
    TYPE(Solar_type), INTENT(OUT) :: Solar
    INTEGER         , INTENT(IN)  :: n_Frequencies
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Frequencies < 1 ) RETURN
    
    ! Perform the allocation
    ALLOCATE( Solar%Frequency( n_Frequencies ), &
              Solar%Irradiance( n_Frequencies ), &
              Solar%Blackbody_Irradiance( n_Frequencies ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    Solar%n_Frequencies = n_Frequencies
    ! ...Arrays
    Solar%Frequency            = ZERO
    Solar%Irradiance           = ZERO
    Solar%Blackbody_Irradiance = ZERO


    ! Set allocation indicator
    Solar%Is_Allocated = .TRUE.

  END SUBROUTINE Solar_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_Frequency
! 
! PURPOSE:
!       Elemental subroutine to compute a regular frequency grid for the
!       solar data object.
!
! CALLING SEQUENCE:
!       CALL Solar_Frequency( Solar          , &
!                             Begin_Frequency, &
!                             End_Frequency    )
!
! OBJECTS:
!       Solar:              Solar object structure which is to have its
!                           frequency component modified.
!                           UNITS:      N/A
!                           TYPE:       Solar_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Begin_Frequency:    The begin frequency of the irradiance spectra.
!                           UNITS:      Inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       End_Frequency:      The end frequency of the irradiance spectra.
!                           UNITS:      Inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Solar_Frequency( &
    Solar             , &  ! In/Output
    Begin_Frequency   , &  ! Input
    End_Frequency       )  ! Input
    ! Arguments
    TYPE(Solar_type), INTENT(IN OUT) :: Solar
    REAL(fp)        , INTENT(IN)     :: Begin_Frequency    
    REAL(fp)        , INTENT(IN)     :: End_Frequency     
    ! Local variables
    INTEGER :: i, n

    ! Setup
    IF ( (.NOT. Solar_Associated( Solar ) ) .OR. &
         (End_Frequency <= Begin_Frequency) ) RETURN
    n = Solar%n_Frequencies
    Solar%f1 = REAL(Begin_Frequency,Double)
    Solar%f2 = REAL(End_Frequency  ,Double)
    
    ! Compute the grid
    Solar%Frequency = (/(REAL(i-1,Double), i=1,n)/) / REAL(n-1,Double)
    Solar%Frequency = Solar%f1 + Solar%Frequency * (Solar%f2 - Solar%f1)

  END SUBROUTINE Solar_Frequency


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a Solar object to stdout.
!
! CALLING SEQUENCE:
!       CALL Solar_Inspect( Solar )
!
! OBJECTS:
!       Solar:         Solar object to display.
!                      UNITS:      N/A
!                      TYPE:       Solar_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Solar_Inspect( Solar )
    TYPE(Solar_type), INTENT(IN) :: Solar
    WRITE(*,'(1x,"Solar OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version           :",1x,i0,".",i0)') Solar%Release, Solar%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Frequencies             :",1x,i0)') Solar%n_Frequencies
    IF ( .NOT. Solar_Associated(Solar) ) RETURN
    ! Scalar info
    WRITE(*,'(3x,"Blackbody Temperature (K) :",1x,es13.6)') Solar%Blackbody_Temperature
    WRITE(*,'(3x,"Radius (m)                :",1x,es13.6)') Solar%Radius               
    WRITE(*,'(3x,"Earth-Sun Distance (m)    :",1x,es13.6)') Solar%Earth_Sun_Distance   
    WRITE(*,'(3x,"Begin Frequency (cm^-1)   :",1x,es13.6)') Solar%f1                   
    WRITE(*,'(3x,"End Frequency (cm^-1)     :",1x,es13.6)') Solar%f2
    ! Data arrays
    WRITE(*,'(3x,"Frequency :")')    
    WRITE(*,'(5(1x,es13.6,:))') Solar%Frequency           
    WRITE(*,'(3x,"Irradiance :")')    
    WRITE(*,'(5(1x,es13.6,:))') Solar%Irradiance          
    WRITE(*,'(3x,"Blackbody Irradiance :")')    
    WRITE(*,'(5(1x,es13.6,:))') Solar%Blackbody_Irradiance
    
  END SUBROUTINE Solar_Inspect
  
  
!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_ValidRelease
!
! PURPOSE:
!       Function to check the Solar Release value.
!
! CALLING SEQUENCE:
!       IsValid = Solar_ValidRelease( Solar )
!
! INPUTS:
!       Solar:         Solar object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       Solar_type
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

  FUNCTION Solar_ValidRelease( Solar ) RESULT( IsValid )
    ! Arguments
    TYPE(Solar_type), INTENT(IN) :: Solar
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Solar_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( Solar%Release < SOLAR_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An Solar data update is needed. ", &
                  &"Solar release is ",i0,". Valid release is ",i0,"." )' ) &
                  Solar%Release, SOLAR_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( Solar%Release > SOLAR_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An Solar software update is needed. ", &
                  &"Solar release is ",i0,". Valid release is ",i0,"." )' ) &
                  Solar%Release, SOLAR_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION Solar_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a Solar object.
!
! CALLING SEQUENCE:
!       CALL Solar_Info( Solar, Info )
!
! OBJECTS:
!       Solar:         Solar object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       Solar_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the Solar object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Solar_Info( Solar, Info )
    ! Arguments
    TYPE(Solar_type), INTENT(IN)  :: Solar
    CHARACTER(*),     INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"Solar RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_FREQUENCIES=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           Solar%Release, Solar%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           Solar%n_Frequencies
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE Solar_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL Solar_DefineVersion( Id )
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

  SUBROUTINE Solar_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE Solar_DefineVersion


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
!       Solar_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two Solar objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = Solar_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two Solar objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       Solar_type
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

  ELEMENTAL FUNCTION Solar_Equal( x, y ) RESULT( is_equal )
    TYPE(Solar_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. Solar_Associated(x)) .OR. &
         (.NOT. Solar_Associated(y))      ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Frequencies /= y%n_Frequencies ) ) RETURN
    ! ...Data
    IF ( (x%Blackbody_Temperature .EqualTo. y%Blackbody_Temperature ) .AND. &
         (x%Radius                .EqualTo. y%Radius                ) .AND. &
         (x%Earth_Sun_Distance    .EqualTo. y%Earth_Sun_Distance    ) .AND. &
         (x%f1                    .EqualTo. y%f1                    ) .AND. &
         (x%f2                    .EqualTo. y%f2                    ) .AND. &
         ALL(x%Frequency            .EqualTo. y%Frequency            ) .AND. &
         ALL(x%Irradiance           .EqualTo. y%Irradiance           ) .AND. &
         ALL(x%Blackbody_Irradiance .EqualTo. y%Blackbody_Irradiance ) ) &
      is_equal = .TRUE.

  END FUNCTION Solar_Equal

END MODULE Solar_Define
