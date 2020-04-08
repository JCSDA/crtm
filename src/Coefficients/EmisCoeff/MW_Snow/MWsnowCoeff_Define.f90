!
! MWsnowCoeff_Define
!
! Module defining the MWsnowCoeff data structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Jan-2011
!                       paul.vandelst@noaa.gov
!

MODULE MWsnowCoeff_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
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
  PUBLIC :: MWsnowCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: MWsnowCoeff_Associated
  PUBLIC :: MWsnowCoeff_Destroy
  PUBLIC :: MWsnowCoeff_Create
  PUBLIC :: MWsnowCoeff_Inspect
  PUBLIC :: MWsnowCoeff_ValidRelease
  PUBLIC :: MWsnowCoeff_Info
  PUBLIC :: MWsnowCoeff_DefineVersion
  

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE MWsnowCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! MWsnowCoeff init values
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: MWSNOWCOEFF_RELEASE = 0  ! This determines structure and file formats.
  INTEGER, PARAMETER :: MWSNOWCOEFF_VERSION = 1  ! This is just the data version for the release.
  ! Meggage string length
  INTEGER, PARAMETER :: ML = 256


  ! --------------------------------
  ! MWsnowCoeff data type definition
  ! --------------------------------
  !:tdoc+:
  TYPE :: MWsnowCoeff_type
    ! Release and version information
    INTEGER(Long) :: Release = MWSNOWCOEFF_RELEASE
    INTEGER(Long) :: Version = MWSNOWCOEFF_VERSION
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Array dimensions
    INTEGER(Long) :: n_Frequencies  = 0   ! I1 dimension 
    INTEGER(Long) :: n_Temperatures = 0   ! I2 dimension
    INTEGER(Long) :: n_Soil_Types   = 0   ! I3 dimension
    ! LUT dimension vectors
    REAL(Double) , ALLOCATABLE :: Frequency(:)      ! I1
    REAL(Double) , ALLOCATABLE :: Temperature(:)    ! I2
    INTEGER(Long), ALLOCATABLE :: Soil_Type(:)      ! I3 
    CHARACTER(ML), ALLOCATABLE :: Soil_Type_Name(:) ! I3 
    ! The permittivity data
    ! ...Ice permittivity
    REAL(Double), ALLOCATABLE :: re_ice(:,:)        ! I1 x I2
    REAL(Double), ALLOCATABLE :: ie_ice(:,:)        ! I1 x I2
    ! ...Soil permittivity
    REAL(Double), ALLOCATABLE :: re_soil(:,:,:)     ! I1 x I2 x I3
    REAL(Double), ALLOCATABLE :: ie_soil(:,:,:)     ! I1 x I2 x I3
  END TYPE MWsnowCoeff_type
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
!       MWsnowCoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a MWsnowCoeff object.
!
! CALLING SEQUENCE:
!       Status = MWsnowCoeff_Associated( MWsnowCoeff )
!
! OBJECTS:
!       MWsnowCoeff: MWsnowCoeff object which is to have its member's
!                    status tested.
!                    UNITS:      N/A
!                    TYPE:       TYPE(MWsnowCoeff_type)
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:      The return value is a logical value indicating the
!                    status of the MWsnowCoeff members.
!                    .TRUE.  - if ANY of the MWsnowCoeff allocatable or
!                              pointer members are in use.
!                    .FALSE. - if ALL of the MWsnowCoeff allocatable or
!                              pointer members are not in use.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input MWsnowCoeff argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION MWsnowCoeff_Associated( MWsnowCoeff ) RESULT( Status )
    TYPE(MWsnowCoeff_type), INTENT(IN) :: MWsnowCoeff
    LOGICAL :: Status
    Status = MWsnowCoeff%Is_Allocated
  END FUNCTION MWsnowCoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWsnowCoeff_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize MWsnowCoeff objects.
!
! CALLING SEQUENCE:
!       CALL MWsnowCoeff_Destroy( MWsnowCoeff )
!
! OBJECTS:
!       MWsnowCoeff:   Re-initialized MWsnowCoeff object.
!                     UNITS:      N/A
!                     TYPE:       TYPE(MWsnowCoeff_type)
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MWsnowCoeff_Destroy( MWsnowCoeff )
    TYPE(MWsnowCoeff_type), INTENT(OUT) :: MWsnowCoeff
    MWsnowCoeff%Is_Allocated = .FALSE.
    MWsnowCoeff%n_Frequencies  = 0
    MWsnowCoeff%n_Temperatures = 0
    MWsnowCoeff%n_Soil_Types   = 0
  END SUBROUTINE MWsnowCoeff_Destroy
  

!--------------------------------------------------------------------------------
!
! NAME:
!       MWsnowCoeff_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of a MWsnowCoeff object.
!
! CALLING SEQUENCE:
!       CALL MWsnowCoeff_Create( MWsnowCoeff   , &
!                                n_Frequencies , &
!                                n_Temperatures, &
!                                n_Soil_Types    )
!
! OBJECTS:
!       MWsnowCoeff:       MWsnowCoeff object.
!                          UNITS:      N/A
!                          TYPE:       TYPE(MWsnowCoeff_type)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Frequencies:     The number of frequencies in the look-up
!                          table (LUT). The "I1" dimension.
!                          Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Temperatures:    The number of temperatures in the LUT. 
!                          The "I2" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Soil_Types:      The number of soil types for which there is
!                          soil permittivity data in the LUT. 
!                          The "I3" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MWsnowCoeff_Create( &
    MWsnowCoeff   , &
    n_Frequencies , &
    n_Temperatures, &
    n_Soil_Types    )
    ! Arguments
    TYPE(MWsnowCoeff_type), INTENT(OUT) :: MWsnowCoeff
    INTEGER,                INTENT(IN)  :: n_Frequencies 
    INTEGER,                INTENT(IN)  :: n_Temperatures
    INTEGER,                INTENT(IN)  :: n_Soil_Types  
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MWsnowCoeff_Create'
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Frequencies  < 1 .OR. &
         n_Temperatures < 1 .OR. &
         n_Soil_Types   < 1 ) RETURN

    ! Perform the allocations.
    ALLOCATE( MWsnowCoeff%Frequency(n_Frequencies), &
              MWsnowCoeff%Temperature(n_Temperatures), &
              MWsnowCoeff%Soil_Type(n_Soil_Types), &
              MWsnowCoeff%Soil_Type_Name(n_Soil_Types), &
              MWsnowCoeff%re_ice(n_Frequencies, n_Temperatures), &
              MWsnowCoeff%ie_ice(n_Frequencies, n_Temperatures), &
              MWsnowCoeff%re_soil(n_Frequencies, n_Temperatures, n_Soil_Types), &
              MWsnowCoeff%ie_soil(n_Frequencies, n_Temperatures, n_Soil_Types), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    MWsnowCoeff%n_Frequencies  = n_Frequencies 
    MWsnowCoeff%n_Temperatures = n_Temperatures
    MWsnowCoeff%n_Soil_Types   = n_Soil_Types  
    ! ...Arrays
    MWsnowCoeff%Frequency      = ZERO
    MWsnowCoeff%Temperature    = ZERO
    MWsnowCoeff%Soil_Type      = 0
    MWsnowCoeff%Soil_Type_Name = ''
    MWsnowCoeff%re_ice         = ZERO
    MWsnowCoeff%ie_ice         = ZERO
    MWsnowCoeff%re_soil        = ZERO
    MWsnowCoeff%ie_soil        = ZERO


    ! Set allocationindicator
    MWsnowCoeff%Is_Allocated = .TRUE.

  END SUBROUTINE MWsnowCoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWsnowCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a MWsnowCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL MWsnowCoeff_Inspect( MWsnowCoeff )
!
! INPUTS:
!       MWsnowCoeff:    MWsnowCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       TYPE(MWsnowCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MWsnowCoeff_Inspect( MWsnowCoeff )
    TYPE(MWsnowCoeff_type), INTENT(IN) :: MWsnowCoeff
    INTEGER :: i
    WRITE(*,'(1x,"MWsnowCoeff OBJECT")')
    WRITE(*,'(3x,"n_Frequencies  :",1x,i0)') MWsnowCoeff%n_Frequencies 
    WRITE(*,'(3x,"n_Temperatures :",1x,i0)') MWsnowCoeff%n_Temperatures
    WRITE(*,'(3x,"n_Soil_Types   :",1x,i0)') MWsnowCoeff%n_Soil_Types  
    IF ( .NOT. MWsnowCoeff_Associated(MWsnowCoeff) ) RETURN
    WRITE(*,'(3x,"Frequency     :")')
    WRITE(*,'(5(1x,es13.6,:))') MWsnowCoeff%Frequency     
    WRITE(*,'(3x,"Temperature   :")')
    WRITE(*,'(5(1x,es13.6,:))') MWsnowCoeff%Temperature   
    WRITE(*,'(3x,"Soil Type Name:")')
    DO i = 1, MWsnowCoeff%n_Soil_Types
      WRITE(*,'(5x,i2," : ",a))') MWsnowCoeff%Soil_Type(i), TRIM(MWsnowCoeff%Soil_Type_Name(i))
    END DO
    WRITE(*,'(3x,"Real(ice permittivity) :")')
    WRITE(*,'(5(1x,es13.6,:))') MWsnowCoeff%re_ice        
    WRITE(*,'(3x,"Imag(ice permittivity) :")')
    WRITE(*,'(5(1x,es13.6,:))') MWsnowCoeff%ie_ice        
    WRITE(*,'(3x,"Real(soil permittivity) :")')
    WRITE(*,'(5(1x,es13.6,:))') MWsnowCoeff%re_soil       
    WRITE(*,'(3x,"Imag(soil permittivity) :")')
    WRITE(*,'(5(1x,es13.6,:))') MWsnowCoeff%ie_soil       
  END SUBROUTINE MWsnowCoeff_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWsnowCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the MWsnowCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = MWsnowCoeff_ValidRelease( MWsnowCoeff )
!
! INPUTS:
!       MWsnowCoeff:   MWsnowCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(MWsnowCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION MWsnowCoeff_ValidRelease( MWsnowCoeff ) RESULT( IsValid )
    ! Arguments
    TYPE(MWsnowCoeff_type), INTENT(IN) :: MWsnowCoeff
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MWsnowCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( MWsnowCoeff%Release < MWSNOWCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A MWsnowCoeff data update is needed. ", &
                  &"MWsnowCoeff release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  MWsnowCoeff%Release, MWSNOWCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( MWsnowCoeff%Release > MWSNOWCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A MWsnowCoeff software update is needed. ", &
                  &"MWsnowCoeff release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  MWsnowCoeff%Release, MWSNOWCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION MWsnowCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWsnowCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a MWsnowCoeff object.
!
! CALLING SEQUENCE:
!       CALL MWsnowCoeff_Info( MWsnowCoeff, Info )
!
! INPUTS:
!       MWsnowCoeff:   MWsnowCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       TYPE(MWsnowCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the passed MWsnowCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MWsnowCoeff_Info( MWsnowCoeff, Info )
    ! Arguments
    TYPE(MWsnowCoeff_type), INTENT(IN)  :: MWsnowCoeff
    CHARACTER(*),       INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '( a,1x,"MWsnowCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
           &"N_FREQUENCIES=",i0,2x,&
           &"N_TEMPERATURES=",i0,2x,&
           &"N_SOIL_TYPES=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           MWsnowCoeff%Release, MWsnowCoeff%Version, &
           MWsnowCoeff%n_Frequencies , &
           MWsnowCoeff%n_Temperatures, &
           MWsnowCoeff%n_Soil_Types  
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE MWsnowCoeff_Info
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWsnowCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL MWsnowCoeff_DefineVersion( Id )
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

  SUBROUTINE MWsnowCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE MWsnowCoeff_DefineVersion




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
!       MWsnowCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two MWsnowCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = MWsnowCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two MWsnowCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       TYPE(MWsnowCoeff_type)
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

  ELEMENTAL FUNCTION MWsnowCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(MWsnowCoeff_type), INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
    
    ! Check the object association status
    IF ( (.NOT. MWsnowCoeff_Associated(x)) .OR. &
         (.NOT. MWsnowCoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (x%n_Frequencies  /= y%n_Frequencies ) .OR. &
         (x%n_Temperatures /= y%n_Temperatures) .OR. &
         (x%n_Soil_Types   /= y%n_Soil_Types  ) ) RETURN
    ! ...Data
    IF ( ALL(x%Frequency      .EqualTo. y%Frequency     ) .AND. &
         ALL(x%Temperature    .EqualTo. y%Temperature   ) .AND. &
         ALL(x%Soil_Type          ==    y%Soil_Type     ) .AND. &
         ALL(x%Soil_Type_Name     ==    y%Soil_Type_Name) .AND. &
         ALL(x%re_ice         .EqualTo. y%re_ice        ) .AND. &
         ALL(x%ie_ice         .EqualTo. y%ie_ice        ) .AND. &
         ALL(x%re_soil        .EqualTo. y%re_soil       ) .AND. &
         ALL(x%ie_soil        .EqualTo. y%ie_soil       ) ) &
      is_equal = .TRUE.

  END FUNCTION MWsnowCoeff_Equal
  
END MODULE MWsnowCoeff_Define
