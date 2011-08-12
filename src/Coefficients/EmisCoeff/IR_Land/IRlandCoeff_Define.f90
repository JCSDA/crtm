!
! IRlandCoeff_Define
!
! Module defining the IRlandCoeff container object to hold
! coefficient data for the infrared land surface emissivity
! and reflectivity models.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Aug-2011
!                       paul.vandelst@noaa.gov
 
MODULE IRlandCoeff_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE IRLSE_NPOESS_Define  , ONLY: IRLSE_NPOESS_type         , &
                                   OPERATOR(==)              , &      
                                   IRLSE_NPOESS_Associated   , & 
                                   IRLSE_NPOESS_Destroy      , & 
                                   IRLSE_NPOESS_Create       , & 
                                   IRLSE_NPOESS_Inspect      , & 
                                   IRLSE_NPOESS_ValidRelease , & 
                                   IRLSE_NPOESS_Info         , & 
                                   IRLSE_NPOESS_DefineVersion, & 
                                   IRLSE_NPOESS_SetValue     , &
                                   IRLSE_NPOESS_GetValue
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: IRlandCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: IRlandCoeff_Associated
  PUBLIC :: IRlandCoeff_Destroy
  PUBLIC :: IRlandCoeff_Create
  PUBLIC :: IRlandCoeff_Inspect
  PUBLIC :: IRlandCoeff_ValidRelease
  PUBLIC :: IRlandCoeff_Info
  PUBLIC :: IRlandCoeff_DefineVersion
  PUBLIC :: IRlandCoeff_SetValue
  PUBLIC :: IRlandCoeff_GetValue
  ! ...Inherited datatypes
  PUBLIC :: IRLSE_NPOESS_type
  ! ...Inherited procedures
  PUBLIC :: IRLSE_NPOESS_Associated   
  PUBLIC :: IRLSE_NPOESS_Destroy      
  PUBLIC :: IRLSE_NPOESS_Create       
  PUBLIC :: IRLSE_NPOESS_Inspect      
  PUBLIC :: IRLSE_NPOESS_ValidRelease 
  PUBLIC :: IRLSE_NPOESS_Info         
  PUBLIC :: IRLSE_NPOESS_DefineVersion
  PUBLIC :: IRLSE_NPOESS_SetValue
  PUBLIC :: IRLSE_NPOESS_GetValue

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE IRlandCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  ! Current valid release and version
  INTEGER, PARAMETER :: IRLANDCOEFF_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: IRLANDCOEFF_VERSION = 1  ! This is just the default data version.


  ! ----------------------------------
  ! IRlandCoeff data type definitions
  ! ----------------------------------
  TYPE :: IRlandCoeff_type
    PRIVATE
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = IRLANDCOEFF_RELEASE
    INTEGER(Long) :: Version = IRLANDCOEFF_VERSION
    ! Derived type components
    TYPE(IRLSE_NPOESS_type) :: NPOESS
  END TYPE IRlandCoeff_type


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
!       IRlandCoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the IRlandCoeff structure.
!
! CALLING SEQUENCE:
!       Status = IRlandCoeff_Associated( IRlandCoeff )
!
! OBJECTS:
!       IRlandCoeff:   Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       IRlandCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the NLTE members.
!                    .TRUE.  - if ANY of the IRlandCoeff allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the IRlandCoeff allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION IRlandCoeff_Associated( self ) RESULT( Status )
    TYPE(IRlandCoeff_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION IRlandCoeff_Associated

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize IRlandCoeff objects.
!
! CALLING SEQUENCE:
!       CALL IRlandCoeff_Destroy( IRlandCoeff )
!
! OBJECTS:
!       IRlandCoeff: Re-initialized IRlandCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       IRlandCoeff_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE IRlandCoeff_Destroy( self )
    TYPE(IRlandCoeff_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE IRlandCoeff_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an IRlandCoeff object.
!
! CALLING SEQUENCE:
!       CALL IRlandCoeff_Create( IRlandCoeff )         
!
! OBJECTS:
!       IRlandCoeff:        IRlandCoeff object structure.
!                           UNITS:      N/A
!                           TYPE:       IRlandCoeff_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE IRlandCoeff_Create( &
    self )         ! Output
    ! Arguments
    TYPE(IRlandCoeff_type), INTENT(OUT) :: self
    ! Set allocation indicator
    self%Is_Allocated = .TRUE.
  END SUBROUTINE IRlandCoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a IRlandCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL IRlandCoeff_Inspect( IRlandCoeff )
!
! OBJECTS:
!       IRlandCoeff:   IRlandCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       IRlandCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRlandCoeff_Inspect( self )
    TYPE(IRlandCoeff_type), INTENT(IN) :: self
    WRITE(*,'(1x,"IRlandCoeff OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version  :",1x,i0,".",i0)') self%Release, self%Version
    ! Derived types
    IF ( IRLSE_NPOESS_Associated( self%NPOESS ) ) CALL IRLSE_NPOESS_Inspect( self%NPOESS )
  END SUBROUTINE IRlandCoeff_Inspect



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the IRlandCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = IRlandCoeff_ValidRelease( IRlandCoeff )
!
! INPUTS:
!       IRlandCoeff:   IRlandCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       IRlandCoeff_type
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

  FUNCTION IRlandCoeff_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(IRlandCoeff_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRlandCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < IRLANDCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An IRlandCoeff data update is needed. ", &
                  &"IRlandCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, IRLANDCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > IRLANDCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An IRlandCoeff software update is needed. ", &
                  &"IRlandCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, IRLANDCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION IRlandCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a IRlandCoeff object.
!
! CALLING SEQUENCE:
!       CALL IRlandCoeff_Info( IRlandCoeff, Info )
!
! OBJECTS:
!       IRlandCoeff:   IRlandCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       IRlandCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the IRlandCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRlandCoeff_Info( self, Info )
    ! Arguments
    TYPE(IRlandCoeff_type), INTENT(IN)  :: self
    CHARACTER(*),            INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"IRlandCoeff RELEASE.VERSION: ",i2,".",i2.2 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE IRlandCoeff_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL IRlandCoeff_DefineVersion( Id )
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

  SUBROUTINE IRlandCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE IRlandCoeff_DefineVersion



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_SetValue
!
! PURPOSE:
!       Subroutine to set the contents of a valid IRlandCoeff object.
!
! CALLING SEQUENCE:
!       CALL IRlandCoeff_SetValue( IRlandCoeff, &
!                                  NPOESS = NPOESS )
!
! OBJECTS:
!       IRlandCoeff:        Valid, allocated IRlandCoeff object for which
!                           values are to be set.
!                           UNITS:      N/A
!                           TYPE:       IRlandCoeff_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       NPOESS:             Object containing the NPOESS land surface emissivity and
!                           refelctivity data.
!                           UNITS:      N/A
!                           TYPE:       IRLSE_NPOESS_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRlandCoeff_SetValue( &
    self  , &  ! Input
    NPOESS  )  ! Optional input
    ! Arguments
    TYPE(IRlandCoeff_type)           , INTENT(IN OUT) :: self
    TYPE(IRLSE_NPOESS_type), OPTIONAL, INTENT(IN)     :: NPOESS

    IF ( PRESENT(NPOESS) ) self%NPOESS = NPOESS

  END SUBROUTINE IRlandCoeff_SetValue
 

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_GetValue
!
! PURPOSE:
!       Subroutine to get the contents of a valid IRlandCoeff object.
!
! CALLING SEQUENCE:
!       CALL IRlandCoeff_GetValue( IRlandCoeff, &
!                                  NPOESS = NPOESS )
!
! OBJECTS:
!       IRlandCoeff:  Valid IRlandCoeff object from which values are
!                     to be retrieved.
!                     UNITS:      N/A
!                     TYPE:       IRlandCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUTS:
!       NPOESS:       Object containing the NPOESS land surface emissivity and
!                     refelctivity data.
!                     UNITS:      N/A
!                     TYPE:       IRLSE_NPOESS_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRlandCoeff_GetValue( &
    self  , &  ! Input
    NPOESS  )  ! Optional output
    ! Arguments
    TYPE(IRlandCoeff_type)           , INTENT(IN)  :: self
    TYPE(IRLSE_NPOESS_type), OPTIONAL, INTENT(OUT) :: NPOESS
   
    IF ( PRESENT(NPOESS) ) NPOESS = self%NPOESS

  END SUBROUTINE IRlandCoeff_GetValue
 

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
!       IRlandCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two IRlandCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = IRlandCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two IRlandCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       IRlandCoeff_type
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

  ELEMENTAL FUNCTION IRlandCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(IRlandCoeff_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Structures
    IF ( IRLSE_NPOESS_Associated( x%NPOESS ) .NEQV. IRLSE_NPOESS_Associated( y%NPOESS ) ) RETURN
    IF ( IRLSE_NPOESS_Associated( x%NPOESS ) .AND.  IRLSE_NPOESS_Associated( y%NPOESS ) ) THEN
      IF ( .NOT. (x%NPOESS == y%NPOESS) ) RETURN
    END IF
    
    is_equal = .TRUE.
    
  END FUNCTION IRlandCoeff_Equal

END MODULE IRlandCoeff_Define
