!
! GrELS_Define
!
! Module defining the data structure for the GrELS (Greenness-adjusted
! Emissivity for Land Surface) infrared land surface emissivity object
! and containing routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, paul.vandelst@noaa.gov
!                       Ron Vogel,      ronald.vogel@noaa.gov
!                       11-Feb-2010
!

MODULE GrELS_Define


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: GrELS_GFS
  PUBLIC :: GrELS_IGBP
  PUBLIC :: GrELS_TYPE_NAME
  ! Datatypes
  PUBLIC :: GrELS_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: GrELS_Associated
  PUBLIC :: GrELS_Destroy
  PUBLIC :: GrELS_Create
  PUBLIC :: GrELS_Inspect
  PUBLIC :: GrELS_ValidRelease
  PUBLIC :: GrELS_Info
  PUBLIC :: GrELS_DefineVersion
  

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE GrELS_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: GrELS_RELEASE = 1  ! This determines object and file formats.
  INTEGER, PARAMETER :: GrELS_VERSION = 1  ! This is just the data version for the release.
  ! GrELS datatypes
  INTEGER, PARAMETER :: N_GrELS_TYPES = 2
  INTEGER, PARAMETER :: GrELS_INVALID = 0
  INTEGER, PARAMETER :: GrELS_GFS     = 1
  INTEGER, PARAMETER :: GrELS_IGBP    = 2
  CHARACTER(*), PARAMETER :: GrELS_TYPE_NAME(0:N_GrELS_TYPES) = (/'Invalid', 'GFS    ', 'IGBP   '/)
  ! String lengths
  INTEGER, PARAMETER :: SL = 80
  INTEGER, PARAMETER :: ML = 256
  ! Literals
  REAL(fp), PARAMETER :: ZERO = 0.0_fp


  ! --------------------------
  ! GrELS data type definition
  ! --------------------------
  !:tdoc+:
  TYPE :: GrELS_type
    ! Release and version information
    INTEGER :: Release = GrELS_RELEASE
    INTEGER :: Version = GrELS_VERSION
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimensions
    INTEGER :: n_Wavelengths    = 0  ! L dim.
    INTEGER :: n_Surface_Types  = 0  ! N dim.
    INTEGER :: n_Weeks          = 0  ! P dim.
    INTEGER :: n_Latitude_Zones = 0  ! Q dim.
    INTEGER :: stsl = SL ! Surface type name string length
    ! Datatype indicator
    INTEGER :: Type = GrELS_INVALID
    ! Dimension vectors
    REAL(fp),      ALLOCATABLE  :: Wavelength(:)         ! Lx1
    CHARACTER(SL), ALLOCATABLE  :: Surface_Type_Name(:)  ! Nx1
    INTEGER,       ALLOCATABLE  :: Week(:)               ! Px1
    REAL(fp),      ALLOCATABLE  :: Latitude_Zone(:)      ! Qx1
    ! Reflectance LUT data
    REAL(fp),      ALLOCATABLE  :: Reflectance(:,:)      ! LxN
    ! Green vegetation fraction LUT data
    REAL(fp),      ALLOCATABLE  :: GVF(:,:,:)            ! NxPxQ
  END TYPE GrELS_type
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
!       GrELS_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a GrELS object.
!
! CALLING SEQUENCE:
!       Status = GrELS_Associated( GrELS )
!
! OBJECTS:
!       GrELS:   GrELS object which is to have its member's
!                status tested.
!                UNITS:      N/A
!                TYPE:       GrELS_type
!                DIMENSION:  Scalar or any rank
!                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of the GrELS members.
!                .TRUE.  - if ANY of the GrELS allocatable or
!                          pointer members are in use.
!                .FALSE. - if ALL of the GrELS allocatable or
!                          pointer members are not in use.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Same as input GrELS argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION GrELS_Associated( GrELS ) RESULT( Status )
    TYPE(GrELS_type), INTENT(IN) :: GrELS
    LOGICAL :: Status
    Status = GrELS%Is_Allocated
  END FUNCTION GrELS_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize GrELS objects.
!
! CALLING SEQUENCE:
!       CALL GrELS_Destroy( GrELS )
!
! OBJECTS:
!       GrELS:   Re-initialized GrELS object.
!                UNITS:      N/A
!                TYPE:       GrELS_type
!                DIMENSION:  Scalar OR any rank
!                ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE GrELS_Destroy( GrELS )
    TYPE(GrELS_type), INTENT(OUT) :: GrELS
    GrELS%Is_Allocated = .FALSE.
    GrELS%n_Wavelengths    = 0
    GrELS%n_Surface_Types  = 0
    GrELS%n_Weeks          = 0
    GrELS%n_Latitude_Zones = 0
  END SUBROUTINE GrELS_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the GrELS object.
!
! CALLING SEQUENCE:
!       CALL GrELS_Create( GrELS, &
!                          n_Wavelengths   , &
!                          n_Surface_Types , &
!                          n_Weeks         , &
!                          n_Latitude_Zones )
!
! OBJECTS:
!       GrELS:        GrELS object.
!                     UNITS:      N/A
!                     TYPE:       GrELS_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Wavelengths:    Number of wavelengths for which the reflectance
!                         LUT data is specified. 
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar or same as GrELS object
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Surface_Types:  Number of surface types for which the reflectance
!                         LUT data is specified. 
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar or same as GrELS object
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Weeks:          Number of weeks for which the green vegetations
!                         fraction LUT data is specified. 
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar or same as GrELS object
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Latitude_Zones: Number of latitude zones for which the green
!                         vegetation fraction LUT data is specified. 
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar or same as GrELS object
!                         ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE GrELS_Create( &
    GrELS           , &
    n_Wavelengths   , &
    n_Surface_Types , &
    n_Weeks         , &
    n_Latitude_Zones  )
    ! Arguments
    TYPE(GrELS_type), INTENT(OUT) :: GrELS
    INTEGER,          INTENT(IN)  :: n_Wavelengths   
    INTEGER,          INTENT(IN)  :: n_Surface_Types 
    INTEGER,          INTENT(IN)  :: n_Weeks         
    INTEGER,          INTENT(IN)  :: n_Latitude_Zones
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Wavelengths    < 1 .OR. &
         n_Surface_Types  < 1 .OR. &
         n_Weeks          < 1 .OR. &
         n_Latitude_Zones < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( GrELS%Wavelength( n_Wavelengths ), &
              GrELS%Surface_Type_Name( n_Surface_Types ), &
              GrELS%Week( n_Weeks ), &
              GrELS%Latitude_Zone( n_Latitude_Zones ), &
              GrELS%Reflectance( n_Wavelengths, n_Surface_Types ), &
              GrELS%GVF( n_Surface_Types, n_Weeks, n_Latitude_Zones ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    GrELS%n_Wavelengths    = n_Wavelengths   
    GrELS%n_Surface_Types  = n_Surface_Types 
    GrELS%n_Weeks          = n_Weeks         
    GrELS%n_Latitude_Zones = n_Latitude_Zones
    ! ...Arrays
    GrELS%Wavelength        = ZERO
    GrELS%Surface_Type_Name = ''
    GrELS%Week              = 0
    GrELS%Latitude_Zone     = ZERO
    GrELS%Reflectance       = ZERO
    GrELS%GVF               = ZERO

    ! Set allocation indicator
    GrELS%Is_Allocated = .TRUE.
        
  END SUBROUTINE GrELS_Create

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a GrELS object to stdout.
!
! CALLING SEQUENCE:
!       CALL GrELS_Inspect( GrELS )
!
! INPUTS:
!       GrELS:    GrELS object to display.
!                 UNITS:      N/A
!                 TYPE:       GrELS_type
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE GrELS_Inspect( GrELS )
    TYPE(GrELS_type), INTENT(IN) :: GrELS
    INTEGER :: i, j, k, lType

    WRITE(*,'(1x,"GrELS OBJECT")')
    ! Dimensions
    WRITE(*,'(3x,"n_Wavelengths    :",1x,i0)') GrELS%n_Wavelengths   
    WRITE(*,'(3x,"n_Surface_Types  :",1x,i0)') GrELS%n_Surface_Types 
    WRITE(*,'(3x,"n_Weeks          :",1x,i0)') GrELS%n_Weeks         
    WRITE(*,'(3x,"n_Latitude_Zones :",1x,i0)') GrELS%n_Latitude_Zones
    ! Data type
    lType = GrELS%Type
    IF ( lType < 1 .OR. lType > N_GrELS_TYPES ) lType = GrELS_INVALID
    WRITE(*, '(3x,"GrELS type    :",1x,a)') GrELS_TYPE_NAME(lType)
    IF ( .NOT. GrELS_Associated(GrELS) ) RETURN
    ! Dimension vectors
    WRITE(*, '(3x,"Wavelength :")') 
    WRITE(*, '(5(1x,es13.6,:))') GrELS%Wavelength
    WRITE(*, '(3x,"Surface Type Name :")')
    DO i = 1, GrELS%n_Surface_Types
      WRITE(*, '(1x,a)') TRIM(GrELS%Surface_Type_Name(i))
    END DO
    WRITE(*, '(3x,"Week :")') 
    WRITE(*, '(13(1x,i3,:))') GrELS%Week
    WRITE(*, '(3x,"Latitude Zone :")') 
    WRITE(*, '(5(1x,es13.6,:))') GrELS%Latitude_Zone
    ! Reflectance LUT data
    DO j = 1, GrELS%n_Surface_Types
      WRITE(*, '(3x,a," reflectance :")') TRIM(GrELS%Surface_Type_Name(j))
      WRITE(*, '(5(1x,es13.6,:))') GrELS%Reflectance(:,j)
    END DO
    ! Greeness vegetation fraction LUT data
    DO k = 1, GrELS%n_Latitude_Zones
      WRITE(*, '(3x,"Latitude zone ",es13.6)') GrELS%Latitude_Zone(k)
      DO j = 1, GrELS%n_Weeks
        WRITE(*, '(3x,"Week ",i0," greenness fraction :")') GrELS%Week(j)
        WRITE(*, '(5(1x,es13.6,:))') GrELS%GVF(:,j,k)
      END DO
    END DO
    
  END SUBROUTINE GrELS_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_ValidRelease
!
! PURPOSE:
!       Function to check the GrELS Release value.
!
! CALLING SEQUENCE:
!       IsValid = GrELS_ValidRelease( GrELS )
!
! INPUTS:
!       GrELS:         GrELS object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(GrELS_type)
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

  FUNCTION GrELS_ValidRelease( GrELS ) RESULT( IsValid )
    ! Arguments
    TYPE(GrELS_type), INTENT(IN)  :: GrELS
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( GrELS%Release < GrELS_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A GrELS data update is needed. ", &
                  &"GrELS release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  GrELS%Release, GrELS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( GrELS%Release > GrELS_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A GrELS software update is needed. ", &
                  &"GrELS release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  GrELS%Release, GrELS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION GrELS_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a GrELS object.
!
! CALLING SEQUENCE:
!       CALL GrELS_Info( GrELS, Info )
!
! INPUTS:
!       GrELS:        GrELS object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       TYPE(GrELS_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the passed GrELS object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE GrELS_Info( GrELS, Info )
    ! Arguments
    TYPE(GrELS_type), INTENT(IN)  :: GrELS
    CHARACTER(*),      INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
   WRITE( Long_String, &
          '(a,1x,"GrELS RELEASE.VERSION: ",i2,".",i2.2,2x,&
          &"N_WAVELENGTHS=",i0,2x,&
          &"N_SURFACE_TYPES=",i0,2x,&
          &"N_WEEKS=",i0,2x,&
          &"N_LATITUDE_ZONES=",i0 )' ) &
          ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
          GrELS%Release, GrELS%Version, &
          GrELS%n_Wavelengths   , &
          GrELS%n_Surface_Types , &
          GrELS%n_Weeks         , &
          GrELS%n_Latitude_Zones
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE GrELS_Info
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL GrELS_DefineVersion( Id )
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

  SUBROUTINE GrELS_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE GrELS_DefineVersion


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
!       GrELS_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two GrELS objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = GrELS_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two GrELS objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       GrELS_type
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

  ELEMENTAL FUNCTION GrELS_Equal( x, y ) RESULT( is_equal )
    TYPE(GrELS_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
    
    ! Check the object association status
    IF ( (.NOT. GrELS_Associated(x)) .OR. &
         (.NOT. GrELS_Associated(y))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (x%n_Wavelengths    /= y%n_Wavelengths   ) .OR. &
         (x%n_Surface_Types  /= y%n_Surface_Types ) .OR. &
         (x%n_Weeks          /= y%n_Weeks         ) .OR. &
         (x%n_Latitude_Zones /= y%n_Latitude_Zones) ) RETURN
    ! ...Data
    IF ( ALL(x%Wavelength        .EqualTo. y%Wavelength       ) .AND. &
         ALL(x%Surface_Type_Name    ==     y%Surface_Type_Name) .AND. &
         ALL(x%Week                 ==     y%Week             ) .AND. &
         ALL(x%Latitude_Zone     .EqualTo. y%Latitude_Zone    ) .AND. &
         ALL(x%Reflectance       .EqualTo. y%Reflectance      ) .AND. &
         ALL(x%GVF               .EqualTo. y%GVF              ) ) is_equal = .TRUE.

  END FUNCTION GrELS_Equal

END MODULE GrELS_Define
