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
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File
  USE SEcategory_Define    , ONLY: SECATEGORY_DATATYPE     , &
                                   SEcategory_type         , &
                                   OPERATOR(==)            , &      
                                   SEcategory_Associated   , & 
                                   SEcategory_Destroy      , & 
                                   SEcategory_Create       , & 
                                   SEcategory_Inspect      , & 
                                   SEcategory_ValidRelease , & 
                                   SEcategory_Info         , & 
                                   SEcategory_Name         , & 
                                   SEcategory_DefineVersion, & 
                                   SEcategory_SetValue     , &
                                   SEcategory_GetValue     , &
                                   SEcategory_InquireFile  , &
                                   SEcategory_ReadFile     , &
                                   SEcategory_WriteFile
  USE LSEatlas_Define      , ONLY: LSEATLAS_DATATYPE     , &
                                   LSEatlas_type         , &
                                   OPERATOR(==)          , &      
                                   LSEatlas_Associated   , & 
                                   LSEatlas_Destroy      , & 
                                   LSEatlas_Create       , & 
                                   LSEatlas_Inspect      , & 
                                   LSEatlas_ValidRelease , & 
                                   LSEatlas_Info         , & 
                                   LSEatlas_Name         , & 
                                   LSEatlas_DefineVersion, & 
                                   LSEatlas_SetValue     , &
                                   LSEatlas_GetValue     , &
                                   LSEatlas_InquireFile  , &
                                   LSEatlas_ReadFile     , &
                                   LSEatlas_WriteFile
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
  PUBLIC :: IRlandCoeff_InquireFile
  ! ...Inherited datatypes and procedures
  PUBLIC :: SECATEGORY_DATATYPE
  PUBLIC :: SEcategory_type
  PUBLIC :: SEcategory_Associated   
  PUBLIC :: SEcategory_Destroy      
  PUBLIC :: SEcategory_Create       
  PUBLIC :: SEcategory_Inspect      
  PUBLIC :: SEcategory_ValidRelease 
  PUBLIC :: SEcategory_Info         
  PUBLIC :: SEcategory_Name       
  PUBLIC :: SEcategory_DefineVersion
  PUBLIC :: SEcategory_SetValue
  PUBLIC :: SEcategory_GetValue
  PUBLIC :: SEcategory_InquireFile
  PUBLIC :: SEcategory_ReadFile
  PUBLIC :: SEcategory_WriteFile
  ! ...
  PUBLIC :: LSEATLAS_DATATYPE
  PUBLIC :: LSEatlas_type
  PUBLIC :: LSEatlas_Associated   
  PUBLIC :: LSEatlas_Destroy      
  PUBLIC :: LSEatlas_Create       
  PUBLIC :: LSEatlas_Inspect      
  PUBLIC :: LSEatlas_ValidRelease 
  PUBLIC :: LSEatlas_Info         
  PUBLIC :: LSEatlas_Name         
  PUBLIC :: LSEatlas_DefineVersion
  PUBLIC :: LSEatlas_SetValue     
  PUBLIC :: LSEatlas_GetValue     
  PUBLIC :: LSEatlas_InquireFile  
  PUBLIC :: LSEatlas_ReadFile     
  PUBLIC :: LSEatlas_WriteFile


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
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Ancillary data indicator
  INTEGER, PARAMETER :: DATA_MISSING = 0
  INTEGER, PARAMETER :: DATA_PRESENT = 1


  ! ----------------------------------
  ! IRlandCoeff data type definitions
  ! ----------------------------------
  !:tdoc+:
  TYPE :: IRlandCoeff_type
!    PRIVATE
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = IRLANDCOEFF_RELEASE
    INTEGER(Long) :: Version = IRLANDCOEFF_VERSION
    ! Derived type components
    TYPE(SEcategory_type) :: LSEcategory
    TYPE(LSEatlas_type)   :: LSEatlas
  END TYPE IRlandCoeff_type
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
!       Status:        The return value is a logical value indicating the
!                      status of the NLTE members.
!                       .TRUE.  - if ANY of the IRlandCoeff allocatable members
!                                 are in use.
!                       .FALSE. - if ALL of the IRlandCoeff allocatable members
!                                 are not in use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input
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
!       IRlandCoeff:  Re-initialized IRlandCoeff structure.
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
!       IRlandCoeff:  IRlandCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       IRlandCoeff_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
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
    IF ( SEcategory_Associated( self%LSEcategory ) ) CALL SEcategory_Inspect( self%LSEcategory )
    IF ( LSEatlas_Associated( self%LSEatlas ) ) CALL LSEatlas_Inspect( self%LSEatlas )
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
    CHARACTER(*),           INTENT(OUT) :: Info
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
!                                  LSEcategory = LSEcategory, &
!                                  LSEatlas    = LSEatlas     )
!
! OBJECTS:
!       IRlandCoeff:  Valid, allocated IRlandCoeff object for which
!                     values are to be set.
!                     UNITS:      N/A
!                     TYPE:       IRlandCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       LSEcategory:  Object containing a land surface SEcategory
!                     emissivity/refelctivity dataset.
!                     UNITS:      N/A
!                     TYPE:       SEcategory_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       LSEatlas:     Object containing an LSEatlas land surface
!                     emissivity/refelctivity dataset.
!                     UNITS:      N/A
!                     TYPE:       LSEatlas_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRlandCoeff_SetValue( &
    self       , &  ! Input
    LSEcategory, &  ! Input
    LSEatlas     )  ! Optional input
    ! Arguments
    TYPE(IRlandCoeff_type)         , INTENT(IN OUT) :: self
    TYPE(SEcategory_type), OPTIONAL, INTENT(IN)     :: LSEcategory
    TYPE(LSEatlas_type)  , OPTIONAL, INTENT(IN)     :: LSEatlas   

    IF ( PRESENT(LSEcategory) ) self%LSEcategory = LSEcategory
    IF ( PRESENT(LSEatlas   ) ) self%LSEatlas    = LSEatlas   

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
!                                  LSEcategory = LSEcategory, &
!                                  LSEatlas    = LSEatlas     )
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
!       LSEcategory:  Object containing an land surface SEcategory
!                     emissivity/reflectivity dataset.
!                     UNITS:      N/A
!                     TYPE:       SEcategory_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       LSEatlas:     Object containing an LSEatlas land surface
!                     emissivity/reflectivity dataset.
!                     UNITS:      N/A
!                     TYPE:       LSEatlas_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRlandCoeff_GetValue( &
    self       , &  ! Input
    LSEcategory, &  ! Optional output
    LSEatlas     )  ! Optional output
    ! Arguments
    TYPE(IRlandCoeff_type)         , INTENT(IN)  :: self
    TYPE(SEcategory_type), OPTIONAL, INTENT(OUT) :: LSEcategory
    TYPE(LSEatlas_type)  , OPTIONAL, INTENT(OUT) :: LSEatlas   
   
    IF ( PRESENT(LSEcategory) ) LSEcategory = self%LSEcategory
    IF ( PRESENT(LSEatlas   ) ) LSEatlas    = self%LSEatlas   

  END SUBROUTINE IRlandCoeff_GetValue
 

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_InquireFile
!
! PURPOSE:
!       Function to inquire a IRlandCoeff component file to determine its
!       dataset type.
!
! CALLING SEQUENCE:
!       Error_Status = IRlandCoeff_InquireFile( Filename, Datatype_Name )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Datatype_Name:      Character string containing the name of the
!                           dataset type.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquire was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION IRlandCoeff_InquireFile( &
    Filename     , &  ! Input
    Datatype_Name) &  ! Output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*), INTENT(IN)  :: Filename
    CHARACTER(*), INTENT(OUT) :: Datatype_Name
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRlandCoeff_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid

 
    ! Setup
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the datatype name
    err_stat = Read_Datatype( fid, Datatype_Name )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Datatype_Name'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open(fid) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp
    
  END FUNCTION IRlandCoeff_InquireFile


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
    IF ( SEcategory_Associated( x%LSEcategory ) .NEQV. SEcategory_Associated( y%LSEcategory ) ) RETURN
    IF ( SEcategory_Associated( x%LSEcategory ) .AND.  SEcategory_Associated( y%LSEcategory ) ) THEN
      IF ( .NOT. (x%LSEcategory == y%LSEcategory) ) RETURN
    END IF
    IF ( LSEatlas_Associated( x%LSEatlas ) .NEQV. LSEatlas_Associated( y%LSEatlas ) ) RETURN
    IF ( LSEatlas_Associated( x%LSEatlas ) .AND.  LSEatlas_Associated( y%LSEatlas ) ) THEN
      IF ( .NOT. (x%LSEatlas == y%LSEatlas) ) RETURN
    END IF
    
    is_equal = .TRUE.
    
  END FUNCTION IRlandCoeff_Equal


  ! Function to read the datatype name from file

  FUNCTION Read_Datatype( fid, datatype_name ) RESULT( err_stat )
    ! Arguments
    INTEGER     , INTENT(IN)  :: fid
    CHARACTER(*), INTENT(OUT) :: datatype_name
    ! Function result
    INTEGER :: err_stat    
    ! Local variables
    CHARACTER(1), ALLOCATABLE :: dummy(:)
    INTEGER :: i, strlen
    INTEGER :: io_stat
    INTEGER :: alloc_stat

    ! Set up
    err_stat = FAILURE
    datatype_name = ''

    ! Get the string length
    READ( fid, IOSTAT=io_stat ) strlen
    IF ( io_stat /= 0 ) RETURN
    
    ! Allocate dummy string array
    ALLOCATE( dummy(strlen), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Read the string into the dummy array
    READ( fid, IOSTAT=io_stat ) dummy
    IF ( io_stat /= 0 ) RETURN

    ! Transfer array into string
    DO i = 1, MIN(strlen,LEN(datatype_name))
      datatype_name(i:i) = dummy(i)
    END DO

    ! Done
    err_stat = SUCCESS
  END FUNCTION Read_Datatype
  
END MODULE IRlandCoeff_Define
