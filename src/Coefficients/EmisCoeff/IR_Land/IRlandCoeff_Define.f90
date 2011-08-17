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
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  USE LSEcategory_Define   , ONLY: LSEcategory_type         , &
                                   OPERATOR(==)             , &      
                                   LSEcategory_Associated   , & 
                                   LSEcategory_Destroy      , & 
                                   LSEcategory_Create       , & 
                                   LSEcategory_Inspect      , & 
                                   LSEcategory_ValidRelease , & 
                                   LSEcategory_Info         , & 
                                   LSEcategory_DefineVersion, & 
                                   LSEcategory_SetValue     , &
                                   LSEcategory_GetValue     , &
                                   LSEcategory_InquireFile  , &
                                   LSEcategory_ReadFile     , &
                                   LSEcategory_WriteFile
  USE LSEatlas_Define      , ONLY: LSEatlas_type         , &
                                   OPERATOR(==)          , &      
                                   LSEatlas_Associated   , & 
                                   LSEatlas_Destroy      , & 
                                   LSEatlas_Create       , & 
                                   LSEatlas_Inspect      , & 
                                   LSEatlas_ValidRelease , & 
                                   LSEatlas_Info         , & 
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
  PUBLIC :: IRlandCoeff_ReadFile
  PUBLIC :: IRlandCoeff_WriteFile
  ! ...Inherited datatypes and procedures
  PUBLIC :: LSEcategory_type
  PUBLIC :: LSEcategory_Associated   
  PUBLIC :: LSEcategory_Destroy      
  PUBLIC :: LSEcategory_Create       
  PUBLIC :: LSEcategory_Inspect      
  PUBLIC :: LSEcategory_ValidRelease 
  PUBLIC :: LSEcategory_Info         
  PUBLIC :: LSEcategory_DefineVersion
  PUBLIC :: LSEcategory_SetValue
  PUBLIC :: LSEcategory_GetValue
  PUBLIC :: LSEcategory_InquireFile
  PUBLIC :: LSEcategory_ReadFile
  PUBLIC :: LSEcategory_WriteFile
  PUBLIC :: LSEatlas_type
  PUBLIC :: LSEatlas_Associated   
  PUBLIC :: LSEatlas_Destroy      
  PUBLIC :: LSEatlas_Create       
  PUBLIC :: LSEatlas_Inspect      
  PUBLIC :: LSEatlas_ValidRelease 
  PUBLIC :: LSEatlas_Info         
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
  TYPE :: IRlandCoeff_type
    PRIVATE
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = IRLANDCOEFF_RELEASE
    INTEGER(Long) :: Version = IRLANDCOEFF_VERSION
    ! Derived type components
    TYPE(LSEcategory_type) :: NPOESS
    TYPE(LSEatlas_type)    :: HSR
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
    IF ( LSEcategory_Associated( self%NPOESS ) ) CALL LSEcategory_Inspect( self%NPOESS )
    IF ( LSEatlas_Associated( self%HSR ) ) CALL LSEatlas_Inspect( self%HSR )
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
!                                  NPOESS = NPOESS, &
!                                  HSR    = HSR     )
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
!       NPOESS:       Object containing the NPOESS land surface emissivity 
!                     category dataset.
!                     UNITS:      N/A
!                     TYPE:       LSEcategory_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       HSR:          Object containing the UW land surface emissivity 
!                     atlas dataset.
!                     UNITS:      N/A
!                     TYPE:       LSEatlas_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRlandCoeff_SetValue( &
    self  , &  ! Input
    NPOESS, &  ! Input
    HSR     )  ! Optional input
    ! Arguments
    TYPE(IRlandCoeff_type)          , INTENT(IN OUT) :: self
    TYPE(LSEcategory_type), OPTIONAL, INTENT(IN)     :: NPOESS
    TYPE(LSEatlas_type)   , OPTIONAL, INTENT(IN)     :: HSR

    IF ( PRESENT(NPOESS) ) self%NPOESS = NPOESS
    IF ( PRESENT(HSR   ) ) self%HSR    = HSR

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
!                                  NPOESS = NPOESS, &
!                                  HSR    = HSR     )
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
!       NPOESS:       Object containing the NPOESS land surface emissivity 
!                     category dataset.
!                     UNITS:      N/A
!                     TYPE:       LSEcategory_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       HSR:          Object containing the UW land surface emissivity 
!                     atlas dataset.
!                     UNITS:      N/A
!                     TYPE:       LSEatlas_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRlandCoeff_GetValue( &
    self  , &  ! Input
    NPOESS, &  ! Optional output
    HSR     )  ! Optional output
    ! Arguments
    TYPE(IRlandCoeff_type)          , INTENT(IN)  :: self
    TYPE(LSEcategory_type), OPTIONAL, INTENT(OUT) :: NPOESS
    TYPE(LSEatlas_type)   , OPTIONAL, INTENT(OUT) :: HSR
   
    IF ( PRESENT(NPOESS) ) NPOESS = self%NPOESS
    IF ( PRESENT(HSR   ) ) HSR    = self%HSR

  END SUBROUTINE IRlandCoeff_GetValue
 

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_InquireFile
!
! PURPOSE:
!       Function to inquire a IRlandCoeff object container file.
!
! CALLING SEQUENCE:
!       Error_Status = IRlandCoeff_InquireFile( &
!                        Filename         , &
!                        Release = Release, &
!                        Version = Version  )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Release:            The data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The data/file version number. Used for
!                           purposes only in identifying the dataset for
!                           a particular release.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
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
    Filename, &  ! Input
    Release , &  ! Optional output
    Version , &  ! Optional output
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRlandCoeff_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(IRlandCoeff_type) :: IRlandCoeff

 
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


    ! Read the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) IRlandCoeff%Release, IRlandCoeff%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    IF ( PRESENT(Release) ) Release = IRlandCoeff%Release
    IF ( PRESENT(Version) ) Version = IRlandCoeff%Version
    
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


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_ReadFile
!
! PURPOSE:
!       Function to read IRlandCoeff object container files.
!
! CALLING SEQUENCE:
!       Error_Status = IRlandCoeff_ReadFile( &
!                        IRlandCoeff  , &
!                        Filename     , &
!                        Quiet = Quiet  )
!
! OBJECTS:
!       IRlandCoeff:    Object containing the data read from file.
!                       UNITS:      N/A
!                       TYPE:       IRlandCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file read was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION IRlandCoeff_ReadFile( &
    IRlandCoeff, &  ! Output
    Filename   , &  ! Input
    Quiet      , &  ! Optional input
    Title      , &  ! Optional output
    History    , &  ! Optional output
    Comment    , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(IRlandCoeff_type), INTENT(OUT) :: IRlandCoeff
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRlandCoeff_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER(Long) :: npoess_present
    INTEGER(Long) :: hsr_present
    TYPE(IRlandCoeff_type) :: dummy
    

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF

   
    ! Open the file
    IF ( File_Exists( Filename ) ) THEN
      err_stat = Open_Binary_File( Filename, fid )
      IF ( err_Stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Read_CleanUp(); RETURN
      END IF
    ELSE
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_CleanUp(); RETURN
    END IF


    ! Read and check the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%Release, &
      dummy%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. IRlandCoeff_ValidRelease( dummy ) ) THEN
      msg = 'IRlandCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Create the return object
    CALL IRlandCoeff_Create( IRlandCoeff )                  
    IF ( .NOT. IRlandCoeff_Associated( IRlandCoeff ) ) THEN
      msg = 'IRlandCoeff object creation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    IRlandCoeff%Version = dummy%Version
    
    
    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the NPOESS data if it's present
    ! ...Read the data indicator
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) npoess_present
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading NPOESS data indicator - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the data
    IF ( npoess_present == DATA_PRESENT ) THEN
      err_stat = LSEcategory_ReadFile( &
                   IRlandCoeff%NPOESS, &
                   Filename          , &
                   No_Close = .TRUE. , &
                   Quiet    = Quiet  , &
                   Debug    = Debug    )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading NPOESS data'
        CALL Read_Cleanup(); RETURN
      END IF
    END IF
    
    
    ! Read the HSR data if it's present
    ! ...Read the data indicator
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) hsr_present
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading HSR data indicator - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the data
    IF ( hsr_present == DATA_PRESENT ) THEN
      err_stat = LSEatlas_ReadFile( &
                   IRlandCoeff%HSR  , &
                   Filename         , &
                   No_Close = .TRUE., &
                   Quiet    = Quiet , &
                   Debug    = Debug   )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading HSR data'
        CALL Read_Cleanup(); RETURN
      END IF
    END IF
    
    
    ! Close the file
    CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL IRlandCoeff_Info( IRlandCoeff, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
       END IF
       CALL IRlandCoeff_Destroy( IRlandCoeff )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION IRlandCoeff_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRlandCoeff_WriteFile
!
! PURPOSE:
!       Function to write IRlandCoeff object container files.
!
! CALLING SEQUENCE:
!       Error_Status = IRlandCoeff_WriteFile( &
!                        IRlandCoeff  , &
!                        Filename     , &
!                        Quiet = Quiet  )
!
! OBJECTS:
!       IRlandCoeff:    Object containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       IRlandCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file write was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION IRlandCoeff_WriteFile( &
    IRlandCoeff, &  ! Output
    Filename   , &  ! Input
    Quiet      , &  ! Optional input
    Title      , &  ! Optional input
    History    , &  ! Optional input
    Comment    , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(IRlandCoeff_type), INTENT(IN) :: IRlandCoeff
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRlandCoeff_WriteFile'
    CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER(Long) :: npoess_present
    INTEGER(Long) :: hsr_present
    

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF
    ! ...Check there is data to write
    IF ( .NOT. IRlandCoeff_Associated( IRlandCoeff ) ) THEN
      msg = 'IRlandCoeff object is empty.'
      CALL Write_Cleanup(); RETURN
    END IF

   
    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output=.TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_CleanUp(); RETURN
    END IF


    ! Write the release and version
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      IRlandCoeff%Release, &
      IRlandCoeff%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts_Binary_File( &
                 fid, &
                 Write_Module = MODULE_VERSION_ID, &
                 Title        = Title  , &
                 History      = History, &
                 Comment      = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attributes'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the NPOESS data if it's present
    IF ( LSEcategory_Associated( IRlandCoeff%NPOESS ) ) THEN
      npoess_present = DATA_PRESENT
    ELSE
      npoess_present = DATA_MISSING
    END IF
    ! ...Write the data indicator
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) npoess_present
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing NPOESS data indicator - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the actual data
    IF ( npoess_present == DATA_PRESENT ) THEN
      err_stat = LSEcategory_WriteFile( &
                   IRlandCoeff%NPOESS, &
                   Filename          , &
                   No_Close = .TRUE. , &
                   Quiet    = Quiet  , &
                   Debug    = Debug    )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing NPOESS data = '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF
    
    
    ! Write the HSR data if it's present
    IF ( LSEatlas_Associated( IRlandCoeff%HSR ) ) THEN
      hsr_present = DATA_PRESENT
    ELSE
      hsr_present = DATA_MISSING
    END IF
    ! ...Write the data indicator
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) hsr_present
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing HSR data indicator - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the actual data
    IF ( hsr_present == DATA_PRESENT ) THEN
      err_stat = LSEatlas_WriteFile( &
                   IRlandCoeff%hsr  , &
                   Filename         , &
                   No_Close = .TRUE., &
                   Quiet    = Quiet , &
                   Debug    = Debug   )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing hsr data = '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF
    
    
    ! Close the file
    CLOSE( fid, STATUS='KEEP', IOSTAT=io_stat, IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL IRlandCoeff_Info( IRlandCoeff, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Write_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, STATUS=WRITE_ERROR_STATUS, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_CleanUp

  END FUNCTION IRlandCoeff_WriteFile


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
    IF ( LSEcategory_Associated( x%NPOESS ) .NEQV. LSEcategory_Associated( y%NPOESS ) ) RETURN
    IF ( LSEcategory_Associated( x%NPOESS ) .AND.  LSEcategory_Associated( y%NPOESS ) ) THEN
      IF ( .NOT. (x%NPOESS == y%NPOESS) ) RETURN
    END IF
    IF ( LSEatlas_Associated( x%HSR ) .NEQV. LSEatlas_Associated( y%HSR ) ) RETURN
    IF ( LSEatlas_Associated( x%HSR ) .AND.  LSEatlas_Associated( y%HSR ) ) THEN
      IF ( .NOT. (x%HSR == y%HSR) ) RETURN
    END IF
    
    is_equal = .TRUE.
    
  END FUNCTION IRlandCoeff_Equal

END MODULE IRlandCoeff_Define
