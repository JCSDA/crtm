!
! GrELS_Binary_IO
!
! Module containing routines to inquire, read, and write Binary
! GrELS object datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Feb-2010
!                       paul.vandelst@noaa.gov
!

MODULE GrELS_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE GrELS_Define       , ONLY: GrELS_type        , &
                                 GrELS_Associated  , &
                                 GrELS_Destroy     , &
                                 GrELS_Create      , &
                                 GrELS_ValidRelease, &
                                 GrELS_Info
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: GrELS_Binary_InquireFile
  PUBLIC :: GrELS_Binary_ReadFile
  PUBLIC :: GrELS_Binary_WriteFile
  PUBLIC :: GrELS_Binary_IOVersion

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256
  ! Old integer flag setting
  INTEGER, PARAMETER :: SET = 1
  

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_Binary_InquireFile
!
! PURPOSE:
!       Function to inquire GrELS object Binary format files.
!
! CALLING SEQUENCE:
!       Error_Status = GrELS_Binary_InquireFile( &
!                        Filename, &
!                        n_Wavelengths    = n_Wavelengths   , &
!                        n_Surface_Types  = n_Surface_Types , &
!                        n_Weeks          = n_Weeks         , &
!                        n_Latitude_Zones = n_Latitude_Zones, &
!                        Release          = Release         , &
!                        Version          = Version           )
!
! INPUTS:
!       Filename:         Character string specifying the name of a
!                         GrELS data file to read.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Wavelengths:    Number of wavelengths for which the reflectance
!                         LUT data is specified. 
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Surface_Types:  Number of surface types for which the reflectance
!                         LUT data is specified. 
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Weeks:          Number of weeks for which the green vegetations
!                         fraction LUT data is specified. 
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Latitude_Zones: Number of latitude zones for which the green
!                         vegetation fraction LUT data is specified. 
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:          The release number of the GrELS file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:          The version number of the GrELS file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS, the file inquire was successful
!                            == FAILURE, an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION GrELS_Binary_InquireFile( &
    Filename        , &  ! Input
    n_Wavelengths   , &  ! Optional output
    n_Surface_Types , &  ! Optional output
    n_Weeks         , &  ! Optional output
    n_Latitude_Zones, &  ! Optional output
    Release         , &  ! Optional output
    Version         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: Filename
    INTEGER, OPTIONAL, INTENT(OUT) :: n_Wavelengths   
    INTEGER, OPTIONAL, INTENT(OUT) :: n_Surface_Types 
    INTEGER, OPTIONAL, INTENT(OUT) :: n_Weeks         
    INTEGER, OPTIONAL, INTENT(OUT) :: n_Latitude_Zones
    INTEGER, OPTIONAL, INTENT(OUT) :: Release         
    INTEGER, OPTIONAL, INTENT(OUT) :: Version         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_InquireFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(GrELS_type) :: GrELS

    ! Setup
    err_stat = SUCCESS
    fid = -100
    ! Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the GrELS data file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the release and version
    READ( fid,IOSTAT=io_stat ) GrELS%Release, GrELS%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    ! Read the dimensions
    READ( fid,IOSTAT=io_stat ) GrELS%n_Wavelengths   , &
                               GrELS%n_Surface_Types , &
                               GrELS%n_Weeks         , &
                               GrELS%n_Latitude_Zones
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimensions from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    ! Set the return arguments
    IF ( PRESENT(n_Wavelengths   ) ) n_Wavelengths    = GrELS%n_Wavelengths   
    IF ( PRESENT(n_Surface_Types ) ) n_Surface_Types  = GrELS%n_Surface_Types 
    IF ( PRESENT(n_Weeks         ) ) n_Weeks          = GrELS%n_Weeks         
    IF ( PRESENT(n_Latitude_Zones) ) n_Latitude_Zones = GrELS%n_Latitude_Zones
    IF ( PRESENT(Release         ) ) Release          = GrELS%Release     
    IF ( PRESENT(Version         ) ) Version          = GrELS%Version     

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION GrELS_Binary_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_Binary_ReadFile
!
! PURPOSE:
!       Function to read GrELS object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = GrELS_Binary_ReadFile( &
!                        Filename     , &
!                        GrELS        , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       GrELS format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       GrELS:          Object containing the reflectance spectra and
!                       greenness vegetation fraction data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(GrELS_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
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

  FUNCTION GrELS_Binary_ReadFile( &
    Filename, &  ! Input
    GrELS   , &  ! Output
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: Filename
    TYPE(GrELS_type),  INTENT(OUT) :: GrELS
    LOGICAL, OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL, OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_ReadFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(GrELS_type) :: dummy

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
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read and check the release and version
    READ( fid,IOSTAT=io_stat ) dummy%Release, dummy%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. GrELS_ValidRelease( dummy ) ) THEN
      msg = 'GrELS Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    
    
    ! Read the geomagnetic field data
    ! ...Read the dimensions
    READ( fid,IOSTAT=io_stat ) dummy%n_Wavelengths   , &
                               dummy%n_Surface_Types , &
                               dummy%n_Weeks         , &
                               dummy%n_Latitude_Zones
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL GrELS_Create( GrELS, &
                       dummy%n_Wavelengths   , &
                       dummy%n_Surface_Types , &
                       dummy%n_Weeks         , &
                       dummy%n_Latitude_Zones )
    IF ( .NOT. GrELS_Associated( GrELS ) ) THEN
      msg = 'GrELS object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the GrELS dimension vectors
    READ( fid,IOSTAT=io_stat ) GrELS%Wavelength       , &
                               GrELS%Surface_Type_Name, &
                               GrELS%Week             , &
                               GrELS%Latitude_Zone    
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimension vectors. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the reflectance LUT
    READ( fid,IOSTAT=io_stat ) GrELS%Reflectance    
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading reflectance data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the greenness vegetation fraction LUT
    READ( fid,IOSTAT=io_stat ) GrELS%GVF
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading greenness vegetation fraction data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Assign the version number read in
    GrELS%Version = dummy%Version


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF

 
    ! Output an info message
    IF ( noisy ) THEN
      CALL GrELS_Info( GrELS, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      CALL GrELS_Destroy( GrELS )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION GrELS_Binary_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_Binary_WriteFile
!
! PURPOSE:
!       Function to write GrELS object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = GrELS_Binary_WriteFile( &
!                        Filename, &
!                        GrELS   , &
!                        Quiet = Quiet )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       GrELS format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GrELS:          Object containing the reflectance spectra and
!                       greenness vegetation fraction data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(GrELS_type)
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
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION GrELS_Binary_WriteFile( &
    Filename, &  ! Input
    GrELS   , &  ! Input
    Quiet   , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN) :: Filename
    TYPE(GrELS_type),  INTENT(IN) :: GrELS
    LOGICAL, OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL, OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_WriteFile(Binary)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
 
    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Check the GrELS object
    ! ...Is there any data?
    IF ( .NOT. GrELS_Associated( GrELS ) ) THEN 
      msg = 'Input GrELS object is not allocated.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. GrELS_ValidRelease( GrELS ) ) THEN
      msg = 'GrELS Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Open the file for writing
    err_stat = Open_Binary_File( Filename, fid, For_Output=SET )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the release and version
    WRITE( fid,IOSTAT=io_stat ) GrELS%Release, GrELS%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    
    
    ! Write the geomagnetic field data
    ! ...Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) GrELS%n_Wavelengths   , &
                                GrELS%n_Surface_Types , &
                                GrELS%n_Weeks         , &
                                GrELS%n_Latitude_Zones
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the GrELS dimension vectors
    WRITE( fid,IOSTAT=io_stat ) GrELS%Wavelength       , &
                                GrELS%Surface_Type_Name, &
                                GrELS%Week             , &
                                GrELS%Latitude_Zone    
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing dimension vectors. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the reflectance LUT
    WRITE( fid,IOSTAT=io_stat ) GrELS%Reflectance    
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing reflectance data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the greenness vegetation fraction LUT
    WRITE( fid,IOSTAT=io_stat ) GrELS%GVF
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing greenness vegetation fraction data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL GrELS_Info( GrELS, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION GrELS_Binary_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_Binary_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL GrELS_Binary_IOVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE GrELS_Binary_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE GrELS_Binary_IOVersion

END MODULE GrELS_Binary_IO
