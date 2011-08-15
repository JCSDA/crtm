!
! IRLSE_NPOESS_netCDF_IO
!
! Module containing routines to read and write IRLSE_NPOESS netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 15-Aug-2011
!                       paul.vandelst@noaa.gov
!

MODULE IRLSE_NPOESS_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Exists
  USE String_Utility     , ONLY: StrClean
  USE IRLSE_NPOESS_Define, ONLY: IRLSE_NPOESS_type        , &
                                 IRLSE_NPOESS_Associated  , &
                                 IRLSE_NPOESS_Destroy     , &
                                 IRLSE_NPOESS_Create      , &
                                 IRLSE_NPOESS_Inspect     , &
                                 IRLSE_NPOESS_ValidRelease, &
                                 IRLSE_NPOESS_Info        , &
                                 IRLSE_NPOESS_DefineVersion
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: IRLSE_NPOESS_netCDF_InquireFile
  PUBLIC :: IRLSE_NPOESS_netCDF_ReadFile
  PUBLIC :: IRLSE_NPOESS_netCDF_WriteFile
  PUBLIC :: IRLSE_NPOESS_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module version
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! Default msg string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double

  ! Global attribute names.
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'History'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'Comment'
  
  ! Dimension names
  CHARACTER(*), PARAMETER :: FREQUENCY_DIMNAME    = 'n_Frequencies'    
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_DIMNAME = 'n_Surface_Types' 
  CHARACTER(*), PARAMETER :: STSL_DIMNAME         = 'stsl'

  ! Variable names
  CHARACTER(*), PARAMETER :: FREQUENCY_VARNAME    = 'Frequency'
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_VARNAME = 'Surface_Type'
  CHARACTER(*), PARAMETER :: REFLECTANCE_VARNAME  = 'Reflectance'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME    = 'Frequency'
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_LONGNAME = 'Surface Type Name'
  CHARACTER(*), PARAMETER :: REFLECTANCE_LONGNAME  = 'Surface Reflectance'
  
  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION    = 'Frequency dimension of reflectance data'
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_DESCRIPTION = 'List of NPOESS surface type classifications'
  CHARACTER(*), PARAMETER :: REFLECTANCE_DESCRIPTION  = 'Spectral reflectance of NPOESS surface types'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS    = 'inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: REFLECTANCE_UNITS  = 'N/A'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  REAL(Double) , PARAMETER :: FREQUENCY_FILLVALUE    = 0.0_Double
  CHARACTER(*) , PARAMETER :: SURFACE_TYPE_FILLVALUE = NF90_FILL_CHAR
  REAL(Double) , PARAMETER :: REFLECTANCE_FILLVALUE  = 0.0_Double


  ! Variable types
  INTEGER, PARAMETER :: FREQUENCY_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: SURFACE_TYPE_TYPE = NF90_CHAR
  INTEGER, PARAMETER :: REFLECTANCE_TYPE  = NF90_DOUBLE


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
!       IRLSE_NPOESS_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire IRLSE_NPOESS object netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = IRLSE_NPOESS_netCDF_InquireFile( &
!                        Filename                         , &
!                        n_Frequencies   = n_Frequencies  , &
!                        n_Surface_Types = n_Surface_Types, &
!                        Release         = Release        , &
!                        Version         = Version        , &
!                        Title           = Title          , &
!                        History         = History        , &
!                        Comment         = Comment          )
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
!       n_Frequencies:      Number of spectral frequencies for which there are
!                           reflectance data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Surface_Types:    Number of land surface types for which is are
!                           reflectance data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
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
!       Title:              Character string written into the TITLE global
!                           attribute field of the file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquiry was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION IRLSE_NPOESS_netCDF_InquireFile( &
    Filename       , &  ! Input
    n_Frequencies  , &  ! Optional output  
    n_Surface_Types, &  ! Optional output  
    Release        , &  ! Optional Output
    Version        , &  ! Optional Output
    Title          , &  ! Optional output
    History        , &  ! Optional output
    Comment        ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Frequencies  
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Surface_Types
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version        
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRLSE_NPOESS_InquireFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: dimid
    TYPE(IRLSE_NPOESS_type) :: IRLSE_NPOESS
    
    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.


    ! Open the file
    nf90_status = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Get the dimensions
    ! ...n_Frequencies dimension
    NF90_Status = NF90_INQ_DIMID( FileId,FREQUENCY_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//FREQUENCY_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=irlse_npoess%n_Frequencies )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//FREQUENCY_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Surface_Types dimension
    NF90_Status = NF90_INQ_DIMID( FileId,SURFACE_TYPE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//SURFACE_TYPE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=irlse_npoess%n_Surface_Types )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//SURFACE_TYPE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
  
  
    ! Get the global attributes
    err_stat = ReadGAtts( Filename, &
                          fileid  , &
                          Release = Release, &
                          Version = Version, &
                          Title   = Title  , &
                          History = History, &
                          Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_status = NF90_CLOSE( fileid )
    close_file = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Frequencies  ) ) n_Frequencies   = IRLSE_NPOESS%n_Frequencies  
    IF ( PRESENT(n_Surface_Types) ) n_Surface_Types = IRLSE_NPOESS%n_Surface_Types

  CONTAINS
 
    SUBROUTINE Inquire_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( fileid )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION IRLSE_NPOESS_netCDF_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_netCDF_WriteFile
!
! PURPOSE:
!       Function to write IRLSE_NPOESS object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = IRLSE_NPOESS_netCDF_WriteFile( &
!                        Filename         , &
!                        IRLSE_NPOESS     , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       IRLSE_NPOESS:   IRLSE_NPOESS object containing the data to write.
!                       UNITS:      N/A
!                       TYPE:       IRLSE_NPOESS_type
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
!       Title:          Character string written into the TITLE global
!                       attribute field of the file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION IRLSE_NPOESS_netCDF_WriteFile( &
    Filename    , &  ! Input
    IRLSE_NPOESS, &  ! Input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional input
    History     , &  ! Optional input
    Comment     ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN) :: Filename
    TYPE(IRLSE_NPOESS_type), INTENT(IN) :: IRLSE_NPOESS
    LOGICAL,       OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: History
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRLSE_NPOESS_WriteFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: varid

    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Check structure pointer association status
    IF ( .NOT. IRLSE_NPOESS_Associated( IRLSE_NPOESS ) ) THEN
      msg = 'IRLSE_NPOESS structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. IRLSE_NPOESS_ValidRelease( IRLSE_NPOESS ) ) THEN
      msg = 'IRLSE_NPOESS Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Create the output file
    err_stat = CreateFile( &
                 Filename                      , &  ! Input
                 IRLSE_NPOESS%n_Frequencies    , &  ! Input
                 IRLSE_NPOESS%n_Surface_Types  , &  ! Input
                 fileid                        , &  ! Output
                 Version = IRLSE_NPOESS%Version, &  ! Optional input
                 Title   = Title               , &  ! Optional input
                 History = History             , &  ! Optional input
                 Comment = Comment               )  ! Optional input
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Write the data items
    ! ...Frequency variable
    NF90_Status = NF90_INQ_VARID( FileId,FREQUENCY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,IRLSE_NPOESS%Frequency )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Surface_Type variable
    NF90_Status = NF90_INQ_VARID( FileId,SURFACE_TYPE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SURFACE_TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,IRLSE_NPOESS%Surface_Type )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SURFACE_TYPE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Reflectance variable
    NF90_Status = NF90_INQ_VARID( FileId,REFLECTANCE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFLECTANCE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,IRLSE_NPOESS%Reflectance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//REFLECTANCE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_status = NF90_CLOSE( fileid )
    close_file = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL IRLSE_NPOESS_Info( IRLSE_NPOESS, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( fileid )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
                TRIM(NF90_STRERROR( nf90_status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION IRLSE_NPOESS_netCDF_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_netCDF_ReadFile
!
! PURPOSE:
!       Function to read IRLSE_NPOESS object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = IRLSE_NPOESS_netCDF_ReadFile( &
!                        Filename         , &
!                        IRLSE_NPOESS     , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       IRLSE_NPOESS data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       IRLSE_NPOESS:   IRLSE_NPOESS object containing the data to read.
!                       UNITS:      N/A
!                       TYPE:       IRLSE_NPOESS_type
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
! OPTIONAL OUTPUTS:
!       Title:          Character string written into the TITLE global
!                       attribute field of the file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION IRLSE_NPOESS_netCDF_ReadFile( &
    Filename    , &  ! Input
    IRLSE_NPOESS, &  ! Output
    Quiet       , &  ! Optional input
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(IRLSE_NPOESS_type), INTENT(OUT) :: IRLSE_NPOESS
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRLSE_NPOESS_ReadFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: n_frequencies  
    INTEGER :: n_surface_types
    INTEGER :: varid


    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Inquire the file to get the dimensions
    err_stat = IRLSE_NPOESS_netCDF_InquireFile( &
                 Filename, &
                 n_Frequencies   = n_frequencies  , &
                 n_Surface_Types = n_surface_types  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining IRLSE_NPOESS dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL IRLSE_NPOESS_Create( &
           IRLSE_NPOESS, &
           n_frequencies  , &
           n_surface_types  )
    IF ( .NOT. IRLSE_NPOESS_Associated(IRLSE_NPOESS) ) THEN
      msg = 'Error allocating output IRLSE_NPOESS'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file for reading
    nf90_status = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Read the global attributes
    err_stat = ReadGAtts( &
                 Filename, &
                 fileid  , &
                 Release = IRLSE_NPOESS%Release, &
                 Version = IRLSE_NPOESS%Version, &
                 Title   = Title               , &
                 History = History             , &
                 Comment = Comment               )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. IRLSE_NPOESS_ValidRelease( IRLSE_NPOESS ) ) THEN
      msg = 'IRLSE_NPOESS Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF

    
    ! Read the IRLSE_NPOESS data
    ! ...Frequency variable
    nf90_status = NF90_INQ_VARID( fileid,FREQUENCY_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,IRLSE_NPOESS%Frequency )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//FREQUENCY_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Surface_Type variable
    nf90_status = NF90_INQ_VARID( fileid,SURFACE_TYPE_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SURFACE_TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,IRLSE_NPOESS%Surface_Type )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SURFACE_TYPE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Reflectance variable
    nf90_status = NF90_INQ_VARID( fileid,REFLECTANCE_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFLECTANCE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,IRLSE_NPOESS%Reflectance )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//REFLECTANCE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_status = NF90_CLOSE( fileid ); CLOSE_FILE = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL IRLSE_NPOESS_Info( IRLSE_NPOESS, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
 
    SUBROUTINE Read_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( fileid )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( nf90_status ))
      END IF
      CALL IRLSE_NPOESS_Destroy( IRLSE_NPOESS )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp
    
  END FUNCTION IRLSE_NPOESS_netCDF_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL IRLSE_NPOESS_netCDF_IOVersion( Id )
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

  SUBROUTINE IRLSE_NPOESS_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE IRLSE_NPOESS_netCDF_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a IRLSE_NPOESS data file.

  FUNCTION WriteGAtts( &
    Filename, &  ! Input
    FileId  , &  ! Input
    Version , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    INTEGER     ,           INTENT(IN) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRLSE_NPOESS_WriteGAtts(netCDF)'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history'
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: gattname
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: ver
    INTEGER :: nf90_status
    TYPE(IRLSE_NPOESS_type) :: IRLSE_NPOESS

    ! Set up
    err_stat = SUCCESS
    msg = ' '

    ! Mandatory global attributes
    ! ...Software ID
    gattname = WRITE_MODULE_HISTORY_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),MODULE_VERSION_ID )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    gattname = CREATION_DATE_AND_TIME_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Release
    gattname = RELEASE_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),IRLSE_NPOESS%Release )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      ver = Version
    ELSE
      ver = IRLSE_NPOESS%Version
    END IF
    gattname = VERSION_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),Ver )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      gattname = TITLE_GATTNAME
      nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),title )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      gattname = HISTORY_GATTNAME
      nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),history )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      gattname = COMMENT_GATTNAME
      nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),comment )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    
 CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      nf90_status = NF90_CLOSE( FileId )
      IF ( nf90_status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( nf90_status ) )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(gattname)//' attribute to '//&
                            TRIM(Filename)//' - '// &
                            TRIM(NF90_STRERROR( nf90_status ) )//TRIM(msg), &
                            err_stat )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


  ! Function to read the global attributes from a IRLSE_NPOESS data file.

  FUNCTION ReadGAtts( &
    Filename, &  ! Input
    FileId  , &  ! Input
    Release , &  ! Optional output
    Version , &  ! Optional output
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: FileId
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRLSE_NPOESS_ReadGAtts(netCDF)'
    ! Local variables
    CHARACTER(ML)   :: msg
    CHARACTER(256)  :: gattname
    CHARACTER(5000) :: gattstring
    INTEGER :: nf90_status
    
    ! Set up
    err_stat = SUCCESS

    ! The global attributes
    ! ...The Release
    IF ( PRESENT(Release) ) THEN
      gattname = RELEASE_GATTNAME
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Release )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      gattname = VERSION_GATTNAME
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Version )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      gattname = TITLE_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( gattstring )
      title = gattstring(1:MIN(LEN(title), LEN_TRIM(gattstring)))
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      gattname = HISTORY_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( gattstring )
      history = gattstring(1:MIN(LEN(history), LEN_TRIM(gattstring)))
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      gattname = COMMENT_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( gattstring )
      comment = gattstring(1:MIN(LEN(comment), LEN_TRIM(gattstring)))
    END IF

  CONTAINS

    SUBROUTINE ReadGAtts_CleanUp()
      err_stat = FAILURE
      msg = 'Error reading '//TRIM(gattname)//' attribute from '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( nf90_status ) )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts


  ! Function to create a IRLSE_NPOESS file for writing

  FUNCTION CreateFile( &
    Filename       , &  ! Input
    n_Frequencies  , &  ! Input
    n_Surface_Types, &  ! Input
    FileId         , &  ! Output
    Version        , &  ! Optional input
    Title          , &  ! Optional input
    History        , &  ! Optional input
    Comment        ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: n_Frequencies  
    INTEGER     ,           INTENT(IN)  :: n_Surface_Types
    INTEGER     ,           INTENT(OUT) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRLSE_NPOESS_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: n_frequencies_dimid   
    INTEGER :: n_surface_types_dimid     
    INTEGER :: stsl_dimid 
    INTEGER :: varid
    INTEGER :: put_status(4)
    TYPE(IRLSE_NPOESS_type) :: dummy
    
    ! Setup
    err_stat = SUCCESS
    close_file = .FALSE.


    ! Create the data file
    nf90_status = NF90_CREATE( Filename,NF90_CLOBBER,FileId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Define the dimensions
    ! ...Frequency dimension of reflectance data
    nf90_status = NF90_DEF_DIM( FileID,FREQUENCY_DIMNAME,n_Frequencies,n_frequencies_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...List of NPOESS surface type classifications
    nf90_status = NF90_DEF_DIM( FileID,SURFACE_TYPE_DIMNAME,n_Surface_Types,n_surface_types_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SURFACE_TYPE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Surface type name string length
    nf90_status = NF90_DEF_DIM( FileID,STSL_DIMNAME,dummy%String_Length,stsl_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//STSL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
 

    ! Write the global attributes
    err_stat = WriteGAtts( &
                 Filename, &
                 FileId  , &
                 Version = Version, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attribute to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the variables
    ! ...Frequency variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                dimIDs=(/n_frequencies_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,FREQUENCY_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,FREQUENCY_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,FREQUENCY_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,FREQUENCY_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Surface_Type variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                SURFACE_TYPE_VARNAME, &
                                SURFACE_TYPE_TYPE, &
                                dimIDs=(/stsl_dimid,n_surface_types_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SURFACE_TYPE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,SURFACE_TYPE_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,SURFACE_TYPE_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,SURFACE_TYPE_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,SURFACE_TYPE_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SURFACE_TYPE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Reflectance variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                REFLECTANCE_VARNAME, &
                                REFLECTANCE_TYPE, &
                                dimIDs=(/n_frequencies_dimid,n_surface_types_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//REFLECTANCE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,REFLECTANCE_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,REFLECTANCE_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,REFLECTANCE_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,REFLECTANCE_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//REFLECTANCE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    nf90_status = NF90_ENDDEF( FileId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error taking file '//TRIM(Filename)// &
            ' out of define mode - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS
 
    SUBROUTINE Create_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( FileID )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR( nf90_status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Create_CleanUp
    
  END FUNCTION CreateFile

END MODULE IRLSE_NPOESS_netCDF_IO
