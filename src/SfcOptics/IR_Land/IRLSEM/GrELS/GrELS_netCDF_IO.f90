!
! GrELS_netCDF_IO
!
! Module containing routines to inquire, read, and write netCDF
! GrELS object datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 15-Feb-2010
!                       paul.vandelst@noaa.gov
!

MODULE GrELS_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility   , ONLY: File_Exists
  USE String_Utility , ONLY: StrClean
  USE GrELS_Define   , ONLY: GrELS_type         , &
                             GrELS_Associated   , &
                             GrELS_Destroy      , &
                             GrELS_Create       , &
                             GrELS_Inspect      , &
                             GrELS_ValidRelease , &
                             GrELS_Info         , &
                             GrELS_DefineVersion
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: GrELS_netCDF_InquireFile
  PUBLIC :: GrELS_netCDF_ReadFile
  PUBLIC :: GrELS_netCDF_WriteFile
  PUBLIC :: GrELS_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module version
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default msg string length
  INTEGER, PARAMETER :: ML = 1024
  
  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME   = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME = 'History' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME = 'Comment' 
  
  ! Dimension names. Case sensitive
  CHARACTER(*), PARAMETER :: WAVELENGTH_DIMNAME    = 'n_Wavelengths' 
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_DIMNAME  = 'n_Surface_Types'
  CHARACTER(*), PARAMETER :: WEEK_DIMNAME          = 'n_Weeks' 
  CHARACTER(*), PARAMETER :: LATITUDE_ZONE_DIMNAME = 'n_Latitude_Zones'
  CHARACTER(*), PARAMETER :: STSL_DIMNAME          = 'stsl'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: WAVELENGTH_VARNAME        = 'Wavelength'
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_NAME_VARNAME = 'Surface_Type_Name'
  CHARACTER(*), PARAMETER :: WEEK_VARNAME              = 'Week'
  CHARACTER(*), PARAMETER :: LATITUDE_ZONE_VARNAME     = 'Latitude_Zone'
  CHARACTER(*), PARAMETER :: REFLECTANCE_VARNAME       = 'Reflectance'
  CHARACTER(*), PARAMETER :: GVF_VARNAME               = 'GVF'

  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'
  CHARACTER(*), PARAMETER :: WAVELENGTH_DESCRIPTION         = 'Wavelengths for which the reflectance '//&
                                                              'LUT data is specified.'
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_NAME_DESCRIPTION  = 'Surface types for which the reflectance '//&
                                                              'LUT data is specified.'
  CHARACTER(*), PARAMETER :: WEEK_DESCRIPTION               = 'Week of year for which the greenness vegetation '//&
                                                              'fraction LUT data is specified.'
  CHARACTER(*), PARAMETER :: LATITUDE_ZONE_DESCRIPTION      = 'Latitude zone for which the greenness vegetation '//&
                                                              'fraction LUT data is specified.'
  CHARACTER(*), PARAMETER :: REFLECTANCE_DESCRIPTION        = 'Surface type reflectance spectra.'
  CHARACTER(*), PARAMETER :: GVF_DESCRIPTION                = 'Surface type spatial and temporal greenness '//&
                                                              'vegetation fraction'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  CHARACTER(*), PARAMETER :: WAVELENGTH_LONGNAME         = 'Wavelength'
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_NAME_LONGNAME  = 'Surface Type'
  CHARACTER(*), PARAMETER :: WEEK_LONGNAME               = 'Week'
  CHARACTER(*), PARAMETER :: LATITUDE_ZONE_LONGNAME      = 'Latitude Zone'
  CHARACTER(*), PARAMETER :: REFLECTANCE_LONGNAME        = 'Reflectance'
  CHARACTER(*), PARAMETER :: GVF_LONGNAME                = 'Greenness Vegetation Fraction'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  CHARACTER(*), PARAMETER :: WAVELENGTH_UNITS         = 'Microns'
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_NAME_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: WEEK_UNITS               = 'Week of year'
  CHARACTER(*), PARAMETER :: LATITUDE_ZONE_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: REFLECTANCE_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: GVF_UNITS                = 'N/A'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  REAL(fp)    , PARAMETER :: WAVELENGTH_FILLVALUE         = 0.0_fp
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_NAME_FILLVALUE  = ACHAR(0)
  INTEGER     , PARAMETER :: WEEK_FILLVALUE               = 0
  REAL(fp)    , PARAMETER :: LATITUDE_ZONE_FILLVALUE      = 0.0_fp
  REAL(fp)    , PARAMETER :: REFLECTANCE_FILLVALUE        = 0.0_fp
  REAL(fp)    , PARAMETER :: GVF_FILLVALUE                = 0.0_fp

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: WAVELENGTH_TYPE         = NF90_DOUBLE
  INTEGER, PARAMETER :: SURFACE_TYPE_NAME_TYPE  = NF90_CHAR
  INTEGER, PARAMETER :: WEEK_TYPE               = NF90_INT
  INTEGER, PARAMETER :: LATITUDE_ZONE_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: REFLECTANCE_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: GVF_TYPE                = NF90_DOUBLE


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
!       GrELS_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire GrELS object netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = GrELS_netCDF_InquireFile( &
!                        Filename, &
!                        n_Wavelengths    = n_Wavelengths   , &
!                        n_Surface_Types  = n_Surface_Types , &
!                        n_Weeks          = n_Weeks         , &
!                        n_Latitude_Zones = n_Latitude_Zones, &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:         Character string specifying the name of the
!                         GrELS data file to inquire.
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
!       Title:            Character string written into the TITLE global
!                         attribute field of the GrELS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the GrELS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the GrELS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the file inquiry was successful
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION GrELS_netCDF_InquireFile( &
    Filename        , &  ! Input
    n_Wavelengths   , &  ! Optional output
    n_Surface_Types , &  ! Optional output
    n_Weeks         , &  ! Optional output
    n_Latitude_Zones, &  ! Optional output
    Release         , &  ! Optional output
    Version         , &  ! Optional output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Wavelengths       
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Surface_Types     
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Weeks             
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Latitude_Zones    
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release         
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_InquireFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: DimId
    TYPE(GrELS_type) :: GrELS

    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.


    ! Open the file
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Get the dimensions
    ! ...n_Wavelengths dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,WAVELENGTH_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//WAVELENGTH_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=GrELS%n_Wavelengths )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//WAVELENGTH_DIMNAME//' - '// &
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
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=GrELS%n_Surface_Types )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//SURFACE_TYPE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Weeks dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,WEEK_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//WEEK_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=GrELS%n_Weeks )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//WEEK_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Latitude_Zones dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,LATITUDE_ZONE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LATITUDE_ZONE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=GrELS%n_Latitude_Zones )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//LATITUDE_ZONE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the global attributes
    err_stat = ReadGAtts( Filename, &
                          FileId  , &
                          Release = GrELS%Release, &
                          Version = GrELS%Version, &
                          Title   = Title  , &
                          History = History, &
                          Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Wavelengths   ) ) n_Wavelengths    = GrELS%n_Wavelengths   
    IF ( PRESENT(n_Surface_Types ) ) n_Surface_Types  = GrELS%n_Surface_Types 
    IF ( PRESENT(n_Weeks         ) ) n_Weeks          = GrELS%n_Weeks         
    IF ( PRESENT(n_Latitude_Zones) ) n_Latitude_Zones = GrELS%n_Latitude_Zones
    IF ( PRESENT(Release         ) ) Release          = GrELS%Release     
    IF ( PRESENT(Version         ) ) Version          = GrELS%Version     

  CONTAINS
 
    SUBROUTINE Inquire_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION GrELS_netCDF_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_netCDF_WriteFile
!
! PURPOSE:
!       Function to write GrELS object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = GrELS_netCDF_WriteFile( &
!                        Filename, &
!                        GrELS   , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       GrELS data file to write.
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
!       Title:          Character string written into the TITLE global
!                       attribute field of the GrELS file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the GrELS file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the GrELS file.
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

  FUNCTION GrELS_netCDF_WriteFile( &
    Filename, &  ! Input
    GrELS   , &  ! Input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(GrELS_type),       INTENT(IN) :: GrELS
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_WriteFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: VarId

    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check structure pointer association status
    IF ( .NOT. GrELS_Associated( GrELS ) ) THEN
      msg = 'GrELS structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. GrELS_ValidRelease( GrELS ) ) THEN
      msg = 'GrELS Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet


    ! Create the output file
    err_stat = CreateFile( &
                 Filename               , &  ! Input
                 GrELS%n_Wavelengths    , &  ! Input
                 GrELS%n_Surface_Types  , &  ! Input
                 GrELS%n_Weeks          , &  ! Input
                 GrELS%n_Latitude_Zones , &  ! Input
                 FileId                 , &  ! Output
                 Version = GrELS%Version, &  ! Optional input
                 Title   = Title        , &  ! Optional input
                 History = History      , &  ! Optional input
                 Comment = Comment        )  ! Optional input
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Write the data items
    ! ...Wavelength variable 
    NF90_Status = NF90_INQ_VARID( FileId,WAVELENGTH_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//WAVELENGTH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,GrELS%Wavelength )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//WAVELENGTH_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Surface_Type_Name variable 
    NF90_Status = NF90_INQ_VARID( FileId,SURFACE_TYPE_NAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SURFACE_TYPE_NAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,GrELS%Surface_Type_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SURFACE_TYPE_NAME_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Week variable 
    NF90_Status = NF90_INQ_VARID( FileId,WEEK_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//WEEK_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,GrELS%Week )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//WEEK_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Latitude_Zone variable 
    NF90_Status = NF90_INQ_VARID( FileId,LATITUDE_ZONE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LATITUDE_ZONE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,GrELS%Latitude_Zone )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LATITUDE_ZONE_VARNAME//' to '//TRIM(Filename)//&
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
    NF90_Status = NF90_PUT_VAR( FileId,VarID,GrELS%Reflectance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//REFLECTANCE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...GVF variable 
    NF90_Status = NF90_INQ_VARID( FileId,GVF_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//GVF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,GrELS%GVF )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//GVF_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL GrELS_Info( GrELS, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION GrELS_netCDF_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_netCDF_ReadFile
!
! PURPOSE:
!       Function to read GrELS object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = GrELS_netCDF_ReadFile( &
!                        Filename, &
!                        GrELS   , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       GrELS data file to write.
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
! OPTIONAL OUTPUTS:
!       Title:          Character string written into the TITLE global
!                       attribute field of the GrELS file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the GrELS file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the GrELS file.
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

  FUNCTION GrELS_netCDF_ReadFile( &
    Filename, &  ! Input
    GrELS   , &  ! Output
    Quiet   , &  ! Optional input
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(GrELS_type),       INTENT(OUT) :: GrELS
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_ReadFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: n_Wavelengths   
    INTEGER :: n_Surface_Types 
    INTEGER :: n_Weeks         
    INTEGER :: n_Latitude_Zones
    INTEGER :: VarId


    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet


    ! Inquire the file to get the dimensions
    err_stat = GrELS_netCDF_InquireFile( &
                 Filename, &
                 n_Wavelengths    = n_Wavelengths   , &
                 n_Surface_Types  = n_Surface_Types , &
                 n_Weeks          = n_Weeks         , &
                 n_Latitude_Zones = n_Latitude_Zones  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining GrELS dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL GrELS_Create( &
           GrELS, &
           n_Wavelengths   , &
           n_Surface_Types , &
           n_Weeks         , &
           n_Latitude_Zones  )
    IF ( .NOT. GrELS_Associated(GrELS) ) THEN
      msg = 'Error allocating output GrELS'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file for reading
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Read the global attributes
    err_stat = ReadGAtts( Filename, &
                          FileID  , &
                          Release = GrELS%Release, &
                          Version = GrELS%Version, &
                          Title   = Title          , &
                          History = History        , &
                          Comment = Comment          )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. GrELS_ValidRelease( GrELS ) ) THEN
      msg = 'GrELS Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF

    
    ! Read the GrELS data
    ! ...Wavelength variable 
    NF90_Status = NF90_INQ_VARID( FileId,WAVELENGTH_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//WAVELENGTH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,GrELS%Wavelength )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//WAVELENGTH_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Surface_Type_Name variable 
    NF90_Status = NF90_INQ_VARID( FileId,SURFACE_TYPE_NAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SURFACE_TYPE_NAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,GrELS%Surface_Type_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SURFACE_TYPE_NAME_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Week variable 
    NF90_Status = NF90_INQ_VARID( FileId,WEEK_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//WEEK_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,GrELS%Week )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//WEEK_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Latitude_Zone variable 
    NF90_Status = NF90_INQ_VARID( FileId,LATITUDE_ZONE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LATITUDE_ZONE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,GrELS%Latitude_Zone )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LATITUDE_ZONE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Reflectance variable 
    NF90_Status = NF90_INQ_VARID( FileId,REFLECTANCE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFLECTANCE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,GrELS%Reflectance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//REFLECTANCE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...GVF variable 
    NF90_Status = NF90_INQ_VARID( FileId,GVF_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//GVF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,GrELS%GVF )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//GVF_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId ); Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL GrELS_Info( GrELS, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
 
    SUBROUTINE Read_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      CALL GrELS_Destroy( GrELS )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp
    
  END FUNCTION GrELS_netCDF_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GrELS_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL GrELS_netCDF_IOVersion( Id )
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

  SUBROUTINE GrELS_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE GrELS_netCDF_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a GrELS data file.

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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_WriteGAtts(netCDF)'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history'
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: GAttName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: Ver
    INTEGER :: NF90_Status
    TYPE(GrELS_type) :: GrELS

    ! Set up
    err_stat = SUCCESS
    msg = ' '

    ! Mandatory global attributes
    ! ...Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),MODULE_VERSION_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),GrELS%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = GrELS%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),history )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    
 CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      NF90_Status = NF90_CLOSE( FileId )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(msg), &
                            err_stat )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


  ! Function to read the global attributes from a GrELS data file.

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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_ReadGAtts(netCDF)'
    ! Local variables
    CHARACTER(ML)   :: msg
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: NF90_Status
    
    ! Set up
    err_stat = SUCCESS

    ! The global attributes
    ! ...The Release
    IF ( PRESENT(Release) ) THEN
      GAttName = RELEASE_GATTNAME
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),Release )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      GAttName = TITLE_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      title = GAttString(1:MIN(LEN(title), LEN_TRIM(GAttString)))
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      GAttName = HISTORY_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      history = GAttString(1:MIN(LEN(history), LEN_TRIM(GAttString)))
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      GAttName = COMMENT_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      comment = GAttString(1:MIN(LEN(comment), LEN_TRIM(GAttString)))
    END IF

  CONTAINS

    SUBROUTINE ReadGAtts_CleanUp()
      err_stat = FAILURE
      msg = 'Error reading '//TRIM(GAttName)//' attribute from '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ) )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts



  ! Function to create a GrELS file for writing

  FUNCTION CreateFile( &
    Filename        , &  ! Input
    n_Wavelengths   , &  ! Input
    n_Surface_Types , &  ! Input
    n_Weeks         , &  ! Input
    n_Latitude_Zones, &  ! Input
    FileId          , &  ! Output
    Version         , &  ! Optional input
    Title           , &  ! Optional input
    History         , &  ! Optional input
    Comment         ) &  ! Optional input
  RESULT( err_stat )

    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: n_Wavelengths   
    INTEGER     ,           INTENT(IN)  :: n_Surface_Types 
    INTEGER     ,           INTENT(IN)  :: n_Weeks         
    INTEGER     ,           INTENT(IN)  :: n_Latitude_Zones
    INTEGER     ,           INTENT(OUT) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GrELS_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: n_Wavelengths_DimID
    INTEGER :: n_Surface_Types_DimID
    INTEGER :: n_Weeks_DimID
    INTEGER :: n_Latitude_Zones_DimID
    INTEGER :: stsl_DimID
    INTEGER :: varID
    INTEGER :: Put_Status(4)
    TYPE(GrELS_type) :: GrELS
    
    ! Setup
    err_stat = SUCCESS
    Close_File = .FALSE.


    ! Create the data file
    NF90_Status = NF90_CREATE( Filename,NF90_CLOBBER,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Define the dimensions
    ! ...Number of wavelengths for which the reflectance LUT data is specified.
    NF90_Status = NF90_DEF_DIM( FileID,WAVELENGTH_DIMNAME,n_Wavelengths,n_Wavelengths_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//WAVELENGTH_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of surface types for which the reflectance LUT data is specified.
    NF90_Status = NF90_DEF_DIM( FileID,SURFACE_TYPE_DIMNAME,n_Surface_Types,n_Surface_Types_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SURFACE_TYPE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of weeks for which the green vegetation fraction LUT data is specified.
    NF90_Status = NF90_DEF_DIM( FileID,WEEK_DIMNAME,n_Weeks,n_Weeks_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//WEEK_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of latitude zones for which the green vegetation fraction LUT data is specified.
    NF90_Status = NF90_DEF_DIM( FileID,LATITUDE_ZONE_DIMNAME,n_Latitude_Zones,n_Latitude_Zones_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LATITUDE_ZONE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...String length for surface type names
    NF90_Status = NF90_DEF_DIM( FileID,STSL_DIMNAME,GrELS%stsl,stsl_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//STSL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts( Filename, &
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
    ! ...Wavelength variable
    NF90_Status = NF90_DEF_VAR( &
                    FileID, &
                    WAVELENGTH_VARNAME, &
                    WAVELENGTH_TYPE, &
                    dimIDs=(/n_Wavelengths_DimID/), &
                    varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//WAVELENGTH_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,WAVELENGTH_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,WAVELENGTH_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,WAVELENGTH_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,WAVELENGTH_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//WAVELENGTH_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Surface_Type_Name variable
    NF90_Status = NF90_DEF_VAR( &
                    FileID, &
                    SURFACE_TYPE_NAME_VARNAME, &
                    SURFACE_TYPE_NAME_TYPE, &
                    dimIDs=(/stsl_DimID, n_Surface_Types_DimID/), &
                    varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SURFACE_TYPE_NAME_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,SURFACE_TYPE_NAME_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,SURFACE_TYPE_NAME_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,SURFACE_TYPE_NAME_UNITS       )
    Put_Status(4) = NF90_NOERR  ! Use default fill value for characters
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SURFACE_TYPE_NAME_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Week variable
    NF90_Status = NF90_DEF_VAR( &
                    FileID, &
                    WEEK_VARNAME, &
                    WEEK_TYPE, &
                    dimIDs=(/n_Weeks_DimID/), &
                    varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//WEEK_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,WEEK_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,WEEK_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,WEEK_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,WEEK_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//WEEK_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Latitude_Zone variable
    NF90_Status = NF90_DEF_VAR( &
                    FileID, &
                    LATITUDE_ZONE_VARNAME, &
                    LATITUDE_ZONE_TYPE, &
                    dimIDs=(/n_Latitude_Zones_DimID/), &
                    varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LATITUDE_ZONE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,LATITUDE_ZONE_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,LATITUDE_ZONE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,LATITUDE_ZONE_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,LATITUDE_ZONE_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//LATITUDE_ZONE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Reflectance variable
    NF90_Status = NF90_DEF_VAR( &
                    FileID, &
                    REFLECTANCE_VARNAME, &
                    REFLECTANCE_TYPE, &
                    dimIDs=(/n_Wavelengths_DimID, n_Surface_Types_DimID/), &
                    varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//REFLECTANCE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,REFLECTANCE_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,REFLECTANCE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,REFLECTANCE_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,REFLECTANCE_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//REFLECTANCE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...GVF variable
    NF90_Status = NF90_DEF_VAR( &
                    FileID, &
                    GVF_VARNAME, &
                    GVF_TYPE, &
                    dimIDs=(/n_Surface_Types_DimID, n_Weeks_DimID, n_Latitude_Zones_DimID/), &
                    varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//GVF_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,GVF_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,GVF_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,GVF_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,GVF_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//GVF_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    NF90_Status = NF90_ENDDEF( FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error taking file '//TRIM(Filename)// &
            ' out of define mode - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS
 
    SUBROUTINE Create_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileID )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Create_CleanUp
    
  END FUNCTION CreateFile

END MODULE GrELS_netCDF_IO
