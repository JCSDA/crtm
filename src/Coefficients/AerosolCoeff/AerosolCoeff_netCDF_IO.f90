!
! AerosolCoeff_netCDF_IO
!
! Module containing routines to read and write AerosolCoeff netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Apr-2007
!                       paul.vandelst@noaa.gov
!

MODULE AerosolCoeff_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: Double, Long
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Exists
  USE String_Utility     , ONLY: StrClean
  USE AerosolCoeff_Define, ONLY: AerosolCoeff_type         , &
                                 AerosolCoeff_Associated   , &
                                 AerosolCoeff_Destroy      , &
                                 AerosolCoeff_Create       , &
                                 AerosolCoeff_Inspect      , &
                                 AerosolCoeff_ValidRelease , &
                                 AerosolCoeff_Info         , &
                                 AerosolCoeff_Frequency
                                 !AerosolCoeff_DefineVersion
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: AerosolCoeff_netCDF_InquireFile
  PUBLIC :: AerosolCoeff_netCDF_ReadFile
  PUBLIC :: AerosolCoeff_netCDF_WriteFile
  PUBLIC :: AerosolCoeff_netCDF_IOVersion


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

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME     = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME     = 'Version'
  CHARACTER(*), PARAMETER :: DATA_SOURCE_GATTNAME = 'Data_Source' 
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME       = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME     = 'History' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME     = 'Comment' 

  ! Dimension names
  CHARACTER(*), PARAMETER :: WAVELENGTH_DIMNAME = 'n_Wavelengths'
  CHARACTER(*), PARAMETER :: REFF_DIMNAME       = 'n_Radii'
  CHARACTER(*), PARAMETER :: TYPE_DIMNAME       = 'n_Types'
  CHARACTER(*), PARAMETER :: RH_DIMNAME         = 'n_RH'
  CHARACTER(*), PARAMETER :: LEGENDRE_DIMNAME   = 'n_Legendre_Terms'
  CHARACTER(*), PARAMETER :: PHASE_DIMNAME      = 'n_Phase_Elements'
  CHARACTER(*), PARAMETER :: TNSL_DIMNAME       = 'tnsl'

  ! Variable names
  CHARACTER(*), PARAMETER :: TYPE_VARNAME       = 'Aerosol_Type'
  CHARACTER(*), PARAMETER :: TYPE_NAME_VARNAME  = 'Aerosol_Type_Name'
  CHARACTER(*), PARAMETER :: WAVELENGTH_VARNAME = 'Wavelength'
  CHARACTER(*), PARAMETER :: REFF_VARNAME       = 'Reff'
  CHARACTER(*), PARAMETER :: RH_VARNAME         = 'RH'
  CHARACTER(*), PARAMETER :: KE_VARNAME         = 'ke'
  CHARACTER(*), PARAMETER :: W_VARNAME          = 'w'
  CHARACTER(*), PARAMETER :: G_VARNAME          = 'g'
  CHARACTER(*), PARAMETER :: PCOEFF_VARNAME     = 'pcoeff'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: TYPE_LONGNAME       = 'Aerosol type'
  CHARACTER(*), PARAMETER :: TYPE_NAME_LONGNAME  = 'Aerosol type name'
  CHARACTER(*), PARAMETER :: WAVELENGTH_LONGNAME = 'Wavelength'
  CHARACTER(*), PARAMETER :: REFF_LONGNAME       = 'Effective radius'
  CHARACTER(*), PARAMETER :: RH_LONGNAME         = 'Relative humidity'
  CHARACTER(*), PARAMETER :: KE_LONGNAME         = 'ke'
  CHARACTER(*), PARAMETER :: W_LONGNAME          = 'w'
  CHARACTER(*), PARAMETER :: G_LONGNAME          = 'g'
  CHARACTER(*), PARAMETER :: PCOEFF_LONGNAME     = 'pcoeff'
  
  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: TYPE_DESCRIPTION       = 'Flag/index value used to identify and reference the aerosol type'
  CHARACTER(*), PARAMETER :: TYPE_NAME_DESCRIPTION  = 'Name of the aerosol type'
  CHARACTER(*), PARAMETER :: WAVELENGTH_DESCRIPTION = 'Wavelength LUT dimension vector'
  CHARACTER(*), PARAMETER :: REFF_DESCRIPTION       = 'Effective radius LUT dimension vector'
  CHARACTER(*), PARAMETER :: RH_DESCRIPTION         = 'Relative humidity LUT dimension vector'
  CHARACTER(*), PARAMETER :: KE_DESCRIPTION         = 'Mass extinction coefficient for aerosol scatterers'
  CHARACTER(*), PARAMETER :: W_DESCRIPTION          = 'Single scatter albedo for aerosol scatterers'
  CHARACTER(*), PARAMETER :: G_DESCRIPTION          = 'Asymmetry parameter for aerosol scatterers'
  CHARACTER(*), PARAMETER :: PCOEFF_DESCRIPTION     = 'Phase coefficients for aerosol scatterers'


  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: TYPE_UNITS       = 'N/A'
  CHARACTER(*), PARAMETER :: TYPE_NAME_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: WAVELENGTH_UNITS = 'Microns (um)'
  CHARACTER(*), PARAMETER :: REFF_UNITS       = 'Microns (um)'
  CHARACTER(*), PARAMETER :: RH_UNITS         = 'fraction (0->1)'
  CHARACTER(*), PARAMETER :: KE_UNITS         = 'Metres squared per kilogram (m^2.kg^-1)'
  CHARACTER(*), PARAMETER :: W_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: G_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: PCOEFF_UNITS     = 'N/A'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER(Long), PARAMETER :: TYPE_FILLVALUE       = 0
  CHARACTER(*) , PARAMETER :: TYPE_NAME_FILLVALUE  = NF90_FILL_CHAR
  REAL(Double) , PARAMETER :: WAVELENGTH_FILLVALUE = ZERO
  REAL(Double) , PARAMETER :: REFF_FILLVALUE       = ZERO
  REAL(Double) , PARAMETER :: RH_FILLVALUE         = ZERO
  REAL(Double) , PARAMETER :: KE_FILLVALUE         = ZERO
  REAL(Double) , PARAMETER :: W_FILLVALUE          = ZERO
  REAL(Double) , PARAMETER :: G_FILLVALUE          = ZERO
  REAL(Double) , PARAMETER :: PCOEFF_FILLVALUE     = ZERO


  ! Variable types
  INTEGER, PARAMETER :: TYPE_TYPE       = NF90_INT
  INTEGER, PARAMETER :: TYPE_NAME_TYPE  = NF90_CHAR
  INTEGER, PARAMETER :: WAVELENGTH_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_TYPE  = NF90_DOUBLE
  INTEGER, PARAMETER :: REFF_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: RH_TYPE         = NF90_DOUBLE
  INTEGER, PARAMETER :: KE_TYPE         = NF90_DOUBLE 
  INTEGER, PARAMETER :: W_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: G_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: PCOEFF_TYPE     = NF90_DOUBLE


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
!       AerosolCoeff_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire AerosolCoeff object netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_netCDF_InquireFile( &
!                        Filename, &
!                        n_Wavelengths    = n_Wavelengths   , &
!                        n_Radii          = n_Radii         , &
!                        n_Types          = n_Types         , &
!                        n_RH             = n_RH            , &
!                        n_Legendre_Terms = n_Legendre_Terms, &
!                        n_Phase_Elements = n_Phase_Elements, &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           AerosolCoeff data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Wavelengths:      The number of wavelengths in the look-up
!                           table (LUT). Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Radii:            The number of discrete effective radii for
!                           scatterers in the LUT. Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Types:            The number of different aerosol types in
!                           the LUT. Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_RH:               The number of relative humidity entries in
!                           the LUT. Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:   The maximum number of Legendre polynomial
!                           terms in the LUT. Can be = 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Phase_Elements:   The maximum number of phase elements in the LUT.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Release:            The release number of the AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the AerosolCoeff file.
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
!                              == FAILURE an error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION AerosolCoeff_netCDF_InquireFile( &
    Filename        , &  ! Input
    n_Wavelengths   , &  ! Optional output
    n_Radii         , &  ! Optional output
    n_Types         , &  ! Optional output
    n_RH            , &  ! Optional output
    n_Legendre_Terms, &  ! Optional output
    n_Phase_Elements, &  ! Optional output
    Release         , &  ! Optional output
    Version         , &  ! Optional output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Wavelengths       
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Radii             
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Types             
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_RH                
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Legendre_Terms    
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Phase_Elements    
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release         
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_InquireFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: DimId
    TYPE(AerosolCoeff_type) :: AerosolCoeff
    
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
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=AerosolCoeff%n_Wavelengths )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//WAVELENGTH_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Radii dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,REFF_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//REFF_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=AerosolCoeff%n_Radii )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//REFF_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Types dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,TYPE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//TYPE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=AerosolCoeff%n_Types )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//TYPE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_RH dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,RH_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//RH_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=AerosolCoeff%n_RH )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//RH_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Legendre_Terms dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,LEGENDRE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LEGENDRE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=AerosolCoeff%n_Legendre_Terms )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//LEGENDRE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Phase_Elements dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,PHASE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//PHASE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=AerosolCoeff%n_Phase_Elements )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//PHASE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
  
  
    ! Get the global attributes
    err_stat = ReadGAtts( Filename, &
                          FileId  , &
                          Release = AerosolCoeff%Release, &
                          Version = AerosolCoeff%Version, &
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
    IF ( PRESENT(n_Wavelengths   ) ) n_Wavelengths    = AerosolCoeff%n_Wavelengths
    IF ( PRESENT(n_Radii         ) ) n_Radii          = AerosolCoeff%n_Radii      
    IF ( PRESENT(n_Types         ) ) n_Types          = AerosolCoeff%n_Types
    IF ( PRESENT(n_RH            ) ) n_RH             = AerosolCoeff%n_RH      
    IF ( PRESENT(n_Legendre_Terms) ) n_Legendre_Terms = AerosolCoeff%n_Legendre_Terms-1  ! Indexed from 0, so subtract 1.
    IF ( PRESENT(n_Phase_Elements) ) n_Phase_Elements = AerosolCoeff%n_Phase_Elements
    IF ( PRESENT(Release         ) ) Release          = AerosolCoeff%Release     
    IF ( PRESENT(Version         ) ) Version          = AerosolCoeff%Version     

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

  END FUNCTION AerosolCoeff_netCDF_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_netCDF_WriteFile
!
! PURPOSE:
!       Function to write AerosolCoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_netCDF_WriteFile( &
!                        Filename, &
!                        AerosolCoeff, &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       AerosolCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AerosolCoeff:   Object containing the aerosol coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AerosolCoeff_type)
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
!                       attribute field of the AerosolCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the AerosolCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the AerosolCoeff file.
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

  FUNCTION AerosolCoeff_netCDF_WriteFile( &
    Filename    , &  ! Input
    AerosolCoeff, &  ! Input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional input
    History     , &  ! Optional input
    Comment     ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN) :: Filename
    TYPE(AerosolCoeff_type), INTENT(IN) :: AerosolCoeff
    LOGICAL,       OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: History
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_WriteFile(netCDF)'
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
    IF ( .NOT. AerosolCoeff_Associated( AerosolCoeff ) ) THEN
      msg = 'AerosolCoeff structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. AerosolCoeff_ValidRelease( AerosolCoeff ) ) THEN
      msg = 'AerosolCoeff Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet


    ! Create the output file
    err_stat = CreateFile( &
                 Filename                              , &  ! Input
                 AerosolCoeff%n_Wavelengths            , &  ! Input
                 AerosolCoeff%n_Radii                  , &  ! Input
                 AerosolCoeff%n_Types                  , &  ! Input
                 AerosolCoeff%n_RH                     , &  ! Input
                 AerosolCoeff%n_Legendre_Terms         , &  ! Input
                 AerosolCoeff%n_Phase_Elements         , &  ! Input
                 FileId                                , &  ! Output
                 Version     = AerosolCoeff%Version    , &  ! Optional input
                 Data_Source = AerosolCoeff%Data_Source, &  ! Optional input
                 Title       = Title                   , &  ! Optional input
                 History     = History                 , &  ! Optional input
                 Comment     = Comment                   )  ! Optional input
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Write the data items
    ! ...Type variable 
    NF90_Status = NF90_INQ_VARID( FileId,TYPE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,AerosolCoeff%Type )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TYPE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Type_Name variable 
    NF90_Status = NF90_INQ_VARID( FileId,TYPE_NAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TYPE_NAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,AerosolCoeff%Type_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TYPE_NAME_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Wavelength variable 
    NF90_Status = NF90_INQ_VARID( FileId,WAVELENGTH_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//WAVELENGTH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,AerosolCoeff%Wavelength )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//WAVELENGTH_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Reff variable 
    NF90_Status = NF90_INQ_VARID( FileId,REFF_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,AerosolCoeff%Reff )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//REFF_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...RH variable 
    NF90_Status = NF90_INQ_VARID( FileId,RH_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//RH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,AerosolCoeff%RH )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//RH_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...ke variable 
    NF90_Status = NF90_INQ_VARID( FileId,KE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//KE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,AerosolCoeff%ke )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//KE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...w variable 
    NF90_Status = NF90_INQ_VARID( FileId,W_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//W_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,AerosolCoeff%w )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//W_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...g variable 
    NF90_Status = NF90_INQ_VARID( FileId,G_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//G_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,AerosolCoeff%g )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//G_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...pcoeff variable 
    NF90_Status = NF90_INQ_VARID( FileId,PCOEFF_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PCOEFF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,AerosolCoeff%pcoeff )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PCOEFF_VARNAME//' to '//TRIM(Filename)//&
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
      CALL AerosolCoeff_Info( AerosolCoeff, msg )
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

  END FUNCTION AerosolCoeff_netCDF_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_netCDF_ReadFile
!
! PURPOSE:
!       Function to read AerosolCoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_netCDF_ReadFile( &
!                        Filename         , &
!                        AerosolCoeff     , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       AerosolCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AerosolCoeff:   AerosolCoeff object containing the aerosol coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AerosolCoeff_type)
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
!                       attribute field of the AerosolCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the AerosolCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the AerosolCoeff file.
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

  FUNCTION AerosolCoeff_netCDF_ReadFile( &
    Filename    , &  ! Input
    AerosolCoeff, &  ! Output
    Quiet       , &  ! Optional input
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(AerosolCoeff_type), INTENT(OUT) :: AerosolCoeff
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_ReadFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: n_Wavelengths
    INTEGER :: n_Radii      
    INTEGER :: n_Types
    INTEGER :: n_RH      
    INTEGER :: n_Legendre_Terms
    INTEGER :: n_Phase_Elements
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
    err_stat = AerosolCoeff_netCDF_InquireFile( &
                 Filename, &
                 n_Wavelengths    = n_Wavelengths, &
                 n_Radii          = n_Radii      , &
                 n_Types          = n_Types, &
                 n_RH             = n_RH      , &
                 n_Legendre_Terms = n_Legendre_Terms, &
                 n_Phase_Elements = n_Phase_Elements  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining AerosolCoeff dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL AerosolCoeff_Create( &
           AerosolCoeff, &
           n_Wavelengths   , &
           n_Radii         , &
           n_Types         , &
           n_RH            , &
           n_Legendre_Terms, &
           n_Phase_Elements  )
    IF ( .NOT. AerosolCoeff_Associated(AerosolCoeff) ) THEN
      msg = 'Error allocating output AerosolCoeff'
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
                          Release     = AerosolCoeff%Release    , &
                          Version     = AerosolCoeff%Version    , &
                          Data_Source = AerosolCoeff%Data_Source, &
                          Title       = Title                   , &
                          History     = History                 , &
                          Comment     = Comment                   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. AerosolCoeff_ValidRelease( AerosolCoeff ) ) THEN
      msg = 'AerosolCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF

    
    ! Read the AerosolCoeff data
    ! ...Type variable 
    NF90_Status = NF90_INQ_VARID( FileId,TYPE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,AerosolCoeff%Type )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TYPE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Type_Name variable 
    NF90_Status = NF90_INQ_VARID( FileId,TYPE_NAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TYPE_NAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,AerosolCoeff%Type_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TYPE_NAME_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Wavelength variable 
    NF90_Status = NF90_INQ_VARID( FileId,WAVELENGTH_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//WAVELENGTH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,AerosolCoeff%Wavelength )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//WAVELENGTH_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Reff variable 
    NF90_Status = NF90_INQ_VARID( FileId,REFF_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,AerosolCoeff%Reff )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//REFF_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...RH variable 
    NF90_Status = NF90_INQ_VARID( FileId,RH_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//RH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,AerosolCoeff%RH )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//RH_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...ke variable 
    NF90_Status = NF90_INQ_VARID( FileId,KE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//KE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,AerosolCoeff%ke )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//KE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...w variable 
    NF90_Status = NF90_INQ_VARID( FileId,W_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//W_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,AerosolCoeff%w )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//W_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...g variable 
    NF90_Status = NF90_INQ_VARID( FileId,G_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//G_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,AerosolCoeff%g )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//G_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...pcoeff variable 
    NF90_Status = NF90_INQ_VARID( FileId,PCOEFF_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PCOEFF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,AerosolCoeff%pcoeff )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PCOEFF_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId ); Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Compute the frequencies
    CALL AerosolCoeff_Frequency( AerosolCoeff )    
    

    ! Output an info message
    IF ( Noisy ) THEN
      CALL AerosolCoeff_Info( AerosolCoeff, msg )
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
      CALL AerosolCoeff_Destroy( AerosolCoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp
    
  END FUNCTION AerosolCoeff_netCDF_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL AerosolCoeff_netCDF_IOVersion( Id )
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

  SUBROUTINE AerosolCoeff_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE AerosolCoeff_netCDF_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a AerosolCoeff data file.

  FUNCTION WriteGAtts( &
    Filename   , &  ! Input
    FileId     , &  ! Input
    Version    , &  ! Optional input
    Data_Source, &  ! Optional input
    Title      , &  ! Optional input
    History    , &  ! Optional input
    Comment    ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    INTEGER     ,           INTENT(IN) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Data_Source
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_WriteGAtts(netCDF)'
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
    TYPE(AerosolCoeff_type) :: AerosolCoeff

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
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),AerosolCoeff%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = AerosolCoeff%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Data_Source
    IF ( PRESENT(Data_Source) ) THEN
      GAttName = DATA_SOURCE_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),Data_Source )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
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


  ! Function to read the global attributes from a AerosolCoeff data file.

  FUNCTION ReadGAtts( &
    Filename   , &  ! Input
    FileId     , &  ! Input
    Release    , &  ! Optional output
    Version    , &  ! Optional output
    Data_Source, &  ! Optional output
    Title      , &  ! Optional output
    History    , &  ! Optional output
    Comment    ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: FileId
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Data_Source
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_ReadGAtts(netCDF)'
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
    ! ...The Data_Source
    IF ( PRESENT(Data_Source) ) THEN
      GAttName = DATA_SOURCE_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      Data_Source = GAttString(1:MIN(LEN(Data_Source), LEN_TRIM(GAttString)))
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


  ! Function to create a AerosolCoeff file for writing

  FUNCTION CreateFile( &
    Filename        , &  ! Input
    n_Wavelengths   , &  ! Input
    n_Radii         , &  ! Input
    n_Types         , &  ! Input
    n_RH            , &  ! Input
    n_Legendre_Terms, &  ! Input
    n_Phase_Elements, &  ! Input
    FileId          , &  ! Output
    Version         , &  ! Optional input
    Data_Source     , &  ! Optional input
    Title           , &  ! Optional input
    History         , &  ! Optional input
    Comment         ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: n_Wavelengths
    INTEGER     ,           INTENT(IN)  :: n_Radii      
    INTEGER     ,           INTENT(IN)  :: n_Types
    INTEGER     ,           INTENT(IN)  :: n_RH      
    INTEGER     ,           INTENT(IN)  :: n_Legendre_Terms
    INTEGER     ,           INTENT(IN)  :: n_Phase_Elements
    INTEGER     ,           INTENT(OUT) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Data_Source
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: n_Wavelengths_DimID
    INTEGER :: n_Radii_DimID
    INTEGER :: n_Types_DimID
    INTEGER :: n_RH_DimID
    INTEGER :: n_Legendre_Terms_DimID
    INTEGER :: n_Phase_Elements_DimID
    INTEGER :: tnsl_DimID
    INTEGER :: varID
    INTEGER :: Put_Status(4)
    TYPE(AerosolCoeff_type) :: dummy
    
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
    ! ...Number of wavelengths
    NF90_Status = NF90_DEF_DIM( FileID,WAVELENGTH_DIMNAME,n_Wavelengths,n_Wavelengths_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//WAVELENGTH_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of radii
    NF90_Status = NF90_DEF_DIM( FileID,REFF_DIMNAME,n_Radii,n_Radii_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//REFF_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of aerosol types
    NF90_Status = NF90_DEF_DIM( FileID,TYPE_DIMNAME,n_Types,n_Types_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TYPE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of relative humidities
    NF90_Status = NF90_DEF_DIM( FileID,RH_DIMNAME,n_RH,n_RH_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//RH_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of Legendre terms
    ! ...Array indexing starts at 0, so +1
    NF90_Status = NF90_DEF_DIM( FileID,LEGENDRE_DIMNAME,n_Legendre_Terms+1,n_Legendre_Terms_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEGENDRE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of phase elements
    NF90_Status = NF90_DEF_DIM( FileID,PHASE_DIMNAME,n_Phase_Elements,n_Phase_Elements_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PHASE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Type_Name string length
    CALL AerosolCoeff_Create(dummy,0,0,1,0,0,0) ! Only n_Types dimension non-zero
    NF90_Status = NF90_DEF_DIM( FileID,TNSL_DIMNAME,LEN(dummy%Type_Name(1)),tnsl_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TNSL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    CALL AerosolCoeff_Destroy(dummy)


    ! Write the global attributes
    err_stat = WriteGAtts( Filename, &
                           FileId  , &
                           Version     = Version    , &
                           Data_Source = Data_Source, &
                           Title       = Title      , &
                           History     = History    , &
                           Comment     = Comment      )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attribute to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the variables
    ! ...Type variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      TYPE_VARNAME, &
      TYPE_TYPE, &
      dimIDs=(/n_Types_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TYPE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,TYPE_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,TYPE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,TYPE_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,TYPE_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TYPE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Type_Name variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      TYPE_NAME_VARNAME, &
      TYPE_NAME_TYPE, &
      dimIDs=(/tnsl_DimID, n_Types_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TYPE_NAME_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,TYPE_NAME_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,TYPE_NAME_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,TYPE_NAME_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,TYPE_NAME_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TYPE_NAME_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Wavelength variable
    NF90_Status = NF90_DEF_VAR( FileID, &
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
    ! ...Reff variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      REFF_VARNAME, &
      REFF_TYPE, &
      dimIDs=(/n_Radii_DimID,n_Types_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//REFF_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,REFF_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,REFF_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,REFF_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,REFF_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//REFF_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...RH variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      RH_VARNAME, &
      RH_TYPE, &
      dimIDs=(/n_RH_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//RH_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,RH_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,RH_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,RH_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,RH_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//RH_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...ke variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      KE_VARNAME, &
      KE_TYPE, &
      dimIDs=(/n_Wavelengths_DimID, n_Radii_DimID, n_Types_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//KE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,KE_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,KE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,KE_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,KE_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//KE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...w variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      W_VARNAME, &
      W_TYPE, &
      dimIDs=(/n_Wavelengths_DimID, n_Radii_DimID, n_Types_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//W_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,W_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,W_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,W_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,W_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//W_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...g variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      G_VARNAME, &
      G_TYPE, &
      dimIDs=(/n_Wavelengths_DimID, n_Radii_DimID, n_Types_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//G_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,G_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,G_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,G_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,G_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//G_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...pcoeff variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      PCOEFF_VARNAME, &
      PCOEFF_TYPE, &
      dimIDs=(/n_Wavelengths_DimID, n_Radii_DimID, n_Types_DimID, &
               n_Legendre_Terms_DimID, n_Phase_Elements_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PCOEFF_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,PCOEFF_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,PCOEFF_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,PCOEFF_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,PCOEFF_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PCOEFF_VARNAME//' variable attributes to '//TRIM(Filename)
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

END MODULE AerosolCoeff_netCDF_IO
