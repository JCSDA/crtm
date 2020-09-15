!
! CloudCoeff_netCDF_IO
!
! Module containing routines to read and write CloudCoeff netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-Apr-2007
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CloudCoeff_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds       , ONLY: Double
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility     , ONLY: File_Exists
  USE String_Utility   , ONLY: StrClean
  USE CloudCoeff_Define, ONLY: CloudCoeff_type         , &
                               CloudCoeff_Associated   , &
                               CloudCoeff_Destroy      , &
                               CloudCoeff_Create       , &
                               CloudCoeff_Inspect      , &
                               CloudCoeff_ValidRelease , &
                               CloudCoeff_Info
                               !CloudCoeff_DefineVersion
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: CloudCoeff_netCDF_InquireFile
  PUBLIC :: CloudCoeff_netCDF_ReadFile
  PUBLIC :: CloudCoeff_netCDF_WriteFile
  PUBLIC :: CloudCoeff_netCDF_IOVersion


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
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME   = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME = 'History' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME = 'Comment' 

  ! Dimension names
  CHARACTER(*), PARAMETER :: MW_FREQ_DIMNAME     = 'n_MW_Frequencies'
  CHARACTER(*), PARAMETER :: MW_REFF_DIMNAME     = 'n_MW_Radii'
  CHARACTER(*), PARAMETER :: IR_FREQ_DIMNAME     = 'n_IR_Frequencies'
  CHARACTER(*), PARAMETER :: IR_REFF_DIMNAME     = 'n_IR_Radii'
  CHARACTER(*), PARAMETER :: TEMPERATURE_DIMNAME = 'n_Temperatures'
  CHARACTER(*), PARAMETER :: DENSITY_DIMNAME     = 'n_Densities'
  CHARACTER(*), PARAMETER :: IR_DENSITY_DIMNAME  = 'n_IR_Densities'
  CHARACTER(*), PARAMETER :: LEGENDRE_DIMNAME    = 'n_Legendre_Terms'
  CHARACTER(*), PARAMETER :: PHASE_DIMNAME       = 'n_Phase_Elements'

  ! Variable names
  CHARACTER(*), PARAMETER :: FREQUENCY_MW_VARNAME = 'Frequency_MW'
  CHARACTER(*), PARAMETER :: FREQUENCY_IR_VARNAME = 'Frequency_IR'
  CHARACTER(*), PARAMETER :: REFF_MW_VARNAME      = 'Reff_MW'
  CHARACTER(*), PARAMETER :: REFF_IR_VARNAME      = 'Reff_IR'
  CHARACTER(*), PARAMETER :: TEMPERATURE_VARNAME  = 'Temperature'
  CHARACTER(*), PARAMETER :: DENSITY_VARNAME      = 'Density'

  CHARACTER(*), PARAMETER :: KE_L_MW_VARNAME     = 'ke_L_MW'
  CHARACTER(*), PARAMETER :: W_L_MW_VARNAME      = 'w_L_MW'
  CHARACTER(*), PARAMETER :: G_L_MW_VARNAME      = 'g_L_MW'
  CHARACTER(*), PARAMETER :: PCOEFF_L_MW_VARNAME = 'pcoeff_L_MW'

  CHARACTER(*), PARAMETER :: KE_S_MW_VARNAME     = 'ke_S_MW'
  CHARACTER(*), PARAMETER :: W_S_MW_VARNAME      = 'w_S_MW'
  CHARACTER(*), PARAMETER :: G_S_MW_VARNAME      = 'g_S_MW'
  CHARACTER(*), PARAMETER :: PCOEFF_S_MW_VARNAME = 'pcoeff_S_MW'

  CHARACTER(*), PARAMETER :: KE_IR_VARNAME       = 'ke_IR'
  CHARACTER(*), PARAMETER :: W_IR_VARNAME        = 'w_IR'
  CHARACTER(*), PARAMETER :: G_IR_VARNAME        = 'g_IR'
  CHARACTER(*), PARAMETER :: PCOEFF_IR_VARNAME   = 'pcoeff_IR'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: FREQUENCY_MW_LONGNAME = 'Frequency'
  CHARACTER(*), PARAMETER :: FREQUENCY_IR_LONGNAME = 'Frequency'
  CHARACTER(*), PARAMETER :: REFF_MW_LONGNAME      = 'Effective radius'
  CHARACTER(*), PARAMETER :: REFF_IR_LONGNAME      = 'Effective radius'
  CHARACTER(*), PARAMETER :: TEMPERATURE_LONGNAME  = 'Temperature'
  CHARACTER(*), PARAMETER :: DENSITY_LONGNAME      = 'Density'

  CHARACTER(*), PARAMETER :: KE_L_MW_LONGNAME     = 'Microwave ke(L)'
  CHARACTER(*), PARAMETER :: W_L_MW_LONGNAME      = 'Microwave w(L)'
  CHARACTER(*), PARAMETER :: G_L_MW_LONGNAME      = 'Microwave g(L)'
  CHARACTER(*), PARAMETER :: PCOEFF_L_MW_LONGNAME = 'Microwave pcoeff(L)'

  CHARACTER(*), PARAMETER :: KE_S_MW_LONGNAME     = 'Microwave ke(S)'
  CHARACTER(*), PARAMETER :: W_S_MW_LONGNAME      = 'Microwave w(S)'
  CHARACTER(*), PARAMETER :: G_S_MW_LONGNAME      = 'Microwave g(S)'
  CHARACTER(*), PARAMETER :: PCOEFF_S_MW_LONGNAME = 'Microwave pcoeff(S)'

  CHARACTER(*), PARAMETER :: KE_IR_LONGNAME       = 'Infrared ke'
  CHARACTER(*), PARAMETER :: W_IR_LONGNAME        = 'Infrared w'
  CHARACTER(*), PARAMETER :: G_IR_LONGNAME        = 'Infrared g'
  CHARACTER(*), PARAMETER :: PCOEFF_IR_LONGNAME   = 'Infrared pcoeff'
  
  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: FREQUENCY_MW_DESCRIPTION = 'Microwave frequency LUT dimension vector'
  CHARACTER(*), PARAMETER :: FREQUENCY_IR_DESCRIPTION = 'Infrared frequency LUT dimension vector'
  CHARACTER(*), PARAMETER :: REFF_MW_DESCRIPTION      = 'Microwave effective radius LUT dimension vector'
  CHARACTER(*), PARAMETER :: REFF_IR_DESCRIPTION      = 'Infrared effective radius LUT dimension vector'
  CHARACTER(*), PARAMETER :: TEMPERATURE_DESCRIPTION  = 'Temperature LUT dimension vector'
  CHARACTER(*), PARAMETER :: DENSITY_DESCRIPTION      = 'Density LUT dimension vector'

  CHARACTER(*), PARAMETER :: KE_L_MW_DESCRIPTION     = 'Mass extinction coefficient for liquid phase microwave scatterers'
  CHARACTER(*), PARAMETER :: W_L_MW_DESCRIPTION      = 'Single scatter albedo for liquid phase microwave scatterers'
  CHARACTER(*), PARAMETER :: G_L_MW_DESCRIPTION      = 'Asymmetry parameter for liquid phase microwave scatterers'
  CHARACTER(*), PARAMETER :: PCOEFF_L_MW_DESCRIPTION = 'Phase coefficients for liquid phase microwave scatterers'

  CHARACTER(*), PARAMETER :: KE_S_MW_DESCRIPTION     = 'Mass extinction coefficient for solid phase microwave scatterers'
  CHARACTER(*), PARAMETER :: W_S_MW_DESCRIPTION      = 'Single scatter albedo for solid phase microwave scatterers'
  CHARACTER(*), PARAMETER :: G_S_MW_DESCRIPTION      = 'Asymmetry parameter for solid phase microwave scatterers'
  CHARACTER(*), PARAMETER :: PCOEFF_S_MW_DESCRIPTION = 'Phase coefficients for solid phase microwave scatterers'

  CHARACTER(*), PARAMETER :: KE_IR_DESCRIPTION       = 'Mass extinction coefficient for infrared scatterers'
  CHARACTER(*), PARAMETER :: W_IR_DESCRIPTION        = 'Single scatter albedo for infrared scatterers'
  CHARACTER(*), PARAMETER :: G_IR_DESCRIPTION        = 'Asymmetry parameter for infrared scatterers'
  CHARACTER(*), PARAMETER :: PCOEFF_IR_DESCRIPTION   = 'Phase coefficients for infrared scatterers'


  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: RELEASE_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: VERSION_UNITS = 'N/A'

  CHARACTER(*), PARAMETER :: FREQUENCY_MW_UNITS = 'GigaHertz (GHz)'
  CHARACTER(*), PARAMETER :: FREQUENCY_IR_UNITS = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: REFF_MW_UNITS      = 'Microns (um)'
  CHARACTER(*), PARAMETER :: REFF_IR_UNITS      = 'Microns (um)'
  CHARACTER(*), PARAMETER :: TEMPERATURE_UNITS  = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: DENSITY_UNITS      = 'Kilograms per cubic metre (kg.m^-3)'

  CHARACTER(*), PARAMETER :: KE_L_MW_UNITS     = 'Metres squared per kilogram (m^2.kg^-1)'
  CHARACTER(*), PARAMETER :: W_L_MW_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: G_L_MW_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: PCOEFF_L_MW_UNITS = 'N/A'

  CHARACTER(*), PARAMETER :: KE_S_MW_UNITS     = 'Metres squared per kilogram (m^2.kg^-1)'
  CHARACTER(*), PARAMETER :: W_S_MW_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: G_S_MW_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: PCOEFF_S_MW_UNITS = 'N/A'

  CHARACTER(*), PARAMETER :: KE_IR_UNITS       = 'Metres squared per kilogram (m^2.kg^-1)'
  CHARACTER(*), PARAMETER :: W_IR_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: G_IR_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: PCOEFF_IR_UNITS   = 'N/A'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  REAL(Double), PARAMETER :: FREQUENCY_MW_FILLVALUE = ZERO
  REAL(Double), PARAMETER :: FREQUENCY_IR_FILLVALUE = ZERO
  REAL(Double), PARAMETER :: REFF_MW_FILLVALUE      = ZERO
  REAL(Double), PARAMETER :: REFF_IR_FILLVALUE      = ZERO
  REAL(Double), PARAMETER :: TEMPERATURE_FILLVALUE  = ZERO
  REAL(Double), PARAMETER :: DENSITY_FILLVALUE      = ZERO

  REAL(Double), PARAMETER :: KE_L_MW_FILLVALUE     = ZERO
  REAL(Double), PARAMETER :: W_L_MW_FILLVALUE      = ZERO
  REAL(Double), PARAMETER :: G_L_MW_FILLVALUE      = ZERO
  REAL(Double), PARAMETER :: PCOEFF_L_MW_FILLVALUE = ZERO

  REAL(Double), PARAMETER :: KE_S_MW_FILLVALUE     = ZERO
  REAL(Double), PARAMETER :: W_S_MW_FILLVALUE      = ZERO
  REAL(Double), PARAMETER :: G_S_MW_FILLVALUE      = ZERO
  REAL(Double), PARAMETER :: PCOEFF_S_MW_FILLVALUE = ZERO

  REAL(Double), PARAMETER :: KE_IR_FILLVALUE       = ZERO
  REAL(Double), PARAMETER :: W_IR_FILLVALUE        = ZERO
  REAL(Double), PARAMETER :: G_IR_FILLVALUE        = ZERO
  REAL(Double), PARAMETER :: PCOEFF_IR_FILLVALUE   = ZERO


  ! Variable types
  INTEGER, PARAMETER :: FREQUENCY_MW_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_IR_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: REFF_MW_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: REFF_IR_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: TEMPERATURE_TYPE  = NF90_DOUBLE
  INTEGER, PARAMETER :: DENSITY_TYPE      = NF90_DOUBLE

  INTEGER, PARAMETER :: KE_L_MW_TYPE     = NF90_DOUBLE
  INTEGER, PARAMETER :: W_L_MW_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: G_L_MW_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: PCOEFF_L_MW_TYPE = NF90_DOUBLE

  INTEGER, PARAMETER :: KE_S_MW_TYPE     = NF90_DOUBLE
  INTEGER, PARAMETER :: W_S_MW_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: G_S_MW_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: PCOEFF_S_MW_TYPE = NF90_DOUBLE

  INTEGER, PARAMETER :: KE_IR_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: W_IR_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: G_IR_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: PCOEFF_IR_TYPE   = NF90_DOUBLE


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
!       CloudCoeff_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire CloudCoeff object netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_netCDF_InquireFile( &
!                        Filename, &
!                        n_MW_Frequencies = n_MW_Frequencies , &
!                        n_MW_Radii       = n_MW_Radii       , &
!                        n_IR_Frequencies = n_IR_Frequencies , &
!                        n_IR_Radii       = n_IR_Radii       , &
!                        n_Temperatures   = n_Temperatures   , &
!                        n_Densities      = n_Densities      , &
!                        n_Legendre_Terms = n_Legendre_Terms , &
!                        n_Phase_Elements = n_Phase_Elements , &
!                        Release          = Release          , &
!                        Version          = Version          , &
!                        Title            = Title            , &
!                        History          = History          , &
!                        Comment          = Comment            )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           CloudCoeff data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_MW_Frequencies:   The number of microwave frequencies in
!                           the look-up table (LUT).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_MW_Radii:         The number of discrete effective radii 
!                           for MW scatterers in the LUT.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_IR_Frequencies:   The number of infrared frequencies in
!                           the LUT.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_IR_Radii:         The number of discrete effective radii 
!                           for IR scatterers in the LUT.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Temperatures:     The number of discrete layer temperatures
!                           in the LUT.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Densities:        The number of fixed densities for snow, graupel,
!                           and hail/ice in the LUT. Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Legendre_Terms:   The maximum number of Legendre polynomial
!                           terms in the LUT.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Phase_Elements:   The maximum number of phase elements in the LUT.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the CloudCoeff file.
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

  FUNCTION CloudCoeff_netCDF_InquireFile( &
    Filename        , &  ! Input
    n_MW_Frequencies, &  ! Optional output
    n_MW_Radii      , &  ! Optional output
    n_IR_Frequencies, &  ! Optional output
    n_IR_Radii      , &  ! Optional output
    n_Temperatures  , &  ! Optional output
    n_Densities     , &  ! Optional output
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
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_MW_Frequencies    
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_MW_Radii          
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_IR_Frequencies    
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_IR_Radii          
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Temperatures      
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Densities         
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_InquireFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: DimId
    TYPE(CloudCoeff_type) :: CloudCoeff
    
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
    ! ...n_MW_Frequencies dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,MW_FREQ_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//MW_FREQ_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=CloudCoeff%n_MW_Frequencies )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//MW_FREQ_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_MW_Radii dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,MW_REFF_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//MW_REFF_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=CloudCoeff%n_MW_Radii )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//MW_REFF_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_IR_Frequencies dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,IR_FREQ_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//IR_FREQ_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=CloudCoeff%n_IR_Frequencies )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//IR_FREQ_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_IR_Radii dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,IR_REFF_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//IR_REFF_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=CloudCoeff%n_IR_Radii )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//IR_REFF_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Temperatures dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,TEMPERATURE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//TEMPERATURE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=CloudCoeff%n_Temperatures )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//TEMPERATURE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Densities dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,DENSITY_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//DENSITY_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=CloudCoeff%n_Densities )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//DENSITY_DIMNAME//' - '// &
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
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=CloudCoeff%n_Legendre_Terms )
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
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=CloudCoeff%n_Phase_Elements )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//PHASE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
  
  
    ! Get the global attributes
    err_stat = ReadGAtts( Filename, &
                          FileId  , &
                          Release = CloudCoeff%Release, &
                          Version = CloudCoeff%Version, &
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
    IF ( PRESENT(n_MW_Frequencies) ) n_MW_Frequencies = CloudCoeff%n_MW_Frequencies
    IF ( PRESENT(n_MW_Radii      ) ) n_MW_Radii       = CloudCoeff%n_MW_Radii      
    IF ( PRESENT(n_IR_Frequencies) ) n_IR_Frequencies = CloudCoeff%n_IR_Frequencies
    IF ( PRESENT(n_IR_Radii      ) ) n_IR_Radii       = CloudCoeff%n_IR_Radii      
    IF ( PRESENT(n_Temperatures  ) ) n_Temperatures   = CloudCoeff%n_Temperatures  
    IF ( PRESENT(n_Densities     ) ) n_Densities      = CloudCoeff%n_Densities     
    IF ( PRESENT(n_Legendre_Terms) ) n_Legendre_Terms = CloudCoeff%n_Legendre_Terms-1  ! Indexed from 0, so subtract 1.
    IF ( PRESENT(n_Phase_Elements) ) n_Phase_Elements = CloudCoeff%n_Phase_Elements
    IF ( PRESENT(Release         ) ) Release          = CloudCoeff%Release     
    IF ( PRESENT(Version         ) ) Version          = CloudCoeff%Version     

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

  END FUNCTION CloudCoeff_netCDF_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_netCDF_WriteFile
!
! PURPOSE:
!       Function to write CloudCoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_netCDF_WriteFile( &
!                        Filename  , &
!                        CloudCoeff, &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       CloudCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       CloudCoeff:     Object containing the cloud coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CloudCoeff_type)
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
!                       attribute field of the CloudCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the CloudCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the CloudCoeff file.
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

  FUNCTION CloudCoeff_netCDF_WriteFile( &
    Filename  , &  ! Input
    CloudCoeff, &  ! Input
    Quiet     , &  ! Optional input
    Title     , &  ! Optional input
    History   , &  ! Optional input
    Comment   ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(CloudCoeff_type),  INTENT(IN) :: CloudCoeff
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_WriteFile(netCDF)'
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
    IF ( .NOT. CloudCoeff_Associated( CloudCoeff ) ) THEN
      msg = 'CloudCoeff structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. CloudCoeff_ValidRelease( CloudCoeff ) ) THEN
      msg = 'CloudCoeff Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet


    ! Create the output file
    err_stat = CreateFile( &
                 Filename                    , &  ! Input
                 CloudCoeff%n_MW_Frequencies , &  ! Input
                 CloudCoeff%n_MW_Radii       , &  ! Input
                 CloudCoeff%n_IR_Frequencies , &  ! Input
                 CloudCoeff%n_IR_Radii       , &  ! Input
                 CloudCoeff%n_Temperatures   , &  ! Input
                 CloudCoeff%n_Densities      , &  ! Input
                 CloudCoeff%n_Legendre_Terms , &  ! Input
                 CloudCoeff%n_Phase_Elements , &  ! Input
                 FileId                      , &  ! Output
                 Version = CloudCoeff%Version, &  ! Optional input
                 Title   = Title             , &  ! Optional input
                 History = History           , &  ! Optional input
                 Comment = Comment             )  ! Optional input
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Write the data items
    ! ...Frequency_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,FREQUENCY_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%Frequency_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//FREQUENCY_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Frequency_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,FREQUENCY_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%Frequency_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//FREQUENCY_IR_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Reff_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,REFF_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFF_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%Reff_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//REFF_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Reff_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,REFF_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFF_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%Reff_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//REFF_IR_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Temperature variable 
    NF90_Status = NF90_INQ_VARID( FileId,TEMPERATURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TEMPERATURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%Temperature )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TEMPERATURE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Density variable 
    NF90_Status = NF90_INQ_VARID( FileId,DENSITY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//DENSITY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%Density )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//DENSITY_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...ke_L_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,KE_L_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//KE_L_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%ke_L_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//KE_L_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...w_L_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,W_L_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//W_L_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%w_L_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//W_L_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...g_L_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,G_L_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//G_L_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%g_L_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//G_L_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...pcoeff_L_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,PCOEFF_L_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PCOEFF_L_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%pcoeff_L_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PCOEFF_L_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...ke_S_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,KE_S_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//KE_S_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%ke_S_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//KE_S_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...w_S_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,W_S_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//W_S_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%w_S_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//W_S_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...g_S_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,G_S_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//G_S_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%g_S_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//G_S_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...pcoeff_S_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,PCOEFF_S_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PCOEFF_S_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%pcoeff_S_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PCOEFF_S_MW_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...ke_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,KE_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//KE_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%ke_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//KE_IR_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...w_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,W_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//W_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%w_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//W_IR_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...g_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,G_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//G_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%g_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//G_IR_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...pcoeff_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,PCOEFF_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PCOEFF_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,CloudCoeff%pcoeff_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PCOEFF_IR_VARNAME//' to '//TRIM(Filename)//&
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
      CALL CloudCoeff_Info( CloudCoeff, msg )
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

  END FUNCTION CloudCoeff_netCDF_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_netCDF_ReadFile
!
! PURPOSE:
!       Function to read CloudCoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_netCDF_ReadFile( &
!                        Filename         , &
!                        CloudCoeff       , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       CloudCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       CloudCoeff:     CloudCoeff object containing the cloud coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CloudCoeff_type)
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
!                       attribute field of the CloudCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the CloudCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the CloudCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CloudCoeff_netCDF_ReadFile( &
    Filename  , &  ! Input
    CloudCoeff, &  ! Output
    Quiet     , &  ! Optional input
    Title     , &  ! Optional output
    History   , &  ! Optional output
    Comment   ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(CloudCoeff_type),  INTENT(OUT) :: CloudCoeff
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_ReadFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: n_MW_Frequencies
    INTEGER :: n_MW_Radii      
    INTEGER :: n_IR_Frequencies
    INTEGER :: n_IR_Radii      
    INTEGER :: n_Temperatures  
    INTEGER :: n_Densities     
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
    err_stat = CloudCoeff_netCDF_InquireFile( &
                 Filename, &
                 n_MW_Frequencies = n_MW_Frequencies, &
                 n_MW_Radii       = n_MW_Radii      , &
                 n_IR_Frequencies = n_IR_Frequencies, &
                 n_IR_Radii       = n_IR_Radii      , &
                 n_Temperatures   = n_Temperatures  , &
                 n_Densities      = n_Densities     , &
                 n_Legendre_Terms = n_Legendre_Terms, &
                 n_Phase_Elements = n_Phase_Elements  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining CloudCoeff dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL CloudCoeff_Create( &
           CloudCoeff, &
           n_MW_Frequencies, &
           n_MW_Radii      , &
           n_IR_Frequencies, &
           n_IR_Radii      , &
           n_Temperatures  , &
           n_Densities     , &
           n_Legendre_Terms, &
           n_Phase_Elements  )
    IF ( .NOT. CloudCoeff_Associated(CloudCoeff) ) THEN
      msg = 'Error allocating output CloudCoeff'
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
                          Release = CloudCoeff%Release, &
                          Version = CloudCoeff%Version, &
                          Title   = Title             , &
                          History = History           , &
                          Comment = Comment             )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. CloudCoeff_ValidRelease( CloudCoeff ) ) THEN
      msg = 'CloudCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF

    
    ! Read the CloudCoeff data
    ! ...Frequency_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,FREQUENCY_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%Frequency_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//FREQUENCY_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Frequency_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,FREQUENCY_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%Frequency_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//FREQUENCY_IR_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Reff_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,REFF_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFF_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%Reff_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//REFF_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Reff_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,REFF_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFF_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%Reff_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//REFF_IR_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Temperature variable 
    NF90_Status = NF90_INQ_VARID( FileId,TEMPERATURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TEMPERATURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%Temperature )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TEMPERATURE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Density variable 
    NF90_Status = NF90_INQ_VARID( FileId,DENSITY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//DENSITY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%Density )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//DENSITY_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...ke_L_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,KE_L_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//KE_L_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%ke_L_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//KE_L_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...w_L_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,W_L_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//W_L_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%w_L_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//W_L_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...g_L_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,G_L_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//G_L_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%g_L_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//G_L_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...pcoeff_L_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,PCOEFF_L_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PCOEFF_L_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%pcoeff_L_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PCOEFF_L_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...ke_S_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,KE_S_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//KE_S_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%ke_S_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//KE_S_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...w_S_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,W_S_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//W_S_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%w_S_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//W_S_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...g_S_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,G_S_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//G_S_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%g_S_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//G_S_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...pcoeff_S_MW variable 
    NF90_Status = NF90_INQ_VARID( FileId,PCOEFF_S_MW_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PCOEFF_S_MW_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%pcoeff_S_MW )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PCOEFF_S_MW_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...ke_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,KE_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//KE_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%ke_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//KE_IR_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...w_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,W_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//W_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%w_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//W_IR_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...g_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,G_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//G_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%g_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//G_IR_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...pcoeff_IR variable 
    NF90_Status = NF90_INQ_VARID( FileId,PCOEFF_IR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PCOEFF_IR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarID,CloudCoeff%pcoeff_IR )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PCOEFF_IR_VARNAME//' from '//TRIM(Filename)//&
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
      CALL CloudCoeff_Info( CloudCoeff, msg )
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
      CALL CloudCoeff_Destroy( CloudCoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp
    
  END FUNCTION CloudCoeff_netCDF_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CloudCoeff_netCDF_IOVersion( Id )
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

  SUBROUTINE CloudCoeff_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CloudCoeff_netCDF_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a CloudCoeff data file.

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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_WriteGAtts(netCDF)'
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
    TYPE(CloudCoeff_type) :: CloudCoeff

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
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),CloudCoeff%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = CloudCoeff%Version
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


  ! Function to read the global attributes from a CloudCoeff data file.

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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_ReadGAtts(netCDF)'
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


  ! Function to create a CloudCoeff file for writing

  FUNCTION CreateFile( &
    Filename        , &  ! Input
    n_MW_Frequencies, &  ! Input
    n_MW_Radii      , &  ! Input
    n_IR_Frequencies, &  ! Input
    n_IR_Radii      , &  ! Input
    n_Temperatures  , &  ! Input
    n_Densities     , &  ! Input
    n_Legendre_Terms, &  ! Input
    n_Phase_Elements, &  ! Input
    FileId          , &  ! Output
    Version         , &  ! Optional input
    Title           , &  ! Optional input
    History         , &  ! Optional input
    Comment         ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: n_MW_Frequencies
    INTEGER     ,           INTENT(IN)  :: n_MW_Radii      
    INTEGER     ,           INTENT(IN)  :: n_IR_Frequencies
    INTEGER     ,           INTENT(IN)  :: n_IR_Radii      
    INTEGER     ,           INTENT(IN)  :: n_Temperatures  
    INTEGER     ,           INTENT(IN)  :: n_Densities     
    INTEGER     ,           INTENT(IN)  :: n_Legendre_Terms
    INTEGER     ,           INTENT(IN)  :: n_Phase_Elements
    INTEGER     ,           INTENT(OUT) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: n_MW_Frequencies_DimID
    INTEGER :: n_MW_Radii_DimID
    INTEGER :: n_IR_Frequencies_DimID
    INTEGER :: n_IR_Radii_DimID
    INTEGER :: n_Temperatures_DimID
    INTEGER :: n_Densities_DimID
    INTEGER :: n_IR_Densities_DimID
    INTEGER :: n_Legendre_Terms_DimID
    INTEGER :: n_Phase_Elements_DimID
    INTEGER :: varID
    INTEGER :: Put_Status(4)
    
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
    ! ...Number of microwave frequencies
    NF90_Status = NF90_DEF_DIM( FileID,MW_FREQ_DIMNAME,n_MW_Frequencies,n_MW_Frequencies_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//MW_FREQ_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of radii for microwave data
    NF90_Status = NF90_DEF_DIM( FileID,MW_REFF_DIMNAME,n_MW_Radii,n_MW_Radii_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//MW_REFF_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of infrared frequencies
    NF90_Status = NF90_DEF_DIM( FileID,IR_FREQ_DIMNAME,n_IR_Frequencies,n_IR_Frequencies_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//IR_FREQ_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of radii for infrared data
    NF90_Status = NF90_DEF_DIM( FileID,IR_REFF_DIMNAME,n_IR_Radii,n_IR_Radii_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//IR_REFF_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of temperatures
    NF90_Status = NF90_DEF_DIM( FileID,TEMPERATURE_DIMNAME,n_Temperatures,n_Temperatures_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TEMPERATURE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of densities for microwave data
    NF90_Status = NF90_DEF_DIM( FileID,DENSITY_DIMNAME,n_Densities,n_Densities_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//DENSITY_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of densities for infrared data
    ! ...Array indexing starts at 0, so +1
    NF90_Status = NF90_DEF_DIM( FileID,IR_DENSITY_DIMNAME,n_Densities+1,n_IR_Densities_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//IR_DENSITY_DIMNAME//' dimension in '//&
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
    ! ...Frequency_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      FREQUENCY_MW_VARNAME, &
      FREQUENCY_MW_TYPE, &
      dimIDs=(/n_MW_Frequencies_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,FREQUENCY_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,FREQUENCY_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,FREQUENCY_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,FREQUENCY_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//FREQUENCY_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Frequency_IR variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      FREQUENCY_IR_VARNAME, &
      FREQUENCY_IR_TYPE, &
      dimIDs=(/n_IR_Frequencies_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_IR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,FREQUENCY_IR_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,FREQUENCY_IR_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,FREQUENCY_IR_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,FREQUENCY_IR_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//FREQUENCY_IR_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Reff_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      REFF_MW_VARNAME, &
      REFF_MW_TYPE, &
      dimIDs=(/n_MW_Radii_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//REFF_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,REFF_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,REFF_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,REFF_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,REFF_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//REFF_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Reff_IR variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      REFF_IR_VARNAME, &
      REFF_IR_TYPE, &
      dimIDs=(/n_IR_Radii_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//REFF_IR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,REFF_IR_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,REFF_IR_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,REFF_IR_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,REFF_IR_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//REFF_IR_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Temperature variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      TEMPERATURE_VARNAME, &
      TEMPERATURE_TYPE, &
      dimIDs=(/n_Temperatures_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TEMPERATURE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,TEMPERATURE_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,TEMPERATURE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,TEMPERATURE_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,TEMPERATURE_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TEMPERATURE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Density variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      DENSITY_VARNAME, &
      DENSITY_TYPE, &
      dimIDs=(/n_Densities_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//DENSITY_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,DENSITY_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,DENSITY_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,DENSITY_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,DENSITY_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//DENSITY_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...ke_L_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      KE_L_MW_VARNAME, &
      KE_L_MW_TYPE, &
      dimIDs=(/n_MW_Frequencies_DimID, n_MW_Radii_DimID, n_Temperatures_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//KE_L_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,KE_L_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,KE_L_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,KE_L_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,KE_L_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//KE_L_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...w_L_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      W_L_MW_VARNAME, &
      W_L_MW_TYPE, &
      dimIDs=(/n_MW_Frequencies_DimID, n_MW_Radii_DimID, n_Temperatures_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//W_L_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,W_L_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,W_L_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,W_L_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,W_L_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//W_L_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...g_L_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      G_L_MW_VARNAME, &
      G_L_MW_TYPE, &
      dimIDs=(/n_MW_Frequencies_DimID, n_MW_Radii_DimID, n_Temperatures_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//G_L_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,G_L_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,G_L_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,G_L_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,G_L_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//G_L_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...pcoeff_L_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      PCOEFF_L_MW_VARNAME, &
      PCOEFF_L_MW_TYPE, &
      dimIDs=(/n_MW_Frequencies_DimID, n_MW_Radii_DimID, n_Temperatures_DimID, &
               n_Legendre_Terms_DimID, n_Phase_Elements_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PCOEFF_L_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,PCOEFF_L_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,PCOEFF_L_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,PCOEFF_L_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,PCOEFF_L_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PCOEFF_L_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...ke_S_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      KE_S_MW_VARNAME, &
      KE_S_MW_TYPE, &
      dimIDs=(/n_MW_Frequencies_DimID, n_MW_Radii_DimID, n_Densities_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//KE_S_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,KE_S_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,KE_S_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,KE_S_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,KE_S_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//KE_S_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...w_S_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      W_S_MW_VARNAME, &
      W_S_MW_TYPE, &
      dimIDs=(/n_MW_Frequencies_DimID, n_MW_Radii_DimID, n_Densities_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//W_S_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,W_S_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,W_S_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,W_S_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,W_S_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//W_S_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...g_S_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      G_S_MW_VARNAME, &
      G_S_MW_TYPE, &
      dimIDs=(/n_MW_Frequencies_DimID, n_MW_Radii_DimID, n_Densities_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//G_S_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,G_S_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,G_S_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,G_S_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,G_S_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//G_S_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...pcoeff_S_MW variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      PCOEFF_S_MW_VARNAME, &
      PCOEFF_S_MW_TYPE, &
      dimIDs=(/n_MW_Frequencies_DimID, n_MW_Radii_DimID, n_Densities_DimID, &
               n_Legendre_Terms_DimID, n_Phase_Elements_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PCOEFF_S_MW_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,PCOEFF_S_MW_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,PCOEFF_S_MW_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,PCOEFF_S_MW_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,PCOEFF_S_MW_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PCOEFF_S_MW_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...ke_IR variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      KE_IR_VARNAME, &
      KE_IR_TYPE, &
      dimIDs=(/n_IR_Frequencies_DimID, n_IR_Radii_DimID, n_IR_Densities_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//KE_IR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,KE_IR_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,KE_IR_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,KE_IR_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,KE_IR_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//KE_IR_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...w_IR variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      W_IR_VARNAME, &
      W_IR_TYPE, &
      dimIDs=(/n_IR_Frequencies_DimID, n_IR_Radii_DimID, n_IR_Densities_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//W_IR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,W_IR_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,W_IR_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,W_IR_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,W_IR_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//W_IR_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...g_IR variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      G_IR_VARNAME, &
      G_IR_TYPE, &
      dimIDs=(/n_IR_Frequencies_DimID, n_IR_Radii_DimID, n_IR_Densities_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//G_IR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,G_IR_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,G_IR_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,G_IR_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,G_IR_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//G_IR_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...pcoeff_IR variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      PCOEFF_IR_VARNAME, &
      PCOEFF_IR_TYPE, &
      dimIDs=(/n_IR_Frequencies_DimID, n_IR_Radii_DimID, n_IR_Densities_DimID, &
               n_Legendre_Terms_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PCOEFF_IR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,PCOEFF_IR_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,PCOEFF_IR_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,PCOEFF_IR_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,PCOEFF_IR_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PCOEFF_IR_VARNAME//' variable attributes to '//TRIM(Filename)
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

END MODULE CloudCoeff_netCDF_IO
