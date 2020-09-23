!
! AerosolCoeff_netCDF_IO
!
! Module containing routines to read and write AerosolCoeff netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Apr-2007
!                       paul.vandelst@ssec.wisc.edu
!

MODULE AerosolCoeff_netCDF_IO_R2


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Display_Message
  USE AerosolCoeff_Define_R2, ONLY: AerosolCoeff_type, &
                                 Associated_AerosolCoeff, &
                                 Destroy_AerosolCoeff, &
                                 Allocate_AerosolCoeff, &
                                 Check_AerosolCoeff_Release, &
                                 Info_AerosolCoeff
  USE netcdf
  USE netCDF_Utility     ,  Open_AerosolCoeff_netCDF =>  Open_netCDF, &
                           Close_AerosolCoeff_netCDF => Close_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_AerosolCoeff_netCDF
  PUBLIC :: Write_AerosolCoeff_netCDF
  PUBLIC :: Read_AerosolCoeff_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME   = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME = 'history' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME = 'comment' 

  ! Dimension names
  CHARACTER(*), PARAMETER :: WAVELENGTH_DIMNAME = 'n_Wavelengths'
  CHARACTER(*), PARAMETER :: REFF_DIMNAME       = 'n_Radii'
  CHARACTER(*), PARAMETER :: TYPE_DIMNAME       = 'n_Types'
  CHARACTER(*), PARAMETER :: RH_DIMNAME         = 'n_RH'
  CHARACTER(*), PARAMETER :: LEGENDRE_DIMNAME   = 'n_Legendre_Terms'
  CHARACTER(*), PARAMETER :: PHASE_DIMNAME      = 'n_Phase_Elements'
  CHARACTER(*), PARAMETER :: STRLEN_DIMNAME     = 'atnsl'

  ! Variable names
  CHARACTER(*), PARAMETER :: RELEASE_VARNAME = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_VARNAME = 'Version'

  CHARACTER(*), PARAMETER :: TYPE_NAME_VARNAME = 'Aerosol_Type_Name'
  CHARACTER(*), PARAMETER :: TYPE_VARNAME      = 'Aerosol_Type'
  
  CHARACTER(*), PARAMETER :: WAVELENGTH_VARNAME = 'Wavelength'
  CHARACTER(*), PARAMETER :: FREQUENCY_VARNAME  = 'Frequency'
  CHARACTER(*), PARAMETER :: REFF_VARNAME       = 'Reff'
  CHARACTER(*), PARAMETER :: RH_VARNAME         = 'RH'

  CHARACTER(*), PARAMETER :: KE_VARNAME       = 'ke'
  CHARACTER(*), PARAMETER :: W_VARNAME        = 'w'
  CHARACTER(*), PARAMETER :: G_VARNAME        = 'g'
  CHARACTER(*), PARAMETER :: PCOEFF_VARNAME   = 'pcoeff'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: RELEASE_LONGNAME = 'File Release'
  CHARACTER(*), PARAMETER :: VERSION_LONGNAME = 'Data Version'

  CHARACTER(*), PARAMETER :: TYPE_NAME_LONGNAME = 'Aerosol type name'
  CHARACTER(*), PARAMETER :: TYPE_LONGNAME      = 'Aerosol type'
  
  CHARACTER(*), PARAMETER :: WAVELENGTH_LONGNAME = 'Wavelength'
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME  = 'Frequency'
  CHARACTER(*), PARAMETER :: REFF_LONGNAME       = 'Effective radius'
  CHARACTER(*), PARAMETER :: RH_LONGNAME         = 'Relative humidity'

  CHARACTER(*), PARAMETER :: KE_LONGNAME       = 'ke'
  CHARACTER(*), PARAMETER :: W_LONGNAME        = 'w'
  CHARACTER(*), PARAMETER :: G_LONGNAME        = 'g'
  CHARACTER(*), PARAMETER :: PCOEFF_LONGNAME   = 'pcoeff'


  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: RELEASE_DESCRIPTION = 'Release indicates structure and file format changes'
  CHARACTER(*), PARAMETER :: VERSION_DESCRIPTION = 'Version indicates data changes'

  CHARACTER(*), PARAMETER :: TYPE_NAME_DESCRIPTION = 'Name of the aerosol type'
  CHARACTER(*), PARAMETER :: TYPE_DESCRIPTION      = 'Flag/index value used to identify and reference the aerosol type'
  
  CHARACTER(*), PARAMETER :: WAVELENGTH_DESCRIPTION = 'Wavelength LUT dimension vector'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION  = 'Frequency LUT dimension vector'
  CHARACTER(*), PARAMETER :: REFF_DESCRIPTION       = 'Effective radius LUT dimension vector'
  CHARACTER(*), PARAMETER :: RH_DESCRIPTION         = 'Relatvie humidity LUT dimension vector'

  CHARACTER(*), PARAMETER :: KE_DESCRIPTION       = 'Mass extinction coefficient for aerosol scatterers'
  CHARACTER(*), PARAMETER :: W_DESCRIPTION        = 'Single scatter albedo for aerosol scatterers'
  CHARACTER(*), PARAMETER :: G_DESCRIPTION        = 'Asymmetry parameter for aerosol scatterers'
  CHARACTER(*), PARAMETER :: PCOEFF_DESCRIPTION   = 'Phase coefficients for aerosol scatterers'


  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: RELEASE_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: VERSION_UNITS = 'N/A'

  CHARACTER(*), PARAMETER :: TYPE_NAME_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: TYPE_UNITS      = 'N/A'
  
  CHARACTER(*), PARAMETER :: WAVELENGTH_UNITS = 'Microns (um)'
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS  = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: REFF_UNITS       = 'Microns (um)'
  CHARACTER(*), PARAMETER :: RH_UNITS         = 'fraction (0->1)'

  CHARACTER(*), PARAMETER :: KE_UNITS       = 'Metres squared per kilogram (m^2.kg^-1)'
  CHARACTER(*), PARAMETER :: W_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: G_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: PCOEFF_UNITS   = 'N/A'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER(Long), PARAMETER :: IP_FILLVALUE = 0_Long
  REAL(Double) , PARAMETER :: FP_FILLVALUE = ZERO


  ! Variable types
  INTEGER, PARAMETER :: RELEASE_TYPE = NF90_INT
  INTEGER, PARAMETER :: VERSION_TYPE = NF90_INT

  INTEGER, PARAMETER :: TYPE_NAME_TYPE  = NF90_CHAR
  INTEGER, PARAMETER :: TYPE_TYPE       = NF90_INT
  
  INTEGER, PARAMETER :: WAVELENGTH_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_TYPE  = NF90_DOUBLE
  INTEGER, PARAMETER :: REFF_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: RH_TYPE         = NF90_DOUBLE

  INTEGER, PARAMETER :: KE_TYPE         = NF90_DOUBLE 
  INTEGER, PARAMETER :: W_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: G_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: PCOEFF_TYPE     = NF90_DOUBLE


CONTAINS


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
!       Write_AerosolCoeff_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF AerosolCoeff
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_AerosolCoeff_GAtts( NC_Filename            , &  ! Input
!                                                NC_FileID              , &  ! Input
!                                                Title      =Title      , &  ! Optional input
!                                                History    =History    , &  ! Optional input
!                                                Comment    =Comment    , &  ! Optional input
!                                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF AerosolCoeff format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_AerosolCoeff_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF AerosolCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF AerosolCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF AerosolCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute write was successful.
!                            == FAILURE an error occurred writing the supplied
!                               global attributes.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Write_AerosolCoeff_GAtts( NC_Filename  , &  ! Input
                                     NC_FileID    , &  ! Input
                                     Title        , &  ! Optional input
                                     History      , &  ! Optional input
                                     Comment      , &  ! Optional input
                                     Message_Log  ) &  ! Error messaging
                                   RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AerosolCoeff_GAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    INTEGER     , PARAMETER :: NPUTGATTS = 5
    ! Local variables
    INTEGER :: Put_Status(NPUTGATTS), n
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone

    ! Set up
    Error_Status = SUCCESS
    Put_Status   = SUCCESS
    n = 0

    ! Software ID
    n = n + 1
    Put_Status(n) = Put_GAttString(WRITE_MODULE_HISTORY_GATTNAME, &
                                   MODULE_RCS_ID, &
                                   Message_Log=Message_Log )

    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    n = n + 1
    Put_Status(n) = Put_GAttString(CREATION_DATE_AND_TIME_GATTNAME, &
                                   cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                   ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                   czone//'UTC', &
                                   Message_Log=Message_Log )

    ! The Title
    n = n + 1
    IF ( PRESENT( Title ) ) THEN
      Put_Status(n) = Put_GAttString(TITLE_GATTNAME, Title, &
                                     Message_Log=Message_Log )
    END IF

    ! The History
    n = n + 1
    IF ( PRESENT( History ) ) THEN
      Put_Status(n) = Put_GAttString(HISTORY_GATTNAME, History, &
                                     Message_Log=Message_Log )
    END IF

    ! The Comment
    n = n + 1
    IF ( PRESENT( Comment ) ) THEN
      Put_Status(n) = Put_GAttString(COMMENT_GATTNAME, Comment, &
                                     Message_Log=Message_Log )
    END IF

    ! Check for any errors
    IF ( ANY( Put_Status /= SUCCESS ) ) Error_Status = WARNING

  CONTAINS

    FUNCTION Put_GAttString(GAttName, GAttString, Message_Log) RESULT(Error_Status)
      CHARACTER(*),           INTENT(IN) :: GAttName
      CHARACTER(*),           INTENT(IN) :: GAttString
      CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
      INTEGER :: Error_Status
      INTEGER :: NF90_Status
      Error_Status = SUCCESS
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  TRIM(GAttString) )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//TRIM(GAttName)//' attribute to '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END FUNCTION Put_GAttString

  END FUNCTION Write_AerosolCoeff_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       Read_AerosolCoeff_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF AerosolCoeff
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_AerosolCoeff_GAtts( NC_Filename            , &  ! Input
!                                               NC_FileID              , &  ! Input
!                                               Title      =Title      , &  ! Optional output
!                                               History    =History    , &  ! Optional output
!                                               Comment    =Comment    , &  ! Optional output
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF AerosolCoeff format data file to read from.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF AerosolCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF AerosolCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF AerosolCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the global attribute read was successful.
!                        == WARNING an error occurred reading the requested
!                           global attributes.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF file is closed.
!
!------------------------------------------------------------------------------

  FUNCTION Read_AerosolCoeff_GAtts( NC_Filename  , &  ! Input
                                    NC_FileID    , &  ! Input
                                    Title        , &  ! Optional output
                                    History      , &  ! Optional output
                                    Comment      , &  ! Optional output
                                    Message_Log  ) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_AerosolCoeff_GAtts'
    INTEGER     , PARAMETER :: NGETGATTS = 3
    ! Local variables
    INTEGER :: Get_Status(NGETGATTS), n

    ! Set up
    Error_Status = SUCCESS
    Get_Status   = SUCCESS
    n = 0

    ! The Title
    n = n + 1
    IF ( PRESENT( Title ) ) THEN
      Get_Status(n) = Get_GAttString(TITLE_GATTNAME, Title, &
                                     Message_Log=Message_Log )
    END IF

    ! The History
    n = n + 1
    IF ( PRESENT( History ) ) THEN
      Get_Status(n) = Get_GAttString(HISTORY_GATTNAME, History, &
                                     Message_Log=Message_Log )
    END IF

    ! The Comment
    n = n + 1
    IF ( PRESENT( Comment ) ) THEN
      Get_Status(n) = Get_GAttString(COMMENT_GATTNAME, Comment, &
                                     Message_Log=Message_Log )
    END IF

    ! Check for any errors
    IF ( ANY( Get_Status /= SUCCESS ) ) Error_Status = WARNING

  CONTAINS

    FUNCTION Get_GAttString(GAttName, GAttString, Message_Log) RESULT(Error_Status)
      CHARACTER(*),           INTENT(IN)  :: GAttName
      CHARACTER(*),           INTENT(OUT) :: GAttString
      CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
      INTEGER :: Error_Status
      CHARACTER(5000) :: LongString
      GAttString = ' '
      LongString = ' '
      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           TRIM(GAttName), &
                                           LongString, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//TRIM(GAttName)//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
      CALL Remove_NULL_Characters( LongString )
      GAttString = LongString(1:MIN( LEN(GAttString), LEN_TRIM(LongString) ))
    END FUNCTION Get_GAttString

  END FUNCTION Read_AerosolCoeff_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       Create_AerosolCoeff_netCDF
!
! PURPOSE:
!       Function to create a netCDF AerosolCoeff data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = Create_AerosolCoeff_netCDF( NC_Filename        , &  ! Input
!                                                  n_Wavelengths      , &  ! Input
!                                                  n_Radii            , &  ! Input
!                                                  n_Types            , &  ! Input
!                                                  n_RH               , &  ! Input
!                                                  n_Legendre_Terms   , &  ! Input
!                                                  n_Phase_Elements   , &  ! Input
!                                                  NC_FileID          , &  ! Output
!                                                  Title      =Title  , &  ! Optional input
!                                                  History    =History, &  ! Optional input
!                                                  Comment    =Comment, &  ! Optional input
!                                                  RCS_Id     =RCS_Id , &  ! Revision control
!                                                  Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF AerosolCoeff format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
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
! OPTIONAL INPUT ARGUMENTS:
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF AerosolCoeff file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       NC_FileID:          NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.  
!                          The error codes are defined in the Message_Handler module. 
!                          If == SUCCESS the netCDF file creation was successful.     
!                             == FAILURE an unrecoverable error occurred.             
!                             == WARNING - an error occurred writing any of the       
!                                          supplied global attributes.                
!                                        - an error occurred closing the netCDF file. 
!                          UNITS:      N/A                                            
!                          TYPE:       INTEGER                                        
!                          DIMENSION:  Scalar                                         
!
!------------------------------------------------------------------------------

  FUNCTION Create_AerosolCoeff_netCDF( NC_Filename     , &  ! Input
                                       n_Wavelengths   , &  ! Input
                                       n_Radii         , &  ! Input
                                       n_Types         , &  ! Input
                                       n_RH            , &  ! Input
                                       n_Legendre_Terms, &  ! Input
                                       n_Phase_Elements, &  ! Input
                                       NC_FileID       , &  ! Output
                                       Title           , &  ! Optional input
                                       History         , &  ! Optional input
                                       Comment         , &  ! Optional input
                                       RCS_Id          , &  ! Revision control
                                       Message_Log     ) &  ! Error messaging
                                     RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: n_Wavelengths   
    INTEGER               , INTENT(IN)  :: n_Radii         
    INTEGER               , INTENT(IN)  :: n_Types         
    INTEGER               , INTENT(IN)  :: n_RH            
    INTEGER               , INTENT(IN)  :: n_Legendre_Terms
    INTEGER               , INTENT(IN)  :: n_Phase_Elements
    INTEGER               , INTENT(OUT) :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_AerosolCoeff_netCDF'
    ! Local variables
    INTEGER :: NF90_Status
    INTEGER :: Status1, Status2, Status3
    INTEGER :: Close_Status
    INTEGER :: StrLen_DimID
    INTEGER :: Wavelength_DimID
    INTEGER :: Frequency_DimID
    INTEGER :: Reff_DimID
    INTEGER :: Type_DimID
    INTEGER :: RH_DimID
    INTEGER :: Legendre_DimID
    INTEGER :: Phase_DimID
    INTEGER :: VarID
    TYPE(AerosolCoeff_type) :: a

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check input
    IF ( n_Wavelengths    < 1 .OR. &
         n_Radii          < 1 .OR. &
         n_Types          < 1 .OR. &
         n_RH             < 1 .OR. &
         n_Legendre_Terms < 0 .OR. &
         n_Phase_Elements < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid dimension input detected.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Create the data file
    ! --------------------
    NF90_Status = NF90_CREATE( NC_Filename, &
                               NF90_CLOBBER, &
                               NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Define the dimensions
    ! ---------------------
    ! The aerosol type string length
    Error_Status = Def_Dim(STRLEN_DIMNAME    , a%StrLen          , StrLen_DimID    )
    IF ( Error_Status /= SUCCESS ) RETURN
    
    ! The number of wavelength/frequencies
    Error_Status = Def_Dim(WAVELENGTH_DIMNAME, n_Wavelengths     , Wavelength_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN
    
    ! The number of effective radii
    Error_Status = Def_Dim(REFF_DIMNAME      , n_Radii           , Reff_DimID      )
    IF ( Error_Status /= SUCCESS ) RETURN
    
    ! The number of aerosol types
    Error_Status = Def_Dim(TYPE_DIMNAME      , n_Types           , Type_DimID      )
    IF ( Error_Status /= SUCCESS ) RETURN
    
    ! ???
    Error_Status = Def_Dim(RH_DIMNAME        , n_RH              , RH_DimID        )
    IF ( Error_Status /= SUCCESS ) RETURN
    
    ! The number of Legendre terms. The data arrays are
    ! indexed from 0, so add one.
    Error_Status = Def_Dim(LEGENDRE_DIMNAME  , n_Legendre_Terms+1, Legendre_DimID  )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of phase matrix elements
    Error_Status = Def_Dim(PHASE_DIMNAME     , n_Phase_Elements  , Phase_DimID     )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Write the global attributes
    ! ---------------------------
    Error_Status = Write_AerosolCoeff_GAtts( NC_Filename            , &
                                             NC_FileID              , &
                                             Title      =Title      , &
                                             History    =History    , &
                                             Comment    =Comment    , &
                                             Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! Define the data file release and version
    ! ----------------------------------------
    ! Define the file release
    Error_Status = Def_Var(RELEASE_VARNAME    , &
                           RELEASE_TYPE       , &
                           RELEASE_LONGNAME   , &
                           RELEASE_DESCRIPTION, &
                           RELEASE_UNITS     )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the file version
    Error_Status = Def_Var(VERSION_VARNAME    , &
                           VERSION_TYPE       , &
                           VERSION_LONGNAME   , &
                           VERSION_DESCRIPTION, &
                           VERSION_UNITS     )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Define the aerosol descriptor vectors
    ! -------------------------------------
    ! Define the aerosol type name vector
    Error_Status = Def_Var(TYPE_NAME_VARNAME    , &
                           TYPE_NAME_TYPE       , &
                           TYPE_NAME_LONGNAME   , &
                           TYPE_NAME_DESCRIPTION, &
                           TYPE_NAME_UNITS      , &
                           DimIDs=(/StrLen_DimID, Type_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the aerosol type vector
    Error_Status = Def_Var(TYPE_VARNAME    , &
                           TYPE_TYPE       , &
                           TYPE_LONGNAME   , &
                           TYPE_DESCRIPTION, &
                           TYPE_UNITS      , &
                           DimIDs=(/Type_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Define the LUT dimension vectors
    ! --------------------------------
    ! Define the wavelength vector
    Error_Status = Def_Var(WAVELENGTH_VARNAME    , &
                           WAVELENGTH_TYPE       , &
                           WAVELENGTH_LONGNAME   , &
                           WAVELENGTH_DESCRIPTION, &
                           WAVELENGTH_UNITS      , &
                           DimIDs=(/Wavelength_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the frequency vector
    Error_Status = Def_Var(FREQUENCY_VARNAME    , &
                           FREQUENCY_TYPE       , &
                           FREQUENCY_LONGNAME   , &
                           FREQUENCY_DESCRIPTION, &
                           FREQUENCY_UNITS      , &
                           DimIDs=(/Wavelength_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the radii vector
    Error_Status = Def_Var(REFF_VARNAME    , &
                           REFF_TYPE       , &
                           REFF_LONGNAME   , &
                           REFF_DESCRIPTION, &
                           REFF_UNITS      , &
                           DimIDs=(/Reff_DimID, Type_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the relative humidity vector
    Error_Status = Def_Var(RH_VARNAME    , &
                           RH_TYPE       , &
                           RH_LONGNAME   , &
                           RH_DESCRIPTION, &
                           RH_UNITS      , &
                           DimIDs=(/RH_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Define the optical property variables
    ! -------------------------------------
    ! Define the extinction coefficient
    Error_Status = Def_Var(KE_VARNAME    , &
                           KE_TYPE       , &
                           KE_LONGNAME   , &
                           KE_DESCRIPTION, &
                           KE_UNITS      , &
                           DimIDs=(/Wavelength_DimID, Reff_DimID, Type_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the single scatter albedo
    Error_Status = Def_Var(W_VARNAME    , &
                           W_TYPE       , &
                           W_LONGNAME   , &
                           W_DESCRIPTION, &
                           W_UNITS      , &
                           DimIDs=(/Wavelength_DimID, Reff_DimID, Type_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the asymmetry parameter
    Error_Status = Def_Var(G_VARNAME    , &
                           G_TYPE       , &
                           G_LONGNAME   , &
                           G_DESCRIPTION, &
                           G_UNITS      , &
                           DimIDs=(/Wavelength_DimID, Reff_DimID, Type_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the phase coefficients
    Error_Status = Def_Var(PCOEFF_VARNAME    , &
                           PCOEFF_TYPE       , &
                           PCOEFF_LONGNAME   , &
                           PCOEFF_DESCRIPTION, &
                           PCOEFF_UNITS      , &
                           DimIDs=(/Wavelength_DimID, Reff_DimID, Type_DimID, &
                                    Legendre_DimID, Phase_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error taking file '//TRIM( NC_Filename )// &
                            ' out of define mode - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


  !=======
  CONTAINS
  !=======

    ! Internal function to define dimensions and handle errors
    ! --------------------------------------------------------
    FUNCTION Def_Dim(DimName, DimSize, DimID) RESULT(Error_Status)
      CHARACTER(*), INTENT(IN)  :: DimName
      INTEGER,      INTENT(IN)  :: DimSize
      INTEGER,      INTENT(OUT) :: DimID
      INTEGER :: Error_Status
      INTEGER :: NF90_Status
      Error_Status = SUCCESS
      NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                  TRIM(DimName), &
                                  DimSize, &
                                  DimID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error defining the '//TRIM(DimName)//' dimension in '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
      END IF
    END FUNCTION Def_Dim


    ! Internal function to define variables and handle errors
    ! -------------------------------------------------------
    FUNCTION Def_Var( VarName    , &
                      VarType    , &
                      LongName   , &
                      Description, &
                      Units      , &
                      DimIds     ) &
                    RESULT(Error_Status)
      ! Arguments
      CHARACTER(*),           INTENT(IN) :: VarName
      INTEGER     ,           INTENT(IN) :: VarType
      CHARACTER(*),           INTENT(IN) :: LongName
      CHARACTER(*),           INTENT(IN) :: Description
      CHARACTER(*),           INTENT(IN) :: Units
      INTEGER     , OPTIONAL, INTENT(IN) :: DimIDs(:)
      ! Function result
      INTEGER :: Error_Status
      ! Local parameters
      INTEGER, PARAMETER :: NATTS=4
      ! Loocal variables
      INTEGER :: NF90_Status
      INTEGER :: Put_Status(NATTS)

      ! Set up
      Error_Status = SUCCESS 
            
      ! Define the variable. The netCDF function dimIDs dummy
      ! argument is not truly optional (in the f90/95 sense),
      ! so the IF is necessary here.
      IF ( PRESENT(DimIDs) ) THEN
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    VarName  , &
                                    VarType  , &
                                    dimIDs=DimIDs, &
                                    varID =VarID   )
      ELSE
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    VarName  , &
                                    VarType  , &
                                    varID =VarID )
      END IF      
      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error defining '//VarName//' variable in '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
  
      ! Write some attributes
      Put_Status(1) = Put_netCDF_Attribute( NC_FileID, &
                                            LONGNAME_ATTNAME, &
                                            LongName, &
                                            Variable_Name=VarName )
      Put_Status(2) = Put_netCDF_Attribute( NC_FileID, &
                                            DESCRIPTION_ATTNAME, &
                                            Description, &
                                            Variable_Name=VarName )
      Put_Status(3) = Put_netCDF_Attribute( NC_FileID, &
                                            UNITS_ATTNAME, &
                                            Units, &
                                            Variable_Name=VarName )
      ! The following yukness is because
      ! I don't want to overload.
      SELECT CASE(VarName)
        CASE (RELEASE_VARNAME, VERSION_VARNAME, TYPE_VARNAME)
          Put_Status(4) = Put_netCDF_Attribute( NC_FileID, &
                                                FILLVALUE_ATTNAME, &
                                                IP_FILLVALUE, &
                                                Variable_Name=VarName )
        CASE (TYPE_NAME_VARNAME)
          ! Do nothing here for char fill value.

        CASE DEFAULT
          Put_Status(4) = Put_netCDF_Attribute( NC_FileID, &
                                                FILLVALUE_ATTNAME, &
                                                FP_FILLVALUE, &
                                                Variable_Name=VarName )
      END SELECT
      
      ! Check attribute write errors
      IF ( ANY(Put_Status /= SUCCESS) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//VarName//&
                              ' variable attributes to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END FUNCTION Def_Var

  END FUNCTION Create_AerosolCoeff_netCDF


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_AerosolCoeff_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF AerosolCoeff format file to obtain the
!       dimensions and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_AerosolCoeff_netCDF( NC_Filename                      , &  ! Input
!                                                   n_Wavelengths   =n_Wavelengths   , &  ! Optional output
!                                                   n_Radii         =n_Radii         , &  ! Optional output
!                                                   n_Types         =n_Types         , &  ! Optional output
!                                                   n_RH            =n_RH            , &  ! Optional output
!                                                   n_Legendre_Terms=n_Legendre_Terms, &  ! Optional output
!                                                   n_Phase_Elements=n_Phase_Elements, &  ! Optional output
!                                                   Release         =Release         , &  ! Optional output
!                                                   Version         =Version         , &  ! Optional output
!                                                   Title           =Title           , &  ! Optional output
!                                                   History         =History         , &  ! Optional output
!                                                   Comment         =Comment         , &  ! Optional output
!                                                   RCS_Id          =RCS_Id          , &  ! Revision control
!                                                   Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           AerosolCoeff netCDF format data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Wavelengths:      The number of wavelengths in the look-up
!                           table (LUT).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Radii:            The number of discrete effective radii for
!                           scatterers in the LUT.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Types:            The number of different aerosol types in
!                           the LUT.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_RH:               The number of relative humidity entries in
!                           the LUT.
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
!       Release:            The release number of the netCDF AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF AerosolCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the netCDF file inquiry was successful.
!                        == FAILURE an error occurred reading any of the requested
!                                   dimension or variable data.
!                        == WARNING - an error occurred reading any of the requested
!                                     global file attributes, or
!                                   - an error occurred closing the netCDF file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_AerosolCoeff_netCDF( NC_Filename     , &  ! Input
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
                                        Comment         , &  ! Optional output
                                        RCS_Id          , &  ! Revision control
                                        Message_Log     ) &  ! Error messaging
                                      RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Wavelengths   
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Radii         
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Types         
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_RH            
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Legendre_Terms
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Phase_Elements
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_AerosolCoeff_netCDF'
    ! Function variables
    CHARACTER(1000) :: Message
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: File_n_Wavelengths   
    INTEGER :: File_n_Radii         
    INTEGER :: File_n_Types         
    INTEGER :: File_n_RH            
    INTEGER :: File_n_Legendre_Terms
    INTEGER :: File_n_Phase_Elements
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_AerosolCoeff_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF AerosolCoeff data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF


    ! Get the dimensions
    ! ------------------
    ! The number of wavelengths
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         WAVELENGTH_DIMNAME, &
                                         File_n_Wavelengths, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//WAVELENGTH_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of radii
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         REFF_DIMNAME, &
                                         File_n_Radii, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//REFF_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of aerosol types
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         TYPE_DIMNAME, &
                                         File_n_Types, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//TYPE_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of ??
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         RH_DIMNAME, &
                                         File_n_RH, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//RH_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of Legendre terms
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         LEGENDRE_DIMNAME, &
                                         File_n_Legendre_Terms, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//LEGENDRE_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of phase matrix elements
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         PHASE_DIMNAME, &
                                         File_n_Phase_Elements, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//PHASE_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Set the dimension return values
    ! -------------------------------
    IF ( PRESENT( n_Wavelengths    ) ) n_Wavelengths    = File_n_Wavelengths   
    IF ( PRESENT( n_Radii          ) ) n_Radii          = File_n_Radii         
    IF ( PRESENT( n_Types          ) ) n_Types          = File_n_Types         
    IF ( PRESENT( n_RH             ) ) n_RH             = File_n_RH            
    IF ( PRESENT( n_Legendre_Terms ) ) n_Legendre_Terms = File_n_Legendre_Terms-1  ! Indexed from 0, so subtract 1.
    IF ( PRESENT( n_Phase_Elements ) ) n_Phase_Elements = File_n_Phase_Elements


    ! Get the Release and Version
    ! ---------------------------
    ! The Release
    IF ( PRESENT( Release ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          RELEASE_VARNAME, &
                                          Release )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//RELEASE_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF
    ! The Version
    IF ( PRESENT( Version ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          VERSION_VARNAME, &
                                          Version )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//VERSION_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF


    ! Get the global attributes
    ! -------------------------
    Error_Status = Read_AerosolCoeff_GAtts( NC_Filename            , &
                                            NC_FileID              , &
                                            Title      =Title      , &
                                            History    =History    , &
                                            Comment    =Comment    , &
                                            Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_AerosolCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF AerosolCoeff data file '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_AerosolCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Inquire_AerosolCoeff_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Write_AerosolCoeff_netCDF
!
! PURPOSE:
!       Function to write AerosolCoeff data to a netCDF format AerosolCoeff
!       file.
!
! CALLING SEQUENCE:
!     Error_Status = Write_AerosolCoeff_netCDF( NC_Filename            , &  ! Input
!                                               AerosolCoeff           , &  ! Input
!                                               Quiet      =Quiet      , &  ! Optional input
!                                               Title      =Title      , &  ! Optional input
!                                               History    =History    , &  ! Optional input
!                                               Comment    =Comment    , &  ! Optional input
!                                               RCS_Id     =RCS_Id     , &  ! Revision control
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the netCDF
!                     format AerosolCoeff data file to write data into.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       AerosolCoeff: Structure containing the aerosol optical property
!                     data to write to file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(AerosolCoeff_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the message log file if
!                     the MESSAGE_LOG optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF AerosolCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF AerosolCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF AerosolCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the netCDF data write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Write_AerosolCoeff_netCDF( NC_Filename , &  ! Input
                                      AerosolCoeff  , &  ! Input
                                      Quiet       , &  ! Optional input
                                      Title       , &  ! Optional input
                                      History     , &  ! Optional input
                                      Comment     , &  ! Optional input
                                      RCS_Id      , &  ! Revision control
                                      Message_Log ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: NC_Filename
    TYPE(AerosolCoeff_type), INTENT(IN)  :: AerosolCoeff
    INTEGER     ,  OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Title  
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AerosolCoeff_netCDF'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff ) ) THEN
      Message = 'Some or all INPUT AerosolCoeff pointer members are NOT associated.'
      GOTO 2000
    END IF


    ! Create the output data file
    ! ---------------------------
    Error_Status = Create_AerosolCoeff_netCDF( NC_Filename                  , &  ! Input
                                               AerosolCoeff%n_Wavelengths   , &  ! Input
                                               AerosolCoeff%n_Radii         , &  ! Input
                                               AerosolCoeff%n_Types         , &  ! Input
                                               AerosolCoeff%n_RH            , &  ! Input
                                               AerosolCoeff%n_Legendre_Terms, &  ! Input
                                               AerosolCoeff%n_Phase_Elements, &  ! Input
                                               NC_FileID                    , &  ! Output
                                               Title      =Title            , &  ! Optional input
                                               History    =History          , &  ! Optional input
                                               Comment    =Comment          , &  ! Optional input
                                               Message_Log=Message_Log        )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating output file '//TRIM( NC_Filename )
      GOTO 2000
    END IF


    ! Write the data file release and version
    ! ---------------------------------------
    ! The Release
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        RELEASE_VARNAME, &
                                        AerosolCoeff%Release )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Release to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The Version
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        VERSION_VARNAME, &
                                        AerosolCoeff%Version )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Version to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    

    ! Write the aerosol descriptor vectors
    ! -------------------------------------
    ! Write the aerosol type name vector
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TYPE_NAME_VARNAME, &
                                        AerosolCoeff%Type_Name )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//TYPE_NAME_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Write the aerosol type vector
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TYPE_VARNAME, &
                                        AerosolCoeff%Type )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//TYPE_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    
    ! Write the LUT dimension vectors
    ! -------------------------------
    ! The wavelength vector
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WAVELENGTH_VARNAME, &
                                        AerosolCoeff%Wavelength )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//WAVELENGTH_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The frequency vector
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_VARNAME, &
                                        AerosolCoeff%Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//FREQUENCY_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The radii vector
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        REFF_VARNAME, &
                                        AerosolCoeff%Reff )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//REFF_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The relative humidity vector
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        RH_VARNAME, &
                                        AerosolCoeff%RH )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//RH_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Write the optical property variables
    ! ------------------------------------
    ! The extinction coefficient
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        KE_VARNAME, &
                                        AerosolCoeff%ke )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//KE_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The single scatter albedo
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        W_VARNAME, &
                                        AerosolCoeff%w )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//W_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The asymmetry parameter
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        G_VARNAME, &
                                        AerosolCoeff%g )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//G_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The phase coefficients parameter
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PCOEFF_VARNAME, &
                                        AerosolCoeff%pcoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//PCOEFF_VARNAME//' to '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_AerosolCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF AerosolCoeff data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_AerosolCoeff( AerosolCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_AerosolCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_AerosolCoeff_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Read_AerosolCoeff_netCDF
!
! PURPOSE:
!       Function to read AerosolCoeff data from a netCDF format AerosolCoeff
!       file.
!
! CALLING SEQUENCE:
!     Error_Status = Read_AerosolCoeff_netCDF( NC_Filename            , &  ! Input
!                                              AerosolCoeff           , &  ! Output
!                                              Quiet      =Quiet      , &  ! Optional input
!                                              Title      =Title      , &  ! Optional output
!                                              History    =History    , &  ! Optional output
!                                              Comment    =Comment    , &  ! Optional output
!                                              RCS_Id     =RCS_Id     , &  ! Revision control
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the
!                     netCDF format AerosolCoeff data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the message log file if
!                     the MESSAGE_LOG optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AerosolCoeff: Structure to contain the aerosol optical property data
!                     read from file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(AerosolCoeff_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF AerosolCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF AerosolCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF AerosolCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the netCDF data read was successful.
!                        == FAILURE an unrecoverable error occurred.
!                        == WARNING an error occurred closing the netCDF
!                                   input file after a successful read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       If specified as the output data type, the INTENT on the output AerosolCoeff
!       structure argument is IN OUT rather than just OUT. This is necessary
!       because the argument may be defined on input. To prevent memory leaks,
!       the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_AerosolCoeff_netCDF( NC_Filename , &  ! Input
                                     AerosolCoeff, &  ! Output
                                     Quiet       , &  ! Optional input
                                     Title       , &  ! Optional output
                                     History     , &  ! Optional output
                                     Comment     , &  ! Optional output
                                     RCS_Id      , &  ! Revision control
                                     Message_Log ) &  ! Error messaging
                                   RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)     :: NC_Filename
    TYPE(AerosolCoeff_type), INTENT(IN OUT) :: AerosolCoeff
    INTEGER,       OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: Title  
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_AerosolCoeff_netCDF'
    ! Function variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: n_Wavelengths   
    INTEGER :: n_Radii         
    INTEGER :: n_Types         
    INTEGER :: n_RH            
    INTEGER :: n_Legendre_Terms
    INTEGER :: n_Phase_Elements
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    
    ! Allocate the structure for the netCDF read
    ! ------------------------------------------
    ! Read the dimension values and global attributes
    Error_Status = Inquire_AerosolCoeff_netCDF( NC_Filename                      , &
                                                n_Wavelengths   =n_Wavelengths   , &
                                                n_Radii         =n_Radii         , &
                                                n_Types         =n_Types         , &
                                                n_RH            =n_RH            , &
                                                n_Legendre_Terms=n_Legendre_Terms, &
                                                n_Phase_Elements=n_Phase_Elements, &
                                                Title           =Title           , &
                                                History         =History         , &
                                                Comment         =Comment         , &
                                                Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining AerosolCoeff dimensions from '//TRIM( NC_Filename )
      GOTO 3000
    END IF

    ! Allocate the structure
    Error_Status = Allocate_AerosolCoeff( n_Wavelengths   , &
                                          n_Radii         , &
                                          n_Types         , &
                                          n_RH            , &
                                          n_Legendre_Terms, &
                                          n_Phase_Elements, &
                                          AerosolCoeff    , &
                                          Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating AerosolCoeff structure.'
      GOTO 3000
    END IF


    ! Open the netCDF file for reading
    ! --------------------------------
    Error_Status = Open_AerosolCoeff_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode='READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF AerosolCoeff data file '//TRIM( NC_Filename )
      GOTO 2000
    END IF


    ! Read the data file release and version
    ! --------------------------------------
    ! The Release
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        RELEASE_VARNAME, &
                                        AerosolCoeff%Release )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Release from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The Version
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        VERSION_VARNAME, &
                                        AerosolCoeff%Version )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Version from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    
    ! Check the release
    Error_Status = Check_AerosolCoeff_Release( AerosolCoeff, &
                                             Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'AerosolCoeff Release check failed for '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    

    ! Read the aerosol descriptor vectors
    ! -------------------------------------
    ! Read the aerosol type name vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TYPE_NAME_VARNAME, &
                                        AerosolCoeff%Type_Name )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//TYPE_NAME_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Read the aerosol type vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TYPE_VARNAME, &
                                        AerosolCoeff%Type )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//TYPE_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    
    ! Read the LUT dimension vectors
    ! ------------------------------
    ! The wavelength vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        WAVELENGTH_VARNAME, &
                                        AerosolCoeff%Wavelength )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//WAVELENGTH_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The frequency vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        FREQUENCY_VARNAME, &
                                        AerosolCoeff%Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//FREQUENCY_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The radii vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        REFF_VARNAME, &
                                        AerosolCoeff%Reff )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//REFF_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The relative humidity vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        RH_VARNAME, &
                                        AerosolCoeff%RH )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//RH_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Read the optical property variables
    ! -----------------------------------
    ! The extinction coefficient
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        KE_VARNAME, &
                                        AerosolCoeff%ke )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//KE_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The single scatter albedo
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        W_VARNAME, &
                                        AerosolCoeff%w )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//W_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The asymmetry parameter
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        G_VARNAME, &
                                        AerosolCoeff%g )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//G_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The phase coefficients parameter
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        PCOEFF_VARNAME, &
                                        AerosolCoeff%pcoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//PCOEFF_VARNAME//' from '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_AerosolCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF AerosolCoeff data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_AerosolCoeff( AerosolCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_AerosolCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'

    2000 CONTINUE
    Destroy_Status = Destroy_AerosolCoeff(AerosolCoeff, Message_Log=Message_Log)
    IF ( Destroy_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error destroying AerosolCoeff during error cleanup.'

    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_AerosolCoeff_netCDF

END MODULE AerosolCoeff_netCDF_IO_R2
