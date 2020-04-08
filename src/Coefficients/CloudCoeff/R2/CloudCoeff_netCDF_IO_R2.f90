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

MODULE CloudCoeff_netCDF_IO_R2


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds       , ONLY: Long, Double
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE CloudCoeff_Define_R2, ONLY: CloudCoeff_type, &
                               Associated_CloudCoeff, &
                               Destroy_CloudCoeff, &
                               Allocate_CloudCoeff, &
                               Check_CloudCoeff_Release, &
                               Info_CloudCoeff
  USE netcdf
  USE netCDF_Utility   ,  Open_CloudCoeff_netCDF =>  Open_netCDF, &
                         Close_CloudCoeff_netCDF => Close_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_CloudCoeff_netCDF
  PUBLIC :: Write_CloudCoeff_netCDF
  PUBLIC :: Read_CloudCoeff_netCDF


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
  CHARACTER(*), PARAMETER :: RELEASE_VARNAME = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_VARNAME = 'Version'

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

  CHARACTER(*), PARAMETER :: RELEASE_LONGNAME = 'File Release'
  CHARACTER(*), PARAMETER :: VERSION_LONGNAME = 'Data Version'

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

  CHARACTER(*), PARAMETER :: RELEASE_DESCRIPTION = 'Release indicates structure and file format changes'
  CHARACTER(*), PARAMETER :: VERSION_DESCRIPTION = 'Version indicates data changes'

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
  
  INTEGER(Long), PARAMETER :: IP_FILLVALUE = -1_Long
  REAL(Double) , PARAMETER :: FP_FILLVALUE = ZERO


  ! Variable types
  INTEGER, PARAMETER :: RELEASE_TYPE = NF90_INT
  INTEGER, PARAMETER :: VERSION_TYPE = NF90_INT

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
!       Write_CloudCoeff_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF CloudCoeff
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_CloudCoeff_GAtts( NC_Filename            , &  ! Input
!                                              NC_FileID              , &  ! Input
!                                              Title      =Title      , &  ! Optional input
!                                              History    =History    , &  ! Optional input
!                                              Comment    =Comment    , &  ! Optional input
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF CloudCoeff format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_CloudCoeff_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF CloudCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF CloudCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF CloudCoeff file.
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

  FUNCTION Write_CloudCoeff_GAtts( NC_Filename  , &  ! Input
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_CloudCoeff_GAtts'
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

  END FUNCTION Write_CloudCoeff_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       Read_CloudCoeff_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF CloudCoeff
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_CloudCoeff_GAtts( NC_Filename            , &  ! Input
!                                             NC_FileID              , &  ! Input
!                                             Title      =Title      , &  ! Optional output
!                                             History    =History    , &  ! Optional output
!                                             Comment    =Comment    , &  ! Optional output
!                                             Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF CloudCoeff format data file to read from.
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
!                         attribute field of the netCDF CloudCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF CloudCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF CloudCoeff file.
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

  FUNCTION Read_CloudCoeff_GAtts( NC_Filename  , &  ! Input
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_CloudCoeff_GAtts'
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

  END FUNCTION Read_CloudCoeff_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       Create_CloudCoeff_netCDF
!
! PURPOSE:
!       Function to create a netCDF CloudCoeff data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = Create_CloudCoeff_netCDF( NC_Filename            , &  ! Input
!                                                n_MW_Frequencies       , &  ! Input
!                                                n_MW_Radii             , &  ! Input
!                                                n_IR_Frequencies       , &  ! Input
!                                                n_IR_Radii             , &  ! Input
!                                                n_Temperatures         , &  ! Input
!                                                n_Densities            , &  ! Input
!                                                n_Legendre_Terms       , &  ! Input
!                                                n_Phase_Elements       , &  ! Input
!                                                NC_FileID              , &  ! Output
!                                                Title      =Title      , &  ! Optional input
!                                                History    =History    , &  ! Optional input
!                                                Comment    =Comment    , &  ! Optional input
!                                                RCS_Id     =RCS_Id     , &  ! Revision control
!                                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF CloudCoeff format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_MW_Frequencies:   The number of microwave frequencies in
!                           the look-up table (LUT). Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_MW_Radii:         The number of discrete effective radii 
!                           for MW scatterers in the LUT. Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_IR_Frequencies:   The number of infrared frequencies in
!                           the LUT. Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_IR_Radii:         The number of discrete effective radii 
!                           for IR scatterers in the LUT. Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Temperatures:     The number of discrete layer temperatures
!                           in the LUT. Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Densities:        The number of fixed densities for snow, graupel,
!                           and hail/ice in the LUT. Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:   The maximum number of Legendre polynomial
!                           terms in the LUT. Must be > 0.
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
!                           attribute field of the netCDF CloudCoeff file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF CloudCoeff file.
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

  FUNCTION Create_CloudCoeff_netCDF( NC_Filename     , &  ! Input
                                     n_MW_Frequencies, &  ! Input
                                     n_MW_Radii      , &  ! Input
                                     n_IR_Frequencies, &  ! Input
                                     n_IR_Radii      , &  ! Input
                                     n_Temperatures  , &  ! Input
                                     n_Densities     , &  ! Input
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
    INTEGER               , INTENT(IN)  :: n_MW_Frequencies
    INTEGER               , INTENT(IN)  :: n_MW_Radii      
    INTEGER               , INTENT(IN)  :: n_IR_Frequencies
    INTEGER               , INTENT(IN)  :: n_IR_Radii      
    INTEGER               , INTENT(IN)  :: n_Temperatures  
    INTEGER               , INTENT(IN)  :: n_Densities     
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_CloudCoeff_netCDF'
    ! Local variables
    INTEGER :: NF90_Status
    INTEGER :: Status1, Status2, Status3
    INTEGER :: Close_Status
    INTEGER :: StrLen_DimID
    INTEGER :: MW_Freq_DimID
    INTEGER :: MW_Reff_DimID
    INTEGER :: IR_Freq_DimID
    INTEGER :: IR_Reff_DimID
    INTEGER :: Temperature_DimID
    INTEGER :: Density_DimID, IR_Density_DimID
    INTEGER :: Legendre_DimID
    INTEGER :: Phase_DimID
    INTEGER :: VarID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check input
    IF ( n_MW_Frequencies < 1 .OR. &
         n_MW_Radii       < 1 .OR. &
         n_IR_Frequencies < 1 .OR. &
         n_IR_Radii       < 1 .OR. &
         n_Temperatures   < 1 .OR. &
         n_Densities      < 1 .OR. &
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
    ! The number of microwave frequencies
    Error_Status = Def_Dim(MW_FREQ_DIMNAME    , n_MW_Frequencies  , MW_Freq_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of microwave radii
    Error_Status = Def_Dim(MW_REFF_DIMNAME    , n_MW_Radii        , MW_Reff_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of infrared frequencies
    Error_Status = Def_Dim(IR_FREQ_DIMNAME    , n_IR_Frequencies  , IR_Freq_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of infrared radii
    Error_Status = Def_Dim(IR_REFF_DIMNAME    , n_IR_Radii        , IR_Reff_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of temperatures
    Error_Status = Def_Dim(TEMPERATURE_DIMNAME, n_Temperatures    , Temperature_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of densities
    Error_Status = Def_Dim(DENSITY_DIMNAME    , n_Densities       , Density_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of IR densities. The data arrays are
    ! indexed from 0 for the liquid phase, so add one.
    Error_Status = Def_Dim(IR_DENSITY_DIMNAME , n_Densities+1     , IR_Density_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of Legendre terms. The data arrays are
    ! indexed from 0, so add one.
    Error_Status = Def_Dim(LEGENDRE_DIMNAME   , n_Legendre_Terms+1, Legendre_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of phase matrix elements
    Error_Status = Def_Dim(PHASE_DIMNAME      , n_Phase_Elements  , Phase_DimID)
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Write the global attributes
    ! ---------------------------
    Error_Status = Write_CloudCoeff_GAtts( NC_Filename            , &
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


    ! Define the LUT dimension vectors
    ! --------------------------------
    ! Define the microwave frequency vector
    Error_Status = Def_Var(FREQUENCY_MW_VARNAME    , &
                           FREQUENCY_MW_TYPE       , &
                           FREQUENCY_MW_LONGNAME   , &
                           FREQUENCY_MW_DESCRIPTION, &
                           FREQUENCY_MW_UNITS      , &
                           DimIDs=(/MW_Freq_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the microwave radii vector
    Error_Status = Def_Var(REFF_MW_VARNAME    , &
                           REFF_MW_TYPE       , &
                           REFF_MW_LONGNAME   , &
                           REFF_MW_DESCRIPTION, &
                           REFF_MW_UNITS      , &
                           DimIDs=(/MW_Reff_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the infrared frequency vector
    Error_Status = Def_Var(FREQUENCY_IR_VARNAME    , &
                           FREQUENCY_IR_TYPE       , &
                           FREQUENCY_IR_LONGNAME   , &
                           FREQUENCY_IR_DESCRIPTION, &
                           FREQUENCY_IR_UNITS      , &
                           DimIDs=(/IR_Freq_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the infrared radii vector
    Error_Status = Def_Var(REFF_IR_VARNAME    , &
                           REFF_IR_TYPE       , &
                           REFF_IR_LONGNAME   , &
                           REFF_IR_DESCRIPTION, &
                           REFF_IR_UNITS      , &
                           DimIDs=(/IR_Reff_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the temperature vector
    Error_Status = Def_Var(TEMPERATURE_VARNAME    , &
                           TEMPERATURE_TYPE       , &
                           TEMPERATURE_LONGNAME   , &
                           TEMPERATURE_DESCRIPTION, &
                           TEMPERATURE_UNITS      , &
                           DimIDs=(/Temperature_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the density vector
    Error_Status = Def_Var(DENSITY_VARNAME    , &
                           DENSITY_TYPE       , &
                           DENSITY_LONGNAME   , &
                           DENSITY_DESCRIPTION, &
                           DENSITY_UNITS      , &
                           DimIDs=(/Density_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Define the microwave liquid phase variables
    ! -------------------------------------------
    ! Define the extinction coefficient
    Error_Status = Def_Var(KE_L_MW_VARNAME    , &
                           KE_L_MW_TYPE       , &
                           KE_L_MW_LONGNAME   , &
                           KE_L_MW_DESCRIPTION, &
                           KE_L_MW_UNITS      , &
                           DimIDs=(/MW_Freq_DimID, MW_Reff_DimID, Temperature_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the single scatter albedo
    Error_Status = Def_Var(W_L_MW_VARNAME    , &
                           W_L_MW_TYPE       , &
                           W_L_MW_LONGNAME   , &
                           W_L_MW_DESCRIPTION, &
                           W_L_MW_UNITS      , &
                           DimIDs=(/MW_Freq_DimID, MW_Reff_DimID, Temperature_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the asymmetry parameter
    Error_Status = Def_Var(G_L_MW_VARNAME    , &
                           G_L_MW_TYPE       , &
                           G_L_MW_LONGNAME   , &
                           G_L_MW_DESCRIPTION, &
                           G_L_MW_UNITS      , &
                           DimIDs=(/MW_Freq_DimID, MW_Reff_DimID, Temperature_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the phase coefficients
    Error_Status = Def_Var(PCOEFF_L_MW_VARNAME    , &
                           PCOEFF_L_MW_TYPE       , &
                           PCOEFF_L_MW_LONGNAME   , &
                           PCOEFF_L_MW_DESCRIPTION, &
                           PCOEFF_L_MW_UNITS      , &
                           DimIDs=(/MW_Freq_DimID, MW_Reff_DimID, Temperature_DimID, &
                                    Legendre_DimID, Phase_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Define the microwave solid phase variables
    ! ------------------------------------------
    ! Define the extinction coefficient
    Error_Status = Def_Var(KE_S_MW_VARNAME    , &
                           KE_S_MW_TYPE       , &
                           KE_S_MW_LONGNAME   , &
                           KE_S_MW_DESCRIPTION, &
                           KE_S_MW_UNITS      , &
                           DimIDs=(/MW_Freq_DimID, MW_Reff_DimID, Density_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the single scatter albedo
    Error_Status = Def_Var(W_S_MW_VARNAME    , &
                           W_S_MW_TYPE       , &
                           W_S_MW_LONGNAME   , &
                           W_S_MW_DESCRIPTION, &
                           W_S_MW_UNITS      , &
                           DimIDs=(/MW_Freq_DimID, MW_Reff_DimID, Density_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the asymmetry parameter
    Error_Status = Def_Var(G_S_MW_VARNAME    , &
                           G_S_MW_TYPE       , &
                           G_S_MW_LONGNAME   , &
                           G_S_MW_DESCRIPTION, &
                           G_S_MW_UNITS      , &
                           DimIDs=(/MW_Freq_DimID, MW_Reff_DimID, Density_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the phase coefficients
    Error_Status = Def_Var(PCOEFF_S_MW_VARNAME    , &
                           PCOEFF_S_MW_TYPE       , &
                           PCOEFF_S_MW_LONGNAME   , &
                           PCOEFF_S_MW_DESCRIPTION, &
                           PCOEFF_S_MW_UNITS      , &
                           DimIDs=(/MW_Freq_DimID, MW_Reff_DimID, Density_DimID, &
                                    Legendre_DimID, Phase_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Define the infrared variables
    ! -----------------------------
    ! Define the extinction coefficient
    Error_Status = Def_Var(KE_IR_VARNAME    , &
                           KE_IR_TYPE       , &
                           KE_IR_LONGNAME   , &
                           KE_IR_DESCRIPTION, &
                           KE_IR_UNITS      , &
                           DimIDs=(/IR_Freq_DimID, IR_Reff_DimID, IR_Density_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the single scatter albedo
    Error_Status = Def_Var(W_IR_VARNAME    , &
                           W_IR_TYPE       , &
                           W_IR_LONGNAME   , &
                           W_IR_DESCRIPTION, &
                           W_IR_UNITS      , &
                           DimIDs=(/IR_Freq_DimID, IR_Reff_DimID, IR_Density_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the asymmetry parameter
    Error_Status = Def_Var(G_IR_VARNAME    , &
                           G_IR_TYPE       , &
                           G_IR_LONGNAME   , &
                           G_IR_DESCRIPTION, &
                           G_IR_UNITS      , &
                           DimIDs=(/IR_Freq_DimID, IR_Reff_DimID, IR_Density_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the phase coefficients
    Error_Status = Def_Var(PCOEFF_IR_VARNAME    , &
                           PCOEFF_IR_TYPE       , &
                           PCOEFF_IR_LONGNAME   , &
                           PCOEFF_IR_DESCRIPTION, &
                           PCOEFF_IR_UNITS      , &
                           DimIDs=(/IR_Freq_DimID, IR_Reff_DimID, IR_Density_DimID, &
                                    Legendre_DimID/) )
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
        CASE (RELEASE_VARNAME, VERSION_VARNAME)
          Put_Status(4) = Put_netCDF_Attribute( NC_FileID, &
                                                FILLVALUE_ATTNAME, &
                                                IP_FILLVALUE, &
                                                Variable_Name=VarName )
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

  END FUNCTION Create_CloudCoeff_netCDF


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
!       Inquire_CloudCoeff_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF CloudCoeff format file to obtain the
!       dimensions and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_CloudCoeff_netCDF( NC_Filename                       , &  ! Input
!                                                 n_MW_Frequencies=n_MW_Frequencies , &  ! Optional output
!                                                 n_MW_Radii      =n_MW_Radii       , &  ! Optional output
!                                                 n_IR_Frequencies=n_IR_Frequencies , &  ! Optional output
!                                                 n_IR_Radii      =n_IR_Radii       , &  ! Optional output
!                                                 n_Temperatures  =n_Temperatures   , &  ! Optional output
!                                                 n_Densities     =n_Densities      , &  ! Optional output
!                                                 n_Legendre_Terms=n_Legendre_Terms , &  ! Optional output
!                                                 n_Phase_Elements=n_Phase_Elements , &  ! Optional output
!                                                 Release         =Release          , &  ! Optional output
!                                                 Version         =Version          , &  ! Optional output
!                                                 Title           =Title            , &  ! Optional output
!                                                 History         =History          , &  ! Optional output
!                                                 Comment         =Comment          , &  ! Optional output
!                                                 RCS_Id          =RCS_Id           , &  ! Revision control
!                                                 Message_Log     =Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           CloudCoeff netCDF format data file to inquire.
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
!       Release:            The release number of the netCDF CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF CloudCoeff file.
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

  FUNCTION Inquire_CloudCoeff_netCDF( NC_Filename     , &  ! Input
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
                                      Comment         , &  ! Optional output
                                      RCS_Id          , &  ! Revision control
                                      Message_Log     ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_MW_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_MW_Radii
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_IR_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_IR_Radii
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Temperatures
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Densities
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_CloudCoeff_netCDF'
    ! Function variables
    CHARACTER(1000) :: Message
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: n_MW_Freq
    INTEGER :: n_MW_Reff
    INTEGER :: n_IR_Freq
    INTEGER :: n_IR_Reff
    INTEGER :: n_Temp
    INTEGER :: n_Dens
    INTEGER :: n_LTerm
    INTEGER :: n_Phase
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_CloudCoeff_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF CloudCoeff data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF


    ! Get the dimensions
    ! ------------------
    ! The number of microwave frequencies
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         MW_FREQ_DIMNAME, &
                                         n_MW_Freq, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//MW_FREQ_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of microwave radii
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         MW_REFF_DIMNAME, &
                                         n_MW_Reff, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//MW_REFF_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of infrared frequencies
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         IR_FREQ_DIMNAME, &
                                         n_IR_Freq, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//IR_FREQ_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of infrared radii
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         IR_REFF_DIMNAME, &
                                         n_IR_Reff, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//IR_REFF_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of temperatures
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         TEMPERATURE_DIMNAME, &
                                         n_Temp, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//TEMPERATURE_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of densities
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         DENSITY_DIMNAME, &
                                         n_Dens, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//DENSITY_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of Legendre terms
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         LEGENDRE_DIMNAME, &
                                         n_LTerm, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//LEGENDRE_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The number of phase matrix elements
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         PHASE_DIMNAME, &
                                         n_Phase, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//PHASE_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Set the dimension return values
    ! -------------------------------
    IF ( PRESENT( n_MW_Frequencies ) ) n_MW_Frequencies = n_MW_Freq
    IF ( PRESENT( n_MW_Radii       ) ) n_MW_Radii       = n_MW_Reff
    IF ( PRESENT( n_IR_Frequencies ) ) n_IR_Frequencies = n_IR_Freq
    IF ( PRESENT( n_IR_Radii       ) ) n_IR_Radii       = n_IR_Reff
    IF ( PRESENT( n_Temperatures   ) ) n_Temperatures   = n_Temp
    IF ( PRESENT( n_Densities      ) ) n_Densities      = n_Dens
    IF ( PRESENT( n_Legendre_Terms ) ) n_Legendre_Terms = n_LTerm-1  ! Indexed from 0, so subtract 1.
    IF ( PRESENT( n_Phase_Elements ) ) n_Phase_Elements = n_Phase


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
    Error_Status = Read_CloudCoeff_GAtts( NC_Filename            , &
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
    Close_Status = Close_CloudCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF CloudCoeff data file '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_CloudCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Inquire_CloudCoeff_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Write_CloudCoeff_netCDF
!
! PURPOSE:
!       Function to write CloudCoeff data to a netCDF format CloudCoeff
!       file.
!
! CALLING SEQUENCE:
!     Error_Status = Write_CloudCoeff_netCDF( NC_Filename            , &  ! Input
!                                             CloudCoeff             , &  ! Input
!                                             Quiet      =Quiet      , &  ! Optional input
!                                             Title      =Title      , &  ! Optional input
!                                             History    =History    , &  ! Optional input
!                                             Comment    =Comment    , &  ! Optional input
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the netCDF
!                     format CloudCoeff data file to write data into.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       CloudCoeff:   Structure containing the cloud optical property
!                     data to write to file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CloudCoeff_type)
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
!                     attribute field of the netCDF CloudCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF CloudCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF CloudCoeff file.
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

  FUNCTION Write_CloudCoeff_netCDF( NC_Filename , &  ! Input
                                    CloudCoeff  , &  ! Input
                                    Quiet       , &  ! Optional input
                                    Title       , &  ! Optional input
                                    History     , &  ! Optional input
                                    Comment     , &  ! Optional input
                                    RCS_Id      , &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(CloudCoeff_type) , INTENT(IN)  :: CloudCoeff
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title  
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_CloudCoeff_netCDF'
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
    IF ( .NOT. Associated_CloudCoeff( CloudCoeff ) ) THEN
      Message = 'Some or all INPUT CloudCoeff pointer members are NOT associated.'
      GOTO 2000
    END IF


    ! Create the output data file
    ! ---------------------------
    Error_Status = Create_CloudCoeff_netCDF( NC_Filename                , &  ! Input
                                             CloudCoeff%n_MW_Frequencies, &  ! Input
                                             CloudCoeff%n_MW_Radii      , &  ! Input
                                             CloudCoeff%n_IR_Frequencies, &  ! Input
                                             CloudCoeff%n_IR_Radii      , &  ! Input
                                             CloudCoeff%n_Temperatures  , &  ! Input
                                             CloudCoeff%n_Densities     , &  ! Input
                                             CloudCoeff%n_Legendre_Terms, &  ! Input
                                             CloudCoeff%n_Phase_Elements, &  ! Input
                                             NC_FileID                  , &  ! Output
                                             Title      =Title          , &  ! Optional input
                                             History    =History        , &  ! Optional input
                                             Comment    =Comment        , &  ! Optional input
                                             Message_Log=Message_Log      )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating output file '//TRIM( NC_Filename )
      GOTO 2000
    END IF


    ! Write the data file release and version
    ! ---------------------------------------
    ! The Release
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        RELEASE_VARNAME, &
                                        CloudCoeff%Release )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Release to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The Version
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        VERSION_VARNAME, &
                                        CloudCoeff%Version )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Version to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    
    
    ! Write the LUT dimension vectors
    ! -------------------------------
    ! The microwave frequency vector
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        FREQUENCY_MW_VARNAME, &
                                        CloudCoeff%Frequency_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Frequency_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The microwave radii vector
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        REFF_MW_VARNAME, &
                                        CloudCoeff%Reff_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Reff_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The infrared frequency vector
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        FREQUENCY_IR_VARNAME, &
                                        CloudCoeff%Frequency_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Frequency_IR to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The infrared radii vector
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        REFF_IR_VARNAME, &
                                        CloudCoeff%Reff_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Reff_IR to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The temperature vector
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TEMPERATURE_VARNAME, &
                                        CloudCoeff%Temperature )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Temperature to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The density vector
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        DENSITY_VARNAME, &
                                        CloudCoeff%Density )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Density to '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Write the microwave liquid phase variables
    ! -------------------------------------------
    ! The extinction coefficient
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        KE_L_MW_VARNAME, &
                                        CloudCoeff%ke_L_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing ke_L_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The single scatter albedo
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        W_L_MW_VARNAME, &
                                        CloudCoeff%w_L_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing w_L_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The asymmetry parameter
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        G_L_MW_VARNAME, &
                                        CloudCoeff%g_L_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing g_L_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The phase coefficients parameter
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        PCOEFF_L_MW_VARNAME, &
                                        CloudCoeff%pcoeff_L_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing pcoeff_L_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Write the microwave solid phase variables
    ! -----------------------------------------
    ! The extinction coefficient
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        KE_S_MW_VARNAME, &
                                        CloudCoeff%ke_S_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing ke_S_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The single scatter albedo
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        W_S_MW_VARNAME, &
                                        CloudCoeff%w_S_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing w_S_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The asymmetry parameter
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        G_S_MW_VARNAME, &
                                        CloudCoeff%g_S_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing g_S_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The phase coefficients parameter
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        PCOEFF_S_MW_VARNAME, &
                                        CloudCoeff%pcoeff_S_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing pcoeff_S_MW to '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Write the infrared variables
    ! ----------------------------
    ! The extinction coefficient
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        KE_IR_VARNAME, &
                                        CloudCoeff%ke_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing ke_IR to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The single scatter albedo
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        W_IR_VARNAME, &
                                        CloudCoeff%w_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing w_IR to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The asymmetry parameter
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        G_IR_VARNAME, &
                                        CloudCoeff%g_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing g_IR to '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The phase coefficients parameter
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        PCOEFF_IR_VARNAME, &
                                        CloudCoeff%pcoeff_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing pcoeff_IR to '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_CloudCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF CloudCoeff data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_CloudCoeff( CloudCoeff, Message )
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
    Close_Status = Close_CloudCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_CloudCoeff_netCDF





!------------------------------------------------------------------------------
!
! NAME:
!       Read_CloudCoeff_netCDF
!
! PURPOSE:
!       Function to read CloudCoeff data from a netCDF format CloudCoeff
!       file.
!
! CALLING SEQUENCE:
!     Error_Status = Read_CloudCoeff_netCDF( NC_Filename            , &  ! Input
!                                            CloudCoeff             , &  ! Output
!                                            Quiet      =Quiet      , &  ! Optional input
!                                            Title      =Title      , &  ! Optional output
!                                            History    =History    , &  ! Optional output
!                                            Comment    =Comment    , &  ! Optional output
!                                            RCS_Id     =RCS_Id     , &  ! Revision control
!                                            Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the
!                     netCDF format CloudCoeff data file to read.
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
!       CloudCoeff:   Structure to contain the cloud optical property data
!                     read from file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CloudCoeff_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF CloudCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF CloudCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF CloudCoeff file.
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
!       If specified as the output data type, the INTENT on the output CloudCoeff
!       structure argument is IN OUT rather than just OUT. This is necessary
!       because the argument may be defined on input. To prevent memory leaks,
!       the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_CloudCoeff_netCDF( NC_Filename, &  ! Input
                                   CloudCoeff , &  ! Output
                                   Quiet      , &  ! Optional input
                                   Title      , &  ! Optional output
                                   History    , &  ! Optional output
                                   Comment    , &  ! Optional output
                                   RCS_Id     , &  ! Revision control
                                   Message_Log) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(CloudCoeff_type),  INTENT(IN OUT) :: CloudCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title  
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_CloudCoeff_netCDF'
    ! Function variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: n_MW_Frequencies
    INTEGER :: n_MW_Radii      
    INTEGER :: n_IR_Frequencies
    INTEGER :: n_IR_Radii      
    INTEGER :: n_Temperatures  
    INTEGER :: n_Densities     
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
    Error_Status = Inquire_CloudCoeff_netCDF( NC_Filename                      , &
                                              n_MW_Frequencies=n_MW_Frequencies, &
                                              n_MW_Radii      =n_MW_Radii      , &
                                              n_IR_Frequencies=n_IR_Frequencies, &
                                              n_IR_Radii      =n_IR_Radii      , &
                                              n_Temperatures  =n_Temperatures  , &
                                              n_Densities     =n_Densities     , &
                                              n_Legendre_Terms=n_Legendre_Terms, &
                                              n_Phase_Elements=n_Phase_Elements, &
                                              Title           =Title           , &
                                              History         =History         , &
                                              Comment         =Comment         , &
                                              Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining CloudCoeff dimensions from '//TRIM( NC_Filename )
      GOTO 3000
    END IF

    ! Allocate the structure
    Error_Status = Allocate_CloudCoeff( n_MW_Frequencies, &
                                        n_MW_Radii      , &
                                        n_IR_Frequencies, &
                                        n_IR_Radii      , &
                                        n_Temperatures  , &
                                        n_Densities     , &
                                        n_Legendre_Terms, &
                                        n_Phase_Elements, &
                                        CloudCoeff      , &
                                        Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating CloudCoeff structure.'
      GOTO 3000
    END IF


    ! Open the netCDF file for reading
    ! --------------------------------
    Error_Status = Open_CloudCoeff_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode='READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF CloudCoeff data file '//TRIM( NC_Filename )
      GOTO 2000
    END IF


    ! Read the data file release and version
    ! --------------------------------------
    ! The Release
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        RELEASE_VARNAME, &
                                        CloudCoeff%Release )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Release from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The Version
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        VERSION_VARNAME, &
                                        CloudCoeff%Version )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Version from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    
    ! Check the release
    Error_Status = Check_CloudCoeff_Release( CloudCoeff, &
                                             Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'CloudCoeff Release check failed for '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    

    ! Read the LUT dimension vectors
    ! -------------------------------
    ! The microwave frequency vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        FREQUENCY_MW_VARNAME, &
                                        CloudCoeff%Frequency_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Frequency_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The microwave radii vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        REFF_MW_VARNAME, &
                                        CloudCoeff%Reff_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Reff_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The infrared frequency vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        FREQUENCY_IR_VARNAME, &
                                        CloudCoeff%Frequency_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Frequency_IR from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The infrared radii vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        REFF_IR_VARNAME, &
                                        CloudCoeff%Reff_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Reff_IR from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The temperature vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TEMPERATURE_VARNAME, &
                                        CloudCoeff%Temperature )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Temperature from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The density vector
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        DENSITY_VARNAME, &
                                        CloudCoeff%Density )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Density from '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Read the microwave liquid phase variables
    ! -------------------------------------------
    ! The extinction coefficient
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        KE_L_MW_VARNAME, &
                                        CloudCoeff%ke_L_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading ke_L_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The single scatter albedo
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        W_L_MW_VARNAME, &
                                        CloudCoeff%w_L_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading w_L_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The asymmetry parameter
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        G_L_MW_VARNAME, &
                                        CloudCoeff%g_L_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading g_L_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The phase coefficients parameter
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        PCOEFF_L_MW_VARNAME, &
                                        CloudCoeff%pcoeff_L_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading pcoeff_L_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Read the microwave solid phase variables
    ! -----------------------------------------
    ! The extinction coefficient
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        KE_S_MW_VARNAME, &
                                        CloudCoeff%ke_S_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading ke_S_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The single scatter albedo
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        W_S_MW_VARNAME, &
                                        CloudCoeff%w_S_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading w_S_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The asymmetry parameter
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        G_S_MW_VARNAME, &
                                        CloudCoeff%g_S_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading g_S_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The phase coefficients parameter
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        PCOEFF_S_MW_VARNAME, &
                                        CloudCoeff%pcoeff_S_MW )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading pcoeff_S_MW from '//TRIM( NC_Filename )
      GOTO 1000
    END IF


    ! Read the infrared variables
    ! ----------------------------
    ! The extinction coefficient
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        KE_IR_VARNAME, &
                                        CloudCoeff%ke_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading ke_IR from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The single scatter albedo
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        W_IR_VARNAME, &
                                        CloudCoeff%w_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading w_IR from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The asymmetry parameter
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        G_IR_VARNAME, &
                                        CloudCoeff%g_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading g_IR from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The phase coefficients parameter
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        PCOEFF_IR_VARNAME, &
                                        CloudCoeff%pcoeff_IR )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading pcoeff_IR from '//TRIM( NC_Filename )
      GOTO 1000
    END IF



    ! Close the file
    ! --------------
    Close_Status = Close_CloudCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF CloudCoeff data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_CloudCoeff( CloudCoeff, Message )
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
    Close_Status = Close_CloudCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'

    2000 CONTINUE
    Destroy_Status = Destroy_CloudCoeff(CloudCoeff, Message_Log=Message_Log)
    IF ( Destroy_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error destroying CloudCoeff during error cleanup.'

    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_CloudCoeff_netCDF

END MODULE CloudCoeff_netCDF_IO_R2
