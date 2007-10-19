!
! Solar_netCDF_IO
!
! Module containing routines to read and write netCDF format
! Solar files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Solar_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds       , ONLY: Long, Double
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE Solar_Define     , ONLY: Solar_type        , &
                               Associated_Solar  , &
                               Destroy_Solar     , &
                               Allocate_Solar    , &
                               CheckRelease_Solar, &
                               Info_Solar        , &
                               Frequency_Solar
  USE netcdf
  USE netCDF_Utility,  Open_Solar_netCDF =>  Open_netCDF, &
                      Close_Solar_netCDF => Close_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_Solar_netCDF
  PUBLIC :: Write_Solar_netCDF
  PUBLIC :: Read_Solar_netCDF



  ! -----------------
  ! Module parameters
  ! -----------------

  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Message character length
  INTEGER, PARAMETER :: ML = 512
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME   = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME   = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME     = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME   = 'history' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME   = 'comment' 
  CHARACTER(*), PARAMETER :: SOURCE_ATTNAME     = 'source' 
  CHARACTER(*), PARAMETER :: REFERENCES_ATTNAME = 'references' 
  
  ! Dimension names. Case sensitive
  CHARACTER(*), PARAMETER :: FREQUENCY_DIMNAME  = 'n_Frequencies'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: BEGIN_FREQUENCY_VARNAME       = 'Begin_Frequency'
  CHARACTER(*), PARAMETER :: END_FREQUENCY_VARNAME         = 'End_Frequency'
  CHARACTER(*), PARAMETER :: FREQUENCY_INTERVAL_VARNAME    = 'Frequency_Interval'
  CHARACTER(*), PARAMETER :: BLACKBODY_TEMPERATURE_VARNAME = 'Blackbody_Temperature'
  CHARACTER(*), PARAMETER :: RADIUS_VARNAME                = 'Radius'
  CHARACTER(*), PARAMETER :: EARTH_SUN_DISTANCE_VARNAME    = 'Earth_Sun_Disance'
  CHARACTER(*), PARAMETER :: IRRADIANCE_VARNAME            = 'Irradiance'
  CHARACTER(*), PARAMETER :: BLACKBODY_IRRADIANCE_VARNAME  = 'Blackbody_Irradiance'

  ! Description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: BEGIN_FREQUENCY_DESC       = 'Begin frequency of irradiance data'
  CHARACTER(*), PARAMETER :: END_FREQUENCY_DESC         = 'End frequency of irradiance data'
  CHARACTER(*), PARAMETER :: FREQUENCY_INTERVAL_DESC    = 'Frequency interval of irradiance data'
  CHARACTER(*), PARAMETER :: BLACKBODY_TEMPERATURE_DESC = 'Solar radiative temperature used to '//&
                                                            'generate the blackbody source function'
  CHARACTER(*), PARAMETER :: RADIUS_DESC                = 'Radius of visible solar disk, or photosphere'
  CHARACTER(*), PARAMETER :: EARTH_SUN_DISTANCE_DESC    = 'Earth-Sun distance'
  CHARACTER(*), PARAMETER :: IRRADIANCE_DESC            = 'Extraterrestrial TOA solar irradiance '//&
                                                            'using Kurucz spectrum'
  CHARACTER(*), PARAMETER :: BLACKBODY_IRRADIANCE_DESC  = 'Extraterrestrial TOA solar blackbody '//&
                                                            'irradiance using blackbody temperature'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: BEGIN_FREQUENCY_LONGNAME       = 'Begin Frequency'
  CHARACTER(*), PARAMETER :: END_FREQUENCY_LONGNAME         = 'End Frequency'
  CHARACTER(*), PARAMETER :: FREQUENCY_INTERVAL_LONGNAME    = 'Frequency Interval'
  CHARACTER(*), PARAMETER :: BLACKBODY_TEMPERATURE_LONGNAME = 'Solar Tb Source Temperature'
  CHARACTER(*), PARAMETER :: RADIUS_LONGNAME                = 'Solar Radius'
  CHARACTER(*), PARAMETER :: EARTH_SUN_DISTANCE_LONGNAME    = 'Earth-Sun distance'
  CHARACTER(*), PARAMETER :: IRRADIANCE_LONGNAME            = 'Solar Irradiance'
  CHARACTER(*), PARAMETER :: BLACKBODY_IRRADIANCE_LONGNAME  = 'Solar Blackbody Irradiance'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: BEGIN_FREQUENCY_UNITS       = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: END_FREQUENCY_UNITS         = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: FREQUENCY_INTERVAL_UNITS    = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: BLACKBODY_TEMPERATURE_UNITS = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: RADIUS_UNITS                = 'Metres (m)'
  CHARACTER(*), PARAMETER :: EARTH_SUN_DISTANCE_UNITS    = 'Metres (m)'
  CHARACTER(*), PARAMETER :: IRRADIANCE_UNITS            = 'mW/(m^2.cm^-1)'
  CHARACTER(*), PARAMETER :: BLACKBODY_IRRADIANCE_UNITS  = 'mW/(m^2.cm^-1)'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  REAL(Double), PARAMETER :: BEGIN_FREQUENCY_FILLVALUE       = ZERO
  REAL(Double), PARAMETER :: END_FREQUENCY_FILLVALUE         = ZERO
  REAL(Double), PARAMETER :: FREQUENCY_INTERVAL_FILLVALUE    = ZERO
  REAL(Double), PARAMETER :: BLACKBODY_TEMPERATURE_FILLVALUE = ZERO
  REAL(Double), PARAMETER :: RADIUS_FILLVALUE                = ZERO
  REAL(Double), PARAMETER :: EARTH_SUN_DISTANCE_FILLVALUE    = ZERO
  REAL(Double), PARAMETER :: IRRADIANCE_FILLVALUE            = ZERO
  REAL(Double), PARAMETER :: BLACKBODY_IRRADIANCE_FILLVALUE  = ZERO

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: BEGIN_FREQUENCY_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: END_FREQUENCY_TYPE         = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_INTERVAL_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: BLACKBODY_TEMPERATURE_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: RADIUS_TYPE                = NF90_DOUBLE
  INTEGER, PARAMETER :: EARTH_SUN_DISTANCE_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: IRRADIANCE_TYPE            = NF90_DOUBLE
  INTEGER, PARAMETER :: BLACKBODY_IRRADIANCE_TYPE  = NF90_DOUBLE


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
!       WriteGAtts
!
! PURPOSE:
!       Function to write the supplied attributes to a netCDF format
!       Solar data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_GAtts( NC_Filename            , &  ! Input
!                                   NC_FileID              , &  ! Input
!                                   Release    =Release    , &  ! Optional input
!                                   Version    =Version    , &  ! Optional input
!                                   Title      =Title      , &  ! Optional input
!                                   History    =History    , &  ! Optional input
!                                   Comment    =Comment    , &  ! Optional input
!                                   Source     =Source     , &  ! Optional input
!                                   References =References , &  ! Optional input
!                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF Solar format data to write to.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Release:          The release number of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Source:           Character string written into the SOURCE
!                         attribute field of the IRRADIANCE
!                         variable.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       References:       Character string written into the REFERENCES
!                         attribute field of the IRRADIANCE
!                         variable. 
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
!                         If == SUCCESS the global attribute write was successful
!                            == FAILURE an error occurred writing the supplied
!                                       global attribute(s).
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION WriteGAtts( NC_Filename, &  ! Input
                       NC_FileID  , &  ! Input
                       Release    , &  ! Optional input
                       Version    , &  ! Optional input
                       Title      , &  ! Optional input
                       History    , &  ! Optional input
                       Comment    , &  ! Optional input
                       Source     , &  ! Optional input
                       References , &  ! Optional input
                       Message_Log) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN) :: Release         
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Source
    CHARACTER(*), OPTIONAL, INTENT(IN) :: References
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteGAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    ! Local variables
    CHARACTER(ML) :: Message
    CHARACTER(256) :: AttName, VarName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: NF90_Status
    INTEGER :: VarID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    Message = ' '
    VarName = 'global'


    ! Global attributes
    ! -----------------
    ! Software ID
    AttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(AttName), &
                                MODULE_RCS_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    
    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    AttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(AttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Release
    IF ( PRESENT(Release) ) THEN
      AttName = RELEASE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  Release )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Version
    IF ( PRESENT(Version) ) THEN
      AttName = VERSION_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      AttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  Title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      AttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  History )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      AttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  Comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    
    ! Variable attributes
    ! -------------------
    ! The Source
    IF ( PRESENT(Source) ) THEN
      VarName = IRRADIANCE_VARNAME
      AttName = SOURCE_ATTNAME
      NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                    VarName, &
                                    VarID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  TRIM(AttName), &
                                  Source )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The References
    IF ( PRESENT(References) ) THEN
      VarName = IRRADIANCE_VARNAME
      AttName = REFERENCES_ATTNAME
      NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                    VarName, &
                                    VarID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  TRIM(AttName), &
                                  References )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(VarName)//' '//TRIM(AttName)//&
                            ' attribute to '//TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       ReadGAtts
!
! PURPOSE:
!       Function to read the requested attributes from a netCDF format
!       Solar data file.
!
! CALLING SEQUENCE:
!       Error_Status = ReadGAtts( NC_Filename            , &  ! Input
!                                 NC_FileID              , &  ! Input
!                                 Release    =Release    , &  ! Optional output
!                                 Version    =Version    , &  ! Optional output
!                                 Title      =Title      , &  ! Optional output
!                                 History    =History    , &  ! Optional output
!                                 Comment    =Comment    , &  ! Optional output
!                                 Source     =Source     , &  ! Optional output
!                                 References =References , &  ! Optional output
!                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF Solar format data to read.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number associated with the input
!                         filename.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
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
!       Release:          The release number of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:          The version number of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Source:           Character string written into the SOURCE
!                         attribute field of the IRRADIANCE variable.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       References:       Character string written into the REFERENCES
!                         attribute field of the IRRADIANCE variable. 
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute read was successful
!                            == FAILURE an error occurred reading the requested
!                                       global attribute(s).
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION ReadGAtts( NC_Filename, &  ! Input
                      NC_FileID  , &  ! Input
                      Release    , &  ! Optional output
                      Version    , &  ! Optional output
                      Title      , &  ! Optional output
                      History    , &  ! Optional output
                      Comment    , &  ! Optional output
                      Source     , &  ! Optional output
                      References , &  ! Optional output
                      Message_Log) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Source
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: References
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadGAtts'
    ! Local variables
    CHARACTER(256)  :: AttName, VarName
    CHARACTER(5000) :: AttString
    INTEGER :: NF90_Status
    INTEGER :: VarID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    VarName = 'global'


    ! Global attributes
    ! -----------------
    ! The Release
    IF ( PRESENT(Release) ) THEN
      AttName = RELEASE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  Release )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Version
    IF ( PRESENT(Version) ) THEN
      AttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      AttString = ' '; Title = ' '
      AttName = TITLE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  AttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( AttString )
      Title = AttString(1:MIN( LEN(Title), LEN_TRIM(AttString) ))
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      AttString = ' '; History = ' '
      AttName = HISTORY_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  AttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( AttString )
      History = AttString(1:MIN( LEN(History), LEN_TRIM(AttString) ))
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      AttString = ' '; Comment = ' '
      AttName = COMMENT_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(AttName), &
                                  AttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( AttString )
      Comment = AttString(1:MIN( LEN(Comment), LEN_TRIM(AttString) ))
    END IF

    
    ! Variable attributes
    ! -------------------
    ! The Source
    IF ( PRESENT(Source) ) THEN
      AttString = ' '; Source = ' '
      VarName = IRRADIANCE_VARNAME
      AttName = SOURCE_ATTNAME
      NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                    VarName, &
                                    VarID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  VarID, &
                                  TRIM(AttName), &
                                  AttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( AttString )
      Source = AttString( 1:MIN(LEN(Source), LEN_TRIM(AttString)) )
    END IF

    ! The References
    IF ( PRESENT(References) ) THEN
      AttString = ' '; References = ' '
      VarName = IRRADIANCE_VARNAME
      AttName = REFERENCES_ATTNAME
      NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                    VarName, &
                                    VarID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  VarID, &
                                  TRIM(AttName), &
                                  AttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( AttString )
      References = AttString( 1:MIN(LEN(References), LEN_TRIM(AttString)) )
    END IF

  CONTAINS
  
    SUBROUTINE ReadGAtts_CleanUp()
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM(VarName)//' '//TRIM(AttName)//&
                            ' attribute from '//TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadGAtts_CleanUp
    
  END FUNCTION ReadGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       DefineVar
!
! PURPOSE:
!       Function to define the Solar variables in an output netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineVar( NC_Filename            , &  ! Input
!                                 NC_FileID              , &  ! Input
!                                 n_Frequencies_DimID    , &  ! Input
!                                 RCS_Id     =RCS_Id     , &  ! Revision control
!                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:         Character string specifying the name of the
!                            already created netCDF Solar format file.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:           NetCDF file ID number of the file in which
!                            the variables are to be defned.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies_DimID: NetCDF dimension ID of the number of frequencies.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:         Character string specifying a filename in which any
!                            messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output messages to standard output.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  FUNCTION DefineVar( NC_Filename        , &  ! Input
                      NC_FileID          , &  ! Input
                      n_Frequencies_DimID, &  ! Input
                      RCS_Id             , &  ! Revision control
                      Message_Log        ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     ,           INTENT(IN)  :: n_Frequencies_DimID  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'DefineVar'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
    INTEGER :: varID
    INTEGER :: Put_Status(4)
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Scalar variables
    ! ----------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BEGIN_FREQUENCY_VARNAME, &
                                BEGIN_FREQUENCY_TYPE, &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//BEGIN_FREQUENCY_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  BEGIN_FREQUENCY_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  BEGIN_FREQUENCY_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  BEGIN_FREQUENCY_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  BEGIN_FREQUENCY_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//BEGIN_FREQUENCY_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF


    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                END_FREQUENCY_VARNAME, &
                                END_FREQUENCY_TYPE, &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//END_FREQUENCY_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  END_FREQUENCY_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  END_FREQUENCY_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  END_FREQUENCY_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  END_FREQUENCY_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//END_FREQUENCY_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF


    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                FREQUENCY_INTERVAL_VARNAME, &
                                FREQUENCY_INTERVAL_TYPE, &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//FREQUENCY_INTERVAL_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  FREQUENCY_INTERVAL_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  FREQUENCY_INTERVAL_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  FREQUENCY_INTERVAL_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  FREQUENCY_INTERVAL_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//FREQUENCY_INTERVAL_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF


    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BLACKBODY_TEMPERATURE_VARNAME, &
                                BLACKBODY_TEMPERATURE_TYPE, &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//BLACKBODY_TEMPERATURE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  BLACKBODY_TEMPERATURE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  BLACKBODY_TEMPERATURE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  BLACKBODY_TEMPERATURE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  BLACKBODY_TEMPERATURE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//BLACKBODY_TEMPERATURE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF


    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                RADIUS_VARNAME, &
                                RADIUS_TYPE, &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//RADIUS_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  RADIUS_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  RADIUS_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  RADIUS_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  RADIUS_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//RADIUS_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                EARTH_SUN_DISTANCE_VARNAME, &
                                EARTH_SUN_DISTANCE_TYPE, &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//EARTH_SUN_DISTANCE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  EARTH_SUN_DISTANCE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  EARTH_SUN_DISTANCE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  EARTH_SUN_DISTANCE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  EARTH_SUN_DISTANCE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//EARTH_SUN_DISTANCE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    
    ! Array variables
    ! ---------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                IRRADIANCE_VARNAME, &
                                IRRADIANCE_TYPE, &
                                dimIDs=(/n_Frequencies_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//IRRADIANCE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  IRRADIANCE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  IRRADIANCE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  IRRADIANCE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  IRRADIANCE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//IRRADIANCE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

 
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BLACKBODY_IRRADIANCE_VARNAME, &
                                BLACKBODY_IRRADIANCE_TYPE, &
                                dimIDs=(/n_Frequencies_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//BLACKBODY_IRRADIANCE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  BLACKBODY_IRRADIANCE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  BLACKBODY_IRRADIANCE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  BLACKBODY_IRRADIANCE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  BLACKBODY_IRRADIANCE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//BLACKBODY_IRRADIANCE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE DefineVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE DefineVar_CleanUp

  END FUNCTION DefineVar


!------------------------------------------------------------------------------
!
! NAME:
!       WriteVar
!
! PURPOSE:
!       Function to write the Solar variables in an output
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = WriteVar( NC_Filename            , &  ! Input
!                                NC_FileID              , &  ! Input
!                                Solar                  , &  ! Input
!                                RCS_Id     =RCS_Id     , &  ! Revision control
!                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF Solar format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be written.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Solar:              Structure containing the data to write to file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Solar_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
!------------------------------------------------------------------------------

  FUNCTION WriteVar( NC_Filename, &  ! Input
                     NC_FileID  , &  ! Input
                     Solar      , &  ! Input
                     RCS_Id     , &  ! Revision control
                     Message_Log) &  ! Error messaging
                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: NC_FileID
    TYPE(Solar_type)      , INTENT(IN)  :: Solar
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteVar'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Write the scalar data
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        Solar%Begin_Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//BEGIN_FREQUENCY_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        END_FREQUENCY_VARNAME, &
                                        Solar%End_Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//END_FREQUENCY_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_INTERVAL_VARNAME, &
                                        Solar%Frequency_Interval )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//FREQUENCY_INTERVAL_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BLACKBODY_TEMPERATURE_VARNAME, &
                                        Solar%Blackbody_Temperature )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//BLACKBODY_TEMPERATURE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        RADIUS_VARNAME, &
                                        Solar%Radius )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//RADIUS_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        EARTH_SUN_DISTANCE_VARNAME, &
                                        Solar%Earth_Sun_Distance )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//EARTH_SUN_DISTANCE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    
    ! Write the array data
    ! --------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        IRRADIANCE_VARNAME, &
                                        Solar%Irradiance )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//IRRADIANCE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BLACKBODY_IRRADIANCE_VARNAME, &
                                        Solar%Blackbody_Irradiance )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//BLACKBODY_IRRADIANCE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
  CONTAINS
  
    SUBROUTINE WriteVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE WriteVar_CleanUp

  END FUNCTION WriteVar


!------------------------------------------------------------------------------
!
! NAME:
!       ReadVar
!
! PURPOSE:
!       Function to read the Solar variables from any input
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = ReadVar( NC_Filename            , &  ! Input
!                               NC_FileID              , &  ! Input
!                               Solar                  , &  ! Output
!                               RCS_Id     =RCS_Id     , &  ! Revision control
!                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF Solar format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be written.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Solar:          Structure containing the data that was read
!                           from file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Solar_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
! COMMENTS:
!       The INTENT on the output Solar argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION ReadVar( NC_Filename, &  ! Input
                    NC_FileID  , &  ! Input
                    Solar      , &  ! Output
                    RCS_Id     , &  ! Revision control
                    Message_Log) &  ! Error messaging
                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: NC_Filename
    INTEGER               , INTENT(IN)     :: NC_FileID
    TYPE(Solar_type)      , INTENT(IN OUT) :: Solar
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadVar'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Write the scalar data
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        Solar%Begin_Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//BEGIN_FREQUENCY_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        END_FREQUENCY_VARNAME, &
                                        Solar%End_Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//END_FREQUENCY_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_INTERVAL_VARNAME, &
                                        Solar%Frequency_Interval )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//FREQUENCY_INTERVAL_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BLACKBODY_TEMPERATURE_VARNAME, &
                                        Solar%Blackbody_Temperature )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//BLACKBODY_TEMPERATURE_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        RADIUS_VARNAME, &
                                        Solar%Radius )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//RADIUS_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        EARTH_SUN_DISTANCE_VARNAME, &
                                        Solar%Earth_Sun_Distance )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//EARTH_SUN_DISTANCE_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    
    ! Write the array data
    ! --------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        IRRADIANCE_VARNAME, &
                                        Solar%Irradiance )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//IRRADIANCE_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BLACKBODY_IRRADIANCE_VARNAME, &
                                        Solar%Blackbody_Irradiance )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//BLACKBODY_IRRADIANCE_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE ReadVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadVar_CleanUp

  END FUNCTION ReadVar


!------------------------------------------------------------------------------
!
! NAME:
!       Create_Solar_netCDF
!
! PURPOSE:
!       Function to create a netCDF Solar data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = Create_Solar_netCDF( NC_Filename                 , &  ! Input
!                                           n_Frequencies               , &  ! Input
!                                           NC_FileID                   , &  ! Output
!                                           Release         =Release    , &  ! Optional input
!                                           Version         =Version    , &  ! Optional input
!                                           Title           =Title      , &  ! Optional input
!                                           History         =History    , &  ! Optional input
!                                           Comment         =Comment    , &  ! Optional input
!                                           Source          =Source     , &  ! Optional input
!                                           References      =References , &  ! Optional input
!                                           Message_Log     =Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF Solar format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies:      Number of spectral frequencies.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       NC_FileID:          NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Release:            The release number of the netCDF Solar file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:            The version number of the netCDF Solar file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF Solar file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF Solar file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF Solar file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Source:             Character string written into the SOURCE
!                           attribute field of the IRRADIANCE variable.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       References:         Character string written into the REFERENCES
!                           attribute field of the IRRADIANCE variable. 
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
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.  
!                           The error codes are defined in the Message_Handler module. 
!                           If == SUCCESS the netCDF file creation was successful.     
!                              == FAILURE an unrecoverable error occurred.             
!                              == WARNING - an error occurred writing any of the       
!                                           supplied global attributes.                
!                                         - an error occurred closing the netCDF file. 
!                           UNITS:      N/A                                            
!                           TYPE:       INTEGER                                        
!                           DIMENSION:  Scalar                                         
!
!------------------------------------------------------------------------------

  FUNCTION Create_Solar_netCDF( NC_Filename     , &  ! Input
                                n_Frequencies   , &  ! Input
                                NC_FileID       , &  ! Output
                                Release         , &  ! Optional input
                                Version         , &  ! Optional input
                                Title           , &  ! Optional input
                                History         , &  ! Optional input
                                Comment         , &  ! Optional input
                                Source          , &  ! Optional input
                                References      , &  ! Optional input
                                Message_Log     ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: n_Frequencies
    INTEGER               , INTENT(OUT) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN)  :: Release         
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Source
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: References
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_Solar_netCDF'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
    INTEGER :: n_Frequencies_DimID
    INTEGER :: VarID
    

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check input
    IF ( n_Frequencies < 1 ) THEN
      Message = 'Invalid dimension input detected.'
      CALL Create_Cleanup(); RETURN
    END IF


    ! Create the data file
    ! --------------------
    NF90_Status = NF90_CREATE( NC_Filename, &
                               NF90_CLOBBER, &
                               NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error creating '//TRIM(NC_Filename)//' - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the dimensions
    ! ---------------------
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                FREQUENCY_DIMNAME, &
                                n_Frequencies, &
                                n_Frequencies_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//FREQUENCY_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=SET); RETURN
    END IF


    ! Define the Solar variables
    ! --------------------------
    Error_Status = DefineVar( NC_Filename            , &
                              NC_FileID              , &
                              n_Frequencies_DimID    , &
                              Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error defining variables in '//TRIM(NC_Filename)
      CALL Create_Cleanup(); RETURN
    END IF

                                             
    ! Write the attributes
    ! --------------------
    Error_Status = WriteGAtts( NC_Filename            , &
                               NC_FileID              , &
                               Release    =Release    , &
                               Version    =Version    , &
                               Title      =Title      , &
                               History    =History    , &
                               Comment    =Comment    , &
                               Source     =Source     , &
                               References =References , &
                               Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing global attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error taking '//TRIM(NC_Filename)//' out of define mode.'
      CALL Create_Cleanup(Close_File=SET); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Create_CleanUp( Close_File )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          NF90_Status = NF90_CLOSE( NC_FileID )
          IF ( NF90_Status /= NF90_NOERR ) &
            Message = '; Error closing input file during error cleanup - '//&
                      TRIM(NF90_STRERROR( NF90_Status ) )
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Create_CleanUp

  END FUNCTION Create_Solar_netCDF





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
!       Inquire_Solar_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF format Solar source data file to obtain
!       the dimensions and attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_Solar_netCDF( NC_Filename                , &  ! Input
!                                            n_Frequencies=n_Frequencies, &  ! Optional output
!                                            Release      =Release      , &  ! Optional output
!                                            Version      =Version      , &  ! Optional output
!                                            Title        =Title        , &  ! Optional output
!                                            History      =History      , &  ! Optional output
!                                            Comment      =Comment      , &  ! Optional output
!                                            Source       =Source       , &  ! Optional output
!                                            References   =References   , &  ! Optional output
!                                            RCS_Id       =RCS_Id       , &  ! Version control
!                                            Message_Log  =Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the netCDF
!                         format Solar data file to inquire.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Frequencies:    The number of spectral points.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:          The release number of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:          The version number of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Source:           Character string written into the SOURCE
!                         attribute field of the IRRADIANCE variable.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       References:       Character string written into the REFERENCES
!                         attribute field of the IRRADIANCE variable. 
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the netCDF file inquiry was successful
!                            == FAILURE an error occurred reading any of the requested
!                                       dimension data.
!                            == WARNING - an error occurred reading any of the requested
!                                         global file attributes, or
!                                       - an error occurred closing the netCDF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_Solar_netCDF( NC_Filename  , &  ! Input
                                 n_Frequencies, &  ! Optional output
                                 Release      , &  ! Optional output
                                 Version      , &  ! Optional output
                                 Title        , &  ! Optional output
                                 History      , &  ! Optional output
                                 Comment      , &  ! Optional output
                                 Source       , &  ! Optional output
                                 References   , &  ! Optional output
                                 RCS_Id       , &  ! Version control
                                 Message_Log  ) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Source
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: References
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_Solar_netCDF'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: NF90_Status
    TYPE(Solar_type) :: Dummy  

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_Solar_netCDF( NC_Filename, &
                                      NC_FileID, &
                                      Mode='READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF Solar data file '//&
                TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the dimensions
    ! ------------------
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         FREQUENCY_DIMNAME, &
                                         Dummy%n_Frequencies, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//FREQUENCY_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Get the global attributes
    ! -------------------------
    Error_Status = ReadGAtts( NC_Filename            , &
                              NC_FileID              , &
                              Release    =Release    , &
                              Version    =Version    , &
                              Title      =Title      , &
                              History    =History    , &
                              Comment    =Comment    , &
                              Source     =Source     , &
                              References =References , &
                              Message_Log=Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_Solar_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Message = 'Error closing netCDF Solar data file '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    ! ---------------------
    ! Dimensions
    IF ( PRESENT(n_Frequencies) ) n_Frequencies = Dummy%n_Frequencies

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          Close_Status = Close_Solar_netCDF(NC_FileID)
          IF ( Close_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_Solar_netCDF




!------------------------------------------------------------------------------
!
! NAME:
!       Write_Solar_netCDF
!
! PURPOSE:
!       Function to write Solar data to a netCDF format Solar file.
!
! CALLING SEQUENCE:
!         Error_Status = Write_Solar_netCDF( NC_Filename            , &  ! Input
!                                            Solar                  , &  ! Input
!                                            Quiet      =Quiet      , &  ! Optional input
!                                            Title      =Title      , &  ! Optional input
!                                            History    =History    , &  ! Optional input
!                                            Comment    =Comment    , &  ! Optional input
!                                            Source     =Source     , &  ! Optional input
!                                            References =References , &  ! Optional input
!                                            RCS_Id     =RCS_Id     , &  ! Version control
!                                            Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the
!                        netCDF format Solar data file to create.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Solar:           Structure containing the solar data to write to file.
!                        UNITS:      N/A
!                        TYPE:       Solar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Source:          Character string written into the SOURCE
!                        attribute field of the IRRADIANCE variable.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       References:      Character string written into the REFERENCES
!                        attribute field of the IRRADIANCE variable. 
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file write was successful
!                           == FAILURE - the input Solar structure contains
!                                        unassociated pointer members, or
!                                      - a unrecoverable write error occurred.
!                           == WARNING an error occurred writing the global
!                                      attributes.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten
!
!------------------------------------------------------------------------------

  FUNCTION Write_Solar_netCDF( NC_Filename, &  ! Input
                               Solar      , &  ! Input
                               Quiet      , &  ! Optional input
                               Title      , &  ! Optional input
                               History    , &  ! Optional input
                               Comment    , &  ! Optional input
                               Source     , &  ! Optional input
                               References , &  ! Optional input
                               RCS_Id     , &  ! Version control
                               Message_Log) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(Solar_type),       INTENT(IN)  :: Solar
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Source
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: References
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_Solar_netCDF'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_Solar( Solar ) ) THEN
      Message = 'Some or all INPUT Solar pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Create the output data file
    ! ---------------------------
    Error_Status = Create_Solar_netCDF( NC_Filename              , &  ! Input
                                        Solar%n_Frequencies      , &  ! Input
                                        NC_FileID                , &  ! Output
                                        Release    =Solar%Release, &  ! Optional input
                                        Version    =Solar%Version, &  ! Optional input
                                        Title      =Title        , &  ! Optional input
                                        History    =History      , &  ! Optional input
                                        Comment    =Comment      , &  ! Optional input
                                        Source     =Source       , &  ! Optional input
                                        References =References   , &  ! Optional input
                                        Message_Log=Message_Log    )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the Solar data
    ! ------------------------
    Error_Status = WriteVar( NC_Filename, &
                             NC_FileID  , &
                             Solar      , &
                             Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Solar variables to output file '//TRIM(NC_Filename)
      CALL Write_Cleanup( Close_File=SET ); RETURN
    END IF
    

    ! Close the file
    ! --------------
    Close_Status = Close_Solar_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF Solar data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_Solar( Solar, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp( Close_File )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          Close_Status = Close_Solar_netCDF(NC_FileID)
          IF ( Close_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_Solar_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Read_Solar_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format Solar file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_Solar_netCDF( NC_Filename            , &  ! Input
!                                         Solar                  , &  ! Output
!                                         Quiet      =Quiet      , &  ! Optional input
!                                         Title      =Title      , &  ! Optional output
!                                         History    =History    , &  ! Optional output
!                                         Comment    =Comment    , &  ! Optional output
!                                         Source     =Source     , &  ! Optional output
!                                         References =References , &  ! Optional output
!                                         RCS_Id     =RCS_Id     , &  ! Version control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF Solar
!                        format Solar data file to read.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Solar:           Structure to contain the Solar data read
!                        from file.
!                        UNITS:      N/A
!                        TYPE:       Solar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Source:          Character string written into the SOURCE
!                        attribute field of the IRRADIANCE variable.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       References:      Character string written into the REFERENCES
!                        attribute field of the IRRADIANCE variable. 
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file read was successful
!                           == FAILURE a unrecoverable read error occurred.
!                           == WARNING an error occurred reading the global
!                                      attributes.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Solar argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_Solar_netCDF( NC_Filename, &  ! Input
                              Solar      , &  ! Output
                              Quiet      , &  ! Optional input
                              Title      , &  ! Optional output
                              History    , &  ! Optional output
                              Comment    , &  ! Optional output
                              Source     , &  ! Optional output
                              References , &  ! Optional output
                              RCS_Id     , &  ! Version control
                              Message_Log) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(Solar_type),       INTENT(IN OUT) :: Solar
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Source
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: References
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_Solar_netCDF'
    ! Function variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: n_Frequencies 

    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    
    ! Allocate the structure for the netCDF read
    ! ------------------------------------------
    ! Read the dimension values
    Error_Status = Inquire_Solar_netCDF( NC_Filename                , &
                                         n_Frequencies=n_Frequencies, &
                                         Message_Log  =Message_Log    ) 
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining Solar dimensions from '//TRIM(NC_Filename)
      CALL Read_Cleanup(); RETURN
    END IF

    ! Allocate the structure
    Error_Status = Allocate_Solar( n_Frequencies, &
                                   Solar        , &
                                   Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating Solar structure.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the netCDF file for reading
    ! --------------------------------
    Error_Status = Open_Solar_netCDF( NC_Filename, &
                                      NC_FileID, &
                                      Mode='READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF Solar data file '//TRIM(NC_Filename)
      CALL Read_Cleanup( Destroy_Structure=SET ); RETURN
    END IF


    ! Read the global attributes
    ! --------------------------
    Error_Status = ReadGAtts( NC_Filename              , &
                              NC_FileID                , &
                              Release    =Solar%Release, &
                              Version    =Solar%Version, &
                              Title      =Title        , & 
                              History    =History      , & 
                              Comment    =Comment      , & 
                              Source     =Source       , &
                              References =References   , &
                              Message_Log=Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF

    ! Check the release
    Error_Status = CheckRelease_Solar( Solar, &
                                       Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Solar Release check failed for '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF
    

    ! Read the Solar data
    ! -------------------
    Error_Status = ReadVar( NC_Filename, &
                            NC_FileID  , &
                            Solar      , &
                            Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Solar variables from '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF


    ! Compute the frequency grid
    ! --------------------------
    Error_Status = Frequency_Solar( Solar, &
                                    Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error computing Solar spectrum frequency grid'
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF
    
    
    ! Close the file
    ! --------------
    Close_Status = Close_Solar_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF Solar data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_Solar( Solar, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File, Destroy_Structure )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      INTEGER, OPTIONAL, INTENT(IN) :: Destroy_Structure
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          Close_Status = Close_Solar_netCDF(NC_FileID)
          IF ( Close_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Destroy the structure if necessary
      IF ( PRESENT(Destroy_Structure) ) THEN
        IF ( Destroy_Structure == SET ) THEN
          Destroy_Status = Destroy_Solar(Solar, Message_Log=Message_Log)
          IF ( Destroy_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error destroying Solar during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_Solar_netCDF

END MODULE Solar_netCDF_IO
