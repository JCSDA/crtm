!
! SRF_netCDF_IO
!
! Module containing routines to read and write netCDF format SRF files.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!
!       Modified by:    Yong Chen, CIRA/CSU/JCSDA 20-Aug-2008
!                       Yong.Chen@noaa.gov
 
MODULE SRF_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,        ONLY: fp
  USE Message_Handler,   ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE String_Utility,    ONLY: StrClean
  USE SRF_Define,        ONLY: N_SENSOR_TYPES    , &
                               INVALID_SENSOR    , &    
                               MICROWAVE_SENSOR  , &  
                               INFRARED_SENSOR   , &   
                               VISIBLE_SENSOR    , &   
                               ULTRAVIOLET_SENSOR, & 
                               SENSOR_TYPE_NAME  , &
                               SRF_type        , &
                               Associated_SRF  , &
                               Destroy_SRF     , &
                               Allocate_SRF    , &
                               CheckRelease_SRF, &
                               Info_SRF, &
                               Frequency_SRF, &
                               Integrate_SRF
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Create_SRF_netCDF
  PUBLIC :: Write_SRF_netCDF
  PUBLIC :: Inquire_SRF_netCDF
  PUBLIC :: Read_SRF_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Invalid flag
  INTEGER, PARAMETER :: INVALID = -1
  ! Keyword set values
  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER ::  ONE = 1.0_fp
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'history' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'comment' 
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  
  ! Dimension names. Case sensitive
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME = 'n_Channels'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_VARNAME      = 'Sensor_Type'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME   = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: INTEGRATED_SRF_VARNAME   = 'Integrated_SRF'
  CHARACTER(*), PARAMETER :: SUMMATION_SRF_VARNAME    = 'Summation_SRF'
 
  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_LONGNAME    = 'Sensor Type'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: INTEGRATED_SRF_LONGNAME = 'Integrated SRF value'
  CHARACTER(*), PARAMETER :: SUMMATION_SRF_LONGNAME  = 'Summed SRF value'
  CHARACTER(*), PARAMETER :: F1_BAND_LONGNAME        = 'Band Begin Frequency'
  CHARACTER(*), PARAMETER :: F2_BAND_LONGNAME        = 'Band End Frequency'
  CHARACTER(*), PARAMETER :: NPTS_BAND_LONGNAME      = 'Number of band spectral points'
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME      = 'Frequency'
  CHARACTER(*), PARAMETER :: RESPONSE_LONGNAME       = 'Relative Response'

  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'
  
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_DESCRIPTION    = 'Sensor type to identify uW, IR, VIS, UV, etc sensor channels'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: INTEGRATED_SRF_DESCRIPTION = 'SRF integral using Simpsons rule'
  CHARACTER(*), PARAMETER :: SUMMATION_SRF_DESCRIPTION  = 'SRF integral using SUM(response)*dF'
  CHARACTER(*), PARAMETER :: F1_BAND_DESCRIPTION        = 'Band Begin Frequency'
  CHARACTER(*), PARAMETER :: F2_BAND_DESCRIPTION        = 'Band End Frequency'
  CHARACTER(*), PARAMETER :: NPTS_BAND_DESCRIPTION      = 'Number of spectral points in a band'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION      = 'Spectral ordinate for channel responses'
  CHARACTER(*), PARAMETER :: RESPONSE_DESCRIPTION       = 'Relative Spectral Response Function (SRF)'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_UNITS    = 'N/A'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: NPTS_BAND_UNITS      = 'N/A' 
  CHARACTER(*), PARAMETER :: INTEGRAL_SRF_UNITS   = 'N/A'
!  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS      = Variable
  CHARACTER(*), PARAMETER :: RESPONSE_UNITS       = 'N/A'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER,  PARAMETER :: SENSOR_TYPE_FILLVALUE    = INVALID_SENSOR
  INTEGER,  PARAMETER :: SENSOR_CHANNEL_FILLVALUE = INVALID
  INTEGER,  PARAMETER :: NPTS_BAND_FILLVALUE      = INVALID
  REAL(fp), PARAMETER :: INTEGRAL_SRF_FILLVALUE   = ZERO
  REAL(fp), PARAMETER :: FREQUENCY_FILLVALUE      = -ONE
  REAL(fp), PARAMETER :: RESPONSE_FILLVALUE       = -ONE

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: SENSOR_TYPE_TYPE    = NF90_INT
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE = NF90_INT
  INTEGER, PARAMETER :: NPTS_BAND_TYPE      = NF90_INT
  INTEGER, PARAMETER :: INTEGRAL_SRF_TYPE   = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: RESPONSE_TYPE       = NF90_DOUBLE


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
!       CreateNames
!
! PURPOSE:
!       Subroutine to create channel-based dimension and variable names
!       for use in accessing netCDF format SRF datafiles.
!
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CreateNames( Channel                            , &  ! Input
!                         Channel_Name     =Channel_Name     , &  ! Output
!                         Point_DimName    =Point_DimName    , &  ! Output
!                         Band_DimName     =Band_DimName     , &  ! Output
!                         Response_VarName =Response_VarName , &  ! Output
!                         f1_Band_VarName  =f1_Band_VarName  , &  ! Output
!                         f2_Band_VarName  =f2_Band_VarName  , &  ! Output
!                         npts_Band_VarName=npts_Band_VarName  )  ! Output
!
! INPUT ARGUMENTS:
!       Channel:           The channel number for which the names are required.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!      Channel_Name:       Character string containing the channel name.
!                          For example, if the input channel number is 124,
!                          the channel name is '124'.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Point_DimName:     Character string containing the dimension name for 
!                          the number of SRF points for the channel.
!                          For example, if the input channel number is 124,
!                          the dimension name is 'channel_124_n_points'.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Band_DimName:      Character string containing the dimension name for 
!                          the number of SRF bands for the channel.
!                          For example, if the input channel number is 11,
!                          the dimension name is 'channel_11_n_bands'.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Response_VarName:  Character string containing the channel-based
!                          SRF response variable name for the requested channel.
!                          For example, if the input channel number is 2124,
!                          the variable name is 'channel_2124_response'
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f1_Band_VarName:   Character string containing the channel-based
!                          SRF band begin frequency variable name for the
!                          requested channel. For example, if the input channel
!                          number is 20, the variable name is 'channel_20_f1_band'
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f2_Band_VarName:   Character string containing the channel-based
!                          SRF band end frequency variable name for the
!                          requested channel. For example, if the input channel
!                          number is 20, the variable name is 'channel_20_f2_band'
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       npts_Band_VarName: Character string containing the channel-based
!                          SRF band point number variable name for the
!                          requested channel. For example, if the input channel
!                          number is 20, the variable name is 'channel_20_npts_band'
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  SUBROUTINE CreateNames( Channel          , &  ! Input
                          Channel_Name     , &  ! Output
                          Point_DimName    , &  ! Output
                          Band_DimName     , &  ! Output
                          Response_VarName , &  ! Output 
                          f1_Band_VarName  , &  ! Output
                          f2_Band_VarName  , &  ! Output
                          npts_Band_VarName  )  ! Output
    ! Arguments
    INTEGER,      INTENT(IN)  :: Channel
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Channel_Name    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Point_DimName    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Band_DimName     
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Response_VarName 
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f1_Band_VarName  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f2_Band_VarName  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: npts_Band_VarName
    ! Local variables
    CHARACTER(256) :: c

    ! Create the channel string
    ! -------------------------
    WRITE( c,'(i0)' ) Channel
    c = ADJUSTL(c)
    
    ! Construct all the other names
    ! -----------------------------
    IF ( PRESENT(Channel_Name     ) ) Channel_Name      = c
    IF ( PRESENT(Point_DimName    ) ) Point_DimName     = 'channel_'//TRIM(c)//'_n_points'
    IF ( PRESENT(Band_DimName     ) ) Band_DimName      = 'channel_'//TRIM(c)//'_n_bands'
    IF ( PRESENT(Response_VarName ) ) Response_VarName  = 'channel_'//TRIM(c)//'_response'
    IF ( PRESENT(f1_Band_VarName  ) ) f1_Band_VarName   = 'channel_'//TRIM(c)//'_f1_band'
    IF ( PRESENT(f2_Band_VarName  ) ) f2_Band_VarName   = 'channel_'//TRIM(c)//'_f2_band'
    IF ( PRESENT(npts_Band_VarName) ) npts_Band_VarName = 'channel_'//TRIM(c)//'_npts_band'

  END SUBROUTINE CreateNames


!------------------------------------------------------------------------------
!
! NAME:
!       WriteGAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF SRF data file.
!
! CALLING SEQUENCE:
!       Error_Status = WriteGAtts( NC_Filename                      , &  ! Input
!                                  NC_FileId                        , &  ! Input
!                                  Version         =Version         , &  ! Optional input
!                                  Sensor_Id       =Sensor_Id       , &  ! Optional input
!                                  WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
!                                  Title           =Title           , &  ! Optional input
!                                  History         =History         , &  ! Optional input
!                                  Comment         =Comment         , &  ! Optional input
!                                  Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF SRF format data file to write to.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileId:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Version:          The version number of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Id:        Character string sensor/platform identifier.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_Id: The WMO code used to identify satellite platforms.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_Id:    The WMO code used to identify sensors.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF SRF file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF SRF file.
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
!                                       global attributes.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF file is closed.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION WriteGAtts( NC_Filename     , &  ! Input
                       NC_FileId       , &  ! Input
                       Version         , &  ! Optional input
                       Sensor_Id       , &  ! Optional input
                       WMO_Satellite_Id, &  ! Optional input
                       WMO_Sensor_Id   , &  ! Optional input
                       Title           , &  ! Optional input
                       History         , &  ! Optional input
                       Comment         , &  ! Optional input
                       Message_Log     ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileId
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteGAtts'
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
    TYPE(SRF_type) :: SRF_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS
    msg = ' '


    ! Mandatory global attributes
    ! ---------------------------
    ! Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                MODULE_RCS_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    
    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                SRF_Default%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! --------------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = SRF_Default%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  History )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileId )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(msg), &
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
!       Function to read the global attributes from a netCDF SRF
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = ReadGAtts( NC_Filename                      , &  ! Input
!                                 NC_FileId                        , &  ! Input
!                                 Release         =Release         , &  ! Optional output
!                                 Version         =Version         , &  ! Optional output
!                                 Sensor_Id       =Sensor_Id       , &  ! Optional output
!                                 WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional output
!                                 WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional output
!                                 Title           =Title           , &  ! Optional output
!                                 History         =History         , &  ! Optional output
!                                 Comment         =Comment         , &  ! Optional output
!                                 Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF SRF format data file to read from.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileId:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
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
!       Release:          The release number of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:        Character string sensor/platform identifier.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id: The WMO code used to identify satellite platforms.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:    The WMO code used to identify sensors.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF SRF file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute read was successful.
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION ReadGAtts( NC_Filename     , &  ! Input
                      NC_FileId       , &  ! Input
                      Release         , &  ! Optional output
                      Version         , &  ! Optional output
                      Sensor_Id       , &  ! Optional output
                      WMO_Satellite_Id, &  ! Optional output
                      WMO_Sensor_Id   , &  ! Optional output
                      Title           , &  ! Optional output
                      History         , &  ! Optional output
                      Comment         , &  ! Optional output
                      Message_Log     ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: NC_FileId
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadGAtts'
    ! Local variables
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: Rel
    INTEGER :: NF90_Status
    TYPE(SRF_type) :: SRF_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! The mandatory GAtts for checking
    ! --------------------------------
    ! The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_GET_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Rel )
    IF ( NF90_Status /= NF90_NOERR .OR. Rel /= SRF_Default%Release) THEN
      CALL ReadGAtts_Cleanup(); RETURN
    END IF
    IF ( PRESENT(Release) ) Release = SRF_Default%Release


    ! The optional GAtts
    ! ------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttString = ' '; Sensor_Id = ' '
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Sensor_Id = GAttString(1:MIN( LEN(Sensor_Id), LEN_TRIM(GAttString) ))
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttString = ' '; Title = ' '
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Title = GAttString(1:MIN( LEN(Title), LEN_TRIM(GAttString) ))
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttString = ' '; History = ' '
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      History = GAttString(1:MIN( LEN(History), LEN_TRIM(GAttString) ))
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttString = ' '; Comment = ' '
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Comment = GAttString(1:MIN( LEN(Comment), LEN_TRIM(GAttString) ))
    END IF

  CONTAINS
  
    SUBROUTINE ReadGAtts_CleanUp()
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM(GAttName)//&
                            ' attribute from '//TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts


!--------------------------------------------------------------------------------
!
! NAME:
!       ReadDim
!
! PURPOSE:
!       Function to retrieve a netCDF file dimension by name.
!
! CALLING SEQUENCE:
!       Error_Status = ReadDim( NC_FileId              , & ! Input
!                               DimName                , & ! Input
!                               DimValue               , & ! Output
!                               DimID      =DimID      , & ! Optional Output
!                               Message_Log=Message_Log  ) ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_FileId:       NetCDF file ID number.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       DimName:         Name of the netCDF dimension to retrieve.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       DimValue:        Value of the requested dimension.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to the screen.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       DimID:           Id of the requested dimension.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!                          
! FUNCTION RESULT
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF dimension retrieval was successful
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION ReadDim( NC_FileId   , & ! Input
                    DimName     , & ! Input
                    DimValue    , & ! Output
                    DimID       , & ! Optional Output
                    Message_Log ) & ! Error messaging
                  RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)  :: NC_FileId
    CHARACTER(*),           INTENT(IN)  :: DimName 
    INTEGER,                INTENT(OUT) :: DimValue
    INTEGER     , OPTIONAL, INTENT(OUT) :: DimID   
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadDim'
    ! Local variables
    INTEGER :: NF90_Status
    INTEGER :: id
    
    ! Setup
    Error_Status = SUCCESS
    NF90_Status = NF90_NOERR


    ! Get the dimension id
    NF90_Status = NF90_INQ_DIMID( NC_FileId,TRIM(DimName),id )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring dimension ID for '//TRIM(DimName)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status )), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Get the dimension value
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,id,Len=DimValue )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading dimension value for '//TRIM(DimName)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status )), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign return arguments
    IF ( PRESENT(DimId) ) Dimid = id

  END FUNCTION ReadDim


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
!       Create_SRF_netCDF
!
! PURPOSE:
!       Function to create a netCDF SRF data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = Create_SRF_netCDF( NC_Filename                      , &  ! Input
!                                         Sensor_Type                      , &  ! Input
!                                         Sensor_Channel                   , &  ! Input
!                                         Version         =Version         , &  ! Optional input
!                                         Sensor_Id       =Sensor_Id       , &  ! Optional input
!                                         WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
!                                         WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
!                                         Title           =Title           , &  ! Optional input
!                                         History         =History         , &  ! Optional input
!                                         Comment         =Comment         , &  ! Optional input
!                                         RCS_Id          =RCS_Id,           &  ! Revision control
!                                         Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF SRF format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Sensor_Channel:     The list of channel numbers to be written
!                           to the SRF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Version:            The version number of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF SRF file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF SRF file.
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
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the SRF netCDF file creation was successful
!                              == FAILURE an unrecoverable error occurred 
!                              == WARNING - an error occurred writing any of the
!                                           supplied global attributes.
!                                         - an error occurred closing the netCDF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Create_SRF_netCDF( NC_Filename     , &  ! Input
                              Sensor_Type     , &  ! Input
                              Sensor_Channel  , &  ! Input
                              Version         , &  ! Optional input
                              Sensor_Id       , &  ! Optional input
                              WMO_Satellite_Id, &  ! Optional input
                              WMO_Sensor_Id   , &  ! Optional input
                              Title           , &  ! Optional input
                              History         , &  ! Optional input
                              Comment         , &  ! Optional input
                              RCS_Id          , &  ! Revision control
                              Message_Log     ) &  ! Error messaging
                            RESULT( Error_Status )

    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: Sensor_Type
    INTEGER     ,           INTENT(IN)  :: Sensor_Channel(:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_ID
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_ID
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), INTENT(OUT), OPTIONAL :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_SRF_netCDF'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NC_FileId
    INTEGER :: NF90_Status(4)
    INTEGER :: n_Channels, DimId
    INTEGER :: Sensor_Type_VarId, Sensor_Channel_VarId, VarID
 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
  
    ! Check the sensor type
    IF ( Sensor_Type < 1 .OR. Sensor_Type > N_SENSOR_TYPES ) THEN
      msg = 'Invalid SENSOR_TYPE input.'
      CALL Create_Cleanup(); RETURN
    END IF
    
    ! Check channel input
    n_Channels = SIZE(Sensor_Channel)
    IF ( n_Channels < 1 ) THEN
      msg = 'SENSOR_CHANNEL array must be non-zero size.'
      CALL Create_Cleanup(); RETURN
    END IF
    IF ( ANY(Sensor_Channel < 1) ) THEN
      msg = 'Invalid SENSOR_CHANNEL value found.'
      CALL Create_Cleanup(); RETURN
    END IF


    ! Create the data file
    ! --------------------
    NF90_Status(1) = NF90_CREATE( NC_Filename,NF90_CLOBBER,NC_FileId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the dimensions
    ! ----------------------
    ! The number of channels
    NF90_Status(1) = NF90_DEF_DIM( NC_FileId,CHANNEL_DIMNAME,n_Channels,DimId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining the '//CHANNEL_DIMNAME//' dimension in '// &
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the global attributes
    ! ---------------------------
    Error_Status = WriteGAtts( NC_Filename                      , &
                               NC_FileId                        , &
                               Version         =Version         , &
                               Sensor_Id       =Sensor_Id       , &
                               WMO_Satellite_Id=WMO_Satellite_Id, &
                               WMO_Sensor_Id   =WMO_Sensor_Id   , &
                               Title           =Title           , &
                               History         =History         , &
                               Comment         =Comment         , &
                               Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error writing global attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! Define the sensor type scalar variable
    ! --------------------------------------
    NF90_Status(1) = NF90_DEF_VAR( NC_FileId,SENSOR_TYPE_VARNAME,SENSOR_TYPE_TYPE,varID=Sensor_Type_VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_TYPE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileId,Sensor_Type_VarID,LONGNAME_ATTNAME,SENSOR_TYPE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileId,Sensor_Type_VarID,DESCRIPTION_ATTNAME,SENSOR_TYPE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileId,Sensor_Type_VarID,UNITS_ATTNAME,SENSOR_TYPE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileId,Sensor_Type_VarID,FILLVALUE_ATTNAME,SENSOR_TYPE_FILLVALUE )
    IF ( ANY(NF90_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SENSOR_TYPE_VARNAME//' variable attributes to '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    ! Define the channel-dimensioned variables and attributes
    ! -------------------------------------------------------
    ! The Sensor_Channel
    NF90_Status(1) = NF90_DEF_VAR( NC_FileId,SENSOR_CHANNEL_VARNAME,SENSOR_CHANNEL_TYPE,&
                                   dimIDs=DimId,varID=Sensor_Channel_VarId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileId,Sensor_Channel_VarID,LONGNAME_ATTNAME,SENSOR_CHANNEL_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileId,Sensor_Channel_VarID,DESCRIPTION_ATTNAME,SENSOR_CHANNEL_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileId,Sensor_Channel_VarID,UNITS_ATTNAME,SENSOR_CHANNEL_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileId,Sensor_Channel_VarID,FILLVALUE_ATTNAME,SENSOR_CHANNEL_FILLVALUE )
    IF ( ANY(NF90_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' variable attributes to '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The Integrated_SRF
    NF90_Status(1) = NF90_DEF_VAR( NC_FileId,INTEGRATED_SRF_VARNAME,INTEGRAL_SRF_TYPE,&
                                   dimIDs=DimId,varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//INTEGRATED_SRF_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileId,VarID,LONGNAME_ATTNAME,INTEGRATED_SRF_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileId,VarID,DESCRIPTION_ATTNAME,INTEGRATED_SRF_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileId,VarID,UNITS_ATTNAME,INTEGRAL_SRF_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileId,VarID,FILLVALUE_ATTNAME,INTEGRAL_SRF_FILLVALUE )
    IF ( ANY(NF90_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//INTEGRATED_SRF_VARNAME//' variable attributes to '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The Summation_SRF
    NF90_Status(1) = NF90_DEF_VAR( NC_FileId,SUMMATION_SRF_VARNAME,INTEGRAL_SRF_TYPE,&
                                   dimIDs=DimId,varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//SUMMATION_SRF_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileId,VarID,LONGNAME_ATTNAME,SUMMATION_SRF_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileId,VarID,DESCRIPTION_ATTNAME,SUMMATION_SRF_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileId,VarID,UNITS_ATTNAME,INTEGRAL_SRF_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileId,VarID,FILLVALUE_ATTNAME,INTEGRAL_SRF_FILLVALUE )
    IF ( ANY(NF90_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SUMMATION_SRF_VARNAME//' variable attributes to '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status(1) = NF90_ENDDEF( NC_FileId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg =  'Error taking file '//TRIM(NC_Filename)// &
             ' out of define mode - '//TRIM(NF90_STRERROR( NF90_Status(1) )) 
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the defining data
    ! -----------------------
    ! The sensor type
    NF90_Status(1) = NF90_PUT_VAR( NC_FileId,Sensor_Type_VarID,Sensor_Type )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg =  'Error writing '//SENSOR_TYPE_VARNAME//' to '//TRIM(NC_Filename)// &
             ' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The sensor channel list
    NF90_Status(1) = NF90_PUT_VAR( NC_FileId,Sensor_Channel_VarID,Sensor_Channel )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg =  'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(NC_Filename)// &
             ' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Close the file
    ! --------------
    NF90_Status(1) = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Create_CleanUp(Close_File)
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status(1) = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status(1) /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status(1) ))
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Create_CleanUp

  END FUNCTION Create_SRF_netCDF





!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Inquire_SRF_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF SRF format file to obtain the dimensions,
!       channel list, sensor IDs, and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_SRF_netCDF( NC_Filename                        , &  ! Input
!                                          n_Channels       = n_Channels      , &  ! Optional output
!                                          n_Points         = n_Points        , &  ! Optional output
!                                          n_Bands          = n_Bands         , &  ! Optional output
!                                          Sensor_Type      = Sensor_Type     , &  ! Optional output
!                                          Sensor_Channel   = Sensor_Channel  , &  ! Optional output
!                                          Begin_Frequency  = Begin_Frequency , &  ! Optional output
!                                          End_Frequency    = End_Frequency   , &  ! Optional output
!                                          Version          = Version         , &  ! Optional output
!                                          Sensor_ID        = Sensor_ID       , &  ! Optional output
!                                          WMO_Satellite_ID = WMO_Satellite_ID, &  ! Optional output
!                                          WMO_Sensor_ID    = WMO_Sensor_ID   , &  ! Optional output
!                                          Title            = Title           , &  ! Optional output
!                                          History          = History         , &  ! Optional output
!                                          Comment          = Comment         , &  ! Optional output
!                                          RCS_Id           = RCS_Id          , &  ! Revision control
!                                          Message_Log      = Message_Log )     )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           SRF netCDF format data file to inquire.
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
!       n_Channels:         The number of channels dimension of the
!                           SRF data data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Points:           The number of spectral points used to represent the
!                           SRF for each channel.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Bands:            The number of bands used to represent the
!                           SRF for each channel.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Channel:     The list of channel numbers present in the netCDF
!                           SRF file. The list may not necessarily
!                           start at 1 or contain contiguous values.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Begin_Frequency:    The list of the begin frequency limits for
!                           each channel's SRF.
!                           UNITS:      Inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       End_Frequency:      The list of the end frequency limits for
!                           each channel's SRF.
!                           UNITS:      Inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_ID:          A character string identifying the sensor and
!                           satellite platform used to contruct filenames.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_ID:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_ID:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF SRF file.
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
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the netCDF file inquiry was successful
!                              == FAILURE - an error occurred opening the netCDF file, or
!                                         - an error occurred reading any of the
!                                           requested dimension or variable data.
!                              == WARNING - an error occurred reading any of the
!                                           requested global attributes, or
!                                         - an error occurred closing the netCDF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! RESTRICTIONS:
!       To successfully return any of the channel dimensioned arrays, the
!       dummy arguments must have at least same size as the dataset in the
!       netCDF file.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Inquire_SRF_netCDF( NC_Filename     , &  ! Input
                               n_Channels      , &  ! Optional output
                               n_Points        , &  ! Optional output
                               n_Bands         , &  ! Optional output
                               Sensor_Type     , &  ! Optional output
                               Sensor_Channel  , &  ! Optional output
                               Begin_Frequency , &  ! Optional output
                               End_Frequency   , &  ! Optional output
                               Version         , &  ! Optional output
                               Sensor_ID       , &  ! Optional output
                               WMO_Satellite_ID, &  ! Optional output
                               WMO_Sensor_ID   , &  ! Optional output
                               Title           , &  ! Optional output
                               History         , &  ! Optional output
                               Comment         , &  ! Optional output
                               RCS_Id          , &  ! Revision control
                               Message_Log     ) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: NC_Filename
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Points(:)
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Bands(:)
    INTEGER,       OPTIONAL, INTENT(OUT) :: Sensor_Type
    INTEGER,       OPTIONAL, INTENT(OUT) :: Sensor_Channel(:)
    REAL(fp),      OPTIONAL, INTENT(OUT) :: Begin_Frequency(:)
    REAL(fp),      OPTIONAL, INTENT(OUT) :: End_Frequency(:)
    INTEGER,       OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Sensor_ID   
    INTEGER,       OPTIONAL, INTENT(OUT) :: WMO_Satellite_ID 
    INTEGER,       OPTIONAL, INTENT(OUT) :: WMO_Sensor_ID
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_SRF_netCDF'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NC_FileId
    INTEGER :: NF90_Status
    INTEGER :: Allocate_Status
    INTEGER :: VarId
    INTEGER :: i, n
    INTEGER,  ALLOCATABLE :: Local_n_Points(:)
    INTEGER,  ALLOCATABLE :: Local_n_Bands(:)
    INTEGER,  ALLOCATABLE :: Local_Sensor_Channel(:)
    REAL(fp), ALLOCATABLE :: Local_Begin_Frequency(:)
    REAL(fp), ALLOCATABLE :: Local_End_Frequency(:)
    REAL(fp), ALLOCATABLE :: f_Band(:)
    CHARACTER(256) :: Point_DimName  
    CHARACTER(256) :: Band_DimName   
    CHARACTER(256) :: f1_Band_VarName
    CHARACTER(256) :: f2_Band_VarName

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) )  RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    NF90_Status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(NC_Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the number of channels dimension
    ! ------------------------------------
    Error_Status = ReadDim( NC_FileId,CHANNEL_DIMNAME,n,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error obtaining '//CHANNEL_DIMNAME//&
            ' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Set the dimension return value
    IF ( PRESENT(n_Channels) ) n_Channels = n


    ! Allocate the local arrays
    ! -------------------------
    ALLOCATE( Local_n_Points(n), &
              Local_n_Bands(n), &
              Local_Sensor_Channel(n), &
              Local_Begin_Frequency(n), &
              Local_End_Frequency(n), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( msg,'("Error allocating local data arrays. STAT = ",i0)' ) Allocate_Status
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Get the sensor type if necessary
    ! --------------------------------
    IF ( PRESENT(Sensor_Type) ) THEN
      ! Get the variable Id
      NF90_Status = NF90_INQ_VARID( NC_FileId,SENSOR_TYPE_VARNAME,VarId )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_TYPE_VARNAME//&
              ' variable id - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
      END IF
      ! Get the data
      NF90_Status = NF90_GET_VAR( NC_FileId,VarId,Sensor_Type )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//SENSOR_TYPE_VARNAME//' data from '//TRIM(NC_Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END IF
    
    ! Get the sensor channel data if necessary
    ! ----------------------------------------
    ! Get the variable Id                                                                  
    NF90_Status = NF90_INQ_VARID( NC_FileId,SENSOR_CHANNEL_VARNAME,VarId )                 
    IF ( NF90_Status /= NF90_NOERR ) THEN                                                  
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_CHANNEL_VARNAME//&      
            ' variable id - '//TRIM(NF90_STRERROR( NF90_Status ))                          
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN                                      
    END IF                                                                                 
    ! Get the data                                                                         
    NF90_Status = NF90_GET_VAR( NC_FileId,VarId,Local_Sensor_Channel )                     
    IF ( NF90_Status /= NF90_NOERR ) THEN                                                  
      msg = 'Error reading '//SENSOR_CHANNEL_VARNAME//' data from '//TRIM(NC_Filename)//&  
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))                                      
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN                                      
    END IF                                                                                 
    IF ( PRESENT(Sensor_Channel) ) THEN
      ! Set the return value
      IF ( SIZE(Sensor_Channel) < n ) THEN
        msg = 'Sensor_Channel array too small to hold data.'
        CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
      END IF
      Sensor_Channel = SENSOR_CHANNEL_FILLVALUE
      Sensor_Channel(1:n) = Local_Sensor_Channel
    END IF


    ! Get the channel specific data if necessary. Note that the assumption
    ! here is that if any of these dimensions or variables are defined,
    ! then they are all defined.
    ! --------------------------------------------------------------------
    IF ( PRESENT(n_Points       ) .OR. &
         PRESENT(n_Bands        ) .OR. &
         PRESENT(Begin_Frequency) .OR. &
         PRESENT(End_Frequency  )      ) THEN
         
      ! Loop over channels
      ! ------------------
      Channel_Loop: DO i = 1, n
    
        ! Create the various dim and var names for this channel
        ! -----------------------------------------------------
        CALL CreateNames( Local_Sensor_Channel(i), &
                          Point_DimName  =Point_DimName  , &
                          Band_DimName   =Band_DimName   , &
                          f1_Band_VarName=f1_Band_VarName, &
                          f2_Band_VarName=f2_Band_VarName  )
                          
        ! Retrieve dimension values
        ! -------------------------                        
        ! Retrieve the number of points dimension value
        Error_Status = ReadDim( NC_FileId,Point_DimName,Local_n_Points(i),Message_Log=Message_Log )
        IF ( Error_Status /= SUCCESS ) THEN
          msg = 'Error obtaining '//TRIM(Point_DimName)//' dimension from '//TRIM(NC_Filename)
          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
        END IF
        ! Retrieve the number of bands dimension value
        Error_Status = ReadDim( NC_FileId,Band_DimName,Local_n_Bands(i),Message_Log=Message_Log )
        IF ( Error_Status /= SUCCESS ) THEN
          msg = 'Error obtaining '//TRIM(Band_DimName)//' dimension from '//TRIM(NC_Filename)
          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
        END IF

        ! Allocate a band-specific array for this channel
        ! -----------------------------------------------
        ALLOCATE( f_Band(Local_n_Bands(i)),STAT=Error_Status )
        IF ( Error_Status /= 0 ) THEN
          WRITE( msg,'("Error allocating band frequency array for channel ",i0,". STAT = ",i0)' ) &
                     Local_Sensor_Channel(i), Error_Status
          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
        END IF
        
        ! Retrieve the band begin frequency values
        ! ----------------------------------------
        ! Get the variable id
        NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(f1_Band_Varname),VarId )
        IF ( NF90_Status /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f1_Band_Varname)//&
                ' variable id - '//TRIM(NF90_STRERROR( NF90_Status ))
          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
        END IF
        ! Get the data
        NF90_Status = NF90_GET_VAR( NC_FileId,VarId,f_Band )
        IF ( NF90_Status /= NF90_NOERR ) THEN
          msg = 'Error reading '//TRIM(f1_Band_Varname)//' data from '//TRIM(NC_Filename)//&
                ' - '//TRIM(NF90_STRERROR( NF90_Status ))
          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
        END IF
        ! Save the SRF begin frequency
        Local_Begin_Frequency(i) = f_Band(1)
        
        ! Retrieve the band end frequency values
        ! ----------------------------------------
        ! Get the variable id
        NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(f2_Band_Varname),VarId )
        IF ( NF90_Status /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f2_Band_Varname)//&
                ' variable id - '//TRIM(NF90_STRERROR( NF90_Status ))
          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
        END IF
        ! Get the data
        NF90_Status = NF90_GET_VAR( NC_FileId,VarId,f_Band )
        IF ( NF90_Status /= NF90_NOERR ) THEN
          msg = 'Error reading '//TRIM(f2_Band_Varname)//' data from '//TRIM(NC_Filename)//&
                ' - '//TRIM(NF90_STRERROR( NF90_Status ))
          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
        END IF
        ! Save the SRF end frequency
        Local_End_Frequency(i) = f_Band(Local_n_Bands(i))
        
        ! Deallocate band specific array for this channel
        ! -----------------------------------------------
        DEALLOCATE( f_Band,STAT=Allocate_Status )
        IF ( Allocate_Status /= 0 ) THEN
          WRITE( msg,'("Error deallocating band frequency array for channel ",i0,". STAT = ",i0)' ) &
                     Local_Sensor_Channel(i), Allocate_Status
          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
        END IF
        
      END DO Channel_Loop

      ! Set the optional return arguments
      ! ---------------------------------
      ! Set the n_Points return value
      IF ( PRESENT(n_Points) ) THEN
        IF ( SIZE(n_Points) < n ) THEN
          msg = 'n_Points array too small to hold data.'
          CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
        END IF
        n_Points = NPTS_BAND_FILLVALUE
        n_Points(1:n) = Local_n_Points
      END IF

      ! Set the n_Bands return value
      IF ( PRESENT(n_Bands) ) THEN
        IF ( SIZE(n_Bands) < n ) THEN
          msg = 'n_Bands array too small to hold data.'
          CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
        END IF
        n_Bands = NPTS_BAND_FILLVALUE
        n_Bands(1:n) = Local_n_Bands
      END IF

      ! Set the Begin_Frequency return value
      IF ( PRESENT(Begin_Frequency) ) THEN
        IF ( SIZE(Begin_Frequency) < n ) THEN
          msg = 'Begin_Frequency array too small to hold data.'
          CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
        END IF
        Begin_Frequency = FREQUENCY_FILLVALUE
        Begin_Frequency(1:n) = Local_Begin_Frequency
      END IF

      ! Set the End_Frequency return value
      IF ( PRESENT(End_Frequency) ) THEN
        IF ( SIZE(End_Frequency) < n ) THEN
          msg = 'End_Frequency array too small to hold data.'
          CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
        END IF
        End_Frequency = FREQUENCY_FILLVALUE
        End_Frequency(1:n) = Local_End_Frequency
      END IF

    END IF
    

    ! Read the global attributes
    ! --------------------------
    Error_Status = ReadGAtts( NC_Filename, &
                              NC_FileId, &
                              Version         =Version         , &
                              Sensor_Id       =Sensor_Id       , &
                              WMO_Satellite_Id=WMO_Satellite_Id, &
                              WMO_Sensor_Id   =WMO_Sensor_Id   , &
                              Title           =Title           , &
                              History         =History         , &
                              Comment         =Comment         , &
                              Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Close the file
    ! --------------
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Deallocate all the local channel dimensioned arrays
    ! ---------------------------------------------------
    DEALLOCATE( Local_n_Points       , &
                Local_n_Bands        , &
                Local_Sensor_Channel , &
                Local_Begin_Frequency, &
                Local_End_Frequency  )

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Deallocate local arrays if necessary
      IF ( ALLOCATED(Local_n_Points       ) ) DEALLOCATE(Local_n_Points       )
      IF ( ALLOCATED(Local_n_Bands        ) ) DEALLOCATE(Local_n_Bands        )
      IF ( ALLOCATED(Local_Sensor_Channel ) ) DEALLOCATE(Local_Sensor_Channel )
      IF ( ALLOCATED(Local_Begin_Frequency) ) DEALLOCATE(Local_Begin_Frequency)
      IF ( ALLOCATED(Local_End_Frequency  ) ) DEALLOCATE(Local_End_Frequency  )
      IF ( ALLOCATED(f_Band               ) ) DEALLOCATE(f_Band               )
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_SRF_netCDF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Write_SRF_netCDF
!
! PURPOSE:
!       Function to write data in an SRF structure to a netCDF format
!       SRF file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_SRF_netCDF( NC_Filename            , &  ! Input
!                                        SRF                    , &  ! Input
!                                        RCS_Id     =RCS_Id     , &  !  Revision control
!                                        Message_Log=Message_Log  )  !  Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF
!                        format SRF data file to write to.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SRF:             Structure containing the SRF data to write to file.
!                        UNITS:      N/A
!                        TYPE:       TYPE(SRF_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
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
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error
!                        status. The error codes are defined in the
!                        Message_Handler module.
!                        If == SUCCESS the netCDF data write was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Write_SRF_netCDF( NC_Filename , &  ! Input
                             SRF         , &  ! Input
                             Quiet       , &  ! Optional input
                             RCS_Id      , &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(SRF_type),         INTENT(IN)  :: SRF
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SRF_netCDF'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: NC_FileId
    INTEGER :: NF90_Status(4)
    INTEGER :: Allocate_Status
    INTEGER :: i, n
    INTEGER :: n_Channels
    INTEGER :: Channel_Idx(1)
    INTEGER, ALLOCATABLE :: Sensor_Channel(:)
    INTEGER :: Sensor_Type
    CHARACTER(256) :: Frequency_Units
    CHARACTER(256) :: Channel_Name     
    CHARACTER(256) :: Point_DimName    
    CHARACTER(256) :: Band_DimName     
    CHARACTER(256) :: Response_VarName 
    CHARACTER(256) :: f1_Band_VarName  
    CHARACTER(256) :: f2_Band_VarName  
    CHARACTER(256) :: npts_Band_VarName
    INTEGER :: n_Points_DimId
    INTEGER :: n_Bands_DimId
    INTEGER :: Response_VarId
    INTEGER :: f1_Band_VarId
    INTEGER :: f2_Band_VarId
    INTEGER :: npts_Band_VarId
    INTEGER :: VarId


    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
 
    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      msg = 'Some or all INPUT SRF pointer members are NOT associated.'
      CALL Write_CleanUp(); RETURN
    END IF

    ! Check SRF channel is valid
    IF ( SRF%Channel < 1 ) THEN
      WRITE( msg,'("Invalid SRF channel, ",i0,". Must be > 0.")' ) SRF%Channel
      CALL Write_CleanUp(); RETURN
    END IF

    ! Check SRF array sizes
    IF ( SRF%n_Points < 1 ) THEN
      WRITE( msg,'("Invalid no. of SRF points, ",i0,". Must be > 0.")' ) SRF%n_Points
      CALL Write_CleanUp(); RETURN
    END IF
    IF ( SRF%n_Bands < 1 ) THEN
      WRITE( msg,'("Invalid no. of SRF bands, ",i0,". Must be > 0.")' ) SRF%n_Bands
      CALL Write_CleanUp(); RETURN
    END IF

    ! Select the frequency units string
    SELECT CASE(SRF%Sensor_Type)
      CASE(MICROWAVE_SENSOR)
        Frequency_Units = 'Gigahertz (GHz)'
      CASE(INFRARED_SENSOR,VISIBLE_SENSOR,ULTRAVIOLET_SENSOR)
        Frequency_Units = 'Inverse centimetres (cm^-1)'
      CASE DEFAULT
        msg = 'Invalid sensor type'
        CALL Write_CleanUp(); RETURN
    END SELECT


    ! Check that the SRF sensor type is consistent for the file
    ! ---------------------------------------------------------
    ! Get the sensor type
    Error_Status = Inquire_SRF_netCDF( NC_Filename,Sensor_Type=Sensor_Type,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error obtaining '//SENSOR_TYPE_VARNAME//' from '//TRIM(NC_Filename)
      CALL Write_CleanUp(); RETURN
    END IF
    ! Check if it's the same
    IF ( SRF%Sensor_Type /= Sensor_Type ) THEN
      msg = 'File and structure sensor type flags are different!'
      CALL Write_CleanUp(); RETURN
    END IF
    
    ! Check that the SRF channel is valid for the file
    ! ------------------------------------------------
    ! Get the channel dimension
    Error_Status = Inquire_SRF_netCDF( NC_Filename,n_Channels=n_Channels,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Write_CleanUp(); RETURN
    END IF
    ! Allocate a sensor channel array
    ALLOCATE( Sensor_Channel(n_Channels),STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( msg,'("Error allocating sensor channel array. STAT = ", i5 )' ) Allocate_Status
      CALL Write_CleanUp(); RETURN
    END IF
    ! Read the sensor channel list
    Error_Status = Inquire_SRF_netCDF( NC_Filename,Sensor_Channel=Sensor_Channel,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading '//SENSOR_CHANNEL_VARNAME//' data from '//TRIM(NC_Filename)
      CALL Write_CleanUp(Dealloc_Arrays=.TRUE.); RETURN
    END IF
    ! Check if the SRF channel is in the list at all, or more than once
    n = COUNT(Sensor_Channel == SRF%Channel)
    IF ( n < 1 ) THEN
      WRITE( msg,'("SRF channel ",i0," is not in the sensor channel list for ",a)' ) &
                 SRF%Channel, TRIM(NC_Filename)
      CALL Write_CleanUp(Dealloc_Arrays=.TRUE.); RETURN
    END IF
    IF ( n > 1 ) THEN
      WRITE( msg,'("Check ",a," file! SRF channel ",i0,&
                  &" occurs multiple times in the sensor channel list")' ) &
                 SRF%Channel, TRIM(NC_Filename)
      CALL Write_CleanUp(Dealloc_Arrays=.TRUE.); RETURN
    END IF
    ! Get the index of the current channel in the sensor channel list
    Channel_Idx = PACK((/(i,i=1,n_Channels)/),Sensor_Channel == SRF%Channel)
    ! Deallocate the sensor channel list array
    DEALLOCATE( Sensor_Channel )


    ! Create the SRF dimension and variable names for the current channel
    ! -------------------------------------------------------------------
    CALL CreateNames( SRF%Channel, &
                      Channel_Name     =Channel_Name     , &
                      Point_DimName    =Point_DimName    , &
                      Band_DimName     =Band_DimName     , &
                      Response_VarName =Response_VarName , &
                      f1_Band_VarName  =f1_Band_VarName  , &
                      f2_Band_VarName  =f2_Band_VarName  , &
                      npts_Band_VarName=npts_Band_VarName  )


    ! Open the file
    ! -------------
    NF90_Status(1) = NF90_OPEN( NC_Filename,NF90_WRITE,NC_FileId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(NC_Filename)//' for channel '//TRIM(Channel_Name)//&
            ' write access - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Put netcdf file into define mode 
    !----------------------------------
    NF90_Status(1) = NF90_REDEF( NC_FileId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error putting file '//TRIM(NC_Filename)//' into define mode for channel '//&
            TRIM(Channel_Name)//'- '//TRIM( NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Define the dimensions for this channel
    ! --------------------------------------
    ! The n_Bands dimension
    NF90_Status(1) = NF90_DEF_DIM( NC_FileId,TRIM(Band_DimName),SRF%n_Bands,n_Bands_DimId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining the '//TRIM(Band_DimName)//' dimension in '// &
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! The n_Points dimension
    NF90_Status(1) = NF90_DEF_DIM( NC_FileId,TRIM(Point_DimName),SRF%n_Points,n_Points_DimId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining the '//TRIM(Point_DimName)//' dimension in '// &
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    
    ! Define the band variables
    ! -------------------------
    ! The band begin frequency variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileId,TRIM(f1_Band_VarName),FREQUENCY_TYPE,&
                                   dimIDs=n_Bands_DimId,varID=f1_Band_VarId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(f1_Band_VarName)//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileId,f1_Band_VarID,LONGNAME_ATTNAME,F1_BAND_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileId,f1_Band_VarID,DESCRIPTION_ATTNAME,F1_BAND_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileId,f1_Band_VarID,UNITS_ATTNAME,TRIM(Frequency_Units) )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileId,f1_Band_VarID,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE )
    IF ( ANY(NF90_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TRIM(f1_Band_VarName)//' variable attributes to '//TRIM(NC_Filename)
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    ! The band end frequency variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileId,TRIM(f2_Band_VarName),FREQUENCY_TYPE,&
                                   dimIDs=n_Bands_DimId,varID=f2_Band_VarId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(f2_Band_VarName)//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileId,f2_Band_VarID,LONGNAME_ATTNAME,F2_BAND_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileId,f2_Band_VarID,DESCRIPTION_ATTNAME,F2_BAND_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileId,f2_Band_VarID,UNITS_ATTNAME,TRIM(Frequency_Units) )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileId,f2_Band_VarID,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE )
    IF ( ANY(NF90_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TRIM(f2_Band_VarName)//' variable attributes to '//TRIM(NC_Filename)
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The band npts variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileId,TRIM(npts_Band_VarName),NPTS_BAND_TYPE,&
                                   dimIDs=n_Bands_DimId,varID=npts_Band_VarId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(npts_Band_VarName)//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileId,npts_Band_VarID,LONGNAME_ATTNAME,NPTS_BAND_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileId,npts_Band_VarID,DESCRIPTION_ATTNAME,NPTS_BAND_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileId,npts_Band_VarID,UNITS_ATTNAME,NPTS_BAND_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileId,npts_Band_VarID,FILLVALUE_ATTNAME,NPTS_BAND_FILLVALUE )
    IF ( ANY(NF90_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TRIM(npts_Band_VarName)//' variable attributes to '//TRIM(NC_Filename)
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Define the response variable
    ! ----------------------------
    NF90_Status(1) = NF90_DEF_VAR( NC_FileId,TRIM(Response_VarName),RESPONSE_TYPE,&
                                   dimIDs=n_Points_DimId,varID=Response_VarId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(Response_VarName)//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileId,Response_VarID,LONGNAME_ATTNAME,RESPONSE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileId,Response_VarID,DESCRIPTION_ATTNAME,RESPONSE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileId,Response_VarID,UNITS_ATTNAME,RESPONSE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileId,Response_VarID,FILLVALUE_ATTNAME,RESPONSE_FILLVALUE )
    IF ( ANY(NF90_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TRIM(Response_VarName)//' variable attributes to '//TRIM(NC_Filename)
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Put the file into data mode 
    !----------------------------
    NF90_Status(1) = NF90_ENDDEF( NC_FileId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error placing '//TRIM(NC_Filename)//' in DATA mode for channel '//&
            TRIM(Channel_Name)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the channel dependent data
    ! --------------------------------
    ! The integrated SRF value
    NF90_Status(1) = NF90_INQ_VARID( NC_FileId,INTEGRATED_SRF_VARNAME,VarId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//INTEGRATED_SRF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_VAR( NC_FileId,VarID,SRF%Integrated_SRF,START=Channel_Idx )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error writing channel '//TRIM(Channel_Name)//' '//INTEGRATED_SRF_VARNAME//&
            ' to '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    ! The summed SRF value
    NF90_Status(1) = NF90_INQ_VARID( NC_FileId,SUMMATION_SRF_VARNAME,VarId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SUMMATION_SRF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_VAR( NC_FileId,VarID,SRF%Summation_SRF,START=Channel_Idx )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error writing channel '//TRIM(Channel_Name)//' '//SUMMATION_SRF_VARNAME//&
            ' to '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    

    ! Write the band dependent data
    ! -----------------------------
    ! The band begin frequencies
    NF90_Status(1) = NF90_PUT_VAR( NC_FileId,f1_Band_VarID,SRF%f1_Band )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(f1_Band_VarName)//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The band end frequencies
    NF90_Status(1) = NF90_PUT_VAR( NC_FileId,f2_Band_VarID,SRF%f2_Band )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(f2_Band_VarName)//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The number of band points
    NF90_Status(1) = NF90_PUT_VAR( NC_FileId,npts_Band_VarID,SRF%npts_Band )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(npts_Band_VarName)//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the SRF response
    ! ----------------------
    NF90_Status(1) = NF90_PUT_VAR( NC_FileId,Response_VarID,SRF%Response )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(Response_VarName)//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    
    ! Close the file
    ! --------------
    NF90_Status(1) = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_SRF( SRF, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp( Dealloc_Arrays,Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Dealloc_Arrays
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Deallocate local arrays if necessary
      IF ( PRESENT(Dealloc_Arrays) ) THEN
        IF ( Dealloc_Arrays ) THEN
          DEALLOCATE( Sensor_Channel,STAT=Allocate_Status )
          IF ( Allocate_Status /= 0 ) &
            WRITE( msg,'(a,"; Error deallocating local arrays during error cleanup. STAT=",i0)') &
                       TRIM(msg), Allocate_Status
        END IF
      END IF
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status(1) = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status(1) /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status(1) ))
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_SRF_netCDF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Read_SRF_netCDF
!
! PURPOSE:
!       Function to read a selected channels SRF data from a netCDF SRF
!       format file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_SRF_netCDF( NC_Filename            , &  ! Input  
!                                       Channel                , &  ! Input  
!                                       SRF                    , &  ! Output 
!                                       Quiet      =Quiet      , &  ! Optional input
!                                       RCS_Id     = RCS_Id    , &  ! Revision control
!                                       Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the netCDF
!                     SRF format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Channel:      Channel number for which the SRF data is required.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SRF:          Structure containing the requested SRF data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(SRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
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
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the netCDF data read was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_SRF_netCDF( NC_Filename , &  ! Input
                            Channel     , &  ! Input
                            SRF         , &  ! Output
                            Quiet       , &  ! Optional input
                            RCS_Id      , &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    INTEGER,                INTENT(IN)     :: Channel
    TYPE(SRF_type),         INTENT(IN OUT) :: SRF
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SRF_netCDF'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: NC_FileId
    INTEGER :: NF90_Status
    INTEGER :: Allocate_Status
    INTEGER :: i, n
    INTEGER :: Version         
    CHARACTER(256) :: Sensor_ID       
    INTEGER :: WMO_Satellite_ID
    INTEGER :: WMO_Sensor_ID   
    INTEGER :: Sensor_Type
    INTEGER :: n_Channels, n_Points, n_Bands
    INTEGER :: Channel_Idx(1)
    INTEGER, ALLOCATABLE :: Sensor_Channel(:)
    CHARACTER(256) :: Channel_Name     
    CHARACTER(256) :: Point_DimName    
    CHARACTER(256) :: Band_DimName     
    CHARACTER(256) :: Response_VarName 
    CHARACTER(256) :: f1_Band_VarName  
    CHARACTER(256) :: f2_Band_VarName  
    CHARACTER(256) :: npts_Band_VarName
    INTEGER :: n_Points_DimId
    INTEGER :: n_Bands_DimId
    INTEGER :: Reponse_VarId
    INTEGER :: f1_Band_VarId
    INTEGER :: f2_Band_VarId
    INTEGER :: npts_Band_VarId
    INTEGER :: VarId

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

    ! Check that the SRF channel is valid for the file
    ! ------------------------------------------------
    ! Get the channel dimension
    Error_Status = Inquire_SRF_netCDF( NC_Filename,n_Channels=n_Channels,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! Allocate a sensor channel array
    ALLOCATE( Sensor_Channel(n_Channels),STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( msg,'("Error allocating sensor channel array. STAT = ", i5 )' ) Allocate_Status
      CALL Read_Cleanup(); RETURN
    END IF
    ! Read the sensor channel list
    Error_Status = Inquire_SRF_netCDF( NC_Filename,Sensor_Channel=Sensor_Channel,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Read_Cleanup(Dealloc_Arrays=.TRUE.); RETURN
    END IF
    ! Check if the requested channel is in the list at all, or more than once
    n = COUNT(Sensor_Channel == Channel)
    IF ( n < 1 ) THEN
      WRITE( msg,'("SRF channel ",i0," is not in the sensor channel list for ",a)' ) &
                 Channel, TRIM(NC_Filename)
      CALL Read_Cleanup(Dealloc_Arrays=.TRUE.); RETURN
    END IF
    IF ( n > 1 ) THEN
      WRITE( msg,'("Check ",a," file! SRF channel ",i0,&
                  &" occurs multiple times in the sensor channel list")' ) &
                 SRF%Channel, TRIM(NC_Filename)
      CALL Read_Cleanup(Dealloc_Arrays=.TRUE.); RETURN
    END IF
    ! Get the index of the current channel in the sensor channel list
    Channel_Idx = PACK((/(i,i=1,n_Channels)/),Sensor_Channel == Channel)
    ! Deallocate the sensor channel list array
    DEALLOCATE( Sensor_Channel )


    ! Read some of the global attributes
    ! ----------------------------------
    Error_Status = Inquire_SRF_netCDF( NC_Filename                        , &  ! Input
                                       Version          = Version         , &  ! Optional output
                                       Sensor_ID        = Sensor_ID       , &  ! Optional output
                                       WMO_Satellite_ID = WMO_Satellite_ID, &  ! Optional output
                                       WMO_Sensor_ID    = WMO_Sensor_ID   , &  ! Optional output
                                       Sensor_Type      = Sensor_Type     , &  ! Optional output
                                       Message_Log      = Message_Log       )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error occurred reading global attributes from '//TRIM(NC_Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    
    
    ! Create the SRF dimension and variable names for the current channel
    ! -------------------------------------------------------------------
    CALL CreateNames( Channel, &
                      Channel_Name     =Channel_Name     , &
                      Point_DimName    =Point_DimName    , &
                      Band_DimName     =Band_DimName     , &
                      Response_VarName =Response_VarName , &
                      f1_Band_VarName  =f1_Band_VarName  , &
                      f2_Band_VarName  =f2_Band_VarName  , &
                      npts_Band_VarName=npts_Band_VarName  )
                      
                      
    ! Open the file
    ! -------------
    NF90_Status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(NC_Filename)//' for channel '//TRIM(Channel_Name)//&
            ' read access - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Retrieve channel dimension values
    ! ---------------------------------                        
    ! Retrieve the number of points dimension value
    Error_Status = ReadDim( NC_FileId,Point_DimName,n_Points,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error obtaining '//TRIM(Point_DimName)//&
            ' dimension from '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Retrieve the number of bands dimension value
    Error_Status = ReadDim( NC_FileId,Band_DimName,n_Bands,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error obtaining '//TRIM(Band_DimName)//&
            ' dimension from '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Allocate the output SRF structure
    ! ---------------------------------
    Error_Status = Allocate_SRF( n_Points,SRF,n_Bands=n_Bands,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error occurred allocating SRF structure.'
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Set the sensor and channel values
    ! ---------------------------------
    SRF%Version          = Version
    SRF%Sensor_ID        = TRIM(Sensor_ID)
    SRF%WMO_Satellite_ID = WMO_Satellite_ID
    SRF%WMO_Sensor_ID    = WMO_Sensor_ID   
    SRF%Sensor_Type      = Sensor_Type
    SRF%Channel          = Channel

      
    ! Read the channel dependent data
    ! -------------------------------
    ! The integrated SRF value
    NF90_Status = NF90_INQ_VARID( NC_FileId,INTEGRATED_SRF_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//INTEGRATED_SRF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SRF%Integrated_SRF,START=Channel_Idx )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading channel '//TRIM(Channel_Name)//' '//INTEGRATED_SRF_VARNAME//&
            ' from '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF
    
    ! The summed SRF value
    NF90_Status = NF90_INQ_VARID( NC_FileId,SUMMATION_SRF_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SUMMATION_SRF_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SRF%Summation_SRF,START=Channel_Idx )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading channel '//TRIM(Channel_Name)//' '//SUMMATION_SRF_VARNAME//&
            ' from '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF
    
    ! Read the band dependent data
    ! ----------------------------
    ! The band begin frequencies
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(f1_Band_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f1_Band_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SRF%f1_Band )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(f1_Band_VarName)//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF

    ! The band end frequencies
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(f2_Band_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f2_Band_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SRF%f2_Band )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(f2_Band_VarName)//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF

    ! The number of band points
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(npts_Band_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(npts_Band_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SRF%npts_Band )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(npts_Band_VarName)//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF


    ! Read the SRF response
    ! ---------------------
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(Response_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(Response_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SRF%Response )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(Response_VarName)//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF

 
    ! Close the file
    ! --------------
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Destroy_Structure=.TRUE.); RETURN
    END IF


    ! Compute the SRF frequency grid 
    !-------------------------------
    Error_Status = Frequency_SRF( SRF )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error computing frequency grid for channel '//TRIM(Channel_Name)//&
            ' SRF from '//TRIM(NC_Filename)
      CALL Read_Cleanup(Destroy_Structure=.TRUE.); RETURN
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_SRF( SRF, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Dealloc_Arrays, Close_File, Destroy_Structure )
      LOGICAL, OPTIONAL, INTENT(IN) :: Dealloc_Arrays
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      LOGICAL, OPTIONAL, INTENT(IN) :: Destroy_Structure
      ! Deallocate local arrays if necessary
      IF ( PRESENT(Dealloc_Arrays) ) THEN
        IF ( Dealloc_Arrays ) THEN
          DEALLOCATE( Sensor_Channel,STAT=Allocate_Status )
          IF ( Allocate_Status /= 0 ) &
            WRITE( msg,'(a,"; Error deallocating local arrays during error cleanup. STAT=",i0)') &
                       TRIM(msg), Allocate_Status
        END IF
      END IF
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Destroy the structure if necessary
      IF ( PRESENT(Destroy_Structure) ) THEN
        IF ( Destroy_Structure ) THEN
          Error_Status = Destroy_SRF(SRF, Message_Log=Message_Log)
          IF ( Error_Status /= SUCCESS ) &
            msg = TRIM(msg)//'; Error destroying SRF structure during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_SRF_netCDF

END MODULE SRF_netCDF_IO
 
