
MODULE CRTMstats_netCDF_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE CRTMstats_Define

  USE netcdf
  USE netCDF_Utility,  Open_CRTMstats_netCDF =>  Open_netCDF, &
                      Close_CRTMstats_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_CRTMstats_netCDF
  PUBLIC :: Write_CRTMstats_netCDF
  PUBLIC :: Read_CRTMstats_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: CRTMstats_netCDF_IO.f90,v 1.2 2006/11/22 16:02:04 dgroff Exp $'

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Maximum angle secant value
  REAL( fp_kind ), PRIVATE, PARAMETER :: ANGLE_LIMIT = 3.0_fp_kind

  ! -- Keyword set value
  INTEGER,        PRIVATE, PARAMETER ::   SET = 1

   ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: TITLE_GATTNAME              = 'title' 
  CHARACTER( * ), PRIVATE, PARAMETER :: HISTORY_GATTNAME            = 'history' 
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_NAME_GATTNAME        = 'sensor_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: PLATFORM_NAME_GATTNAME      = 'platform_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: COMMENT_GATTNAME            = 'comment' 
  CHARACTER( * ), PRIVATE, PARAMETER :: LBL_PROFILE_ID_TAG_GATTNAME = 'LBL_Profile_id_tag' 
  CHARACTER( * ), PRIVATE, PARAMETER :: REG_PROFILE_ID_TAG_GATTNAME = 'REG_Profile_id_tag' 

  ! -- Dimension names
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_DIMNAME        = 'n_Layers'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_DIMNAME      = 'n_Channels'
  CHARACTER( * ), PRIVATE, PARAMETER :: ANGLE_DIMNAME        = 'n_Angles'
  CHARACTER( * ), PRIVATE, PARAMETER :: PROFILE_DIMNAME      = 'n_Profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: MOLECULE_SET_DIMNAME = 'n_Molecule_Sets'

  ! -- Variable names
  CHARACTER( * ), PRIVATE, PARAMETER :: INT_WATER_VAPOR_VARNAME    = 'Int_Water_Vapor'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_VARNAME     = 'NCEP_Sensor_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_VARNAME   = 'WMO_Satellite_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_VARNAME      = 'WMO_Sensor_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_LIST_VARNAME       = 'Channel_list'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_VARNAME          = 'Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: ANGLE_LIST_VARNAME         = 'Angle_list'
  CHARACTER( * ), PRIVATE, PARAMETER :: PROFILE_LIST_VARNAME       = 'Profile_list'
  CHARACTER( * ), PRIVATE, PARAMETER :: MOLECULE_SET_LIST_VARNAME  = 'Molecule_Set_list'
  CHARACTER( * ), PRIVATE, PARAMETER :: LBL_OD_VARNAME             = 'LBL_OD'
  CHARACTER( * ), PRIVATE, PARAMETER :: REG_OD_VARNAME             = 'REG_OD'
  CHARACTER( * ), PRIVATE, PARAMETER :: dOD_VARNAME                = 'dOD'
  CHARACTER( * ), PRIVATE, PARAMETER :: LBL_TAU_VARNAME            = 'LBL_Tau'
  CHARACTER( * ), PRIVATE, PARAMETER :: REG_TAU_VARNAME            = 'REG_Tau'
  CHARACTER( * ), PRIVATE, PARAMETER :: MEAN_DTAU_VARNAME          = 'Mean_dTau'
  CHARACTER( * ), PRIVATE, PARAMETER :: RMS_DTAU_VARNAME           = 'RMS_dTau'
  CHARACTER( * ), PRIVATE, PARAMETER :: MEAN_DTAU_BY_ANGLE_VARNAME = 'Mean_dTau_by_Angle'
  CHARACTER( * ), PRIVATE, PARAMETER :: RMS_DTAU_BY_ANGLE_VARNAME  = 'RMS_dTau_by_Angle'
  CHARACTER( * ), PRIVATE, PARAMETER :: LBL_BT_VARNAME             = 'LBL_BT'
  CHARACTER( * ), PRIVATE, PARAMETER :: REG_BT_VARNAME             = 'REG_BT'
  CHARACTER( * ), PRIVATE, PARAMETER :: MEAN_DBT_VARNAME           = 'Mean_dBT'
  CHARACTER( * ), PRIVATE, PARAMETER :: RMS_DBT_VARNAME            = 'RMS_dBT'
  CHARACTER( * ), PRIVATE, PARAMETER :: MEAN_DBT_BY_ANGLE_VARNAME  = 'Mean_dBT_by_Angle'
  CHARACTER( * ), PRIVATE, PARAMETER :: RMS_DBT_BY_ANGLE_VARNAME   = 'RMS_dBT_by_Angle'

  ! -- Variable long name attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER( * ), PRIVATE, PARAMETER :: INT_WATER_VAPOR_LONGNAME     = &
    'Integrated Water Vapor of listed Profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_LONGNAME      = &
    'ID used at NOAA/NCEP/EMC to identify a satellite/sensor (-1 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_LONGNAME    = &
    'WMO code for identifying satellite platforms (1023 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_LONGNAME       = &
    'WMO code for identifying a satellite sensor (2047 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_LIST_LONGNAME        = &
    'List of sensor channel numbers.'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_LONGNAME           = &
    'Central frequency of listed sensor channels.'
  CHARACTER( * ), PRIVATE, PARAMETER :: ANGLE_LIST_LONGNAME          = &
    'List of the secant of the zenith angles.'
  CHARACTER( * ), PRIVATE, PARAMETER :: PROFILE_LIST_LONGNAME        = &
    'List of atmospheric profile set indices used in generating the transmittance data.'
  CHARACTER( * ), PRIVATE, PARAMETER :: MOLECULE_SET_LIST_LONGNAME   = &
    'List of molecular species set indices used in generating the transmittance data.'
  CHARACTER( * ), PRIVATE, PARAMETER :: LBL_OD_LONGNAME              = &
    'Line-by-line model generated, instrument resolution layer optical depths'
  CHARACTER( * ), PRIVATE, PARAMETER :: REG_OD_LONGNAME              = &
    'Regression model generated, instrument resolution layer optical depths'
  CHARACTER( * ), PRIVATE, PARAMETER :: dOD_LONGNAME                 = &
    'LBL - REG layer optical depths' 
  CHARACTER( * ), PRIVATE, PARAMETER :: LBL_TAU_LONGNAME             = &
    'Line-by-line model generated, top-of-atmosphere instrument resolution channel transmittances'
  CHARACTER( * ), PRIVATE, PARAMETER :: REG_TAU_LONGNAME             = &
    'Regression model generated, top-of-atmosphere instrument resolution channel transmittances'
  CHARACTER( * ), PRIVATE, PARAMETER :: MEAN_DTAU_LONGNAME           = &
    'Mean channel transmittance difference, LBL-REG, over all angles and profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: RMS_DTAU_LONGNAME            = &
    'Root-mean-square channel transmittance difference, LBL-REG, over all angles and profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: MEAN_DTAU_BY_ANGLE_LONGNAME  = &
    'Mean channel transmittance difference, LBL-REG, for each angle over all profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: RMS_DTAU_BY_ANGLE_LONGNAME   = &
    'Root-mean-square channel transmittance difference, LBL-REG, for each angle over all profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: LBL_BT_LONGNAME              = &
    'Channel brightness temperatures derived from line-by-line model generated transmittances'
  CHARACTER( * ), PRIVATE, PARAMETER :: REG_BT_LONGNAME              = &
    'Channel brightness temperatures derived from regression model generated transmittances'
  CHARACTER( * ), PRIVATE, PARAMETER :: MEAN_DBT_LONGNAME            = &
    'Mean channel brightness temperature difference, LBL-REG, over all angles and profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: RMS_DBT_LONGNAME             = &
    'Root-mean-square channel brightness temperature difference, LBL-REG, over all angles and profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: MEAN_DBT_BY_ANGLE_LONGNAME   = &
    'Mean channel brightness temperature difference, LBL-REG, for each angle over all profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: RMS_DBT_BY_ANGLE_LONGNAME    = &
    'Root-mean-square channel brightness temperature difference, LBL-REG, for each angle over all profiles'

  ! -- Variable units attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: UNITS_ATTNAME   = 'units'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_UNITS = 'Inverse centimetres, cm^-1'
  CHARACTER( * ), PRIVATE, PARAMETER :: OD_UNITS        = 'None'
  CHARACTER( * ), PRIVATE, PARAMETER :: TAU_UNITS       = 'None'
  CHARACTER( * ), PRIVATE, PARAMETER :: BT_UNITS        = 'Kelvin, K'
  CHARACTER( * ), PRIVATE, PARAMETER :: PWV_UNITS       = 'g/cm^2'

  ! -- Variable _FillValue attribute.
  CHARACTER( * ),  PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  INTEGER,         PRIVATE, PARAMETER :: NCEP_SENSOR_ID_FILLVALUE    = -1
  INTEGER,         PRIVATE, PARAMETER :: WMO_SATELLITE_ID_FILLVALUE  = 1023
  INTEGER,         PRIVATE, PARAMETER :: WMO_SENSOR_ID_FILLVALUE     = 2047
  INTEGER,         PRIVATE, PARAMETER :: CHANNEL_LIST_FILLVALUE      = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: FREQUENCY_FILLVALUE         = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ANGLE_LIST_FILLVALUE        = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: INT_WATER_VAPOR_FILLVALUE   = -1.0_fp_kind
  INTEGER,         PRIVATE, PARAMETER :: PROFILE_LIST_FILLVALUE      = -1
  INTEGER,         PRIVATE, PARAMETER :: MOLECULE_SET_LIST_FILLVALUE = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: OD_FILLVALUE                = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TAU_FILLVALUE               = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: BT_FILLVALUE                = -1.0_fp_kind

  ! -- Variable types
  INTEGER, PRIVATE, PARAMETER :: NCEP_SENSOR_ID_TYPE    = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: WMO_SATELLITE_ID_TYPE  = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: WMO_SENSOR_ID_TYPE     = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: CHANNEL_LIST_TYPE      = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: FREQUENCY_TYPE         = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: ANGLE_LIST_TYPE        = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: INT_WATER_VAPOR_TYPE   = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: PROFILE_LIST_TYPE      = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: MOLECULE_SET_LIST_TYPE = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: OD_TYPE                = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: TAU_TYPE               = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: BT_TYPE                = NF90_DOUBLE


CONTAINS







  FUNCTION Write_CRTMstats_GAtts( NC_Filename,        &  ! Input
                                   NC_fileID,          &  ! Input
                                   LBL_Profile_ID_Tag, &  ! Optional input
                                   REG_Profile_ID_Tag, &  ! Optional input
                                   Title,              &  ! Optional input
                                   History,            &  ! Optional input
                                   Sensor_Name,        &  ! Optional input
                                   Platform_Name,      &  ! Optional input
                                   Comment,            &  ! Optional input
                                   Message_Log )       &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN ) :: NC_Filename
    INTEGER,                  INTENT( IN ) :: NC_fileID

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: LBL_Profile_ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: REG_Profile_ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Comment

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_CRTMstats_GAtts'

    ! -- "Internal" global attributes
    CHARACTER( * ), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER( * ), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NF90_Status

    CHARACTER(  8 ) :: cdate
    CHARACTER( 10 ) :: ctime
    CHARACTER(  5 ) :: czone



    !#--------------------------------------------------------------------------#
    !#              -- WRITE THE "INTERNAL" GLOBAL ATTRIBUTES --                #
    !#--------------------------------------------------------------------------#

    ! -----------
    ! Software ID
    ! -----------

    Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                         WRITE_MODULE_HISTORY_GATTNAME, &
                                         MODULE_RCS_ID, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WRITE_MODULE_HISTORY_GATTNAME//&
                            ' attribute to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -------------
    ! Creation date
    ! -------------

    CALL DATE_AND_TIME( cdate, ctime, czone )

    Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                         CREATION_DATE_AND_TIME_GATTNAME, &
                                         cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                         ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                         czone//'UTC', &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CREATION_DATE_AND_TIME_GATTNAME//&
                            ' attribute to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- DEFINE THE USER ACCESSIBLE GLOBAL ATTRIBUTES --            #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! LBL profile set ID tag
    ! ----------------------

    IF ( PRESENT( LBL_Profile_ID_Tag ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           LBL_PROFILE_ID_TAG_GATTNAME, &
                                           TRIM( LBL_Profile_ID_Tag ), &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//LBL_PROFILE_ID_TAG_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! ----------------------
    ! REG profile set ID tag
    ! ----------------------

    IF ( PRESENT( REG_Profile_ID_Tag ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           REG_PROFILE_ID_TAG_GATTNAME, &
                                           TRIM( REG_Profile_ID_Tag ), &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//REG_PROFILE_ID_TAG_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -----
    ! Title
    ! -----

    IF ( PRESENT( Title ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           TITLE_GATTNAME, &
                                           TRIM( Title ), &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//TITLE_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------
    ! History
    ! -------

    IF ( PRESENT( History ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           HISTORY_GATTNAME, &
                                           TRIM( History ), &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//HISTORY_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -----------
    ! Sensor name
    ! -----------

    IF ( PRESENT( Sensor_Name ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           SENSOR_NAME_GATTNAME, &
                                           TRIM( Sensor_Name ), &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//SENSOR_NAME_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------------
    ! Platform name
    ! -------------

    IF ( PRESENT( Platform_Name ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           PLATFORM_NAME_GATTNAME, &
                                           TRIM( Platform_Name ), &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//PLATFORM_NAME_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------
    ! Comment
    ! -------

    IF ( PRESENT( Comment ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           COMMENT_GATTNAME, &
                                           TRIM( Comment ), &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//COMMENT_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF

  END FUNCTION Write_CRTMstats_GAtts






  FUNCTION Read_CRTMstats_GAtts( NC_Filename,        &  ! Input
                                  NC_fileID,          &  ! Input
                                  LBL_Profile_ID_Tag, &  ! Optional output
                                  REG_Profile_ID_Tag, &  ! Optional output
                                  Title,              &  ! Optional output
                                  History,            &  ! Optional output
                                  Sensor_Name,        &  ! Optional output
                                  Platform_Name,      &  ! Optional output
                                  Comment,            &  ! Optional output
                                  Message_Log )       &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: NC_Filename
    INTEGER,                  INTENT( IN )  :: NC_fileID

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: LBL_Profile_ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: REG_Profile_ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Comment

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_CRTMstats_GAtts'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 10000 ) :: Long_String



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                     -- READ THE GLOBAL ATTRIBUTES --                     #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! The LBL_PROFILE_ID_TAG
    ! ----------------------

    IF ( PRESENT( LBL_Profile_ID_Tag ) ) THEN

      LBL_Profile_ID_Tag = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           LBL_PROFILE_ID_TAG_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//LBL_PROFILE_ID_TAG_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      LBL_Profile_ID_Tag = Long_String(1:MIN( LEN( LBL_Profile_ID_Tag ), LEN_TRIM( Long_String ) ))

    END IF


    ! ----------------------
    ! The REG_PROFILE_ID_TAG
    ! ----------------------

    IF ( PRESENT( REG_Profile_ID_Tag ) ) THEN

      REG_Profile_ID_Tag = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           REG_PROFILE_ID_TAG_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//REG_PROFILE_ID_TAG_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      REG_Profile_ID_Tag = Long_String(1:MIN( LEN( REG_Profile_ID_Tag ), LEN_TRIM( Long_String ) ))

    END IF


    ! ---------
    ! The TITLE
    ! ---------

    IF ( PRESENT( Title ) ) THEN

      Title = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           TITLE_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//TITLE_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      Title = Long_String(1:MIN( LEN( Title ), LEN_TRIM( Long_String ) ))

    END IF


    ! -----------
    ! The HISTORY
    ! -----------

    IF ( PRESENT( History ) ) THEN

      History = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           HISTORY_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//HISTORY_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      History = Long_String(1:MIN( LEN( History ), LEN_TRIM( Long_String ) ))

    END IF


    ! ---------------
    ! The SENSOR_NAME
    ! ---------------

    IF ( PRESENT( Sensor_Name ) ) THEN

      Sensor_Name = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           SENSOR_NAME_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//SENSOR_NAME_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      Sensor_Name = Long_String(1:MIN( LEN( Sensor_Name ), LEN_TRIM( Long_String ) ))

    END IF


    ! -----------------
    ! The PLATFORM_NAME
    ! -----------------

    IF ( PRESENT( Platform_Name ) ) THEN

      Platform_Name = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           PLATFORM_NAME_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//PLATFORM_NAME_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      Platform_Name = Long_String(1:MIN( LEN( Platform_Name ), LEN_TRIM( Long_String ) ))

    END IF


    ! -----------
    ! The COMMENT
    ! -----------

    IF ( PRESENT( Comment ) ) THEN

      Comment = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           COMMENT_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//COMMENT_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      Comment = Long_String(1:MIN( LEN( Comment ), LEN_TRIM( Long_String ) ))

    END IF

  END FUNCTION Read_CRTMstats_GAtts






  FUNCTION Create_CRTMstats_netCDF( NC_Filename,         &  ! Input
                                     n_Layers,           &  ! Input
                                     n_Channels,         &  ! Input
                                     n_Angles,           &  ! Input
                                     n_Profiles,         &  ! Input
                                     n_Molecule_Sets,    &  ! Input
                                     NC_FileID,          &  ! Output
                                     LBL_Profile_ID_Tag, &  ! Optional input
                                     REG_Profile_ID_Tag, &  ! Optional input
                                     Title,              &  ! Optional input
                                     History,            &  ! Optional input
                                     Sensor_Name,        &  ! Optional input
                                     Platform_Name,      &  ! Optional input
                                     Comment,            &  ! Optional input
                                     RCS_Id,             &  ! Revision control
                                     Message_Log )       &  ! Error messaging
                                   RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: NC_Filename
    INTEGER,                  INTENT( IN )  :: n_Layers
    INTEGER,                  INTENT( IN )  :: n_Channels      
    INTEGER,                  INTENT( IN )  :: n_Angles        
    INTEGER,                  INTENT( IN )  :: n_Profiles      
    INTEGER,                  INTENT( IN )  :: n_Molecule_Sets 

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: NC_FileID

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: LBL_Profile_ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: REG_Profile_ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Comment

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Create_CRTMstats_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NF90_Status
    INTEGER :: Status1, Status2, Status3
    INTEGER :: Close_Status

    INTEGER :: Layer_DimID
    INTEGER :: Channel_DimID
    INTEGER :: Angle_DimID
    INTEGER :: Profile_DimID
    INTEGER :: Molecule_Set_DimID

    INTEGER :: VarID


    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                  -- SET RCS_ID ARGUMENT IF SUPPLIED --                   #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#
    
    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'N_LAYERS dimensions must > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
      

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'N_CHANNELS dimensions must > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Angles < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'N_ANGLES dimension must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Profiles < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'N_PROFILES dimensionmust be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Molecule_Sets < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'N_MOLECULE_SETs dimension must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CREATE THE NETCDF DATA FILE --                     #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_CREATE( TRIM( NC_Filename ), &
                               NF90_CLOBBER, &
                               NC_fileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- DEFINE THE DIMENSIONS --                      #
    !#--------------------------------------------------------------------------#
    
    ! --------------------
    ! The number of Layers
    ! --------------------
    
    NF90_Status = NF90_DEF_DIM( NC_FileID,     &
                                LAYER_DIMNAME, &
                                n_Layers,      &
                                Layer_DimID  )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//LAYER_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! ----------------------
    ! The number of channels
    ! ----------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                CHANNEL_DIMNAME, &
                                n_Channels, &
                                Channel_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//CHANNEL_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! The number of angles
    ! --------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                ANGLE_DIMNAME, &
                                n_Angles, &
                                Angle_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//ANGLE_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The number of profiles
    ! ----------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                PROFILE_DIMNAME, &
                                n_Profiles, &
                                Profile_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//PROFILE_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------
    ! The number of molecule sets
    ! ---------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                MOLECULE_SET_DIMNAME, &
                                NF90_UNLIMITED, &
                                Molecule_Set_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//MOLECULE_SET_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE GLOBAL ATTRIBUTES --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Write_CRTMstats_GAtts( NC_Filename, &  ! Input
                                           NC_fileID,   &  ! Input
                                           LBL_Profile_ID_Tag = LBL_Profile_ID_Tag, &
                                           REG_Profile_ID_Tag = REG_Profile_ID_Tag, &
                                           Title              = Title, &
                                           History            = History, &
                                           Sensor_Name        = Sensor_Name, &
                                           Platform_Name      = Platform_Name, &
                                           Comment            = Comment, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- DEFINE THE VARIABLES --                       #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! NCEP Sensor ID
    ! --------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                NCEP_SENSOR_ID_VARNAME, &
                                NCEP_SENSOR_ID_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//NCEP_SENSOR_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    NCEP_SENSOR_ID_LONGNAME, &
                                    Variable_Name = NCEP_SENSOR_ID_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    NCEP_SENSOR_ID_FILLVALUE, &
                                    Variable_Name = NCEP_SENSOR_ID_VARNAME )

    IF ( Status1 /= SUCCESS .OR. Status2 /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NCEP_SENSOR_ID_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! WMO satellite ID
    ! ----------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                WMO_SATELLITE_ID_VARNAME, &
                                WMO_SATELLITE_ID_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//WMO_SATELLITE_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    WMO_SATELLITE_ID_LONGNAME, &
                                    Variable_Name = WMO_SATELLITE_ID_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    WMO_SATELLITE_ID_FILLVALUE, &
                                    Variable_Name = WMO_SATELLITE_ID_VARNAME )

    IF ( Status1 /= SUCCESS .OR. Status2 /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SATELLITE_ID_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! WMO Sensor ID
    ! -------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                WMO_SENSOR_ID_VARNAME, &
                                WMO_SENSOR_ID_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//WMO_SENSOR_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    WMO_SENSOR_ID_LONGNAME, &
                                    Variable_Name = WMO_SENSOR_ID_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    WMO_SENSOR_ID_FILLVALUE, &
                                    Variable_Name = WMO_SENSOR_ID_VARNAME )

    IF ( Status1 /= SUCCESS .OR. Status2 /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SENSOR_ID_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------
    ! Channel list
    ! ------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                CHANNEL_LIST_VARNAME, &
                                CHANNEL_LIST_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//CHANNEL_LIST_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    CHANNEL_LIST_LONGNAME, &
                                    Variable_Name = CHANNEL_LIST_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    CHANNEL_LIST_FILLVALUE, &
                                    Variable_Name = CHANNEL_LIST_VARNAME )

    IF ( Status1 /= SUCCESS .OR. Status2 /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CHANNEL_LIST_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------
    ! Central frequencies
    ! -------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//FREQUENCY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    FREQUENCY_LONGNAME, &
                                    Variable_Name = FREQUENCY_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    FREQUENCY_UNITS, &
                                    Variable_Name = FREQUENCY_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    FREQUENCY_FILLVALUE, &
                                    Variable_Name = FREQUENCY_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------
    ! Angle list
    ! ----------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                ANGLE_LIST_VARNAME, &
                                ANGLE_LIST_TYPE, &
                                dimids = Angle_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ANGLE_LIST_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    ANGLE_LIST_LONGNAME, &
                                    Variable_Name = ANGLE_LIST_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    ANGLE_LIST_FILLVALUE, &
                                    Variable_Name = ANGLE_LIST_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ANGLE_LIST_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------
    ! Profile list
    ! ------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                PROFILE_LIST_VARNAME, &
                                PROFILE_LIST_TYPE, &
                                dimids = Profile_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//PROFILE_LIST_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    PROFILE_LIST_LONGNAME, &
                                    Variable_Name = PROFILE_LIST_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    PROFILE_LIST_FILLVALUE, &
                                    Variable_Name = PROFILE_LIST_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PROFILE_LIST_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Molecule set list
    ! -----------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                MOLECULE_SET_LIST_VARNAME, &
                                MOLECULE_SET_LIST_TYPE, &
                                dimids = Molecule_Set_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//MOLECULE_SET_LIST_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    MOLECULE_SET_LIST_LONGNAME, &
                                    Variable_Name = MOLECULE_SET_LIST_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    MOLECULE_SET_LIST_FILLVALUE, &
                                    Variable_Name = MOLECULE_SET_LIST_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MOLECULE_SET_LIST_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! --------------------------
    ! INT_WATER_VAPOR data
    ! --------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                INT_WATER_VAPOR_VARNAME, &
                                INT_WATER_VAPOR_TYPE, &
                                dimids = (/Profile_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//INT_WATER_VAPOR_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    INT_WATER_VAPOR_LONGNAME, &
                                    Variable_Name = INT_WATER_VAPOR_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    PWV_UNITS, &
                                    Variable_Name = INT_WATER_VAPOR_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    INT_WATER_VAPOR_FILLVALUE, &
                                    Variable_Name = INT_WATER_VAPOR_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//INT_WATER_VAPOR_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    
    ! -----------------------
    ! LBL Optical Depth data
    ! -----------------------
    
    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                LBL_OD_VARNAME, &
                                OD_TYPE, &
                                dimids = (/ Layer_DimID,           &
                                            Channel_DimID,         &
                                            Angle_DimID,           &
                                            Profile_DimID,         &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LBL_OD_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    
    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    LBL_OD_LONGNAME, &
                                    Variable_Name = LBL_OD_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    OD_UNITS, &
                                    Variable_Name = LBL_OD_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    OD_FILLVALUE, &
                                    Variable_Name = LBL_OD_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LBL_OD_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    
    ! -----------------------------
    ! Regression Optical Depth data
    ! -----------------------------
    
    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                REG_OD_VARNAME, &
                                OD_TYPE, &
                                dimids = (/ Layer_DimID,           &
                                            Channel_DimID,         &
                                            Angle_DimID,           &
                                            Profile_DimID,         &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//REG_OD_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    
    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    REG_OD_LONGNAME, &
                                    Variable_Name = REG_OD_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    OD_UNITS, &
                                    Variable_Name = REG_OD_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    OD_FILLVALUE, &
                                    Variable_Name = REG_OD_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//REG_OD_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
        
    ! ----------------------------------
    ! (LBL-REG) Optical depth difference
    ! ----------------------------------
    
    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                dOD_VARNAME, &
                                OD_TYPE, &
                                dimids = (/ Layer_DimID,           &
                                            Channel_DimID,         &
                                            Angle_DimID,           &
                                            Profile_DimID,         &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//dOD_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    
    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    dOD_LONGNAME, &
                                    Variable_Name = dOD_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    OD_UNITS, &
                                    Variable_Name = dOD_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    OD_FILLVALUE, &
                                    Variable_Name = dOD_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//dOD_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
        
    ! --------------------------
    ! LBL TOA transmittance data
    ! --------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                LBL_TAU_VARNAME, &
                                TAU_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Angle_DimID, &
                                            Profile_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LBL_TAU_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    LBL_TAU_LONGNAME, &
                                    Variable_Name = LBL_TAU_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    TAU_UNITS, &
                                    Variable_Name = LBL_TAU_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    TAU_FILLVALUE, &
                                    Variable_Name = LBL_TAU_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LBL_TAU_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------------
    ! REG TOA transmittance data
    ! --------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                REG_TAU_VARNAME, &
                                TAU_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Angle_DimID, &
                                            Profile_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//REG_TAU_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    REG_TAU_LONGNAME, &
                                    Variable_Name = REG_TAU_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    TAU_UNITS, &
                                    Variable_Name = REG_TAU_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    TAU_FILLVALUE, &
                                    Variable_Name = REG_TAU_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//REG_TAU_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------
    ! Mean LBL-REG transmittance data
    ! -------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                MEAN_DTAU_VARNAME, &
                                TAU_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//MEAN_DTAU_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    MEAN_DTAU_LONGNAME, &
                                    Variable_Name = MEAN_DTAU_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    TAU_UNITS, &
                                    Variable_Name = MEAN_DTAU_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    TAU_FILLVALUE, &
                                    Variable_Name = MEAN_DTAU_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MEAN_DTAU_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------
    ! RMS LBL-REG transmittance data
    ! ------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                RMS_DTAU_VARNAME, &
                                TAU_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//RMS_DTAU_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    RMS_DTAU_LONGNAME, &
                                    Variable_Name = RMS_DTAU_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    TAU_UNITS, &
                                    Variable_Name = RMS_DTAU_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    TAU_FILLVALUE, &
                                    Variable_Name = RMS_DTAU_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RMS_DTAU_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! Mean LBL-REG transmittance data by angle
    ! ----------------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                MEAN_DTAU_BY_ANGLE_VARNAME, &
                                TAU_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Angle_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//MEAN_DTAU_BY_ANGLE_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    MEAN_DTAU_BY_ANGLE_LONGNAME, &
                                    Variable_Name = MEAN_DTAU_BY_ANGLE_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    TAU_UNITS, &
                                    Variable_Name = MEAN_DTAU_BY_ANGLE_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    TAU_FILLVALUE, &
                                    Variable_Name = MEAN_DTAU_BY_ANGLE_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MEAN_DTAU_BY_ANGLE_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! RMS LBL-REG transmittance data by angle
    ! ---------------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                RMS_DTAU_BY_ANGLE_VARNAME, &
                                TAU_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Angle_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//RMS_DTAU_BY_ANGLE_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    RMS_DTAU_BY_ANGLE_LONGNAME, &
                                    Variable_Name = RMS_DTAU_BY_ANGLE_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    TAU_UNITS, &
                                    Variable_Name = RMS_DTAU_BY_ANGLE_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    TAU_FILLVALUE, &
                                    Variable_Name = RMS_DTAU_BY_ANGLE_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RMS_DTAU_BY_ANGLE_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------------
    ! LBL TOA brightness temperature data
    ! -----------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                LBL_BT_VARNAME, &
                                BT_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Angle_DimID, &
                                            Profile_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LBL_BT_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    LBL_BT_LONGNAME, &
                                    Variable_Name = LBL_BT_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    BT_UNITS, &
                                    Variable_Name = LBL_BT_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    BT_FILLVALUE, &
                                    Variable_Name = LBL_BT_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LBL_BT_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------------
    ! REG TOA brightness temperature data
    ! -----------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                REG_BT_VARNAME, &
                                BT_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Angle_DimID, &
                                            Profile_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//REG_BT_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    REG_BT_LONGNAME, &
                                    Variable_Name = REG_BT_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    BT_UNITS, &
                                    Variable_Name = REG_BT_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    BT_FILLVALUE, &
                                    Variable_Name = REG_BT_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//REG_BT_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! Mean LBL-REG brightness temperature data
    ! ----------------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                MEAN_DBT_VARNAME, &
                                BT_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//MEAN_DBT_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    MEAN_DBT_LONGNAME, &
                                    Variable_Name = MEAN_DBT_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    BT_UNITS, &
                                    Variable_Name = MEAN_DBT_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    BT_FILLVALUE, &
                                    Variable_Name = MEAN_DBT_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MEAN_DBT_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! RMS LBL-REG brightness temperature data
    ! ---------------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                RMS_DBT_VARNAME, &
                                BT_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//RMS_DBT_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    RMS_DBT_LONGNAME, &
                                    Variable_Name = RMS_DBT_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    BT_UNITS, &
                                    Variable_Name = RMS_DBT_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    BT_FILLVALUE, &
                                    Variable_Name = RMS_DBT_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RMS_DBT_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------------------------
    ! Mean LBL-REG brightness temperature data by angle
    ! -------------------------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                MEAN_DBT_BY_ANGLE_VARNAME, &
                                BT_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Angle_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//MEAN_DBT_BY_ANGLE_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    MEAN_DBT_BY_ANGLE_LONGNAME, &
                                    Variable_Name = MEAN_DBT_BY_ANGLE_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    BT_UNITS, &
                                    Variable_Name = MEAN_DBT_BY_ANGLE_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    BT_FILLVALUE, &
                                    Variable_Name = MEAN_DBT_BY_ANGLE_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MEAN_DBT_BY_ANGLE_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------------------------
    ! RMS LBL-REG brightness temperature data by angle
    ! ------------------------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                RMS_DBT_BY_ANGLE_VARNAME, &
                                BT_TYPE, &
                                dimids = (/ Channel_DimID, &
                                            Angle_DimID, &
                                            Molecule_Set_DimID /), &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//RMS_DBT_BY_ANGLE_varNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    RMS_DBT_BY_ANGLE_LONGNAME, &
                                    Variable_Name = RMS_DBT_BY_ANGLE_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    BT_UNITS, &
                                    Variable_Name = RMS_DBT_BY_ANGLE_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    BT_FILLVALUE, &
                                    Variable_Name = RMS_DBT_BY_ANGLE_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RMS_DBT_BY_ANGLE_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                -- TAKE THE NETCDF FILE OUT OF DEFINE MODE --             #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_ENDDEF( NC_fileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error taking file '//TRIM( NC_Filename )// &
                            ' out of define mode - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

  END FUNCTION Create_CRTMstats_netCDF







  FUNCTION Inquire_CRTMstats_netCDF( NC_Filename,         &  ! Input
                                      n_Layers,           &  ! Optional output
                                      n_Channels,         &  ! Optional output
                                      n_Angles,           &  ! Optional output
                                      n_Profiles,         &  ! Optional output
                                      n_Molecule_Sets,    &  ! Optional output
                                      LBL_Profile_ID_Tag, &  ! Optional output
                                      REG_Profile_ID_Tag, &  ! Optional output
                                      Title,              &  ! Optional output
                                      History,            &  ! Optional output
                                      Sensor_Name,        &  ! Optional output
                                      Platform_Name,      &  ! Optional output
                                      Comment,            &  ! Optional output
                                      RCS_Id,             &  ! Revision control
                                      Message_Log )       &  ! Error messaging
                                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )  :: NC_Filename

    ! -- Optional output
    INTEGER,         OPTIONAL, INTENT( OUT ) :: n_Layers
    INTEGER,         OPTIONAL, INTENT( OUT ) :: n_Channels
    INTEGER,         OPTIONAL, INTENT( OUT ) :: n_Angles
    INTEGER,         OPTIONAL, INTENT( OUT ) :: n_Profiles
    INTEGER,         OPTIONAL, INTENT( OUT ) :: n_Molecule_Sets
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: LBL_Profile_ID_Tag
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: REG_Profile_ID_Tag
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: Sensor_Name
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: Platform_Name
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: Comment

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_CRTMstats_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Close_Status

    INTEGER :: k, l, m, i, j



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_CRTMstats_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF CRTMstats data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- GET THE DIMENSIONS --                        #
    !#--------------------------------------------------------------------------#
    
    ! --------------------
    ! The number of layers
    ! --------------------
    
    Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                         LAYER_DIMNAME, &
                                         k, &
                                         Message_Log = Message_Log )
                                               
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining '//LAYER_DIMNAME//&
                            ' dimension from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF                                   
    
    ! ----------------------
    ! The number of channels
    ! ----------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                         CHANNEL_DIMNAME, &
                                         l, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining '//CHANNEL_DIMNAME//&
                            ' dimension from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! --------------------
    ! The number of angles
    ! --------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                         ANGLE_DIMNAME, &
                                         i, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining '//ANGLE_DIMNAME//&
                            ' dimension from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ----------------------
    ! The number of profiles
    ! ----------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                         PROFILE_DIMNAME, &
                                         m, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining '//PROFILE_DIMNAME//&
                            ' dimension from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------
    ! The number of molecule sets
    ! ---------------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                         MOLECULE_SET_DIMNAME, &
                                         j, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining '//MOLECULE_SET_DIMNAME//&
                            ' dimension from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- SET THE DIMENSION RETURN VALUES --                  #
    !#--------------------------------------------------------------------------#
    
    IF ( PRESENT( n_Layers        ) ) n_Layers        = k
    IF ( PRESENT( n_Channels      ) ) n_Channels      = l
    IF ( PRESENT( n_Angles        ) ) n_Angles        = i
    IF ( PRESENT( n_Profiles      ) ) n_Profiles      = m
    IF ( PRESENT( n_Molecule_Sets ) ) n_Molecule_Sets = j



    !#--------------------------------------------------------------------------#
    !#                     -- GET THE GLOBAL ATTRIBUTES --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_CRTMstats_GAtts( TRIM( NC_Filename ), &
                                          NC_fileID, &
                                          LBL_Profile_ID_Tag = LBL_Profile_ID_Tag, &
                                          REG_Profile_ID_Tag = REG_Profile_ID_Tag, &
                                          Title              = Title, &
                                          History            = History, &
                                          Sensor_Name        = Sensor_Name, &
                                          Platform_Name      = Platform_Name, &
                                          Comment            = Comment, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading global attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_CRTMstats_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF CRTMstats data file '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Inquire_CRTMstats_netCDF






  FUNCTION Write_CRTMstats_netCDF( NC_Filename,   &  ! Input
                                    CRTMstats,    &  ! Input
                                    Title,         &  ! Optional input
                                    History,       &  ! Optional input
                                    Sensor_Name,   &  ! Optional input
                                    Platform_Name, &  ! Optional input
                                    Comment,       &  ! Optional input
                                    Quiet,         &  ! Optional input
                                    RCS_Id,        &  ! Revision control
                                    Message_Log )  &  ! Error messaging
                                  RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: NC_Filename
    TYPE( CRTMstats_type ),  INTENT( IN )  :: CRTMstats

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Comment
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_CRTMstats_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy
    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status

    INTEGER :: l, i, m, j



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- CREATE THE OUTPUT DATA FILE --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Create_CRTMstats_netCDF( TRIM( NC_Filename ),        &  ! Input
                                             CRTMstats%n_Layers,        &  ! Input
                                             CRTMstats%n_Channels,      &  ! Input
                                             CRTMstats%n_Angles,        &  ! Input
                                             CRTMstats%n_Profiles,      &  ! Input
                                             CRTMstats%n_Molecule_Sets, &  ! Input
                                             NC_FileID,                &  ! Output 
                                             LBL_Profile_ID_Tag = TRIM( CRTMstats%LBL_Profile_ID_Tag ), &  ! Optional input
                                             REG_Profile_ID_Tag = TRIM( CRTMstats%REG_Profile_ID_Tag ), &  ! Optional input
                                             Title              = Title,         &  ! Optional input
                                             History            = History,       &  ! Optional input
                                             Sensor_Name        = Sensor_Name,   &  ! Optional input
                                             Platform_Name      = Platform_Name, &  ! Optional input
                                             Comment            = Comment,       &  ! Optional input
                                             RCS_Id  = RCS_Id,         &  ! Revision control
                                             Message_Log = Message_Log )  ! Error messaging


    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error creating output netCDF CRTMstats file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE DATA ITEMS --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! The NCEP_SENSOR_ID
    ! ------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        NCEP_SENSOR_ID_VARNAME, &
                                        CRTMstats%NCEP_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error writing '//NCEP_SENSOR_ID_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! The WMO_SATELLITE_ID
    ! --------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WMO_SATELLITE_ID_VARNAME, &
                                        CRTMstats%WMO_Satellite_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error writing '//WMO_SATELLITE_ID_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! The WMO_SENSOR_ID
    ! -----------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WMO_SENSOR_ID_VARNAME, &
                                        CRTMstats%WMO_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error writing '//WMO_SENSOR_ID_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------
    ! The sensor channel list
    ! -----------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        CHANNEL_LIST_VARNAME, &
                                        CRTMstats%Sensor_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CHANNEL_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -----------------------
    ! The central frequencies
    ! -----------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        FREQUENCY_VARNAME, &
                                        CRTMstats%Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ----------------------------
    ! The secant zenith angle list
    ! ----------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        ANGLE_LIST_VARNAME, &
                                        CRTMstats%Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ANGLE_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! ----------------------------
    ! The integrated water vapor
    ! ----------------------------
    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        INT_WATER_VAPOR_VARNAME, &
                                        CRTMstats%Int_Water_Vapor )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//INT_WATER_VAPOR_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    ! ----------------
    ! The profile list
    ! ----------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        PROFILE_LIST_VARNAME, &
                                        CRTMstats%Profile )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PROFILE_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------
    ! The molecule set list
    ! ---------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        MOLECULE_SET_LIST_VARNAME, &
                                        CRTMstats%Molecule_Set )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MOLECULE_SET_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! --------------------------
    ! The LBL Optical Depth data
    ! --------------------------
    
    Error_Status = Put_netCDF_Variable( NC_fileID,       & 
                                        LBL_OD_VARNAME,  &
                                        CRTMstats%LBL_OD )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LBL_OD_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
                                        
    ! ---------------------------------
    ! The Regression Optical Depth data
    ! ---------------------------------
    
    Error_Status = Put_netCDF_Variable( NC_fileID,       &
                                        REG_OD_VARNAME,  &
                                        CRTMstats%REG_OD )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//REG_OD_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
                                        
    ! ------------------------------------
    ! (LBL - REG) Optical Depth difference
    ! ------------------------------------
    
    Error_Status = Put_netCDF_Variable( NC_fileID,      &
                                        dOD_VARNAME,    &
                                        CRTMstats%dOD   )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//dOD_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    
    ! ------------------------------
    ! The LBL TOA transmittance data
    ! ------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        LBL_TAU_VARNAME, &
                                        CRTMstats%LBL_Tau )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LBL_TAU_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------
    ! The REG TOA transmittance data
    ! ------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        REG_TAU_VARNAME, &
                                        CRTMstats%REG_Tau )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//REG_TAU_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ----------------------------------------------
    ! The mean LBL-REG transmittance difference data
    ! ----------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        MEAN_DTAU_VARNAME, &
                                        CRTMstats%Mean_dTau )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MEAN_DTAU_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------------
    ! The RMS LBL-REG transmittance difference data
    ! ---------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        RMS_DTAU_VARNAME, &
                                        CRTMstats%RMS_dTau )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RMS_DTAU_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -------------------------------------------------------
    ! The mean LBL-REG transmittance difference data by angle
    ! -------------------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        MEAN_DTAU_BY_ANGLE_VARNAME, &
                                        CRTMstats%Mean_dTau_by_Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MEAN_DTAU_BY_ANGLE_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------------------------------
    ! The RMS LBL-REG transmittance difference data by angle
    ! ------------------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        RMS_DTAU_BY_ANGLE_VARNAME, &
                                        CRTMstats%RMS_dTau_by_Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RMS_DTAU_BY_ANGLE_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! The LBL TOA brightness temperature data
    ! ---------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        LBL_BT_VARNAME, &
                                        CRTMstats%LBL_BT )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LBL_BT_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! The REG TOA brightness temperature data
    ! ---------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        REG_BT_VARNAME, &
                                        CRTMstats%REG_BT )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//REG_BT_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -------------------------------------------------------
    ! The mean LBL-REG brightness temperature difference data
    ! -------------------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        MEAN_DBT_VARNAME, &
                                        CRTMstats%Mean_dBT )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MEAN_DBT_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------------------------------
    ! The RMS LBL-REG brightness temperature difference data
    ! ------------------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        RMS_DBT_VARNAME, &
                                        CRTMstats%RMS_dBT )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RMS_DBT_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ----------------------------------------------------------------
    ! The mean LBL-REG brightness temperature difference data by angle
    ! ----------------------------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        MEAN_DBT_BY_ANGLE_VARNAME, &
                                        CRTMstats%Mean_dBT_by_Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MEAN_DBT_BY_ANGLE_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------------------------------
    ! The RMS LBL-REG brightness temperature difference data by angle
    ! ---------------------------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        RMS_DBT_BY_ANGLE_VARNAME, &
                                        CRTMstats%RMS_dBT_by_Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RMS_DBT_BY_ANGLE_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Close_CRTMstats_netCDF( NC_FileID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF CRTMstats data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Information_CRTMstats( CRTMstats, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_CRTMstats_netCDF






  FUNCTION Read_CRTMstats_netCDF( NC_Filename,   &  ! Input
                                   CRTMstats,    &  ! Output
                                   Quiet,         &  ! Optional input
                                   Title,         &  ! Optional output
                                   History,       &  ! Optional output
                                   Sensor_Name,   &  ! Optional output
                                   Platform_Name, &  ! Optional output
                                   Comment,       &  ! Optional output
                                   RCS_Id,        &  ! Revision control
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: NC_Filename

    ! -- Output
    TYPE( CRTMstats_type ),  INTENT( IN OUT ) :: CRTMstats

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Quiet

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Comment

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_CRTMstats_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status
    
    INTEGER :: n_Layers
    INTEGER :: n_Channels
    INTEGER :: n_Angles
    INTEGER :: n_Profiles
    INTEGER :: n_Molecule_Sets



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#   -- GET THE DIMENSION VALUES AND ALLOCATE THE CRTMstats STRUCTURE --   #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Read the dimension values
    ! -------------------------

    Error_Status = Inquire_CRTMstats_netCDF( TRIM( NC_Filename ), &
                                              n_Layers        = n_Layers,   &
                                              n_Channels      = n_Channels, &
                                              n_Angles        = n_Angles, &
                                              n_Profiles      = n_Profiles, &
                                              n_Molecule_Sets = n_Molecule_Sets, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining CRTMstats dimensions from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_CRTMstats(  n_Layers,   &
                                        n_Channels, &
                                        n_Angles, &
                                        n_Profiles, &
                                        n_Molecule_Sets, &
                                        CRTMstats, &
                                        Message_Log = Message_Log )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error occurred allocating CRTMstats structure.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_CRTMstats_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF CRTMstats data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE CRTMstats DATA --                     #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! The NCEP Sensor ID
    ! ------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        NCEP_SENSOR_ID_VARNAME, &
                                        CRTMstats%NCEP_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//NCEP_SENSOR_ID_VARNAME//&
                            ' data from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! --------------------
    ! The WMO satellite ID
    ! --------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        WMO_SATELLITE_ID_VARNAME, &
                                        CRTMstats%WMO_Satellite_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//WMO_SATELLITE_ID_VARNAME//&
                            ' data from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -----------------
    ! The WMO Sensor ID
    ! -----------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        WMO_SENSOR_ID_VARNAME, &
                                        CRTMstats%WMO_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//WMO_SENSOR_ID_VARNAME//&
                            ' data from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    ! ----------------
    ! The channel list
    ! ----------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        CHANNEL_LIST_VARNAME, &
                                        CRTMstats%Sensor_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//CHANNEL_LIST_VARNAME//&
                            ' data from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! --------------
    ! The angle list
    ! --------------

      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          ANGLE_LIST_VARNAME, &
                                          CRTMstats%Angle )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//ANGLE_LIST_VARNAME//&
                              ' data from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF


    ! ----------------
    ! The profile list
    ! ----------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        PROFILE_LIST_VARNAME, &
                                        CRTMstats%Profile )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//PROFILE_LIST_VARNAME//&
                            ' data from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    
    ! -------------------
    ! The PWV by profile
    ! -------------------
    
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        INT_WATER_VAPOR_VARNAME, &
                                        CRTMstats%Int_Water_Vapor )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//INT_WATER_VAPOR_VARNAME//&
                            ' data from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------
    ! The molecule set list
    ! ---------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        MOLECULE_SET_LIST_VARNAME, &
                                        CRTMstats%Molecule_Set )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//MOLECULE_SET_LIST_VARNAME//&
                            ' data from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -----------------------
    ! The central frequencies
    ! -----------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        FREQUENCY_VARNAME, &
                                        CRTMstats%Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//FREQUENCY_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! --------------------------
    ! The LBL Optical Depth data
    ! --------------------------
    
    Error_Status = Get_netCDF_Variable( NC_fileID,       &
                                        LBL_OD_VARNAME,  &
                                        CRTMstats%LBL_OD )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LBL_OD_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    
    ! ---------------------------------
    ! The Regression Optical Depth data
    ! ---------------------------------
    
    Error_Status = Get_netCDF_Variable( NC_fileID,       &
                                        REG_OD_VARNAME,  &
                                        CRTMstats%REG_OD )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//REG_OD_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
           
    ! ------------------------------------
    ! (LBL - REG) Optical Depth difference
    ! ------------------------------------    
    
    Error_Status = Get_netCDF_Variable( NC_fileID,       &
                                        dOD_VARNAME,     &
                                        CRTMstats%dOD    )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//dOD_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    
    ! ------------------------------
    ! The LBL TOA transmittance data
    ! ------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        LBL_TAU_VARNAME, &
                                        CRTMstats%LBL_Tau )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LBL_TAU_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------
    ! The REG TOA transmittance data
    ! ------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        REG_TAU_VARNAME, &
                                        CRTMstats%REG_Tau )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//REG_TAU_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ----------------------------------------------
    ! The mean LBL-REG transmittance difference data
    ! ----------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        MEAN_DTAU_VARNAME, &
                                        CRTMstats%Mean_dTau )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//MEAN_DTAU_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------------
    ! The RMS LBL-REG transmittance difference data
    ! ---------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        RMS_DTAU_VARNAME, &
                                        CRTMstats%RMS_dTau )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//RMS_DTAU_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -------------------------------------------------------
    ! The mean LBL-REG transmittance difference data by angle
    ! -------------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        MEAN_DTAU_BY_ANGLE_VARNAME, &
                                        CRTMstats%Mean_dTau_by_Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//MEAN_DTAU_BY_ANGLE_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------------------------------
    ! The RMS LBL-REG transmittance difference data by angle
    ! ------------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        RMS_DTAU_BY_ANGLE_VARNAME, &
                                        CRTMstats%RMS_dTau_by_Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//RMS_DTAU_BY_ANGLE_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! The LBL TOA brightness temperature data
    ! ---------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        LBL_BT_VARNAME, &
                                        CRTMstats%LBL_BT )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LBL_BT_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! The REG TOA brightness temperature data
    ! ---------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        REG_BT_VARNAME, &
                                        CRTMstats%REG_BT )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//REG_BT_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -------------------------------------------------------
    ! The mean LBL-REG brightness temperature difference data
    ! -------------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        MEAN_DBT_VARNAME, &
                                        CRTMstats%Mean_dBT )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//MEAN_DBT_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------------------------------
    ! The RMS LBL-REG brightness temperature difference data
    ! ------------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        RMS_DBT_VARNAME, &
                                        CRTMstats%RMS_dBT )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//RMS_DBT_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ----------------------------------------------------------------
    ! The mean LBL-REG brightness temperature difference data by angle
    ! ----------------------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        MEAN_DBT_BY_ANGLE_VARNAME, &
                                        CRTMstats%Mean_dBT_by_Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//MEAN_DBT_BY_ANGLE_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------------------------------
    ! The RMS LBL-REG brightness temperature difference data by angle
    ! ---------------------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        RMS_DBT_BY_ANGLE_VARNAME, &
                                        CRTMstats%RMS_dBT_by_Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//RMS_DBT_BY_ANGLE_VARNAME//&
                            ' from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- FILL THE PROFILE ID DESCRIPTORS AND GET --              #
    !#               -- THE GLOBAL ATTRIBUTES IF REQUIRED       --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_CRTMstats_GAtts( TRIM( NC_Filename ), &
                                          NC_FileID, &
                                          LBL_Profile_ID_Tag = CRTMstats%LBL_Profile_ID_Tag, &
                                          REG_Profile_ID_Tag = CRTMstats%REG_Profile_ID_Tag, &
                                          Title         = Title, &
                                          History       = History, &
                                          Sensor_Name   = Sensor_Name, &
                                          Platform_Name = Platform_Name, &
                                          Comment       = Comment, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining CRTMstats global attributes from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF




    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_CRTMstats_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF CRTMstats data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- COUNT THE NUMBER OF SENSORS --                      #
    !#--------------------------------------------------------------------------#

    CALL Count_CRTMstats_Sensors( CRTMstats, Use_WMO_ID = SET )



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Information_CRTMstats( CRTMstats, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_CRTMstats_netCDF

END MODULE CRTMstats_netCDF_IO



