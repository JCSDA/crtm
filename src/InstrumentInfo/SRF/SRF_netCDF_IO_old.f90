
MODULE SRF_netCDF_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE SRF_Define

  USE netcdf
  USE netCDF_Utility,  Open_SRF_netCDF =>  Open_netCDF, &
                      Close_SRF_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

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

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: SRF_netCDF_IO.f90 774 2007-07-24 18:24:06Z paul.vandelst@noaa.gov $'

  ! -- Invalid flag
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Numeric constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER ::  ONE = 1.0_fp_kind

  ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: TITLE_GATTNAME         = 'title' 
  CHARACTER( * ), PRIVATE, PARAMETER :: HISTORY_GATTNAME       = 'history' 
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_NAME_GATTNAME   = 'sensor_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: PLATFORM_NAME_GATTNAME = 'platform_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: COMMENT_GATTNAME       = 'comment' 

  ! -- Static dimension names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_DIMNAME = 'n_channels'

  ! -- Static variable names. Case sensitive.
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_VARNAME   = 'NCEP_Sensor_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_VARNAME = 'WMO_Satellite_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_VARNAME    = 'WMO_Sensor_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_LIST_VARNAME     = 'channel_list'
  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_VARNAME  = 'begin_frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_VARNAME    = 'end_frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: INTEGRATED_SRF_VARNAME   = 'integrated_srf'
  CHARACTER( * ), PRIVATE, PARAMETER :: SUMMATION_SRF_VARNAME    = 'summation_srf'

  ! -- Variable long name attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_LONGNAME   = &
'ID used at NOAA/NCEP/EMC to identify a satellite/sensor (-1 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_LONGNAME = &
'WMO code for identifying satellite platforms (1023 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_LONGNAME    = &
'WMO code for identifying a satellite sensor (2047 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_LIST_LONGNAME     = &
'List of sensor channel numbers associated with the SRF data'
  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_LONGNAME  = &
'Begin frequencies of SRF response data'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_LONGNAME    = &
'End frequencies of SRF response data'
  CHARACTER( * ), PRIVATE, PARAMETER :: INTEGRATED_SRF_LONGNAME  = &
'Integrated spectral response using Simpsons rule'
  CHARACTER( * ), PRIVATE, PARAMETER :: SUMMATION_SRF_LONGNAME    = &
'Integrated spectral response by summation: = SUM( response ) * df'

  ! -- Variable units attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_UNITS       = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_UNITS     = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_UNITS        = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_LIST_UNITS         = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_UNITS            = 'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: RESPONSE_UNITS             = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: INTEGRATED_SRF_UNITS       = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: SUMMATION_SRF_UNITS        = 'N/A'

  ! -- Variable _FillValue attribute.
  CHARACTER( * ),  PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  INTEGER,         PRIVATE, PARAMETER :: NCEP_SENSOR_ID_FILLVALUE    = -1
  INTEGER,         PRIVATE, PARAMETER :: WMO_SATELLITE_ID_FILLVALUE  = -1
  INTEGER,         PRIVATE, PARAMETER :: WMO_SENSOR_ID_FILLVALUE     = -1
  INTEGER,         PRIVATE, PARAMETER :: CHANNEL_LIST_FILLVALUE      = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: FREQUENCY_FILLVALUE         = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: RESPONSE_FILLVALUE          = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: INTEGRATED_SRF_FILLVALUE    = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: SUMMATION_SRF_FILLVALUE     = -1.0_fp_kind

  ! -- Variable netCDF datatypes
  INTEGER,        PRIVATE, PARAMETER :: NCEP_SENSOR_ID_TYPE   = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: WMO_SATELLITE_ID_TYPE = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: WMO_SENSOR_ID_TYPE    = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: CHANNEL_LIST_TYPE     = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: FREQUENCY_TYPE        = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: RESPONSE_TYPE         = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: INTEGRATED_SRF_TYPE   = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: SUMMATION_SRF_TYPE    = NF90_DOUBLE


CONTAINS







  SUBROUTINE Create_Variable_Names( Channel,        &  ! Input
                                    Channel_Name,   &  ! Optional Output
                                    Dimension_Name, &  ! Optional Output
                                    Variable_Name   )  ! Optional Output

    INTEGER,                  INTENT( IN  ) :: Channel
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Channel_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Dimension_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Variable_Name

    INTEGER,         PARAMETER :: MAX_CHAR = 5
    CHARACTER( * ),  PARAMETER :: MAX_FMT  = '(i5)'
    REAL( fp_kind ), PARAMETER :: TEN = 10.0_fp_kind

    INTEGER :: Max_Channel
    CHARACTER( 256 ) :: C_String
    CHARACTER( 256 ) :: D_String
    CHARACTER( 256 ) :: V_String

    ! -- Determine the maximum allowed channel + 1
    Max_Channel = INT( TEN**MAX_CHAR )

    ! -- Fill the channel string. Output is 'XXXXX' if
    ! -- an invalid channel is supplied.
    IF ( Channel > 0 .AND. Channel < Max_Channel ) THEN
      WRITE( C_String, FMT = MAX_FMT ) Channel
    ELSE
      C_String = REPEAT( 'X', MAX_CHAR )
    END IF

    C_String     = ADJUSTL( C_String )
    IF ( PRESENT( Channel_Name ) ) Channel_Name = C_String

    ! -- Create the dimension name
    D_String = 'channel_'//TRIM( C_String )//'_n_points'
    IF ( PRESENT( Dimension_Name ) ) Dimension_Name = D_String

    ! -- Create the variable name
    V_String = 'channel_'//TRIM( C_String )//'_response'
    IF ( PRESENT( Variable_Name ) ) Variable_Name = V_String

  END SUBROUTINE Create_Variable_Names






  FUNCTION Write_SRF_GAtts( NC_Filename,   &  ! Input
                            NC_FileID,     &  ! Input
                            Title,         &  ! Optional input
                            History,       &  ! Optional input
                            Sensor_Name,   &  ! Optional input
                            Platform_Name, &  ! Optional input
                            Comment,       &  ! Optional input
                            Message_Log )  &  ! Error messaging
                          RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN ) :: NC_Filename
    INTEGER,                  INTENT( IN ) :: NC_FileID

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Comment

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SRF_GAtts'

    ! -- "Internal" global attributes
    CHARACTER( * ), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER( * ), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    CHARACTER(  8 ) :: cdate
    CHARACTER( 10 ) :: ctime
    CHARACTER(  5 ) :: czone



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#              -- WRITE THE "INTERNAL" GLOBAL ATTRIBUTES --                #
    !#--------------------------------------------------------------------------#

    ! -----------
    ! Software ID
    ! -----------

    NF90_Status = NF90_PUT_ATT( NC_FileID,   &
                                NF90_GLOBAL, &
                                WRITE_MODULE_HISTORY_GATTNAME,    &
                                MODULE_RCS_ID )


    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WRITE_MODULE_HISTORY_GATTNAME//&
                            ' attribute to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -------------
    ! Creation date
    ! -------------

    CALL DATE_AND_TIME( cdate, ctime, czone )

    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                CREATION_DATE_AND_TIME_GATTNAME, &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CREATION_DATE_AND_TIME_GATTNAME//&
                            ' attribute to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- DEFINE THE USER ACCESSIBLE GLOBAL ATTRIBUTES --            #
    !#--------------------------------------------------------------------------#

    ! -----
    ! Title
    ! -----

    IF ( PRESENT( Title ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TITLE_GATTNAME, &
                                  TRIM( Title ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//TITLE_GATTNAME//' attribute to '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------
    ! History
    ! -------

    IF ( PRESENT( History ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  HISTORY_GATTNAME, &
                                  TRIM( History ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//HISTORY_GATTNAME//' attribute to '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -----------
    ! Sensor name
    ! -----------

    IF ( PRESENT( Sensor_Name ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  SENSOR_NAME_GATTNAME, &
                                  TRIM( Sensor_Name ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//SENSOR_NAME_GATTNAME//' attribute to '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------------
    ! Platform name
    ! -------------

    IF ( PRESENT( Platform_Name ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID,    &
                                  NF90_GLOBAL,   &
                                  PLATFORM_NAME_GATTNAME, &
                                  TRIM( Platform_Name ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//PLATFORM_NAME_GATTNAME//' attribute to '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------
    ! Comment
    ! -------

    IF ( PRESENT( Comment ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  COMMENT_GATTNAME, &
                                  TRIM( Comment ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//COMMENT_GATTNAME//' attribute to '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF

  END FUNCTION Write_SRF_GAtts






  FUNCTION Read_SRF_GAtts( NC_Filename,   &  ! Input
                           NC_FileID,     &  ! Input
                           Title,         &  ! Optional output
                           History,       &  ! Optional output
                           Sensor_Name,   &  ! Optional output
                           Platform_Name, &  ! Optional output
                           Comment,       &  ! Optional output
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
    INTEGER,                  INTENT( IN )  :: NC_FileID

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Comment

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SRF_GAtts'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                     -- READ THE GLOBAL ATTRIBUTES --                     #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! The TITLE
    ! ---------

    IF ( PRESENT( Title ) ) THEN

      Title = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TITLE_GATTNAME, &
                                  Title )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//TITLE_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Title )

    END IF


    ! -----------
    ! The HISTORY
    ! -----------

    IF ( PRESENT( History ) ) THEN

      History = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  HISTORY_GATTNAME, &
                                  History )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//HISTORY_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( History )

    END IF


    ! ---------------
    ! The SENSOR_NAME
    ! ---------------

    IF ( PRESENT( Sensor_Name ) ) THEN

      Sensor_Name = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  SENSOR_NAME_GATTNAME, &
                                  Sensor_Name )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//SENSOR_NAME_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Sensor_Name )

    END IF


    ! -----------------
    ! The PLATFORM_NAME
    ! -----------------

    IF ( PRESENT( Platform_Name ) ) THEN

      Platform_Name = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  PLATFORM_NAME_GATTNAME, &
                                  Platform_Name )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//PLATFORM_NAME_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Platform_Name )

    END IF


    ! -----------
    ! The COMMENT
    ! -----------

    IF ( PRESENT( Comment ) ) THEN

      Comment = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  COMMENT_GATTNAME, &
                                  Comment )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//COMMENT_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Comment )

    END IF

  END FUNCTION Read_SRF_GAtts








  FUNCTION Create_SRF_netCDF( NC_Filename,       &  ! Input
                              Channel_List,      &  ! Input
                              NCEP_Sensor_ID,    &  ! Optional Input
                              WMO_Satellite_ID,  &  ! Optional Input
                              WMO_Sensor_ID,     &  ! Optional Input
                              Title,             &  ! Optional input
                              History,           &  ! Optional input
                              Sensor_Name,       &  ! Optional input
                              Platform_Name,     &  ! Optional input
                              Comment,           &  ! Optional input
                              RCS_Id,            &  ! Revision control
                              Message_Log )      &  ! Error messaging
                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                  INTENT( IN )  :: NC_Filename
    INTEGER,         DIMENSION( : ), INTENT( IN )  :: Channel_List

    ! -- Optional input
    INTEGER,               OPTIONAL, INTENT( IN )  :: NCEP_Sensor_ID
    INTEGER,               OPTIONAL, INTENT( IN )  :: WMO_Satellite_ID
    INTEGER,               OPTIONAL, INTENT( IN )  :: WMO_Sensor_ID
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Sensor_Name
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Platform_Name
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Comment

    ! -- Revision control
    CHARACTER( * ),        OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Create_SRF_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Status1, Status2, Status3
    INTEGER :: Close_Status

    INTEGER :: n_Channels, Channel_DimID

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

    n_Channels = SIZE( Channel_List )

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'CHANNEL_LIST array must be non-zero size.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( ANY( Channel_List < 1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid CHANNEL_LIST value found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CREATE THE NETCDF DATA FILE --                     #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_CREATE( NC_Filename, &
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
    !#                     -- DEFINE THE STATIC DIMENSIONS --                   #
    !#--------------------------------------------------------------------------#

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



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE GLOBAL ATTRIBUTES --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Write_SRF_GAtts( NC_Filename, &  ! Input
                                    NC_fileID,   &  ! Input
                                    Title         = Title, &
                                    History       = History, &
                                    Sensor_Name   = Sensor_Name, &
                                    Platform_Name = Platform_Name, &
                                    Comment       = Comment, &
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
                                    UNITS_ATTNAME, &
                                    NCEP_SENSOR_ID_UNITS, &
                                    Variable_Name = NCEP_SENSOR_ID_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    NCEP_SENSOR_ID_FILLVALUE, &
                                    Variable_Name = NCEP_SENSOR_ID_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
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
                                    UNITS_ATTNAME, &
                                    WMO_SATELLITE_ID_UNITS, &
                                    Variable_Name = WMO_SATELLITE_ID_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    WMO_SATELLITE_ID_FILLVALUE, &
                                    Variable_Name = WMO_SATELLITE_ID_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
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
                                    UNITS_ATTNAME, &
                                    WMO_SENSOR_ID_UNITS, &
                                    Variable_Name = WMO_SENSOR_ID_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    WMO_SENSOR_ID_FILLVALUE, &
                                    Variable_Name = WMO_SENSOR_ID_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
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
                                    UNITS_ATTNAME, &
                                    CHANNEL_LIST_UNITS, &
                                    Variable_Name = CHANNEL_LIST_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    CHANNEL_LIST_FILLVALUE, &
                                    Variable_Name = CHANNEL_LIST_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CHANNEL_LIST_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! Begin frequency
    ! ---------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                BEGIN_FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//BEGIN_FREQUENCY_VARNAME//' variable in '// &
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
                                    BEGIN_FREQUENCY_LONGNAME, &
                                    Variable_Name = BEGIN_FREQUENCY_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    FREQUENCY_UNITS, &
                                    Variable_Name = BEGIN_FREQUENCY_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    FREQUENCY_FILLVALUE, &
                                    Variable_Name = BEGIN_FREQUENCY_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BEGIN_FREQUENCY_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! End frequency
    ! -------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                END_FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//END_FREQUENCY_VARNAME//' variable in '// &
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
                                    END_FREQUENCY_LONGNAME, &
                                    Variable_Name = END_FREQUENCY_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    FREQUENCY_UNITS, &
                                    Variable_Name = END_FREQUENCY_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    FREQUENCY_FILLVALUE, &
                                    Variable_Name = END_FREQUENCY_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//END_FREQUENCY_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Integrated SRF
    ! --------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                INTEGRATED_SRF_VARNAME, &
                                INTEGRATED_SRF_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//INTEGRATED_SRF_VARNAME//' variable in '// &
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
                                    INTEGRATED_SRF_LONGNAME, &
                                    Variable_Name = INTEGRATED_SRF_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    INTEGRATED_SRF_UNITS, &
                                    Variable_Name = INTEGRATED_SRF_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    INTEGRATED_SRF_FILLVALUE, &
                                    Variable_Name = INTEGRATED_SRF_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//INTEGRATED_SRF_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! Summation SRF
    ! -------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                SUMMATION_SRF_VARNAME, &
                                SUMMATION_SRF_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//SUMMATION_SRF_VARNAME//' variable in '// &
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
                                    SUMMATION_SRF_LONGNAME, &
                                    Variable_Name = SUMMATION_SRF_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    SUMMATION_SRF_UNITS, &
                                    Variable_Name = SUMMATION_SRF_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    SUMMATION_SRF_FILLVALUE, &
                                    Variable_Name = SUMMATION_SRF_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SUMMATION_SRF_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE INPUT DATA --                        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Take netCDF file out of define mode
    ! -----------------------------------

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


    ! ------------------------
    ! The sensor/satellite IDs
    ! ------------------------

    ! -- NCEP Sensor ID
    IF ( PRESENT( NCEP_Sensor_ID ) ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          NCEP_SENSOR_ID_VARNAME, &
                                          NCEP_Sensor_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//NCEP_SENSOR_ID_VARNAME//&
                              ' to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF
    END IF

    ! -- WMO Satellite ID
    IF ( PRESENT( WMO_Satellite_ID ) ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          WMO_SATELLITE_ID_VARNAME, &
                                          WMO_Satellite_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//WMO_SATELLITE_ID_VARNAME//&
                              ' to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF
    END IF

    ! -- WMO Sensor ID
    IF ( PRESENT( WMO_Sensor_ID ) ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          WMO_SENSOR_ID_VARNAME, &
                                          WMO_Sensor_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//WMO_SENSOR_ID_VARNAME//&
                              ' to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! Write the channel list
    ! ----------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        CHANNEL_LIST_VARNAME, &
                                        Channel_List )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CHANNEL_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SRF_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SRF data file '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Create_SRF_netCDF






  FUNCTION Inquire_SRF_netCDF( NC_Filename,      &  ! Input
                               n_Channels,       &  ! Optional output
                               n_Points,         &  ! Optional output
                               Channel_List,     &  ! Optional output
                               Begin_Frequency,  &  ! Optional output
                               End_Frequency,    &  ! Optional output
                               NCEP_Sensor_ID,   &  ! Optional output
                               WMO_Satellite_ID, &  ! Optional output
                               WMO_Sensor_ID,    &  ! Optional output
                               Title,            &  ! Optional output
                               History,          &  ! Optional output
                               Sensor_Name,      &  ! Optional output
                               Platform_Name,    &  ! Optional output
                               Comment,          &  ! Optional output
                               RCS_Id,           &  ! Revision control
                               Message_Log )     &  ! Error messaging
                             RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                            INTENT( IN )  :: NC_Filename

    ! -- Optional output
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: n_Channels
    INTEGER,         OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: n_Points
    INTEGER,         OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: Channel_List
    REAL( fp_kind),  OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: Begin_Frequency
    REAL( fp_kind),  OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: End_Frequency
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: NCEP_Sensor_ID   
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: WMO_Satellite_ID 
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: WMO_Sensor_ID
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: Title
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: History
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: Sensor_Name
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: Platform_Name
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: Comment

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ),  OPTIONAL,                 INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_SRF_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Allocate_Status

    CHARACTER( 256 ) :: DimName
    INTEGER :: i, l

    INTEGER, DIMENSION(:), ALLOCATABLE :: Local_Channel_List



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

    Error_Status = Open_SRF_netCDF( TRIM( NC_Filename ), &
                                    NC_FileID, &
                                    Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF SRF data file '//TRIM( NC_Filename )
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- GET THE CHANNEL DIMENSION --                    #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! The number of Channels
    ! ----------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                         CHANNEL_DIMNAME, &
                                         l, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//CHANNEL_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 2000
    END IF


    ! ------------------------------
    ! Set the dimension return value
    ! ------------------------------

    IF ( PRESENT( n_Channels ) ) n_Channels = l



    !#--------------------------------------------------------------------------#
    !#                      -- ALLOCATE THE LOCAL ARRAYS --                     #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Local_Channel_List( l ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating local data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      GOTO 2000
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE CHANNEL LIST --                        #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Get the channel list data
    ! -------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        CHANNEL_LIST_VARNAME, &
                                        Local_Channel_List )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//CHANNEL_LIST_VARNAME//&
                ' data from '//TRIM( NC_Filename )
      GOTO 3000
    END IF


    ! -------------------------
    ! Set the data return value
    ! -------------------------

    IF ( PRESENT( Channel_List ) ) THEN

      ! -- Check the dummy argument size
      IF ( SIZE( Channel_List ) < l ) THEN
        Error_Status = FAILURE
        Message = 'Channel_List array too small to hold data.'
        GOTO 3000
      END IF

      ! -- Initialise the entire array (in case it's bigger
      ! -- than the number of elements in the file)
      Channel_List = CHANNEL_LIST_FILLVALUE

      ! -- Save the data
      Channel_List(1:l) = Local_Channel_List

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE n_Points DIMENSION LIST --                  #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Points ) ) THEN


      ! -----------------------------
      ! Check the dummy argument size
      ! -----------------------------

      IF ( SIZE( n_Points ) < l ) THEN
        Error_Status = FAILURE
        Message = 'n_Points array too small to hold data.'
        GOTO 3000
      END IF


      ! ------------------------------------------------
      ! Initialise the entire array (in case it's bigger
      ! than the number of elements in the file)
      ! ------------------------------------------------

      n_Points = 0


      ! ----------------------
      ! Get the dimension data
      ! ----------------------

      ! -- Loop over channels
      DO i = 1, l

        ! -- Create the dimension name for this channel
        CALL Create_Variable_Names( Local_Channel_List(i), &
                                    Dimension_Name = DimName )

        ! -- Retrieve the dimension value
        Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                             TRIM( DimName ),  &
                                             n_Points(i) )

        IF ( Error_Status /= SUCCESS ) THEN
          Message = 'Error reading '//TRIM( DimName )//&
                    ' dimension from '//TRIM( NC_Filename )
          GOTO 3000
        END IF

      END DO

    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- DEALLOCATE THE LOCAL ARRAYS --                    #
    !#--------------------------------------------------------------------------#

    DEALLOCATE( Local_Channel_List, &
                STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating local data arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- GET THE FREQUENCY LIMITS --                     #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The begin frequency
    ! -------------------

    IF ( PRESENT( Begin_Frequency ) ) THEN

      ! -- Check the dummy argument size
      IF ( SIZE( Begin_Frequency ) < l ) THEN
        Error_Status = FAILURE
        Message = 'Begin_Frequency array too small to hold data.'
        GOTO 2000
      END IF

      ! -- Initialise the entire array (in case it's bigger
      ! -- than the number of elements in the file)
      Begin_Frequency = FREQUENCY_FILLVALUE


      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          BEGIN_FREQUENCY_VARNAME, &
                                          Begin_Frequency(1:l) )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//BEGIN_FREQUENCY_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF


    ! -----------------
    ! The end frequency
    ! -----------------

    IF ( PRESENT( End_Frequency ) ) THEN

      ! -- Check the dummy argument size
      IF ( SIZE( End_Frequency ) < l ) THEN
        Error_Status = FAILURE
        Message = 'End_Frequency array too small to hold data.'
        GOTO 2000
      END IF

      ! -- Initialise the entire array (in case it's bigger
      ! -- than the number of elements in the file)
      End_Frequency = FREQUENCY_FILLVALUE

      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          END_FREQUENCY_VARNAME, &
                                          End_Frequency(1:l) )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//END_FREQUENCY_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE SENSOR IDs --                          #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! The NCEP Sensor ID
    ! ------------------

    IF ( PRESENT( NCEP_Sensor_ID ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          NCEP_SENSOR_ID_VARNAME, &
                                          NCEP_Sensor_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//NCEP_SENSOR_ID_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF


    ! --------------------
    ! The WMO satellite ID
    ! --------------------

    IF ( PRESENT( WMO_Satellite_ID ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          WMO_SATELLITE_ID_VARNAME, &
                                          WMO_Satellite_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//WMO_SATELLITE_ID_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF


    ! -----------------
    ! The WMO Sensor ID
    ! -----------------

    IF ( PRESENT( WMO_Sensor_ID ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          WMO_SENSOR_ID_VARNAME, &
                                          WMO_Sensor_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//WMO_SENSOR_ID_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- GET THE GLOBAL ATTRIBUTES --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_SRF_GAtts( TRIM( NC_Filename ), &
                                   NC_fileID, &
                                   Title         = Title, &
                                   History       = History, &
                                   Sensor_Name   = Sensor_Name, &
                                   Platform_Name = Platform_Name, &
                                   Comment       = Comment, &
                                   Message_Log   = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      Message = 'Error reading global attribute from '//TRIM( NC_Filename )
      GOTO 2000
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Close_SRF_netCDF( NC_FileID )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      Message = 'Error closing netCDF SRF data file '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    RETURN



    !#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
    !#                      =- CLEAN-UP AFTER ERRORS -=                         #
    !#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#

    3000 CONTINUE
    DEALLOCATE( Local_Channel_List, &
                STAT = Allocate_Status )
    2000 CONTINUE
    NF90_Status = NF90_CLOSE( NC_fileID )
    1000 CONTINUE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Inquire_SRF_netCDF






  FUNCTION Write_SRF_netCDF( NC_Filename,  &  ! Input
                             SRF,          &  ! Input
                             RCS_Id,       &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: NC_Filename
    TYPE( SRF_type ),         INTENT( IN )  :: SRF

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SRF_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Allocate_Status
    INTEGER :: Status1, Status2, Status3
    INTEGER :: Close_Status

    INTEGER :: n_Channels
    INTEGER, DIMENSION( : ), ALLOCATABLE :: Channel_List
    INTEGER :: Channel_Index

    INTEGER :: NCEP_Sensor_ID
    INTEGER :: WMO_Satellite_ID
    INTEGER :: WMO_Sensor_ID

    CHARACTER(  4 ) :: Channel_String
    CHARACTER( 25 ) :: DimNAME
    CHARACTER( 25 ) :: ResponseNAME

    INTEGER :: DimID
    INTEGER :: VarID



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- GROSS SRF INPUT CHECK --                        #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Check channel is valid
    ! ----------------------

    IF ( SRF%Channel < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF CHANNEL member is < 1.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Check SRF array size is valid
    ! -----------------------------

    IF ( SRF%n_Points < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF N_POINTS member is < 1.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- CHECK THAT SRF CHANNEL IS VALID FOR THIS NETCDF FILE --        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------------
    ! Read the channel dimension and sensor ID values
    ! -----------------------------------------------

    Error_Status = Inquire_SRF_netCDF( NC_Filename, &
                                       n_Channels = n_Channels, &
                                       NCEP_Sensor_ID   = NCEP_Sensor_ID, &
                                       WMO_Satellite_ID = WMO_Satellite_ID, &
                                       WMO_Sensor_ID    = WMO_Sensor_ID, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining SRF dimensions from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------
    ! Check the sensor ID values first
    ! --------------------------------

    IF ( SRF%NCEP_Sensor_ID   /= NCEP_Sensor_ID   .OR. &
         SRF%WMO_Satellite_ID /= WMO_Satellite_ID .OR. &
         SRF%WMO_Sensor_ID    /= WMO_Sensor_ID         ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'netCDF file '//TRIM( NC_Filename )//&
                            ' sensor IDs different from SRF structure values.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
         

    ! --------------------
    ! Get the channel list
    ! --------------------

    ! -- Allocate the array
    ALLOCATE( Channel_List( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating channel list array. STAT = ", i5 )' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Read it
    Error_Status = Inquire_SRF_netCDF( NC_Filename, &
                                       Channel_List = Channel_List, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//CHANNEL_LIST_VARNAME//' data from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      DEALLOCATE( Channel_List )
      RETURN
    END IF


    ! ---------------------------
    ! Check the SRF channel value
    ! ---------------------------

    ! -- Get the channel index
    Channel_Index = MINLOC( ABS( Channel_List - SRF%Channel ), DIM = 1 )
    IF ( ( Channel_List( Channel_Index ) - SRF%Channel ) /= 0 ) Channel_Index = -1

    ! -- Deallocate the channel list array
    DEALLOCATE( Channel_List )

    ! -- Is the channel valid?
    IF ( Channel_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "SRF channel ", i4, " not in channel list for ", a, "." )' ) &
                      SRF%Channel, TRIM( NC_Filename )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- OPEN THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_SRF_netCDF( TRIM( NC_Filename ), &
                                    NC_FileID, &
                                    Mode = 'READWRITE' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF SRF data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- PUT NETCDF FILE INTO DEFINE MODE --                  #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_REDEF( NC_fileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error putting file '//TRIM( NC_Filename )// &
                            ' into define mode - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- DEFINE THE DATA --                          #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------------
    ! Create the channel SRF dimension and variable names
    ! ---------------------------------------------------

    CALL Create_Variable_Names( SRF%Channel, & 
                                Channel_String, &
                                DimNAME, &
                                ResponseNAME )


    ! ----------------------------------------------
    ! Define the N_POINTS dimension for this channel
    ! ----------------------------------------------

    NF90_Status = NF90_DEF_DIM( NC_fileID, &
                                TRIM( DimNAME ), &
                                SRF%n_Points, &
                                DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//TRIM( DimNAME )//&
                            ' dimension in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    

    ! ---------------------------------------------
    ! Define the RESPONSE variable for this channel
    ! ---------------------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                TRIM( ResponseNAME ), &
                                RESPONSE_TYPE, &
                                dimids = DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//TRIM( ResponseNAME )//&
                            ' variable in '// &
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
                                    'Channel '//TRIM( Channel_String )//' normalised response.', &
                                    Variable_Name = TRIM( ResponseNAME ) )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    RESPONSE_UNITS, &
                                    Variable_Name = TRIM( ResponseNAME ) )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    RESPONSE_FILLVALUE, &
                                    Variable_Name = TRIM( ResponseNAME ) )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( ResponseNAME )//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- PUT THE FILE INTO DATA MODE --                     #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_ENDDEF( NC_fileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error placing '//TRIM( NC_Filename )//&
                            ' in DATA mode for channel '//TRIM( Channel_String )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- WRITE THE DATA --                          #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Write the begin frequency
    ! -------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID,   &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        SRF%Begin_Frequency, &
                                        START = (/ Channel_Index /) )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing channel '//TRIM( Channel_String )//&
                            ' '//BEGIN_FREQUENCY_VARNAME//' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -----------------------
    ! Write the end frequency
    ! -----------------------

    Error_Status = Put_netCDF_Variable( NC_fileID,   &
                                        END_FREQUENCY_VARNAME, &
                                        SRF%End_Frequency, &
                                        START = (/ Channel_Index /) )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing channel '//TRIM( Channel_String )//&
                            ' '//END_FREQUENCY_VARNAME//' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------
    ! Write the integrated SRF value
    ! ------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID,   &
                                        INTEGRATED_SRF_VARNAME, &
                                        SRF%Integrated_SRF, &
                                        START = (/ Channel_Index /) )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing channel '//TRIM( Channel_String )//&
                            ' '//INTEGRATED_SRF_VARNAME//' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! --------------------------
    ! Write the summed SRF value
    ! --------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID,   &
                                        SUMMATION_SRF_VARNAME, &
                                        SRF%Summation_SRF, &
                                        START = (/ Channel_Index /) )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing channel '//TRIM( Channel_String )//&
                            ' '//SUMMATION_SRF_VARNAME//' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------
    ! Write the SRF response data
    ! ---------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        TRIM( ResponseNAME ), &
                                        SRF%Response )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( ResponseNAME )//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SRF_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SRF data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_SRF_netCDF






  FUNCTION Read_SRF_netCDF( NC_Filename,  &   ! Input
                            Channel,      &   ! Input
                            SRF,          &   ! Output
                            RCS_Id,       &   ! Revision control
                            Message_Log ) &   ! Error messaging
                          RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: NC_Filename
    INTEGER,                  INTENT( IN )     :: Channel

    ! -- Output
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SRF_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NC_FileID

    INTEGER :: Allocate_Status
    INTEGER :: NF90_Status
    INTEGER :: Close_Status

    INTEGER :: n_Channels
    INTEGER :: NCEP_Sensor_ID
    INTEGER :: WMO_Satellite_ID
    INTEGER :: WMO_Sensor_ID
    INTEGER, DIMENSION( : ), ALLOCATABLE :: Channel_List
    INTEGER :: Channel_Index

    INTEGER :: n_Points
    CHARACTER(  4 ) :: Channel_String
    CHARACTER( 25 ) :: DimNAME
    CHARACTER( 25 ) :: ResponseNAME




    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- CHECK THAT SRF CHANNEL IS VALID FOR THIS NETCDF FILE --        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------------
    ! Read the channel dimension and sensor Id values
    ! -----------------------------------------------

    Error_Status = Inquire_SRF_netCDF( NC_Filename, &
                                       n_Channels = n_Channels, &
                                       NCEP_Sensor_ID   = NCEP_Sensor_ID,   &
                                       WMO_Satellite_ID = WMO_Satellite_ID, &
                                       WMO_Sensor_ID    = WMO_Sensor_ID,    &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining channel dimension from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! Get the channel list
    ! --------------------

    ! -- Allocate the array
    ALLOCATE( Channel_List( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating channel list array. STAT = ", i5 )' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Read it
    Error_Status = Inquire_SRF_netCDF( NC_Filename, &
                                       Channel_List = Channel_List, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//CHANNEL_LIST_VARNAME//' data from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      DEALLOCATE( Channel_List )
      RETURN
    END IF


    ! ---------------------------
    ! Check the SRF channel value
    ! ---------------------------

    ! -- Get the channel index
    Channel_Index = MINLOC( ABS( Channel_List - Channel ), DIM = 1 )
    IF ( ( Channel_List( Channel_Index ) - Channel ) /= 0 ) Channel_Index = -1

    ! -- Deallocate the channel list array
    DEALLOCATE( Channel_List )

    ! -- Is the channel valid?
    IF ( Channel_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "SRF channel ", i4, " not in channel list for ", a, "." )' ) &
                      Channel, TRIM( NC_Filename )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_SRF_netCDF( TRIM( NC_Filename ), &
                                    NC_FileID, &
                                    Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF SRF data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE SRF DATA --                         #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------------
    ! Create the channel SRF dimension and variable names
    ! ---------------------------------------------------

    CALL Create_Variable_Names( Channel, & 
                                Channel_String, &
                                DimNAME, &
                                ResponseNAME )


    ! --------------------------
    ! Get the n_Points dimension
    ! --------------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                         TRIM( DimNAME ), &
                                         n_Points, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining '//TRIM( DimNAME )//' dimension.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------
    ! Allocate the output SRF structure
    ! ---------------------------------

    Error_Status = Allocate_SRF( n_Points, &
                                 SRF, &
                                 Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating SRF data array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------------------
    ! Get the SRF response data for the requested channel
    ! ---------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        TRIM( ResponseNAME ), &
                                        SRF%Response, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM( ResponseNAME )//' data.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -------------------------
    ! Read the frequency limits
    ! -------------------------

    ! -- Begin frequency
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        SRF%Begin_Frequency, &
                                        START = (/ Channel_Index /), &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//BEGIN_FREQUENCY_VARNAME//' data for channel '//&
                            TRIM( Channel_String ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- End frequency
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        END_FREQUENCY_VARNAME, &
                                        SRF%End_Frequency, &
                                        START = (/ Channel_Index /), &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//END_FREQUENCY_VARNAME//' data for channel '//&
                            TRIM( Channel_String ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------
    ! Read the integrated SRF values
    ! ------------------------------

    ! -- The integrated value
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        INTEGRATED_SRF_VARNAME, &
                                        SRF%Integrated_SRF, &
                                        START = (/ Channel_Index /), &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//INTEGRATED_SRF_VARNAME//' data for channel '//&
                            TRIM( Channel_String ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -- The summation value
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        SUMMATION_SRF_VARNAME, &
                                        SRF%Summation_SRF, &
                                        START = (/ Channel_Index /), &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//INTEGRATED_SRF_VARNAME//' data for channel '//&
                            TRIM( Channel_String ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -------------------------------------------------------------
    ! All reads successful, so assign channel number and sensor Ids
    ! -------------------------------------------------------------

    SRF%Channel = Channel

    SRF%NCEP_Sensor_ID   = NCEP_Sensor_ID
    SRF%WMO_Satellite_ID = WMO_Satellite_ID
    SRF%WMO_Sensor_ID    = WMO_Sensor_ID



    !#--------------------------------------------------------------------------#
    !#                       -- CLOSE THE netCDF FILE --                        #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SRF_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SRF data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- COMPUTE THE SRF FREQUENCY GRID --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Frequency_SRF( SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing frequency grid for channel '//TRIM( Channel_String )//&
                            ' SRF from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Read_SRF_netCDF

END MODULE SRF_netCDF_IO


