!
! AntCorr_netCDF_IO
!
! Module containing routines to read and write AntCorr netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-Apr-2007
!                       paul.vandelst@ssec.wisc.edu
!

MODULE AntCorr_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: Long, Double
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                             Display_Message
  USE AntCorr_Define , ONLY: AntCorr_type, &
                             Associated_AntCorr, &
                             Destroy_AntCorr, &
                             Allocate_AntCorr, &
                             CheckRelease_AntCorr, &
                             Info_AntCorr
  USE netcdf
  USE netCDF_Utility
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: DefineVar_AntCorr_netCDF
  PUBLIC :: WriteVar_AntCorr_netCDF
  PUBLIC :: ReadVar_AntCorr_netCDF
  PUBLIC :: Inquire_AntCorr_netCDF
  PUBLIC :: Write_AntCorr_netCDF
  PUBLIC :: Read_AntCorr_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
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

  ! Dimension names
  CHARACTER(*), PARAMETER :: FOV_DIMNAME     = 'n_FOVs'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME = 'n_Channels'

  ! Variable names
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME   = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: A_EARTH_VARNAME          = 'A_earth'
  CHARACTER(*), PARAMETER :: A_SPACE_VARNAME          = 'A_space'
  CHARACTER(*), PARAMETER :: A_PLATFORM_VARNAME       = 'A_platform'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME   = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: A_EARTH_LONGNAME          = 'A(earth)'
  CHARACTER(*), PARAMETER :: A_SPACE_LONGNAME          = 'A(space)'
  CHARACTER(*), PARAMETER :: A_PLATFORM_LONGNAME       = 'A(platform)'

 
  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION   = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: A_EARTH_DESCRIPTION          = 'Antenna efficiency for earth view'
  CHARACTER(*), PARAMETER :: A_SPACE_DESCRIPTION          = 'Antenna efficiency for cold space view'
  CHARACTER(*), PARAMETER :: A_PLATFORM_DESCRIPTION       = 'Antenna efficiency for satellite platform view'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: A_EARTH_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: A_SPACE_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: A_PLATFORM_UNITS       = 'N/A'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER(Long), PARAMETER :: SENSOR_CHANNEL_FILLVALUE   = 0
  REAL(Double) , PARAMETER :: A_EARTH_FILLVALUE          = ONE
  REAL(Double) , PARAMETER :: A_SPACE_FILLVALUE          = ZERO
  REAL(Double) , PARAMETER :: A_PLATFORM_FILLVALUE       = ZERO

  ! Variable types
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE   = NF90_INT
  INTEGER, PARAMETER :: A_EARTH_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: A_SPACE_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: A_PLATFORM_TYPE       = NF90_DOUBLE


CONTAINS


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
!       DefineVar_AntCorr_netCDF
!
! PURPOSE:
!       Function to define the antenna correction variables in any
!       output netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineVar_AntCorr_netCDF( NC_Filename     , &  ! Input
!                                                NC_FileID       , &  ! Input
!                                                n_FOVs_DimID    , &  ! Input
!                                                n_Channels_DimID, &  ! Input
!                                                RCS_Id          , &  ! Revision control
!                                                Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF AntCorr format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be defned.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_FOVs_DimID:       NetCDF dimension ID of the number of fields
!                           of view (n_FOVs) dimension of the antenna
!                           correction data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels_DimID:   NetCDF dimension ID of the number of sensor
!                           channels (n_Channels) dimension of the antenna
!                           correction data
!                           UNITS:      N/A
!                           TYPE:       INTEGER
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
!------------------------------------------------------------------------------

  FUNCTION DefineVar_AntCorr_netCDF( NC_Filename     , &  ! Input
                                     NC_FileID       , &  ! Input
                                     n_FOVs_DimID    , &  ! Input
                                     n_Channels_DimID, &  ! Input
                                     RCS_Id          , &  ! Revision control
                                     Message_Log     ) &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     ,           INTENT(IN)  :: n_FOVs_DimID    
    INTEGER     ,           INTENT(IN)  :: n_Channels_DimID
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'DefineVar_AntCorr_netCDF'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
    INTEGER :: varID
    INTEGER :: Put_Status(4)
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Define the sensor channels
    ! --------------------------
    ! Inquire the netCDF file for the sensor channel variable ID
    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  SENSOR_CHANNEL_VARNAME, &
                                  varID )
    
    ! Define it if required
    IF ( NF90_Status /= NF90_NOERR ) THEN
      NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                  SENSOR_CHANNEL_VARNAME, &
                                  SENSOR_CHANNEL_TYPE, &
                                  dimIDs=(/n_Channels_DimID/), &
                                  varID =VarID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        Message = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
                  TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL DefineVar_Cleanup(); RETURN
      END IF

      Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                    VarID, &
                                    LONGNAME_ATTNAME, &
                                    SENSOR_CHANNEL_LONGNAME )
      Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                    VarID, &
                                    DESCRIPTION_ATTNAME, &
                                    SENSOR_CHANNEL_DESCRIPTION )
      Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                    VarID, &
                                    UNITS_ATTNAME, &
                                    SENSOR_CHANNEL_UNITS )
      Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                    VarID, &
                                    FILLVALUE_ATTNAME, &
                                    SENSOR_CHANNEL_FILLVALUE )
      IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
        Message = 'Error writing '//SENSOR_CHANNEL_VARNAME//&
                  ' variable attributes to '//TRIM(NC_Filename)
        CALL DefineVar_Cleanup(); RETURN
      END IF
    END IF


    ! Define the antenna correction data
    ! ----------------------------------
    ! The earth view antenna efficiencies
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                A_EARTH_VARNAME, &
                                A_EARTH_TYPE, &
                                dimIDs=(/n_FOVs_DimID, n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//A_EARTH_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  A_EARTH_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  A_EARTH_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  A_EARTH_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  A_EARTH_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      Message = 'Error writing '//A_EARTH_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! The space antenna efficiencies
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                A_SPACE_VARNAME, &
                                A_SPACE_TYPE, &
                                dimIDs=(/n_FOVs_DimID, n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//A_SPACE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  A_SPACE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  A_SPACE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  A_SPACE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  A_SPACE_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      Message = 'Error writing '//A_SPACE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! The platform view antenna efficiencies
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                A_PLATFORM_VARNAME, &
                                A_PLATFORM_TYPE, &
                                dimIDs=(/n_FOVs_DimID, n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//A_PLATFORM_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  A_PLATFORM_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  A_PLATFORM_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  A_PLATFORM_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  A_PLATFORM_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      Message = 'Error writing '//A_PLATFORM_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE DefineVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = TRIM(Message)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE DefineVar_CleanUp

  END FUNCTION DefineVar_AntCorr_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       WriteVar_AntCorr_netCDF
!
! PURPOSE:
!       Function to write the AntCorr variables in an output netCDF file
!       in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = WriteVar_AntCorr_netCDF( NC_Filename            , &  ! Input
!                                               NC_FileID              , &  ! Input
!                                               AntCorr                , &  ! Input
!                                               RCS_Id     =RCS_Id     , &  ! Revision control
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:     Character string specifying the name of the
!                        already opened netCDF AntCorr format file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:       NetCDF file ID number of the file from which
!                        the variables are to be read.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       AntCorr:         Structure containing the data to write to file.
!                        UNITS:      N/A
!                        TYPE:       TYPE(AntCorr_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
!------------------------------------------------------------------------------

  FUNCTION WriteVar_AntCorr_netCDF( NC_Filename, &  ! Input
                                    NC_FileID  , &  ! Input
                                    AntCorr    , &  ! Input
                                    RCS_Id     , &  ! Revision control
                                    Message_Log) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: NC_FileID
    TYPE(AntCorr_type)    , INTENT(IN)  :: AntCorr
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteVar_AntCorr_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NF90_Status
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Write the antenna correction data
    ! ---------------------------------
    ! Sensor channel list
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        SENSOR_CHANNEL_VARNAME, &
                                        AntCorr%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! Earth view antenna efficiency
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        A_EARTH_VARNAME, &
                                        AntCorr%A_earth )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//A_EARTH_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! Space view antenna efficiency
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        A_SPACE_VARNAME, &
                                        AntCorr%A_space )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//A_SPACE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! Platform view antenna efficiency
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        A_PLATFORM_VARNAME, &
                                        AntCorr%A_platform )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//A_PLATFORM_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE WriteVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = TRIM(Message)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE WriteVar_CleanUp

  END FUNCTION WriteVar_AntCorr_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       ReadVar_AntCorr_netCDF
!
! PURPOSE:
!       Function to read the AntCorr variables from any input
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = ReadVar_AntCorr_netCDF( NC_Filename            , &  ! Input
!                                              NC_FileID              , &  ! Input
!                                              AntCorr                , &  ! Output
!                                              RCS_Id     =RCS_Id     , &  ! Revision control
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:     Character string specifying the name of the
!                        already opened netCDF AntCorr format file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:       NetCDF file ID number of the file from which
!                        the variables are to be read.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AntCorr:         Structure containing the data that was read
!                        from file.
!                        UNITS:      N/A
!                        TYPE:       TYPE(AntCorr_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
! COMMENTS:
!       The INTENT on the output AntCorr argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION ReadVar_AntCorr_netCDF( NC_Filename, &  ! Input
                                   NC_FileID  , &  ! Input
                                   AntCorr    , &  ! Output
                                   RCS_Id     , &  ! Revision control
                                   Message_Log) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: NC_Filename
    INTEGER               , INTENT(IN)     :: NC_FileID
    TYPE(AntCorr_type)    , INTENT(IN OUT) :: AntCorr
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadVar_AntCorr_netCDF'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Read the data
    ! -------------
    ! Sensor channel list
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        SENSOR_CHANNEL_VARNAME, &
                                        AntCorr%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//SENSOR_CHANNEL_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! Earth view antenna efficiency
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        A_EARTH_VARNAME, &
                                        AntCorr%A_earth )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//A_EARTH_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! Space view antenna efficiency
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        A_SPACE_VARNAME, &
                                        AntCorr%A_space )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//A_SPACE_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! Platform view antenna efficiency
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        A_PLATFORM_VARNAME, &
                                        AntCorr%A_platform )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//A_PLATFORM_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE ReadVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = TRIM(Message)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadVar_CleanUp

  END FUNCTION ReadVar_AntCorr_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_AntCorr_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF AntCorr format file to obtain the
!       dimensions and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_AntCorr_netCDF( NC_Filename                       , &  ! Input
!                                              n_FOVs           =n_FOVs          , &  ! Optional output
!                                              n_Channels       =n_Channels      , &  ! Optional output
!                                              Release          =Release         , &  ! Optional output
!                                              Version          =Version         , &  ! Optional output
!                                              Sensor_Id        =Sensor_Id       , &  ! Optional output
!                                              WMO_Satellite_Id =WMO_Satellite_Id, &  ! Optional output
!                                              WMO_Sensor_Id    =WMO_Sensor_Id   , &  ! Optional output
!                                              Title            =Title           , &  ! Optional output
!                                              History          =History         , &  ! Optional output
!                                              Comment          =Comment         , &  ! Optional output
!                                              RCS_Id           =RCS_Id          , &  ! Revision control
!                                              Message_Log      =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           AntCorr netCDF format data file to inquire.
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
!       n_FOVs:             The number of fields-of-view for the sensor.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         The number of spectral channels for the sensor.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the netCDF AntCorr file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF AntCorr file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF AntCorr file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF AntCorr file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF AntCorr file.
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

  FUNCTION Inquire_AntCorr_netCDF( NC_Filename     , &  ! Input
                                   n_FOVs          , &  ! Optional output
                                   n_Channels      , &  ! Optional output
                                   Release         , &  ! Optional output
                                   Version         , &  ! Optional output
                                   Sensor_Id       , &  ! Optional output
                                   WMO_Satellite_Id, &  ! Optional output
                                   WMO_Sensor_Id   , &  ! Optional output
                                   Title           , &  ! Optional output
                                   History         , &  ! Optional output
                                   Comment         , &  ! Optional output
                                   RCS_Id          , &  ! Revision control
                                   Message_Log     ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_FOVs    
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_AntCorr_netCDF'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    TYPE(AntCorr_type) :: AntCorr
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_netCDF( TRIM(NC_Filename), &
                                NC_FileID, &
                                Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF AntCorr data file '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the dimensions
    ! ------------------
    ! The number of fields of view
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         FOV_DIMNAME, &
                                         AntCorr%n_FOVs, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//FOV_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! The number of spectral channels
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         CHANNEL_DIMNAME, &
                                         AntCorr%n_Channels, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Get the global attributes
    ! -------------------------
    Error_Status = ReadGAtts( NC_Filename                               , &
                              NC_FileID                                 , &
                              Version          =AntCorr%Version         , &
                              Sensor_Id        =AntCorr%Sensor_Id       , &
                              WMO_Satellite_Id =AntCorr%WMO_Satellite_Id, &
                              WMO_Sensor_Id    =AntCorr%WMO_Sensor_Id   , &
                              Title            =Title                   , &
                              History          =History                 , &
                              Comment          =Comment                 , &
                              Message_Log      =Message_Log               )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Message = 'Error closing netCDF AntCorr data file '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    ! ---------------------
    ! Dimensions
    IF ( PRESENT(n_FOVs    ) ) n_FOVs     = AntCorr%n_FOVs
    IF ( PRESENT(n_Channels) ) n_Channels = AntCorr%n_Channels
    
    ! Release/Version information
    IF ( PRESENT(Release) ) Release = AntCorr%Release
    IF ( PRESENT(Version) ) Version = AntCorr%Version

    ! Sensor ids
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = AntCorr%Sensor_Id(1:MIN(LEN(Sensor_Id),LEN_TRIM(AntCorr%Sensor_Id)))
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = AntCorr%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = AntCorr%WMO_Sensor_Id   
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          Close_Status = Close_netCDF(NC_FileID)
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

  END FUNCTION Inquire_AntCorr_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Write_AntCorr_netCDF
!
! PURPOSE:
!       Function to write AntCorr data to a netCDF format AntCorr file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_AntCorr_netCDF( NC_Filename            , &  ! Input
!                                            AntCorr                , &  ! Input
!                                            Quiet      =Quiet      , &  ! Optional input
!                                            Title      =Title      , &  ! Optional input
!                                            History    =History    , &  ! Optional input
!                                            Comment    =Comment    , &  ! Optional input
!                                            RCS_Id     =RCS_Id     , &  ! Revision control
!                                            Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the netCDF
!                     format AntCorr data file to write data into.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       AntCorr:      Structure containing the antenna correction data
!                     to write to file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(AntCorr_type)
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
!                     attribute field of the netCDF AntCorr file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF AntCorr file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF AntCorr file.
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

  FUNCTION Write_AntCorr_netCDF( NC_Filename , &  ! Input
                                 AntCorr     , &  ! Input
                                 Quiet       , &  ! Optional input
                                 Title       , &  ! Optional input
                                 History     , &  ! Optional input
                                 Comment     , &  ! Optional input
                                 RCS_Id      , &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(AntCorr_type)    , INTENT(IN)  :: AntCorr
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title  
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AntCorr_netCDF'
    ! Local variables
    CHARACTER(ML) :: Message
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
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_AntCorr( AntCorr ) ) THEN
      Message = 'Some or all INPUT AntCorr pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Create the output data file
    ! ---------------------------
    Error_Status = CreateFile( NC_Filename                               , &  ! Input
                               AntCorr%n_FOVs                            , &  ! Input
                               AntCorr%n_Channels                        , &  ! Input
                               NC_FileID                                 , &  ! Output
                               Version          =AntCorr%Version         , &  ! Optional input
                               Sensor_Id        =AntCorr%Sensor_Id       , &  ! Optional input
                               WMO_Satellite_Id =AntCorr%WMO_Satellite_Id, &  ! Optional input
                               WMO_Sensor_Id    =AntCorr%WMO_Sensor_Id   , &  ! Optional input
                               Title            =Title                   , &  ! Optional input
                               History          =History                 , &  ! Optional input
                               Comment          =Comment                 , &  ! Optional input
                               Message_Log      =Message_Log               )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the antenna correction data
    ! ---------------------------------
    Error_Status = WriteVar_AntCorr_netCDF( NC_Filename            , &
                                            NC_FileID              , &
                                            AntCorr                , &
                                            Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing AC variables to output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    

    ! Close the file
    ! --------------
    Close_Status = Close_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF AntCorr data file '//TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_AntCorr( AntCorr, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          Close_Status = Close_netCDF(NC_FileID)
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

  END FUNCTION Write_AntCorr_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Read_AntCorr_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format AntCorr file.
!
! CALLING SEQUENCE:
!     Error_Status = Read_AntCorr_netCDF( NC_Filename            , &  ! Input
!                                         AntCorr                , &  ! Output
!                                         Quiet      =Quiet      , &  ! Optional input
!                                         Title      =Title      , &  ! Optional output
!                                         History    =History    , &  ! Optional output
!                                         Comment    =Comment    , &  ! Optional output
!                                         RCS_Id     =RCS_Id     , &  ! Revision control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the
!                     netCDF format AntCorr data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AntCorr:      Structure to contain the antenna correction data
!                     read from file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(AntCorr_type)
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
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF AntCorr file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF AntCorr file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF AntCorr file.
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
!       If specified as the output data type, the INTENT on the output AntCorr
!       structure argument is IN OUT rather than just OUT. This is necessary
!       because the argument may be defined on input. To prevent memory leaks,
!       the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_AntCorr_netCDF( NC_Filename, &  ! Input
                                AntCorr    , &  ! Output
                                Quiet      , &  ! Optional input
                                Title      , &  ! Optional output
                                History    , &  ! Optional output
                                Comment    , &  ! Optional output
                                RCS_Id     , &  ! Revision control
                                Message_Log) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(AntCorr_type)    , INTENT(IN OUT) :: AntCorr
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title  
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_AntCorr_netCDF'
    ! Function variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: n_FOVs    
    INTEGER :: n_Channels

    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    
    ! Allocate the structure for the netCDF read
    ! ------------------------------------------
    ! Read the dimension values
    Error_Status = Inquire_AntCorr_netCDF( NC_Filename            , &
                                           n_FOVs     =n_FOVs     , &
                                           n_Channels =n_Channels , &
                                           Title      =Title      , &
                                           History    =History    , &
                                           Comment    =Comment    , &
                                           Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining AntCorr dimensions from '//TRIM(NC_Filename)
      CALL Read_Cleanup(); RETURN
    END IF

    ! Allocate the structure
    Error_Status = Allocate_AntCorr( n_FOVs    , &
                                     n_Channels, &
                                     AntCorr   , &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating AntCorr structure.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the netCDF file for reading
    ! --------------------------------
    Error_Status = Open_netCDF( NC_Filename, &
                                NC_FileID, &
                                Mode='READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF AntCorr data file '//TRIM(NC_Filename)
      CALL Read_Cleanup( Destroy_Structure=.TRUE. ); RETURN
    END IF


    ! Read the remaining global attributes
    ! ------------------------------------
    Error_Status = ReadGAtts( NC_Filename                               , &
                              NC_FileID                                 , &
                              Release          =AntCorr%Release         , &
                              Version          =AntCorr%Version         , &
                              Sensor_Id        =AntCorr%Sensor_Id       , &
                              WMO_Satellite_Id =AntCorr%WMO_Satellite_Id, &
                              WMO_Sensor_Id    =AntCorr%WMO_Sensor_Id   , &
                              Message_Log      =Message_Log               )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=.TRUE., Destroy_Structure=.TRUE. ); RETURN
    END IF

    ! Check the release
    Error_Status = CheckRelease_AntCorr( AntCorr, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'AntCorr Release check failed for '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=.TRUE., Destroy_Structure=.TRUE. ); RETURN
    END IF
    

    ! Read the antenna correction data
    ! ---------------------------------
    Error_Status = ReadVar_AntCorr_netCDF( NC_Filename            , &
                                           NC_FileID              , &
                                           AntCorr                , &
                                           Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading AC variables from '//TRIM(NC_Filename)
      CALL Read_Cleanup( Destroy_Structure=.TRUE. ); RETURN
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF AntCorr data file '//TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_AntCorr( AntCorr, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File, Destroy_Structure )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      LOGICAL, OPTIONAL, INTENT(IN) :: Destroy_Structure
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          Close_Status = Close_netCDF(NC_FileID)
          IF ( Close_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Destroy the structure if necessary
      IF ( PRESENT(Destroy_Structure) ) THEN
        IF ( Destroy_Structure ) THEN
          Destroy_Status = Destroy_AntCorr(AntCorr, Message_Log=Message_Log)
          IF ( Destroy_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error destroying AntCorr structure during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_AntCorr_netCDF


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
!       Function to write the global attributes to a netCDF AntCorr
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = WriteGAtts( NC_Filename                      , &  ! Input
!                                  NC_FileID                        , &  ! Input
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
!                         netCDF AntCorr format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Version:          The version number of the netCDF AntCorr file.
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
!                         attribute field of the netCDF AntCorr file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF AntCorr file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF AntCorr file.
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
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION WriteGAtts( NC_Filename     , &  ! Input
                       NC_FileID       , &  ! Input
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
    INTEGER     ,           INTENT(IN) :: NC_FileID
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
    CHARACTER(ML) :: Message
    CHARACTER(ML) :: GAttName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: Ver
    INTEGER :: NF90_Status
    TYPE(AntCorr_type) :: AntCorr_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS
    Message = ' '


    ! Mandatory global attributes
    ! ---------------------------
    ! Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                MODULE_RCS_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    
    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
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
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                AntCorr_Default%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! --------------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = AntCorr_Default%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
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
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
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
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
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
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
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
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
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
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
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
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(NC_Filename)//' - '// &
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
!       Function to read the global attributes from a netCDF AntCorr
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = ReadGAtts( NC_Filename                      , &  ! Input
!                                 NC_FileID                        , &  ! Input
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
!                         netCDF AntCorr format data file to read from.
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
!       Release:          The release number of the netCDF AntCorr file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF AntCorr file.
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
!                         attribute field of the netCDF AntCorr file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF AntCorr file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF AntCorr file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
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
                      NC_FileID       , &  ! Input
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
    INTEGER,                INTENT(IN)  :: NC_FileID
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
    TYPE(AntCorr_type) :: AntCorr_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! The mandatory GAtts for checking
    ! --------------------------------
    ! The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_GET_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Rel )
    IF ( NF90_Status /= NF90_NOERR .OR. Rel /= AntCorr_Default%Release) THEN
      CALL ReadGAtts_Cleanup(); RETURN
    END IF
    IF ( PRESENT(Release) ) Release = AntCorr_Default%Release


    ! The optional GAtts
    ! ------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
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
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( GAttString )
      Sensor_Id = GAttString(1:MIN( LEN(Sensor_Id), LEN_TRIM(GAttString) ))
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
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
      NF90_Status = NF90_GET_ATT( NC_FileID, &
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
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( GAttString )
      Title = GAttString(1:MIN( LEN(Title), LEN_TRIM(GAttString) ))
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttString = ' '; History = ' '
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( GAttString )
      History = GAttString(1:MIN( LEN(History), LEN_TRIM(GAttString) ))
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttString = ' '; Comment = ' '
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( GAttString )
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


!------------------------------------------------------------------------------
!
! NAME:
!       CreateFile
!
! PURPOSE:
!       Function to create a netCDF AntCorr data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = CreateFile( NC_Filename                      , &  ! Input
!                                  n_FOVs                           , &  ! Input
!                                  n_Channels                       , &  ! Input
!                                  NC_FileID                        , &  ! Output
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
!       NC_Filename:        Character string specifying the name of the
!                           netCDF AntCorr format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_FOVs:             The number of fields-of-view for the sensor.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels:         The number of spectral channels for the sensor.
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
!       Version:            The version number of the netCDF AntCorr file.
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
!                           attribute field of the netCDF AntCorr file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF AntCorr file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF AntCorr file.
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

  FUNCTION CreateFile( NC_Filename     , &  ! Input
                       n_FOVs          , &  ! Input
                       n_Channels      , &  ! Input
                       NC_FileID       , &  ! Output
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
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: n_FOVs    
    INTEGER               , INTENT(IN)  :: n_Channels
    INTEGER               , INTENT(OUT) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CreateFile'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NF90_Status
    INTEGER :: n_FOVs_DimID
    INTEGER :: n_Channels_DimID
    INTEGER :: VarID
    

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check input
    IF ( n_FOVs     < 1 .OR. &
         n_Channels < 1      ) THEN
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
    ! The number of fields-of-view
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                FOV_DIMNAME, n_FOVs, n_FOVs_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//FOV_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The number of spectral channels
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                CHANNEL_DIMNAME, n_Channels, n_Channels_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the global attributes
    ! ---------------------------
    Error_Status = WriteGAtts( NC_Filename                      , &
                               NC_FileID                        , &
                               Version         =Version         , &
                               Sensor_Id       =Sensor_Id       , &
                               WMO_Satellite_Id=WMO_Satellite_Id, &
                               WMO_Sensor_Id   =WMO_Sensor_Id   , &
                               Title           =Title           , &
                               History         =History         , &
                               Comment         =Comment         , &
                               Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing global attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Define the antenna correction variables
    ! ---------------------------------------
    Error_Status = DefineVar_AntCorr_netCDF( NC_Filename            , &
                                             NC_FileID              , &
                                             n_FOVs_DimID           , &
                                             n_Channels_DimID       , &
                                             Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error defining variables in '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
                                             


    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error taking '//TRIM(NC_Filename)//' out of define mode.'
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Create_CleanUp(Close_File)
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileID )
          IF ( NF90_Status /= NF90_NOERR ) &
            Message = '; Error closing input file during error cleanup - '//&
                      TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Create_CleanUp

  END FUNCTION CreateFile

END MODULE AntCorr_netCDF_IO
