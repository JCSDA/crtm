!
! NLTECoeff_IO
!
! Container module for Binary and netCDF NLTECoeff I/O modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Jan-2011
!                       paul.vandelst@noaa.gov
!

MODULE NLTECoeff_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: fp
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Exists
  USE NLTECoeff_Define   , ONLY: NLTECoeff_type, OPERATOR(==)
  USE NLTECoeff_Binary_IO, ONLY: NLTECoeff_Binary_InquireFile, &
                                 NLTECoeff_Binary_ReadFile   , &
                                 NLTECoeff_Binary_WriteFile  , &
                                 NLTECoeff_Binary_IOVersion
  USE NLTECoeff_netCDF_IO, ONLY: NLTECoeff_netCDF_InquireFile, &
                                 NLTECoeff_netCDF_ReadFile   , &
                                 NLTECoeff_netCDF_WriteFile  , &
                                 NLTECoeff_netCDF_IOVersion
  ! Disable implicit typing
  IMPLICIT NONE
  
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: NLTECoeff_InquireFile
  PUBLIC :: NLTECoeff_ReadFile
  PUBLIC :: NLTECoeff_WriteFile
  PUBLIC :: NLTECoeff_netCDF_to_Binary
  PUBLIC :: NLTECoeff_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  

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
!       NLTECoeff_InquireFile
!
! PURPOSE:
!       Function to inquire NLTECoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_InquireFile( &
!                        Filename, &
!                        netCDF           = netCDF          , &
!                        n_Predictors     = n_Predictors    , &
!                        n_Sensor_Angles  = n_Sensor_Angles , &
!                        n_Solar_Angles   = n_Solar_Angles  , &
!                        n_NLTE_Channels  = n_NLTE_Channels , &
!                        n_Channels       = n_Channels      , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Sensor_Id        = Sensor_Id       , &
!                        WMO_Satellite_Id = WMO_Satellite_Id, &
!                        WMO_Sensor_Id    = WMO_Sensor_Id   , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          NLTECoeff data file to read.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:            Set this logical argument to access netCDF format
!                          NLTECoeff datafiles.
!                          If == .FALSE., file format is BINARY [DEFAULT].
!                             == .TRUE.,  file format is NETCDF.
!                          If not specified, default is .FALSE.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Predictors:      The number of predictor functions used in generating
!                          the NLTE correction coefficients.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Sensor_Angles:   Number of sensor zenith angles.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Solar_Angles:    Number of solar zenith angles.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_NLTE_Channels:   Number of NLTE channels for the sensor.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:        Total number of sensor channels.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:           The release number of the NLTECoeff file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The version number of the NLTECoeff file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:         Character string sensor/platform identifier.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id:  The WMO code used to identify satellite platforms.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:     The WMO code used to identify sensors.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:             Character string written into the TITLE global
!                          attribute field of the NLTECoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the NLTECoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the NLTECoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS, the file inquire was successful
!                             == FAILURE, an unrecoverable error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION NLTECoeff_InquireFile( &
    Filename        , &  ! Input
    netCDF          , &  ! Optional input
    n_Predictors    , &  ! Optional output
    n_Sensor_Angles , &  ! Optional output
    n_Solar_Angles  , &  ! Optional output
    n_NLTE_Channels , &  ! Optional output  
    n_Channels      , &  ! Optional output  
    Release         , &  ! Optional Output
    Version         , &  ! Optional Output
    Sensor_Id       , &  ! Optional Output
    WMO_Satellite_Id, &  ! Optional Output
    WMO_Sensor_Id   , &  ! Optional Output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Predictors   
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Sensor_Angles
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Solar_Angles 
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_NLTE_Channels
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Channels     
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release         
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id                
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id         
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id            
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function variables
    LOGICAL :: binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    binary = .TRUE.
    IF ( PRESENT(netCDF) ) binary = .NOT. netCDF


    ! Call the appropriate function
    IF ( binary ) THEN
      err_stat = NLTECoeff_Binary_InquireFile( &
                   Filename, &
                   n_Predictors     = n_Predictors    , &
                   n_Sensor_Angles  = n_Sensor_Angles , &
                   n_Solar_Angles   = n_Solar_Angles  , &
                   n_NLTE_Channels  = n_NLTE_Channels , &
                   n_Channels       = n_Channels      , &
                   Release          = Release         , &
                   Version          = Version         , &
                   Sensor_Id        = Sensor_Id       , &
                   WMO_Satellite_Id = WMO_Satellite_Id, &
                   WMO_Sensor_Id    = WMO_Sensor_Id     )
    ELSE
      err_stat = NLTECoeff_netCDF_InquireFile( &
                   Filename, &
                   n_Predictors     = n_Predictors    , &
                   n_Sensor_Angles  = n_Sensor_Angles , &
                   n_Solar_Angles   = n_Solar_Angles  , &
                   n_NLTE_Channels  = n_NLTE_Channels , &
                   n_Channels       = n_Channels      , &
                   Release          = Release         , &
                   Version          = Version         , &
                   Sensor_Id        = Sensor_Id       , &
                   WMO_Satellite_Id = WMO_Satellite_Id, &
                   WMO_Sensor_Id    = WMO_Sensor_Id   , &
                   Title            = Title           , &
                   History          = History         , &
                   Comment          = Comment           )
    END IF

  END FUNCTION NLTECoeff_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_ReadFile
!
! PURPOSE:
!       Function to read NLTECoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_ReadFile( &
!                        Filename         , &
!                        NLTECoeff        , &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       NLTECoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       NLTECoeff:      NLTECoeff object containing the NLTE correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       NLTECoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       NLTECoeff datafiles.
!                       If == .FALSE., file format is BINARY [DEFAULT].
!                          == .TRUE.,  file format is NETCDF.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
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
!                       attribute field of the NLTECoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the NLTECoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the NLTECoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
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

  FUNCTION NLTECoeff_ReadFile( &
    Filename , &  ! Input
    NLTECoeff, &  ! Output
    netCDF   , &  ! Optional input
    Quiet    , &  ! Optional input
    Title    , &  ! Optional output
    History  , &  ! Optional output
    Comment  ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(NLTECoeff_type),   INTENT(OUT) :: NLTECoeff
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function variables
    LOGICAL :: binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    binary = .TRUE.
    IF ( PRESENT(netCDF) ) binary = .NOT. netCDF

    ! Call the appropriate function
    IF ( binary ) THEN
      err_stat = NLTECoeff_Binary_ReadFile( &
                   Filename , &
                   NLTECoeff, &
                   Quiet = Quiet )
    ELSE
      err_stat = NLTECoeff_netCDF_ReadFile( &
                   Filename , &
                   NLTECoeff, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION NLTECoeff_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_WriteFile
!
! PURPOSE:
!       Function to write NLTECoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_WriteFile( &
!                        Filename , &
!                        NLTECoeff, &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       NLTECoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       NLTECoeff:      NLTECoeff object containing the NLTE correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       NLTECoeff_type 
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       NLTECoeff datafiles.
!                       If == .FALSE., file format is BINARY [DEFAULT].
!                          == .TRUE.,  file format is NETCDF.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
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
!                       attribute field of the NLTECoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the NLTECoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the NLTECoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
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

  FUNCTION NLTECoeff_WriteFile( &
    Filename , &  ! Input
    NLTECoeff, &  ! Input
    netCDF   , &  ! Optional input
    Quiet    , &  ! Optional input
    Title    , &  ! Optional input
    History  , &  ! Optional input
    Comment  ) &  ! Optional input
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(NLTECoeff_type),   INTENT(IN) :: NLTECoeff
    LOGICAL,      OPTIONAL, INTENT(IN) :: netCDF
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local variables
    LOGICAL :: binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    binary = .TRUE.
    IF ( PRESENT(netCDF) ) binary = .NOT. netCDF

    ! Call the appropriate function
    IF ( binary ) THEN
      err_stat = NLTECoeff_Binary_WriteFile( &
                   Filename , &
                   NLTECoeff, &
                   Quiet = Quiet )
    ELSE
      err_stat = NLTECoeff_netCDF_WriteFile( &
                   Filename , &
                   NLTECoeff, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION NLTECoeff_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_netCDF_to_Binary
!
! PURPOSE:
!       Function to convert a netCDF NLTECoeff file to Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_netCDF_to_Binary( &
!                        NC_Filename  , &
!                        BIN_Filename , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format NLTECoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       BIN_Filename:   Character string specifying the name of the
!                       Binary format NLTECoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
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
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the file conversion was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION NLTECoeff_netCDF_to_Binary( &
    NC_Filename , &  ! Input
    BIN_Filename, &  ! Input
    Quiet       ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: NC_Filename
    CHARACTER(*),      INTENT(IN)  :: BIN_Filename
    LOGICAL, OPTIONAL, INTENT(IN)  :: Quiet
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_netCDF_to_Binary'
    ! Function variables
    CHARACTER(256) :: msg
    TYPE(NLTECoeff_type) :: nltecoeff, nltecoeff_copy
    
    ! Set up
    err_stat = SUCCESS

    ! Read the netCDF file
    err_stat = NLTECoeff_ReadFile( NC_Filename, nltecoeff, Quiet = Quiet, netCDF = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading netCDF file '//TRIM(NC_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Write the Binary file
    err_stat = NLTECoeff_WriteFile( BIN_Filename, nltecoeff, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Binary file '//TRIM(BIN_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Check the write was successful
    ! ...Read the Binary file
    err_stat = NLTECoeff_ReadFile( BIN_Filename, nltecoeff_copy, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Binary file '//TRIM(BIN_Filename)//' for test'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Compare the NLTECoeff objects
    IF ( .NOT. (nltecoeff == nltecoeff_copy) ) THEN
      err_stat = FAILURE
      msg = 'NLTECoeff object comparison failed.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION NLTECoeff_netCDF_to_Binary


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the I/O modules.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_IOVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information
!               for the I/O module(s). If the string length is sufficient,
!               the version information for all the modules (this, the
!               Binary I/O, and netCDF I/O modules) are concatenated. Otherwise
!               only the version id for this module is returned.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    INTEGER, PARAMETER :: SL = 256
    CHARACTER(SL)   :: Binary_IO_Id, netCDF_IO_Id
    CHARACTER(SL*3) :: IO_Id
    CALL NLTECoeff_Binary_IOVersion( Binary_IO_Id )
    CALL NLTECoeff_netCDF_IOVersion( netCDF_IO_Id )
    IO_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '  '//TRIM(Binary_IO_Id)//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '  '//TRIM(netCDF_IO_Id)
    IF ( LEN_TRIM(IO_Id) <= LEN(Id) ) THEN
      Id = IO_Id
    ELSE
      Id = MODULE_VERSION_ID
    END IF
  END SUBROUTINE NLTECoeff_IOVersion

END MODULE NLTECoeff_IO
