!
! SpcCoeff_IO
!
! Container module for Binary and netCDF SpcCoeff I/O modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 02-Feb-2011
!                       paul.vandelst@noaa.gov
!

MODULE SpcCoeff_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: fp
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Exists
  USE SpcCoeff_Define    , ONLY: SpcCoeff_type, OPERATOR(==)
  USE SpcCoeff_Binary_IO , ONLY: SpcCoeff_Binary_InquireFile, &
                                 SpcCoeff_Binary_ReadFile   , &
                                 SpcCoeff_Binary_WriteFile 
  USE SpcCoeff_netCDF_IO , ONLY: SpcCoeff_netCDF_InquireFile, &
                                 SpcCoeff_netCDF_ReadFile   , &
                                 SpcCoeff_netCDF_WriteFile
  USE ACCoeff_netCDF_IO  , ONLY: ACCoeff_netCDF_ReadFile
  USE NLTECoeff_netCDF_IO, ONLY: NLTECoeff_netCDF_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE
  
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: SpcCoeff_InquireFile
  PUBLIC :: SpcCoeff_ReadFile
  PUBLIC :: SpcCoeff_WriteFile
  PUBLIC :: SpcCoeff_netCDF_to_Binary
  !PUBLIC :: SpcCoeff_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  

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
!       SpcCoeff_InquireFile
!
! PURPOSE:
!       Function to inquire SpcCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_InquireFile( &
!                        Filename, &
!                        netCDF           = netCDF          , &
!                        n_Channels       = n_Channels      , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Sensor_Id        = Sensor_Id       , &
!                        WMO_Satellite_Id = WMO_Satellite_Id, &
!                        WMO_Sensor_Id    = WMO_Sensor_Id   , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          SpcCoeff data file to read.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:            Set this logical argument to access netCDF format
!                          SpcCoeff datafiles.
!                          If == .FALSE., file format is BINARY [DEFAULT].
!                             == .TRUE.,  file format is NETCDF.
!                          If not specified, default is .FALSE.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Channels:        Number of sensor channels.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:           The release number of the SpcCoeff file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The version number of the SpcCoeff file.
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
!                          attribute field of the SpcCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the SpcCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the SpcCoeff file.
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

  FUNCTION SpcCoeff_InquireFile( &
    Filename        , &  ! Input
    netCDF          , &  ! Optional input
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
      err_stat = SpcCoeff_Binary_InquireFile( &
                   Filename, &
                   n_Channels       = n_Channels      , &
                   Release          = Release         , &
                   Version          = Version         , &
                   Sensor_Id        = Sensor_Id       , &
                   WMO_Satellite_Id = WMO_Satellite_Id, &
                   WMO_Sensor_Id    = WMO_Sensor_Id     )
    ELSE
      err_stat = SpcCoeff_netCDF_InquireFile( &
                   Filename, &
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

  END FUNCTION SpcCoeff_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_ReadFile
!
! PURPOSE:
!       Function to read SpcCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_ReadFile( &
!                        Filename, &
!                        SpcCoeff , &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       SpcCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SpcCoeff:       SpcCoeff object containing the spectral
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       SpcCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       SpcCoeff datafiles.
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
!                       attribute field of the SpcCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the SpcCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the SpcCoeff file.
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

  FUNCTION SpcCoeff_ReadFile( &
    Filename, &  ! Input
    SpcCoeff, &  ! Output
    netCDF  , &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(SpcCoeff_type),    INTENT(OUT) :: SpcCoeff
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
      err_stat = SpcCoeff_Binary_ReadFile( &
                   Filename, &
                   SpcCoeff, &
                   Quiet = Quiet )
    ELSE
      err_stat = SpcCoeff_netCDF_ReadFile( &
                   Filename, &
                   SpcCoeff, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION SpcCoeff_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_WriteFile
!
! PURPOSE:
!       Function to write SpcCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_WriteFile( &
!                        Filename, &
!                        SpcCoeff, &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       SpcCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
! 
!       SpcCoeff:       SpcCoeff object containing the spectral
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       SpcCoeff_type 
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       SpcCoeff datafiles.
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
!                       attribute field of the SpcCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the SpcCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the SpcCoeff file.
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

  FUNCTION SpcCoeff_WriteFile( &
    Filename, &  ! Input
    SpcCoeff, &  ! Input
    netCDF  , &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(SpcCoeff_type),    INTENT(IN) :: SpcCoeff
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
      err_stat = SpcCoeff_Binary_WriteFile( &
                   Filename, &
                   SpcCoeff, &
                   Quiet = Quiet )
    ELSE
      err_stat = SpcCoeff_netCDF_WriteFile( &
                   Filename, &
                   SpcCoeff, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION SpcCoeff_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_netCDF_to_Binary
!
! PURPOSE:
!       Function to convert a netCDF SpcCoeff file to Binary format.
!
!       NOTE: If the netCDF files for the SpcCoeff substructure components
!             are also present, they are read and included in the binary
!             file output. The substructure components filenames are
!             constructed from the SpcCoeff sensor id, e.g.
!               sensor_id.ACCoeff.nc
!               sensor_id.NLTECoeff.nc
!             etc..
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_netCDF_to_Binary( &
!                        NC_Filename , &
!                        BIN_Filename, &
!                        Quiet   = Quiet  , &
!                        Version = Version  )
!
! INPUTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format SpcCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       BIN_Filename:   Character string specifying the name of the
!                       Binary format SpcCoeff data file to write.
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
!       Version:        Set this argument to the version number value to be
!                       used in the output binary file.
!                       If >  0, the value REPLACES the current Version.
!                          <= 0, the current version is INCREMENTED by the |value|.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
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

  FUNCTION SpcCoeff_netCDF_to_Binary( &
    NC_Filename , &  ! Input
    BIN_Filename, &  ! Input
    Quiet       , &  ! Optional input
    Version     ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: NC_Filename
    CHARACTER(*),      INTENT(IN)  :: BIN_Filename
    LOGICAL, OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER, OPTIONAL, INTENT(IN)  :: Version
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_netCDF_to_Binary'
    ! Function variables
    CHARACTER(256) :: msg
    CHARACTER(256) :: sub_filename
    TYPE(SpcCoeff_type) :: spccoeff, spccoeff_copy
    
    ! Set up
    err_stat = SUCCESS
    

    ! Read the netCDF file
    WRITE(*,'(/5x,"Reading the input netCDF datafile(s)...")')
    err_stat = SpcCoeff_netCDF_ReadFile( NC_Filename, spccoeff, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading netCDF file '//TRIM(NC_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF


    ! Update version number if necessary
    IF ( PRESENT(Version) ) THEN
      IF ( Version > 0 ) THEN
        spccoeff%Version = Version                          ! Replace
      ELSE
        spccoeff%Version = spccoeff%Version + ABS(Version)  ! Increment
      END IF
    END IF
      
    
    ! Read the substructure netCDF filenames
    ! ...ACCoeff
    sub_filename = TRIM(spccoeff%Sensor_Id)//'.ACCoeff.nc'
    IF ( File_Exists(sub_filename) ) THEN
      err_stat = ACCoeff_netCDF_ReadFile( sub_filename, spccoeff%AC, Quiet = Quiet )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading netCDF file '//TRIM(sub_filename)
        CALL Display_Message( ROUTINE_NAME, msg, err_stat )
        RETURN
      END IF
    END IF
    ! ...NLTECoeff
    sub_filename = TRIM(spccoeff%Sensor_Id)//'.NLTECoeff.nc'
    IF ( File_Exists(sub_filename) ) THEN
      err_stat = NLTECoeff_netCDF_ReadFile( sub_filename, spccoeff%NC, Quiet = Quiet )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading netCDF file '//TRIM(sub_filename)
        CALL Display_Message( ROUTINE_NAME, msg, err_stat )
        RETURN
      END IF
    END IF


    ! Write the Binary file
    WRITE(*,'(/5x,"Writing the output binary datafile...")')
    err_stat = SpcCoeff_WriteFile( BIN_Filename, spccoeff, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Binary file '//TRIM(BIN_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF


    ! Check the write was successful
    WRITE(*,'(/5x,"Test reading the output binary datafile...")')
    ! ...Read the Binary file
    err_stat = SpcCoeff_ReadFile( BIN_Filename, spccoeff_copy, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Binary file '//TRIM(BIN_Filename)//' for test'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Compare the SpcCoeff objects
    IF ( .NOT. (spccoeff == spccoeff_copy) ) THEN
      err_stat = FAILURE
      msg = 'SpcCoeff object comparison failed.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION SpcCoeff_netCDF_to_Binary


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the I/O modules.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_IOVersion( Id )
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

!  SUBROUTINE SpcCoeff_IOVersion( Id )
!    CHARACTER(*), INTENT(OUT) :: Id
!    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
!    INTEGER, PARAMETER :: LINEFEED = 10
!    INTEGER, PARAMETER :: SL = 256
!    CHARACTER(SL)   :: Binary_IO_Id, netCDF_IO_Id
!    CHARACTER(SL*3) :: IO_Id
!    Binary_IO_Id = 'Empty'
!    netCDF_IO_Id = 'Empty'
!    !CALL SpcCoeff_Binary_IOVersion( Binary_IO_Id )
!    !CALL SpcCoeff_netCDF_IOVersion( netCDF_IO_Id )
!    IO_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
!            '  '//TRIM(Binary_IO_Id)//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
!            '  '//TRIM(netCDF_IO_Id)
!    IF ( LEN_TRIM(IO_Id) <= LEN(Id) ) THEN
!      Id = IO_Id
!    ELSE
!      Id = MODULE_VERSION_ID
!    END IF
!  END SUBROUTINE SpcCoeff_IOVersion

END MODULE SpcCoeff_IO
