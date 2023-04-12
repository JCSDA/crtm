!
! IRsnowCoeff_IO
!
! Container module for Binary and netCDF IRsnowCoeff I/O modules.
! All Binary related modules are placeholder for now.
!
! CREATION HISTORY:
!
!       Written by:    Cheng Dang, 23-May-2022
!                      dangch@ucar.edu

MODULE IRsnowCoeff_IO

    ! -----------------
    ! Environment setup
    ! -----------------
    ! Module use
    USE Type_Kinds             , ONLY: fp
    USE Message_Handler        , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
    USE Compare_Float_Numbers  , ONLY: OPERATOR(.EqualTo.)
    USE File_Utility           , ONLY: File_Exists
    USE IRsnowCoeff_Define     , ONLY: IRsnowCoeff_type, &
                                       OPERATOR(==), &
                                       IRsnowCoeff_Associated
    USE IRsnowCoeff_Binary_IO  , ONLY: IRsnowCoeff_Binary_InquireFile , &
                                       IRsnowCoeff_Binary_ReadFile    , &
                                       IRsnowCoeff_Binary_WriteFile
    USE IRsnowCoeff_netCDF_IO  , ONLY: IRsnowCoeff_netCDF_InquireFile , &
                                       IRsnowCoeff_netCDF_ReadFile    , &
                                       IRsnowCoeff_netCDF_WriteFile
    ! Disable implicit typing
    IMPLICIT NONE

    ! ------------
    ! Visibilities
    ! ------------
    PRIVATE
    PUBLIC :: IRsnowCoeff_InquireFile
    PUBLIC :: IRsnowCoeff_ReadFile
    PUBLIC :: IRsnowCoeff_WriteFile
    PUBLIC :: IRsnowCoeff_netCDF_to_Binary
    PUBLIC :: IRsnowCoeff_Binary_to_netCDF

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
!       IRsnowCoeff_InquireFile
!
! PURPOSE:
!       Function to inquire IRsnowCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = IRsnowCoeff_InquireFile( &
!                        Filename, &
!                        netCDF           = netCDF          , &
!                        n_Angles         = n_Angles        , &
!                        n_Frequencies    = n_Frequencies   , &
!                        n_Grain_Sizes    = n_Grain_Sizes   , &
!                        n_Temperature    = n_Temperature   , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          IRsnowCoeff data file to read.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:            Set this logical argument to access netCDF format
!                          IRsnowCoeff datafiles.
!                          If == .FALSE., file format is BINARY [DEFAULT].
!                             == .TRUE.,  file format is NETCDF.
!                          If not specified, default is .FALSE.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Angles:          The number of angles in the look-up
!                          table (LUT). Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies:     The number of frequencies in the LUT.
!                          Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Grain_Sizes:     The number of grain size in
!                          the LUT. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!       n_Temperature:     The number of temperature in
!                          the LUT. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Release:           The release number of the IRsnowCoeff file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The version number of the IRsnowCoeff file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:             Character string written into the TITLE global
!                          attribute field of the IRsnowCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the IRsnowCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the IRsnowCoeff file.
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

  FUNCTION IRsnowCoeff_InquireFile( &
    Filename       , &  ! Input
    netCDF         , &  ! Optional input
    n_Angles       , &  ! Optional output
    n_Frequencies  , &  ! Optional output
    n_Grain_Sizes  , &  ! Optional output
    n_Temperature  , &  ! Optional output
    Release        , &  ! Optional output
    Version        , &  ! Optional output
    Title          , &  ! Optional output
    History        , &  ! Optional output
    Comment        ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Angles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Grain_Sizes
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Temperature
    LOGICAL     , OPTIONAL, INTENT(IN)  :: netCDF
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function variables
    LOGICAL :: Binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    Binary = .TRUE.
    IF ( PRESENT(netCDF) ) Binary = .NOT. netCDF


    ! Call the appropriate function
    IF ( Binary ) THEN
      err_stat = IRsnowCoeff_Binary_InquireFile( &
                   Filename                         , &
                   n_Angles        = n_Angles       , &
                   n_Frequencies   = n_Frequencies  , &
                   n_Grain_Sizes   = n_Grain_Sizes  , &
                   n_Temperature   = n_Temperature  , &
                   Release         = Release        , &
                   Version         = Version          )
    ELSE
      err_stat = IRsnowCoeff_netCDF_InquireFile( &
                  Filename                         , &
                  n_Angles        = n_Angles       , &
                  n_Frequencies   = n_Frequencies  , &
                  n_Grain_Sizes   = n_Grain_Sizes  , &
                  n_Temperature   = n_Temperature  , &
                  Release         = Release        , &
                  Version         = Version        , &
                  Title           = Title          , &
                  History         = History        , &
                  Comment         = Comment          )
    END IF

  END FUNCTION IRsnowCoeff_InquireFile

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRsnowCoeff_ReadFile
!
! PURPOSE:
!       Function to read IRsnowCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = IRsnowCoeff_ReadFile( &
!                        IRsnowCoeff, &
!                        Filename, &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          IRsnowCoeff data file to read.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       IRsnowCoeff:   Object containing the IRsnow coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(IRsnowCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       IRsnowCoeff datafiles.
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
!       Title:             Character string written into the TITLE global
!                          attribute field of the IRsnowCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the IRsnowCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the IRsnowCoeff file.
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
  FUNCTION IRsnowCoeff_ReadFile( &
    IRsnowCoeff , &  ! Output
    Filename    , &  ! Input
    netCDF      , &  ! Optional input
    No_Close    , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     , &  ! Optional output
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(IRsnowCoeff_type), INTENT(OUT) :: IRsnowCoeff
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function variables
    LOGICAL :: Binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    Binary = .TRUE.
    IF ( PRESENT(netCDF) ) Binary = .NOT. netCDF

    ! Call the appropriate function
    IF ( Binary ) THEN
      err_stat = IRsnowCoeff_Binary_ReadFile( &
                    IRsnowCoeff , &
                    Filename    , &
                    No_Close    , &
                    Quiet         )
    ELSE
      err_stat = IRsnowCoeff_netCDF_ReadFile( &
                    IRsnowCoeff , &
                    Filename    , &
                    Quiet       , &
                    Title       , &
                    History     , &
                    Comment     , &
                    Debug       )
    END IF

  END FUNCTION IRsnowCoeff_ReadFile

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRsnowCoeff_WriteFile
!
! PURPOSE:
!       Function to write IRsnowCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = IRsnowCoeff_WriteFile( &
!                        IRsnowCoeff, &
!                        Filename, &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       IRsnowCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       IRsnowCoeff:   Object containing the IRsnow coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(IRsnowCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       IRsnowCoeff datafiles.
!                       If == .FALSE., file format is BINARY [DEFAULT].
!                          == .TRUE.,  file format is NETCDF.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the IRsnowCoeff data is embedded within another file.
!                       If == .FALSE., File is closed upon function exit [DEFAULT].
!                          == .TRUE.,  File is NOT closed upon function exit
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
!                       attribute field of the IRsnowCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the IRsnowCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the IRsnowCoeff file.
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

  FUNCTION IRsnowCoeff_WriteFile( &
    IRsnowCoeff, &  ! Input
    Filename    , &  ! Input
    netCDF      , &  ! Optional input
    No_Close    , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional input
    History     , &  ! Optional input
    Comment     , &  ! Optional input
    Debug       ) &  ! Optional input (Debug output control)
  RESULT ( err_stat )
    ! Arguments
    TYPE(IRsnowCoeff_type),  INTENT(IN) :: IRsnowCoeff
    CHARACTER(*),             INTENT(IN) :: Filename
    LOGICAL,        OPTIONAL, INTENT(IN) :: netCDF
    LOGICAL,        OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,        OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*),   OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*),   OPTIONAL, INTENT(IN) :: History
    CHARACTER(*),   OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,        OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local variables
    LOGICAL :: Binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    Binary = .TRUE.
    IF ( PRESENT(netCDF) ) Binary = .NOT. netCDF

    ! Call the appropriate function
    IF ( Binary ) THEN
      err_stat = IRsnowCoeff_Binary_WriteFile( &
                    IRsnowCoeff, &
                    Filename   , &
                    No_Close   , &
                    Quiet      , &
                    Title      , &
                    History    , &
                    Comment    , &
                    Debug        )
    ELSE
      err_stat = IRsnowCoeff_netCDF_WriteFile( &
                    IRsnowCoeff, &
                    Filename   , &
                    Quiet      , &
                    Title      , &
                    History    , &
                    Comment    , &
                    Debug        )
    END IF

  END FUNCTION IRsnowCoeff_WriteFile

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRsnowCoeff_netCDF_to_Binary
!
! PURPOSE:
!       Function to convert a netCDF IRsnowCoeff file to Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = IRsnowCoeff_netCDF_to_Binary( &
!                        NC_Filename  , &
!                        BIN_Filename , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format IRsnowCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       BIN_Filename:   Character string specifying the name of the
!                       Binary format IRsnowCoeff data file to write.
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

  FUNCTION IRsnowCoeff_netCDF_to_Binary( &
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRsnowCoeff_netCDF_to_Binary'
    ! Function variables
    CHARACTER(256) :: msg
    TYPE(IRsnowCoeff_type) :: cc, cc_copy

    ! Set up
    err_stat = SUCCESS

    ! Read the netCDF file
    err_stat = IRsnowCoeff_ReadFile(cc, NC_Filename, Quiet = Quiet, netCDF = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading netCDF file '//TRIM(NC_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Write the Binary file
    err_stat = IRsnowCoeff_WriteFile(cc, BIN_Filename, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Binary file '//TRIM(BIN_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Check the write was successful
    ! ...Read the Binary file
    err_stat =  IRsnowCoeff_ReadFile(cc_copy, BIN_Filename, Quiet = Quiet)
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Binary file '//TRIM(BIN_Filename)//' for test'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! ...Compare the IRsnowCoeff objects
    IF ( .NOT. (cc == cc_copy) ) THEN
      msg = 'IRsnowCoeff object comparison failed.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION IRsnowCoeff_netCDF_to_Binary

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRsnowCoeff_Binary_to_netCDF
!
! PURPOSE:
!       Function to convert a binary IRsnowCoeff file to Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = IRsnowCoeff_Binary_to_netCDF( &
!                        BIN_Filename , &
!                        NC_Filename  , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       BIN_Filename:   Character string specifying the name of the
!                       Binary format IRsnowCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format IRsnowCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
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

  FUNCTION IRsnowCoeff_Binary_to_netCDF( &
    BIN_Filename, &  ! Input
    NC_Filename , &  ! Input
    Quiet       ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: BIN_Filename
    CHARACTER(*),      INTENT(IN)  :: NC_Filename
    LOGICAL, OPTIONAL, INTENT(IN)  :: Quiet
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRsnowCoeff_Binary_to_NetCDF'
    ! Function variables
    CHARACTER(256) :: msg
    TYPE(IRsnowCoeff_type) :: cc, cc_copy

    ! Set up
    err_stat = SUCCESS

    ! Read the binary file
    err_stat = IRsnowCoeff_ReadFile(cc, BIN_Filename, Quiet = Quiet)
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Binary file '//TRIM(BIN_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Write the netCDF file
    err_stat = IRsnowCoeff_WriteFile(cc, NC_Filename, Quiet = Quiet, netCDF = .TRUE.)
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing netCDF file '//TRIM(NC_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Check the write was successful
    ! ...Read the netCDF file
    err_stat =  IRsnowCoeff_ReadFile(cc_copy, NC_Filename, Quiet = Quiet, netCDF = .TRUE.)
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading netCDF file '//TRIM(NC_Filename)//' for test'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! ...Compare the IRsnowCoeff objects
    IF ( .NOT. (cc == cc_copy) ) THEN
      msg = 'IRsnowCoeff object comparison failed.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION IRsnowCoeff_Binary_to_netCDF

END MODULE IRsnowCoeff_IO
