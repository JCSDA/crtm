!
! IRLSE_NPOESS_IO
!
! Container module for Binary and netCDF IRLSE_NPOESS I/O modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 15-Aug-2011
!                       paul.vandelst@noaa.gov
!

MODULE IRLSE_NPOESS_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility          , ONLY: File_Exists
  USE IRLSE_NPOESS_Define   , ONLY: IRLSE_NPOESS_type, OPERATOR(==)
  USE IRLSE_NPOESS_Binary_IO, ONLY: IRLSE_NPOESS_Binary_InquireFile, &
                                    IRLSE_NPOESS_Binary_ReadFile   , &
                                    IRLSE_NPOESS_Binary_WriteFile  , &
                                    IRLSE_NPOESS_Binary_IOVersion
  USE IRLSE_NPOESS_netCDF_IO, ONLY: IRLSE_NPOESS_netCDF_InquireFile, &
                                    IRLSE_NPOESS_netCDF_ReadFile   , &
                                    IRLSE_NPOESS_netCDF_WriteFile  , &
                                    IRLSE_NPOESS_netCDF_IOVersion
  ! Disable implicit typing
  IMPLICIT NONE
  
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: IRLSE_NPOESS_InquireFile
  PUBLIC :: IRLSE_NPOESS_ReadFile
  PUBLIC :: IRLSE_NPOESS_WriteFile
  PUBLIC :: IRLSE_NPOESS_netCDF_to_Binary
  PUBLIC :: IRLSE_NPOESS_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  

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
!       IRLSE_NPOESS_InquireFile
!
! PURPOSE:
!       Function to inquire IRLSE_NPOESS object files.
!
! CALLING SEQUENCE:
!       Error_Status = IRLSE_NPOESS_InquireFile( &
!                        Filename, &
!                        netCDF          = netCDF         , &
!                        n_Frequencies   = n_Frequencies  , &
!                        n_Surface_Types = n_Surface_Types, &
!                        Release         = Release        , &
!                        Version         = Version        , &
!                        Title           = Title          , &
!                        History         = History        , &
!                        Comment         = Comment          )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          IRLSE_NPOESS data file to inquire.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:            Set this logical argument to access netCDF format
!                          IRLSE_NPOESS datafiles.
!                          If == .FALSE., file format is BINARY [DEFAULT].
!                             == .TRUE.,  file format is NETCDF.
!                          If not specified, default is .FALSE.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Frequencies:     Number of spectral frequencies for which there are
!                          reflectance data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Surface_Types:   Number of land surface types for which is are
!                          reflectance data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:           The data/file release number. Used to check
!                          for data/software mismatch.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The data/file version number. Used for
!                          purposes only in identifying the dataset for
!                          a particular release.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:             Character string written into the TITLE global
!                          attribute field of the file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
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

  FUNCTION IRLSE_NPOESS_InquireFile( &
    Filename       , &  ! Input
    netCDF         , &  ! Optional input
    n_Frequencies  , &  ! Optional output  
    n_Surface_Types, &  ! Optional output  
    Release        , &  ! Optional Output
    Version        , &  ! Optional Output
    Title          , &  ! Optional output
    History        , &  ! Optional output
    Comment        ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Surface_Types
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version        
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
      err_stat = IRLSE_NPOESS_Binary_InquireFile( &
                   Filename                         , &
                   n_Frequencies   = n_Frequencies  , &
                   n_Surface_Types = n_Surface_Types, &
                   Release         = Release        , &
                   Version         = Version          )
    ELSE
      err_stat = IRLSE_NPOESS_netCDF_InquireFile( &
                   Filename                         , &
                   n_Frequencies   = n_Frequencies  , &
                   n_Surface_Types = n_Surface_Types, &
                   Release         = Release        , &
                   Version         = Version        , &
                   Title           = Title          , &
                   History         = History        , &
                   Comment         = Comment          )
    END IF

  END FUNCTION IRLSE_NPOESS_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_ReadFile
!
! PURPOSE:
!       Function to read IRLSE_NPOESS object files.
!
! CALLING SEQUENCE:
!       Error_Status = IRLSE_NPOESS_ReadFile( &
!                        Filename         , &
!                        IRLSE_NPOESS     , &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       IRLSE_NPOESS data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       IRLSE_NPOESS:   IRLSE_NPOESS object containing the data read.
!                       UNITS:      N/A
!                       TYPE:       IRLSE_NPOESS_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       IRLSE_NPOESS datafiles.
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
!                       attribute field of the file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the file.
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

  FUNCTION IRLSE_NPOESS_ReadFile( &
    Filename    , &  ! Input
    IRLSE_NPOESS, &  ! Output
    netCDF      , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(IRLSE_NPOESS_type), INTENT(OUT) :: IRLSE_NPOESS
    LOGICAL,       OPTIONAL, INTENT(IN)  :: netCDF
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Comment
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
      err_stat = IRLSE_NPOESS_Binary_ReadFile( &
                   Filename , &
                   IRLSE_NPOESS, &
                   Quiet = Quiet )
    ELSE
      err_stat = IRLSE_NPOESS_netCDF_ReadFile( &
                   Filename , &
                   IRLSE_NPOESS, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION IRLSE_NPOESS_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_WriteFile
!
! PURPOSE:
!       Function to write IRLSE_NPOESS object files.
!
! CALLING SEQUENCE:
!       Error_Status = IRLSE_NPOESS_WriteFile( &
!                        Filename , &
!                        IRLSE_NPOESS, &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       IRLSE_NPOESS data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       IRLSE_NPOESS:   IRLSE_NPOESS object containing the data to write.
!                       UNITS:      N/A
!                       TYPE:       IRLSE_NPOESS_type 
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       IRLSE_NPOESS datafiles.
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
!                       attribute field of the file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the file.
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

  FUNCTION IRLSE_NPOESS_WriteFile( &
    Filename    , &  ! Input
    IRLSE_NPOESS, &  ! Input
    netCDF      , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional input
    History     , &  ! Optional input
    Comment     ) &  ! Optional input
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN) :: Filename
    TYPE(IRLSE_NPOESS_type), INTENT(IN) :: IRLSE_NPOESS
    LOGICAL,       OPTIONAL, INTENT(IN) :: netCDF
    LOGICAL,       OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: History
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Comment
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
      err_stat = IRLSE_NPOESS_Binary_WriteFile( &
                   Filename , &
                   IRLSE_NPOESS, &
                   Quiet = Quiet )
    ELSE
      err_stat = IRLSE_NPOESS_netCDF_WriteFile( &
                   Filename , &
                   IRLSE_NPOESS, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION IRLSE_NPOESS_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_netCDF_to_Binary
!
! PURPOSE:
!       Function to convert a netCDF IRLSE_NPOESS file to Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = IRLSE_NPOESS_netCDF_to_Binary( &
!                        NC_Filename  , &
!                        BIN_Filename , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format IRLSE_NPOESS data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       BIN_Filename:   Character string specifying the name of the
!                       Binary format IRLSE_NPOESS data file to write.
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

  FUNCTION IRLSE_NPOESS_netCDF_to_Binary( &
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRLSE_NPOESS_netCDF_to_Binary'
    ! Function variables
    CHARACTER(256) :: msg
    TYPE(IRLSE_NPOESS_type) :: IRLSE_NPOESS, IRLSE_NPOESS_copy
    
    ! Set up
    err_stat = SUCCESS

    ! Read the netCDF file
    err_stat = IRLSE_NPOESS_ReadFile( NC_Filename, IRLSE_NPOESS, Quiet = Quiet, netCDF = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading netCDF file '//TRIM(NC_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Write the Binary file
    err_stat = IRLSE_NPOESS_WriteFile( BIN_Filename, IRLSE_NPOESS, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Binary file '//TRIM(BIN_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Check the write was successful
    ! ...Read the Binary file
    err_stat = IRLSE_NPOESS_ReadFile( BIN_Filename, IRLSE_NPOESS_copy, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Binary file '//TRIM(BIN_Filename)//' for test'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Compare the IRLSE_NPOESS objects
    IF ( .NOT. (IRLSE_NPOESS == IRLSE_NPOESS_copy) ) THEN
      err_stat = FAILURE
      msg = 'IRLSE_NPOESS object comparison failed.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION IRLSE_NPOESS_netCDF_to_Binary


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the I/O modules.
!
! CALLING SEQUENCE:
!       CALL IRLSE_NPOESS_IOVersion( Id )
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

  SUBROUTINE IRLSE_NPOESS_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    INTEGER, PARAMETER :: SL = 256
    CHARACTER(SL)   :: Binary_IO_Id, netCDF_IO_Id
    CHARACTER(SL*3) :: IO_Id
    CALL IRLSE_NPOESS_Binary_IOVersion( Binary_IO_Id )
    CALL IRLSE_NPOESS_netCDF_IOVersion( netCDF_IO_Id )
    IO_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '  '//TRIM(Binary_IO_Id)//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '  '//TRIM(netCDF_IO_Id)
    IF ( LEN_TRIM(IO_Id) <= LEN(Id) ) THEN
      Id = IO_Id
    ELSE
      Id = MODULE_VERSION_ID
    END IF
  END SUBROUTINE IRLSE_NPOESS_IOVersion

END MODULE IRLSE_NPOESS_IO
