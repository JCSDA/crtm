!
! BeCoeff_IO
!
! Container module for Binary and netCDF BeCoeff I/O modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Feb-2010
!                       paul.vandelst@noaa.gov
!

MODULE BeCoeff_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility     , ONLY: File_Exists
  USE BeCoeff_Define   , ONLY: BeCoeff_type
  USE BeCoeff_Binary_IO, ONLY: BeCoeff_Binary_InquireFile, &
                               BeCoeff_Binary_ReadFile   , &
                               BeCoeff_Binary_WriteFile  , &
                               BeCoeff_Binary_IOVersion
  USE BeCoeff_netCDF_IO, ONLY: BeCoeff_netCDF_InquireFile, &
                               BeCoeff_netCDF_ReadFile   , &
                               BeCoeff_netCDF_WriteFile  , &
                               BeCoeff_netCDF_IOVersion
  ! Disable implicit typing
  IMPLICIT NONE
  
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: BeCoeff_InquireFile
  PUBLIC :: BeCoeff_ReadFile
  PUBLIC :: BeCoeff_WriteFile
  PUBLIC :: BeCoeff_IOVersion


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
!       BeCoeff_InquireFile
!
! PURPOSE:
!       Function to inquire BeCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = BeCoeff_InquireFile( &
!                        Filename, &
!                        netCDF       = netCDF      , &
!                        n_Latitudes  = n_Latitudes , &
!                        n_Longitudes = n_Longitudes, &
!                        Release      = Release     , &
!                        Version      = Version     , &
!                        Title        = Title       , &
!                        History      = History     , &
!                        Comment      = Comment       )  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       BeCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       BeCoeff datafiles.
!                       If == .FALSE., file format is BINARY [DEFAULT].
!                          == .TRUE.,  file format is NETCDF.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Latitudes:    Number of regularly-spaced latitudes from 90S to 90N for
!                       which there is geomagnetic field data. 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Longitudes:   Number of regularly-spaced longitudes from 0 to 360E for
!                       which there is geomagnetic field data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:        The release number of the BeCoeff file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:        The version number of the BeCoeff file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the BeCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the BeCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the BeCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file inquire was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION BeCoeff_InquireFile( &
    Filename    , &  ! Input
    netCDF      , &  ! Optional input
    n_Latitudes , &  ! Optional output
    n_Longitudes, &  ! Optional output
    Release     , &  ! Optional output
    Version     , &  ! Optional output
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Latitudes     
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Longitudes    
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release         
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version         
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
      err_stat = BeCoeff_Binary_InquireFile( &
                   Filename, &
                   n_Latitudes  = n_Latitudes , &
                   n_Longitudes = n_Longitudes, &
                   Release      = Release     , &
                   Version      = Version       )
    ELSE
      err_stat = BeCoeff_netCDF_InquireFile( &
                   Filename, &
                   n_Latitudes  = n_Latitudes , &
                   n_Longitudes = n_Longitudes, &
                   Release      = Release     , &
                   Version      = Version     , &
                   Title        = Title       , &
                   History      = History     , &
                   Comment      = Comment       )
    END IF

  END FUNCTION BeCoeff_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_ReadFile
!
! PURPOSE:
!       Function to read BeCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = BeCoeff_ReadFile( &
!                        Filename         , &
!                        BeCoeff          , &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       BeCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       BeCoeff:        BeCoeff object containing the geomagnetic field data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(BeCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       BeCoeff datafiles.
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
!                       attribute field of the BeCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the BeCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the BeCoeff file.
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

  FUNCTION BeCoeff_ReadFile( &
    Filename, &  ! Input
    BeCoeff , &  ! Output
    netCDF  , &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(BeCoeff_type),     INTENT(OUT) :: BeCoeff
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
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
      err_stat = BeCoeff_Binary_ReadFile( &
                   Filename, &
                   BeCoeff , &
                   Quiet = Quiet )
    ELSE
      err_stat = BeCoeff_netCDF_ReadFile( &
                   Filename, &
                   BeCoeff , &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION BeCoeff_ReadFile

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_WriteFile
!
! PURPOSE:
!       Function to write BeCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = BeCoeff_WriteFile( &
!                        Filename, &
!                        BeCoeff , &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       BeCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       BeCoeff:        Object containing the geomagnetic field data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(BeCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       BeCoeff datafiles.
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
!                       attribute field of the BeCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the BeCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the BeCoeff file.
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

  FUNCTION BeCoeff_WriteFile( &
    Filename, &  ! Input
    BeCoeff , &  ! Input
    netCDF  , &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(BeCoeff_type),     INTENT(IN) :: BeCoeff
    LOGICAL,      OPTIONAL, INTENT(IN) :: netCDF
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
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
      err_stat = BeCoeff_Binary_WriteFile( &
                   Filename, &
                   BeCoeff , &
                   Quiet = Quiet )
    ELSE
      err_stat = BeCoeff_netCDF_WriteFile( &
                   Filename, &
                   BeCoeff , &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION BeCoeff_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the I/O modules.
!
! CALLING SEQUENCE:
!       CALL BeCoeff_IOVersion( Id )
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

  SUBROUTINE BeCoeff_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    INTEGER, PARAMETER :: SL = 256
    CHARACTER(SL)   :: Binary_IO_Id, netCDF_IO_Id
    CHARACTER(SL*3) :: IO_Id
    CALL BeCoeff_Binary_IOVersion( Binary_IO_Id )
    CALL BeCoeff_netCDF_IOVersion( netCDF_IO_Id )
    IO_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '  '//TRIM(Binary_IO_Id)//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '  '//TRIM(netCDF_IO_Id)
    IF ( LEN_TRIM(IO_Id) <= LEN(Id) ) THEN
      Id = IO_Id
    ELSE
      Id = MODULE_VERSION_ID
    END IF
  END SUBROUTINE BeCoeff_IOVersion

END MODULE BeCoeff_IO
