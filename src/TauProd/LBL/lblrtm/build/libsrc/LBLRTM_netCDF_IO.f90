!
! LBLRTM_netCDF_IO
!
! Container module for reading and writing netCDF format LBLRTM filez.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-Feb-2014
!                       paul.vandelst@noaa.gov
!

MODULE LBLRTM_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: FP, IP, DP => Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE LBLRTM_File_Define   , ONLY: LBLRTM_File_type
  USE LBLRTM_File_IO       , ONLY: LBLRTM_File_Read!, &
                                   !LBLRTM_File_IOVersion
  USE LBLRTM_File_netCDF_IO, ONLY: LBLRTM_netCDF_InquireFile => LBLRTM_File_netCDF_Inquire  , &
                                   LBLRTM_netCDF_WriteFile   => LBLRTM_File_netCDF_Write    , &
                                   LBLRTM_netCDF_ReadFile    => LBLRTM_File_netCDF_Read     !, &
!                                   LBLRTM_netCDF_IOVersion   => LBLRTM_File_netCDF_IOVersion
  USE netcdf
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_netCDF_InquireFile
  PUBLIC :: LBLRTM_netCDF_WriteFile
  PUBLIC :: LBLRTM_netCDF_ReadFile
  !PUBLIC :: LBLRTM_netCDF_IOVersion
  PUBLIC :: LBLRTM_netCDF_ConvertFile


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = 'Dummy'
  ! Default message string length
  INTEGER, PARAMETER :: ML = 1024


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE MODULE ROUTINES ##                     ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_netCDF_ConvertFile
!
! PURPOSE:
!       Function to convert an LBLRTM "binary" format file to a netCDF
!       format file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_netCDF_ConvertFile( &
!                        Filename, &
!                        n_Layers     = n_Layers    , &
!                        Double_Panel = Double_Panel, &
!                        Title        = Title       , &
!                        History      = History     , &
!                        Comment      = Comment     , &
!                        Quiet        = Quiet         )
!
! INPUTS:
!       Filename:      Character string specifying the name of the
!                      LBLRTM "binary" file to convert.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_Layers:      Number of layers of spectral data to read from the
!                      LBLRTM "binary" format file.
!                      If not specified, the number of layers read is 1.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Double_Panel:  Set this logical argument to indicate that the LBLRTM
!                      "binary" format file is a double-panel file.
!                      If == .FALSE., the input file is assumed to be single panel. [DEFAULT]
!                         == .TRUE.,  the input file is assumed to be double panel.
!                      If not specified, default is .FALSE.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:         Character string written into the TITLE global
!                      attribute field of the LBLRTM netCDF file.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:       Character string written into the HISTORY global
!                      attribute field of the LBLRTM netCDF file.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:       Character string written into the COMMENT global
!                      attribute field of the LBLRTM netCDF file.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Quiet:         Set this logical argument to suppress INFORMATION
!                      messages being printed to stdout
!                      If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                         == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                      If not specified, default is .FALSE.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error
!                      status. The error codes are defined in the
!                      Message_Handler module.
!                      If == SUCCESS the file conversion was successful
!                         == FAILURE an error occurred.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION LBLRTM_netCDF_ConvertFile( &
    Filename    , &  ! Input
    n_Layers    , &  ! Optional input
    Double_Panel, &  ! Optional input
    Title       , &  ! Optional input
    History     , &  ! Optional input
    Comment     , &  ! Optional input
    Quiet       , &  ! Optional input
    Debug       ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    INTEGER,      OPTIONAL, INTENT(IN) :: n_Layers
    LOGICAL,      OPTIONAL, INTENT(IN) :: Double_Panel
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_netCDF_IO::ConvertFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: nc_file
    TYPE(LBLRTM_File_type) :: ofile

    ! Set up
    err_stat = SUCCESS
    nc_file  = TRIM(Filename)//'.nc'


    ! Read the input file
    err_stat = LBLRTM_File_Read( &
      ofile   , &
      Filename, &
      n_Layers     = n_Layers    , &
      Double_Panel = Double_Panel, &
      Quiet        = Quiet       , &
      Debug        = Debug       )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading the "binary" LBLRTM file '//TRIM(Filename)
      CALL Convert_CleanUp(); RETURN
    END IF
      

    ! Write the output file
    err_stat = LBLRTM_netCDF_WriteFile( &
      ofile  , &
      nc_file, &
      Quiet   = Quiet  , &
      Clobber = .TRUE. , &
      Title   = Title  , &
      History = History, &
      Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing the netCDF LBLRTM file '//TRIM(nc_file)
      CALL Convert_CleanUp(); RETURN
    END IF
      
  CONTAINS

    SUBROUTINE Convert_CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Convert_CleanUp

  END FUNCTION LBLRTM_netCDF_ConvertFile

END MODULE LBLRTM_netCDF_IO
