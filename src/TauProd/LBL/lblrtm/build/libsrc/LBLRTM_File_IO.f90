!
! LBLRTM_File_IO
!
! Module containing procedures to read and write LBLRTM Layer objects
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 23-Jan-2000
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_File_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds         , ONLY: FP, IP, DP => Double
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility       , ONLY: Get_Lun, File_Open
  USE LBLRTM_Utility     , ONLY: LBLRTM_File_Open, &
                                 LBLRTM_EoF_Message
  USE LBLRTM_Parameters  , ONLY: LBLRTM_FILE_EOF, &
                                 LBLRTM_FILE_EOL, &
                                 LBLRTM_FILE_OK , &
                                 LBLRTM_FILE_UNDEF
  USE LBLRTM_Layer_Define, ONLY: LBLRTM_Layer_type      , &
                                 LBLRTM_Layer_Associated, &
                                 LBLRTM_Layer_SetValid  , &
                                 LBLRTM_Layer_IsValid   , &
                                 LBLRTM_Layer_Destroy   , &
                                 LBLRTM_Layer_Create
  USE LBLRTM_Layer_IO    , ONLY: LBLRTM_Layer_Read, &
                                 LBLRTM_Layer_Write
  USE LBLRTM_File_Define , ONLY: LBLRTM_File_type       , &
                                 LBLRTM_File_Associated, &
                                 LBLRTM_File_SetValid  , &
                                 LBLRTM_File_IsValid   , &
                                 LBLRTM_File_Destroy   , &
                                 LBLRTM_File_Create    , &
                                 LBLRTM_File_Inspect
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_File_Read
  PUBLIC :: LBLRTM_File_Write
  PUBLIC :: LBLRTM_File_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_File_Read
!
! PURPOSE:
!       Function to read an LBLRTM format file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_File_Read( &
!                        LBLRTM_File , &
!                        Filename    , &
!                        n_Layers     = n_Layers    , &
!                        Double_Panel = Double_Panel, &
!                        Quiet        = Quiet         )
!
! OBJECTS:
!       LBLRTM_File:   LBLRTM File object to hold the data.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_File_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:      The name of the LBLRTM file to read
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_Layers:      Number of layers of spectral data to read.
!                      If not specified, the number of layers read is 1.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Double_Panel:  Set this logical argument to indicate a double-panel file.
!                      If == .FALSE., the file is assumed to be single panel. [DEFAULT]
!                         == .TRUE.,  the file is assumed to be double panel.
!                      If not specified, default is .FALSE.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
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
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the LBLRTM file read was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LBLRTM_File_Read( &
    oFile       , &  ! Output
    Filename    , &  ! Input
    n_Layers    , &  ! Optional input
    Double_Panel, &  ! Optional input
    Quiet       , &  ! Optional input
    Debug       ) &  ! Optional input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_File_type), INTENT(OUT) :: oFile
    CHARACTER(*)          , INTENT(IN)  :: Filename
    INTEGER,     OPTIONAL , INTENT(IN)  :: n_Layers
    LOGICAL,     OPTIONAL , INTENT(IN)  :: Double_Panel
    LOGICAL,     OPTIONAL , INTENT(IN)  :: Quiet
    LOGICAL,     OPTIONAL , INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_File_IO::Read'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER :: k, n_layers_to_read
    INTEGER :: fid
    INTEGER :: eof

    ! Setup
    err_stat = SUCCESS
    ! ...Set layer count
    n_layers_to_read = 1
    IF ( PRESENT(n_Layers) ) n_layers_to_read = MAX(1,n_Layers)
    ! ...Set info output status
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(Debug) ) debug_output = Debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Open the file
    err_stat = LBLRTM_File_Open(Filename,fid,Quiet=Quiet,Debug=Debug)
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening LBLRTM file '//TRIM(Filename)
      CALL Read_CleanUp(); RETURN
    END IF


    ! Create the file object
    CALL LBLRTM_File_Create( oFile,n_layers_to_read )
    IF ( .NOT. LBLRTM_File_Associated(oFile) ) THEN
      msg = 'File object allocation failed'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Begin layer read loop
    Layer_Read_loop: DO k = 1, n_layers_to_read

      ! Progress info
      IF ( noisy ) THEN
        WRITE(msg,'("Reading layer #",i0,"...")') k
        CALL Display_Message(ROUTINE_NAME,msg,INFORMATION)
      END IF

      ! Read a layer of data
      err_stat = LBLRTM_Layer_Read( oFile%Layer(k),fid,eof,Double_Panel=Double_Panel,Quiet=Quiet,Debug=debug )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE(msg,'("Error reading layer #",i0)') k
        CALL Read_Cleanup(); RETURN
      END IF

      ! Check for End-of-File. An End-of-Layer is only
      ! flagged during a PANEL HEADER read.
      IF ( eof == LBLRTM_FILE_EOF ) EXIT Layer_Read_loop

    END DO Layer_Read_loop


    ! Output file status
    IF ( noisy ) CALL Display_Message( ROUTINE_NAME,LBLRTM_EoF_Message(eof),INFORMATION )


    ! Close the input file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing input file after read - '//TRIM(io_msg)//'... continuing'
      CALL Display_Message( ROUTINE_NAME,msg,INFORMATION)
    END IF


    ! Tag object as valid
    CALL LBLRTM_File_SetValid(oFile)
    IF ( debug_output ) CALL LBLRTM_File_Inspect(oFile)

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL LBLRTM_File_Destroy(oFile)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION LBLRTM_File_Read


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_File_Write
!
! PURPOSE:
!       Function to write an LBLRTM format file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_File_Write( &
!                        LBLRTM_File , &
!                        Filename    , &
!                        Quiet = Quiet )
!
! OBJECTS:
!       LBLRTM_File:   LBLRTM File object to write to file.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_File_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:      The name of the LBLRTM file to write.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
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
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the LBLRTM file write was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LBLRTM_File_Write( &
    oFile       , &  ! Input
    Filename    , &  ! Input
    Quiet       , &  ! Optional input
    Debug       ) &  ! Optional input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_File_type), INTENT(IN) :: oFile
    CHARACTER(*)          , INTENT(IN) :: Filename
    LOGICAL,     OPTIONAL , INTENT(IN) :: Quiet
    LOGICAL,     OPTIONAL , INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_File_IO::Write'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER :: k
    INTEGER :: fid

    ! Setup
    err_stat = SUCCESS
    ! ...Set info output status
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(Debug) ) debug_output = Debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Open the file
    err_stat = LBLRTM_File_Open(Filename,fid,For_Output=.TRUE.,Quiet=Quiet,Debug=Debug)
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening LBLRTM file '//TRIM(Filename)
      CALL Write_CleanUp(); RETURN
    END IF


    ! Begin layer write loop
    Layer_Write_loop: DO k = 1, oFile%n_Layers

      ! Progress info
      IF ( noisy ) THEN
        WRITE(msg,'("Writing layer #",i0,"...")') k
        CALL Display_Message(ROUTINE_NAME,msg,INFORMATION)
      END IF

      ! Write a layer of data
      err_stat = LBLRTM_Layer_Write( oFile%Layer(k),fid,Quiet=Quiet,Debug=debug )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE(msg,'("Error writing layer #",i0)') k
        CALL Write_Cleanup(); RETURN
      END IF

    END DO Layer_Write_loop


    ! Close the output file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing output file after write - '//TRIM(io_msg)//'... continuing'
      CALL Display_Message( ROUTINE_NAME,msg,INFORMATION)
    END IF


  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,STATUS='DELETE',IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION LBLRTM_File_Write


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_File_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the module.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_File_IOVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information for the
!               module.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTM_File_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_File_IOVersion

END MODULE LBLRTM_File_IO
