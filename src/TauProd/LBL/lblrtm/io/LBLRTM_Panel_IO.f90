!
! LBLRTM_Panel_IO
!
! Module containing procedures to read and write LBLRTM Panel objects
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!

MODULE LBLRTM_Panel_IO

  ! ------------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds         , ONLY: FP, IP, DP => Double
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Open
  USE LBLRTM_Parameters  , ONLY: LBLRTM_FILE_EOF, &
                                 LBLRTM_FILE_EOL, &
                                 LBLRTM_FILE_OK , &
                                 LBLRTM_FILE_UNDEF
  USE LBLRTM_Panel_Define, ONLY: LBLRTM_Panel_type      , &
                                 LBLRTM_Panel_Associated, &
                                 LBLRTM_Panel_SetValid  , &
                                 LBLRTM_Panel_IsValid   , &
                                 LBLRTM_Panel_Destroy   , &
                                 LBLRTM_Panel_Create    , &
                                 LBLRTM_Panel_Inspect
  USE LBLRTM_Phdr_Define , ONLY: LBLRTM_Phdr_type
  USE LBLRTM_Phdr_IO     , ONLY: LBLRTM_Phdr_Read , &
                                 LBLRTM_Phdr_Write
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_Panel_Read
  PUBLIC :: LBLRTM_Panel_Write
  PUBLIC :: LBLRTM_Panel_IOVersion


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
!       LBLRTM_Panel_Read
!
! PURPOSE:
!       Function to read an LBLRTM panel from an LBLRTM format
!       file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_Panel_Read( &
!                        LBLRTM_Panel, &
!                        FileId      , &
!                        EOF         , &
!                        Double_Panel = Double_Panel, &
!                        Quiet        = Quiet         )
!
! OBJECTS:
!       LBLRTM_Panel:  LBLRTM panel object to hold the data.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Panel_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       FileId:        The unit number for the already open LBLRTM file.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       EOF:           Integer flag indicating end-of-file status for the
!                      LBLRTM format file after the read. Valid return values
!                      are defined in the LBLRTM_Parameters module.
!                      If == LBLRTM_FILE_EOF:   End-Of-File has been reached.
!                                               The file is then closed.
!                         == LBLRTM_FILE_OK:    No EOF or EOL condition. The
!                                               next read should be the panel
!                                               data.
!                         == LBLRTM_FILE_UNDEF: An error occurred. The file is
!                                               closed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Double_Panel:  Set this logical flag to indicate a double-panel file.
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
!                      If == SUCCESS the LBLRTM panel read was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs or the end-of-file is encountered, the input file is
!       closed.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LBLRTM_Panel_Read( &
    Panel       , &  ! Output
    FileId      , &  ! Input
    EoF         , &  ! Output
    Double_Panel, &  ! Optional input
    Quiet       , &  ! Optional input
    Debug       ) &  ! Optional input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_Panel_type), INTENT(OUT) :: Panel
    INTEGER                , INTENT(IN)  :: FileId
    INTEGER                , INTENT(OUT) :: EoF
    LOGICAL,      OPTIONAL , INTENT(IN)  :: Double_Panel
    LOGICAL,      OPTIONAL , INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL , INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Panel_IO::Read'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    CHARACTER(ML) :: err_msg
    INTEGER :: io_stat
    LOGICAL :: single_panel
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER :: i, n_spectra
    TYPE(LBLRTM_Phdr_type) :: phdr

    ! Setup
    err_stat = SUCCESS
    ! ...Set panel count
    single_panel = .TRUE.
    IF ( PRESENT(Double_Panel) ) single_panel = .NOT. Double_Panel
    IF ( single_panel ) THEN
      n_spectra = 1
    ELSE
      n_spectra = 2
    END IF
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


    ! Check if file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'LBLRTM file is not open'
      CALL Read_CleanUp(); RETURN
    END IF


    ! Read the panel header
    err_stat = LBLRTM_Phdr_Read( phdr,FileId,EoF,Debug=Debug )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading panel header'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Check for End-of-Layer or End-of-File
    IF ( EoF == LBLRTM_FILE_EOL .OR. &
         EoF == LBLRTM_FILE_EOF      ) RETURN


    ! Allocate the panel object
    CALL LBLRTM_Panel_Create( Panel,phdr,n_spectra,Err_Msg=err_msg )
    IF ( .NOT. LBLRTM_Panel_Associated(Panel) ) THEN
      msg = 'Panel object allocation failed - '//TRIM(err_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the spectral data
    DO i = 1, n_spectra

      ! Progress info
      IF ( noisy ) THEN
        WRITE(msg,'(4x,"Reading spectrum #",i0,"...")') i
        CALL Display_Message(ROUTINE_NAME,msg,INFORMATION)
      END IF

      ! Read data and process result
      READ( FileId,IOSTAT=io_stat,IOMSG=io_msg ) Panel%Spectrum(:,i)
      SELECT CASE (io_stat)
        ! ...Error occurred in read
        CASE ( 1: )
          msg = 'Error reading LBLRTM panel data - '//TRIM(io_msg)
          CALL Read_CleanUp(); RETURN
        ! ...End of file has been reached
        CASE (:-1)
          EoF = LBLRTM_FILE_EOF
          CLOSE(FileId)
          RETURN
        ! ...Read was successful, no errors
        CASE DEFAULT
          EoF = LBLRTM_FILE_OK
      END SELECT
    END DO


    ! Tag object as valid
    CALL LBLRTM_Panel_SetValid(Panel)
    IF ( debug_output ) CALL LBLRTM_Panel_Inspect(Panel)

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open(FileId) ) THEN
        CLOSE( FileId,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL LBLRTM_Panel_Destroy(Panel)
      err_stat = FAILURE
      EoF      = LBLRTM_FILE_UNDEF
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION LBLRTM_Panel_Read


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Panel_Write
!
! PURPOSE:
!       Function to write an LBLRTM panel to an LBLRTM format
!       file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_Panel_Write( &
!                        LBLRTM_Panel, &
!                        FileId      , &
!                        Quiet = Quiet )
!
! OBJECTS:
!       LBLRTM_Panel:  LBLRTM panel object to write to file.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Panel_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       FileId:        The unit number for the already open LBLRTM file.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
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
!                      If == SUCCESS the LBLRTM panel write was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs, the output file is closed.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LBLRTM_Panel_Write( &
    Panel , &  ! Input
    FileId, &  ! Input
    Quiet , &  ! Optional input
    Debug ) &  ! Optional input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_Panel_type), INTENT(IN) :: Panel
    INTEGER                , INTENT(IN) :: FileId
    LOGICAL,       OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Panel_IO::Write'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER :: i

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


    ! Check input
    IF ( .NOT. LBLRTM_Panel_IsValid(Panel) ) THEN
      msg = 'Input panel does not contain valid data'
      CALL Write_CleanUp(); RETURN
    END IF


    ! Check if file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'LBLRTM file is not open'
      CALL Write_CleanUp(); RETURN
    END IF


    ! Write the panel header
    err_stat = LBLRTM_Phdr_Write( Panel%Header,FileId,Debug=Debug )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing panel header'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the spectral data
    DO i = 1, Panel%n_Spectra

      ! Progress info
      IF ( noisy ) THEN
        WRITE(msg,'(4x,"Writing spectrum #",i0,"...")') i
        CALL Display_Message(ROUTINE_NAME,msg,INFORMATION)
      END IF

      ! Write spectrum and process result
      WRITE( FileId,IOSTAT=io_stat,IOMSG=io_msg ) Panel%Spectrum(:,i)
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing LBLRTM panel data - '//TRIM(io_msg)
        CALL Write_CleanUp(); RETURN
      END IF
    END DO

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( File_Open(FileId) ) THEN
        CLOSE( FileId,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION LBLRTM_Panel_Write


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Panel_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the module.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Panel_IOVersion( Id )
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

  SUBROUTINE LBLRTM_Panel_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_Panel_IOVersion

END MODULE LBLRTM_Panel_IO
