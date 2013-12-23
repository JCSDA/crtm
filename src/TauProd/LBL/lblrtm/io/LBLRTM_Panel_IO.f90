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
                                 LBLRTM_Panel_Create
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


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
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
!                        Double_Panel  )
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
    Debug       ) &  ! Optional input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_Panel_type), INTENT(OUT) :: Panel
    INTEGER                , INTENT(IN)  :: FileId
    INTEGER                , INTENT(OUT) :: EoF
    LOGICAL,      OPTIONAL , INTENT(IN)  :: Double_Panel
    LOGICAL,      OPTIONAL , INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Panel_IO::Read'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: single_panel
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
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(debug) ) debug_output = debug
    IF ( debug_output ) CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
    ! ...Check if file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'LBLRTM file is not open'
      CALL Read_CleanUp(); RETURN
    END IF


    ! Read the panel header
    err_stat = LBLRTM_Phdr_Read( phdr,FileId,EoF,Debug=debug )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading panel header'
      CALL Read_Cleanup(); RETURN
    END IF

    
    ! Check for End-of-Layer or End-of-File
    IF ( EoF == LBLRTM_FILE_EOL .OR. &
         EoF == LBLRTM_FILE_EOF      ) RETURN


    ! Allocate the panel object
    CALL LBLRTM_Panel_Create( Panel,phdr,n_spectra )
    IF ( .NOT. LBLRTM_Panel_Associated(Panel) ) THEN
      msg = 'Panel object allocation failed'
      CALL Read_Cleanup(); RETURN
    END IF
    
    
    ! Read the spectral data
    DO i = 1, n_spectra
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
!                        FileId        )
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
    FileId) &  ! Input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_Panel_type), INTENT(IN) :: Panel
    INTEGER                , INTENT(IN) :: FileId
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Panel_IO::Write'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: i

    ! Setup
    err_stat = SUCCESS
    ! ...Check input
    IF ( .NOT. LBLRTM_Panel_IsValid(Panel) ) THEN
      msg = 'Input panel does not contain valid data'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'LBLRTM file is not open'
      CALL Write_CleanUp(); RETURN
    END IF


    ! Write the panel header
    err_stat = LBLRTM_Phdr_Write( Panel%Header,FileId )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading panel header'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the spectral data
    DO i = 1, Panel%n_Spectra
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

END MODULE LBLRTM_Panel_IO
