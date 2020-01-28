!
! LBLRTM_Phdr_IO
!
! Module containing procedures to read and write LBLRTM Panel header objects
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 23-Jan-2000
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_Phdr_IO

  ! ------------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds        , ONLY: FP, IP, DP => Double
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility      , ONLY: File_Open
  USE LBLRTM_Parameters , ONLY: LBLRTM_FILE_EOF, &
                                LBLRTM_FILE_EOL, &
                                LBLRTM_FILE_OK , &
                                LBLRTM_FILE_UNDEF
  USE LBLRTM_Phdr_Define, ONLY: LBLRTM_Phdr_type, &
                                LBLRTM_Phdr_SetValid, &
                                LBLRTM_Phdr_IsValid , &
                                LBLRTM_Phdr_Destroy , &
                                LBLRTM_Phdr_Inspect
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_Phdr_Read
  PUBLIC :: LBLRTM_Phdr_Write
  PUBLIC :: LBLRTM_Phdr_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: LBLRTM_Phdr_IO.f90 35139 2013-12-26 18:14:43Z paul.vandelst@noaa.gov $'
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
!       LBLRTM_Phdr_Read
!
! PURPOSE:
!       Function to read an LBLRTM panel header from an LBLRTM format
!       file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_Phdr_Read( &
!                        LBLRTM_Phdr, &
!                        FileId     , &
!                        EOF          )
!
! OBJECTS:
!       LBLRTM_Phdr:  LBLRTM panel header object to hold the data.
!                     UNITS:      N/A
!                     TYPE:       LBLRTM_Phdr_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       FileId:       The unit number for the already open LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       EOF:          Integer flag indicating end-of-file status for the
!                     LBLRTM format file after the read. Valid return values
!                     are defined in the LBLRTM_Parameters module.
!                     If == LBLRTM_FILE_EOF:   End-Of-File has been reached.
!                                              The file is then closed.
!                        == LBLRTM_FILE_EOL:   End-Of-Layer has been reached.
!                                              In this case, the next read
!                                              should be of the file header
!                                              for the (possible) next layer.
!                        == LBLRTM_FILE_OK:    No EOF or EOL condition. The
!                                              next read should be the panel
!                                              data.
!                        == LBLRTM_FILE_UNDEF: An error occurred. The file is
!                                              closed.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM panel header read was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs or the end-of-file is encountered, the input file is
!       closed.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LBLRTM_Phdr_Read( &
    Phdr  , &  ! Output
    FileId, &  ! Input
    EoF   , &  ! Output
    Debug ) &  ! Optional input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_Phdr_type), INTENT(OUT) :: Phdr
    INTEGER               , INTENT(IN)  :: FileId
    INTEGER               , INTENT(OUT) :: EoF
    LOGICAL,     OPTIONAL , INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Phdr_IO::Read'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: debug_output

    ! Setup
    err_stat = SUCCESS
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(debug) ) debug_output = debug
    IF ( debug_output ) CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)


    ! Check if file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'LBLRTM file is not open'
      CALL Read_CleanUp(); RETURN
    END IF


    ! Read the data
    READ( FileId,IOSTAT=io_stat,IOMSG=io_msg ) &
      Phdr%Begin_Frequency   , &
      Phdr%End_Frequency     , &
      Phdr%Frequency_Interval, &
      Phdr%n_Points


    ! Check the read status
    SELECT CASE (io_stat)

      ! ...Error occurred in read
      CASE ( 1: )
        msg = 'Error reading LBLRTM file panel header - '//TRIM(io_msg)
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


    ! Check for End-Of-Layer
    IF ( Phdr%n_Points < 0 ) EoF = LBLRTM_FILE_EOL


    ! Tag object as valid
    CALL LBLRTM_Phdr_SetValid(Phdr)
    IF ( debug_output ) CALL LBLRTM_Phdr_Inspect(Phdr)

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open(FileId) ) THEN
        CLOSE( FileId,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL LBLRTM_Phdr_Destroy(Phdr)
      err_stat = FAILURE
      EoF      = LBLRTM_FILE_UNDEF
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION LBLRTM_Phdr_Read


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Phdr_Write
!
! PURPOSE:
!       Function to write an LBLRTM panel header to an LBLRTM format
!       file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_Phdr_Write( &
!                        LBLRTM_Phdr, &
!                        FileId       )
!
! OBJECTS:
!       LBLRTM_Phdr:  LBLRTM panel header object to write to file.
!                     UNITS:      N/A
!                     TYPE:       LBLRTM_Phdr_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       FileId:       The unit number for the already open LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM panel header write was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs, the output file is closed.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LBLRTM_Phdr_Write( &
    Phdr  , &  ! Input
    FileId, &  ! Input
    Debug ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    TYPE(LBLRTM_Phdr_type), INTENT(IN) :: Phdr
    INTEGER               , INTENT(IN) :: FileId
    LOGICAL,     OPTIONAL , INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Phdr_IO::Write'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: debug_output

    ! Setup
    err_stat = SUCCESS
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(debug) ) debug_output = debug
    IF ( debug_output ) CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)


    ! Check if structure is valid
    IF ( .NOT. LBLRTM_Phdr_IsValid(Phdr) ) THEN
      msg = 'Invalid LBLRTM Panel header'
      CALL Write_CleanUp(); RETURN
    END IF


    ! Check if file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'LBLRTM file is not open'
      CALL Write_CleanUp(); RETURN
    END IF


    ! Write the data
    WRITE( FileId,IOSTAT=io_stat,IOMSG=io_msg ) &
      Phdr%Begin_Frequency   , &
      Phdr%End_Frequency     , &
      Phdr%Frequency_Interval, &
      Phdr%n_Points
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing LBLRTM file panel header - '//TRIM(io_msg)
      CALL Write_CleanUp(); RETURN
    END IF

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

  END FUNCTION LBLRTM_Phdr_Write


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Phdr_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the module.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Phdr_IOVersion( Id )
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

  SUBROUTINE LBLRTM_Phdr_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_Phdr_IOVersion

END MODULE LBLRTM_Phdr_IO
