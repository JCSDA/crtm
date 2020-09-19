!
! LBLRTM_Layer_IO
!
! Module containing procedures to read and write LBLRTM Layer objects
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!

MODULE LBLRTM_Layer_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds         , ONLY: FP, IP, DP => Double
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Open
  USE LBLRTM_Parameters  , ONLY: LBLRTM_FILE_EOF, &
                                 LBLRTM_FILE_EOL, &
                                 LBLRTM_FILE_OK , &
                                 LBLRTM_FILE_UNDEF, &
                                 LBLRTM_MAX_CHUNK_POINTS
  USE LBLRTM_Utility     , ONLY: LBLRTM_EoL_Write
  USE LBLRTM_Phdr_Define , ONLY: LBLRTM_Phdr_type    , &
                                 LBLRTM_Phdr_SetValid, &
                                 LBLRTM_Phdr_Destroy
  USE LBLRTM_Panel_Define, ONLY: LBLRTM_Panel_type      , &
                                 LBLRTM_Panel_Associated, &
                                 LBLRTM_Panel_SetValid  , &
                                 LBLRTM_Panel_Destroy   , &
                                 LBLRTM_Panel_Create
  USE LBLRTM_Panel_IO    , ONLY: LBLRTM_Panel_Read , &
                                 LBLRTM_Panel_Write
  USE LBLRTM_Fhdr_Define , ONLY: LBLRTM_Fhdr_type
  USE LBLRTM_Fhdr_IO     , ONLY: LBLRTM_Fhdr_Read , &
                                 LBLRTM_Fhdr_Write
  USE LBLRTM_Layer_Define, ONLY: LBLRTM_Layer_type      , &
                                 LBLRTM_Layer_Associated, &
                                 LBLRTM_Layer_SetValid  , &
                                 LBLRTM_Layer_IsValid   , &
                                 LBLRTM_Layer_Destroy   , &
                                 LBLRTM_Layer_Create    , &
                                 LBLRTM_Layer_Inspect
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_Layer_Read
  PUBLIC :: LBLRTM_Layer_Write
  PUBLIC :: LBLRTM_Layer_IOVersion


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
!       LBLRTM_Layer_Read
!
! PURPOSE:
!       Function to read an LBLRTM Layer from an LBLRTM format
!       file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_Layer_Read( &
!                        LBLRTM_Layer, &
!                        FileId      , &
!                        EOF         , &
!                        Double_Panel = Double_Panel, &
!                        Quiet        = Quiet         )
!
! OBJECTS:
!       LBLRTM_Layer:  LBLRTM Layer object to hold the data.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Layer_type
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
!       Double_Panel:  Set this logical argument to indicate a double-panel file.
!                      If == .FALSE., the file is assumed to be single panel [DEFAULT].
!                         == .TRUE.,  the file is assumed to be double panel.
!                      If not specified, default is .FALSE.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:         Set this logical flag to suppress INFORMATION
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
!                      If == SUCCESS the LBLRTM layer read was successful
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

  FUNCTION LBLRTM_Layer_Read( &
    Layer       , &  ! Output
    FileId      , &  ! Input
    EoF         , &  ! Output
    Double_Panel, &  ! Optional input
    Quiet       , &  ! Optional input
    Debug       ) &  ! Optional input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_Layer_type), INTENT(OUT) :: Layer
    INTEGER                , INTENT(IN)  :: FileId
    INTEGER                , INTENT(OUT) :: EoF
    LOGICAL,      OPTIONAL , INTENT(IN)  :: Double_Panel
    LOGICAL,      OPTIONAL , INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL , INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Layer_IO::Read'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: single_panel
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER :: l1, l2 !, n_points
    INTEGER :: i, n_spectra
    INTEGER :: n_chunks
    TYPE(LBLRTM_Fhdr_type)  :: fhdr
    TYPE(LBLRTM_Panel_type) :: panel

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
    IF ( PRESENT(debug) ) debug_output = debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Check if file is open
    IF ( .NOT. File_Open(FileId) ) THEN
      msg = 'LBLRTM file is not open'
      CALL Read_CleanUp(); RETURN
    END IF


    ! Read the file header
    err_stat = LBLRTM_Fhdr_Read( fhdr,FileId,EoF,Debug=debug )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading file header'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check for End-of-File. An End-of-Layer is only
    ! ...flagged during a PANEL HEADER read.
    IF ( EoF == LBLRTM_FILE_EOF ) RETURN


    ! Create the layer object
    CALL LBLRTM_Layer_Create( Layer,fhdr,n_spectra )
    IF ( .NOT. LBLRTM_Layer_Associated(Layer) ) THEN
      msg = 'Layer object allocation failed'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Initialise counters and indices
    n_chunks = 0
    l1 = 1


    ! Begin the panel "chunk" read loop
    Read_Chunk_loop: DO

      ! Increment the chunk counter
      n_chunks = n_chunks + 1

      ! Progress info
      IF ( noisy ) THEN
        WRITE(msg,'(2x,"Reading spectral chunk #",i0,"...")') n_chunks
        CALL Display_Message(ROUTINE_NAME,msg,INFORMATION)
      END IF

      ! Read the panel
      err_stat = LBLRTM_Panel_Read( panel,FileId,EoF,Double_Panel=Double_Panel,Quiet=Quiet,Debug=debug )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE(msg,'("Error reading spectral chunk #",i0)' ) n_chunks
        CALL Read_Cleanup(); RETURN
      END IF

      ! Check for End-of-Layer or End-of-File
      IF ( EoF == LBLRTM_FILE_EOL .OR. &
           EoF == LBLRTM_FILE_EOF      ) EXIT Read_Chunk_loop

      ! Determine end spectral point index for current chunk
      l2 = l1 + panel%n_Points - 1
      IF ( l2 > Layer%n_points ) THEN
        WRITE(msg,'("End point index for spectral chunk #",i0,&
                   &" (",i0,") exceeds the spectral array bounds (",i0,")")') &
                   n_chunks, l2, Layer%n_points
        CALL Read_Cleanup(); RETURN
      END IF

      ! Copy over the spectral data from panel to layer
      DO i = 1, n_spectra
        Layer%Spectrum(l1:l2,i) = panel%Spectrum(:,i)
      END DO

      ! Update the begin index
      l1 = l2 + 1

      ! Cleanup
      CALL LBLRTM_Panel_Destroy( panel )

    END DO Read_Chunk_loop


    ! Check the number of points read
    IF ( l2 /= Layer%n_points ) THEN

      ! Issue info message if the difference is more than one point.
      ! ...A 1-point difference can be expected due to rounding
      ! ...in the utility function LBLRTM_N_POINTS().
      IF ( ABS(l2-Layer%n_points) > 1 ) THEN
        WRITE(msg,'(i0," points read, ",i0," points expected" )') l2, Layer%n_points
        CALL Display_Message( ROUTINE_NAME,msg,INFORMATION )
      END IF

      ! Ensure layer object n_points and end frequency is correct
      Layer%n_Points      = l2
      Layer%End_Frequency = Layer%Begin_Frequency + &
                            (REAL(Layer%n_Points-1,DP)*REAL(Layer%Frequency_Interval,DP))

    END IF


    ! Tag object as valid
    CALL LBLRTM_Layer_SetValid(Layer)
    IF ( debug_output ) CALL LBLRTM_Layer_Inspect(Layer)

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open(FileId) ) THEN
        CLOSE( FileId,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL LBLRTM_Layer_Destroy(Layer)
      err_stat = FAILURE
      EoF      = LBLRTM_FILE_UNDEF
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION LBLRTM_Layer_Read


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_Write
!
! PURPOSE:
!       Function to write an LBLRTM Layer to an LBLRTM format
!       file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_Layer_Write( &
!                        LBLRTM_Layer, &
!                        FileId      , &
!                        No_EoL = No_EoL, &
!                        Quiet  = Quiet   )
!
! OBJECTS:
!       LBLRTM_Layer:  LBLRTM Layer object to write to file.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Layer_type
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
!       No_EoL:        Set this logical flag to indicate an End-Of-Level (EoL)
!                      marker should NOT be written to the output LBLRTM file.
!                      If == .FALSE., an EoL marker is written [DEFAULT]
!                         == .TRUE.,  an EoL marker is NOT written
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
!                      If == SUCCESS the LBLRTM layer write was successful
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

  FUNCTION LBLRTM_Layer_Write( &
    Layer , &  ! Input
    FileId, &  ! Input
    No_EoL, &  ! Optional Input
    Quiet , &  ! Optional Input
    Debug ) &  ! Optional Input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_Layer_type), INTENT(IN) :: Layer
    INTEGER                , INTENT(IN) :: FileId
    LOGICAL,      OPTIONAL , INTENT(IN) :: No_EoL
    LOGICAL,      OPTIONAL , INTENT(IN) :: Quiet
    LOGICAL,      OPTIONAL , INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Layer_IO::Write'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: err_msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: write_eol
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER :: i, j, l1, l2
    INTEGER :: n_chunks
    REAL(DP) :: f1, f2
    TYPE(LBLRTM_Phdr_type)  :: phdr
    TYPE(LBLRTM_Panel_type) :: panel

    ! Setup
    err_stat = SUCCESS
    ! ...Check EoL keyword
    write_eol = .TRUE.
    IF ( PRESENT(No_EoL) ) write_eol = .NOT. No_EoL
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
      CALL Write_CleanUp(); RETURN
    END IF


    ! Write the file header
    err_stat = LBLRTM_Fhdr_Write( Layer%Header,FileId )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing file header'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Determine how panel chunks to write
    ! ...Complete chunks
    n_chunks = Layer%n_Points / LBLRTM_MAX_CHUNK_POINTS
    ! ...The leftovers
    IF ( MOD(Layer%n_Points,LBLRTM_MAX_CHUNK_POINTS) /= 0 ) THEN
      n_chunks = n_chunks + 1
    END IF


    ! Initialise spectral begin index
    l1 = 1


    ! Begin the panel "chunk" write loop
    Write_Chunk_loop: DO j = 1, n_chunks

      ! Progress info
      IF ( noisy ) THEN
        WRITE(msg,'(2x,"Writing spectral chunk #",i0,"...")') j
        CALL Display_Message(ROUTINE_NAME,msg,INFORMATION)
      END IF

      ! Compute spectral chunk end index
      l2 = MIN(l1 + LBLRTM_MAX_CHUNK_POINTS - 1, Layer%n_Points )

      ! Construct panel header
      phdr%Frequency_Interval = Layer%Frequency_Interval
      f1 = REAL(l1-1,DP) * phdr%Frequency_Interval
      f2 = REAL(l2-1,DP) * phdr%Frequency_Interval
      phdr%Begin_Frequency = Layer%Begin_Frequency + f1
      phdr%End_Frequency   = Layer%Begin_Frequency + f2
      phdr%n_Points = l2 - l1 + 1
      CALL LBLRTM_Phdr_SetValid( phdr )

      ! Create a panel object
      CALL LBLRTM_Panel_Create( panel,phdr,Layer%n_Spectra,Err_Msg=err_msg )
      IF ( .NOT. LBLRTM_Panel_Associated(panel) ) THEN
        WRITE(msg,'("Error creating panel object for spectral chunk #",i0," - ",a)') j, TRIM(err_msg)
        CALL Write_Cleanup(); RETURN
      END IF

      ! Copy over the spectral data from layer to panel
      DO i = 1, Layer%n_Spectra
        Panel%Spectrum(:,i) = Layer%Spectrum(l1:l2,i)
      END DO
      CALL LBLRTM_Panel_SetValid( panel )

      ! Write the panel
      err_stat = LBLRTM_Panel_Write( panel,FileId,Quiet=Quiet )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE(msg,'("Error writing spectral chunk #",i0)' ) j
        CALL Write_Cleanup(); RETURN
      END IF

      ! Update the begin index
      l1 = l2 + 1

      ! Cleanup
      CALL LBLRTM_Phdr_Destroy( phdr )
      CALL LBLRTM_Panel_Destroy( panel )

    END DO Write_Chunk_loop


    ! Write an End-of-Layer marker
    IF ( write_eol ) THEN
      err_stat = LBLRTM_EoL_Write( FileId )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing EoL marker'
        CALL Write_Cleanup(); RETURN
      END IF
      IF ( noisy ) THEN
        msg = 'Writing End-of-Layer marker'
        CALL Display_Message(ROUTINE_NAME,msg,INFORMATION)
      END IF
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

  END FUNCTION LBLRTM_Layer_Write


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the module.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Layer_IOVersion( Id )
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

  SUBROUTINE LBLRTM_Layer_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_Layer_IOVersion

END MODULE LBLRTM_Layer_IO
