!
! LBLRTM_File_IO
!
! Module containing procedures to read and write LBLRTM Layer objects
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!

MODULE LBLRTM_File_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds         , ONLY: FP, IP, DP => Double
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility       , ONLY: Get_Lun, File_Open
  USE LBLRTM_Utility
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
                                 LBLRTM_File_Create
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_File_Read
!  PUBLIC :: LBLRTM_File_Write


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
!       LBLRTM_File_Read
!
! PURPOSE:
!       Function to read an LBLRTM format file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_File_Read( &
!                        LBLRTM_File , &
!                        Filename    , &
!                        Double_Panel  )
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
!       Double_Panel:  Set this logical flag to indicate a double-panel file.
!                      If == .FALSE., the file single/double panel format is
!                                     determined from the header. [DEFAULT]
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
    Double_Panel, &  ! Optional input
    Debug       ) &  ! Optional input
  RESULT( err_stat)
    ! Arguments
    TYPE(LBLRTM_File_type), INTENT(OUT) :: oFile
    CHARACTER(*)          , INTENT(IN)  :: Filename
    LOGICAL,     OPTIONAL , INTENT(IN)  :: Double_Panel
    LOGICAL,     OPTIONAL , INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_File_IO::Read'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: single_panel
    LOGICAL :: debug_output
    INTEGER :: fid
    INTEGER :: eof
    INTEGER :: l1, l2 !, n_points
    INTEGER :: i, n_spectra
    INTEGER :: n_chunks
    TYPE(LBLRTM_layer_type) :: layer

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


    ! Open the file
    err_stat = LBLRTM_File_Open(Filename,fid)
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening LBLRTM file '//TRIM(Filename)
      CALL Read_CleanUp(); RETURN
    END IF


    ! Read a layer
    err_stat = LBLRTM_Layer_Read( layer,fid,eof,Debug=debug )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading layer'
      CALL Read_Cleanup(); RETURN
    END IF
!    ! ...Check for End-of-File. An End-of-Layer is only
!    ! ...flagged during a PANEL HEADER read.
!    IF ( EoF == LBLRTM_FILE_EOF ) RETURN
print *, lblrtm_eof_message(eof) 
close(fid) 


!    ! Create the layer object
!    CALL LBLRTM_File_Create( Layer,fhdr,n_spectra )
!    IF ( .NOT. LBLRTM_File_Associated(Layer) ) THEN
!      msg = 'Layer object allocation failed'
!      CALL Read_Cleanup(); RETURN
!    END IF
!    
!    
!    ! Initialise counters and indices
!    n_chunks = 0
!    l1 = 1
!    
!
!    ! Begin the panel "chunk" read loop
!    Read_Chunk_loop: DO
!    
!      ! Increment the chunk counter
!      n_chunks = n_chunks + 1
!      
!      ! Read the panel
!      err_stat = LBLRTM_Panel_Read( panel,FileId,EoF,Double_Panel=Double_Panel )
!      IF ( err_stat /= SUCCESS ) THEN
!        WRITE(msg,'("Error reading spectral chunk #",i0)' ) n_chunks
!        CALL Read_Cleanup(); RETURN
!      END IF
!      
!      ! Check for End-of-Layer or End-of-File
!      IF ( EoF == LBLRTM_FILE_EOL .OR. &
!           EoF == LBLRTM_FILE_EOF      ) EXIT Read_Chunk_loop
!      
!      ! Determine end spectral point index for current chunk
!      l2 = l1 + panel%n_Points - 1
!      IF ( l2 > Layer%n_points ) THEN
!        WRITE(msg,'("End point index for spectral chunk #",i0,&
!                   &" (",i0,") exceeds the spectral array bounds (",i0,")")') &
!                   n_chunks, l2, Layer%n_points
!        CALL Read_Cleanup(); RETURN
!      END IF
!
!      ! Copy over the spectral data from panel to layer
!      DO i = 1, n_spectra
!        Layer%Spectrum(l1:l2,i) = panel%Spectrum(:,i)
!      END DO
!      
!      ! Update the begin index
!      l1 = l2 + 1
!      
!      ! Cleanup
!      CALL LBLRTM_Panel_Destroy( panel )
!
!    END DO Read_Chunk_loop
!
!        
!    ! Check the number of points read
!    IF ( l2 /= Layer%n_points ) THEN
!
!      ! Issue info message if the difference is more than one point.
!      ! ...A 1-point difference can be expected due to rounding
!      ! ...in the utility function LBLRTM_N_POINTS().
!      IF ( ABS(l2-Layer%n_points) > 1 ) THEN
!        WRITE(msg,'(i0," points read, ",i0," points expected" )') l2, Layer%n_points
!        CALL Display_Message( ROUTINE_NAME,msg,INFORMATION )
!      END IF
!
!      ! Ensure layer object n_points and end frequency is correct
!      Layer%n_Points      = l2
!      Layer%End_Frequency = Layer%Begin_Frequency + &
!                            (REAL(Layer%n_Points-1,DP)*REAL(Layer%Frequency_Interval,DP))
!
!    END IF
!    
!
!    ! Tag object as valid
!    CALL LBLRTM_File_SetValid(Layer)

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
!       Function to write an LBLRTM Layer to an LBLRTM format
!       file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_File_Write( &
!                        LBLRTM_File, &
!                        FileId      , &
!                        No_EoL = No_EoL )
!
! OBJECTS:
!       LBLRTM_File:  LBLRTM Layer object to write to file.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_File_type
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

!  FUNCTION LBLRTM_File_Write( &
!    Layer , &  ! Input
!    FileId, &  ! Input
!    No_EoL) &  ! Optional Input
!  RESULT( err_stat)
!    ! Arguments
!    TYPE(LBLRTM_File_type), INTENT(IN) :: Layer
!    INTEGER                , INTENT(IN) :: FileId
!    LOGICAL,      OPTIONAL , INTENT(IN) :: No_EoL
!    ! Function result
!    INTEGER :: err_stat
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_File_IO::Write'
!    ! Local variables
!    CHARACTER(ML) :: msg
!    CHARACTER(ML) :: io_msg
!    INTEGER :: io_stat
!    LOGICAL :: write_eol
!    INTEGER :: i, j, l1, l2
!    INTEGER :: n_chunks
!    REAL(DP) :: f1, f2
!    TYPE(LBLRTM_Phdr_type)  :: phdr
!    TYPE(LBLRTM_Panel_type) :: panel
!
!    ! Setup
!    err_stat = SUCCESS
!    ! ...Check EoL keyword
!    write_eol = .TRUE.
!    IF ( PRESENT(No_EoL) ) write_eol = .NOT. No_EoL
!    ! ...Check if file is open
!    IF ( .NOT. File_Open(FileId) ) THEN
!      msg = 'LBLRTM file is not open'
!      CALL Write_CleanUp(); RETURN
!    END IF
!
!
!    ! Write the file header
!    err_stat = LBLRTM_Fhdr_Write( Layer%Header,FileId )
!    IF ( err_stat /= SUCCESS ) THEN
!      msg = 'Error writing file header'
!      CALL Write_Cleanup(); RETURN
!    END IF
!
!
!    ! Determine how panel chunks to write
!    ! ...Complete chunks
!    n_chunks = Layer%n_Points / LBLRTM_MAX_CHUNK_POINTS
!    ! ...The leftovers
!    IF ( MOD(Layer%n_Points,LBLRTM_MAX_CHUNK_POINTS) /= 0 ) THEN
!      n_chunks = n_chunks + 1
!    END IF
!
!    
!    ! Initialise spectral begin index
!    l1 = 1
!    
!
!    ! Begin the panel "chunk" write loop
!    Write_Chunk_loop: DO j = 1, n_chunks
!    
!      ! Compute spectral chunk end index
!      l2 = MIN(l1 + LBLRTM_MAX_CHUNK_POINTS - 1, Layer%n_Points )
!      
!      ! Construct panel header
!      phdr%Frequency_Interval = Layer%Frequency_Interval
!      f1 = REAL(l1-1,DP) * phdr%Frequency_Interval
!      f2 = REAL(l2-1,DP) * phdr%Frequency_Interval
!      phdr%Begin_Frequency = Layer%Begin_Frequency + f1
!      phdr%End_Frequency   = Layer%Begin_Frequency + f2
!      phdr%n_Points = l2 - l1 + 1     
!      CALL LBLRTM_Phdr_SetValid( phdr )
!      
!      ! Create a panel object
!      CALL LBLRTM_Panel_Create( panel,phdr,Layer%n_Spectra )
!      IF ( .NOT. LBLRTM_Panel_Associated(panel) ) THEN
!        WRITE(msg,'("Error creating panel object for spectral chunk #",i0)') j
!        CALL Write_Cleanup(); RETURN
!      END IF
!       
!      ! Copy over the spectral data from layer to panel
!      DO i = 1, Layer%n_Spectra
!        Panel%Spectrum(:,i) = Layer%Spectrum(l1:l2,i)
!      END DO
!      CALL LBLRTM_Panel_SetValid( panel )
!      
!      ! Write the panel
!      err_stat = LBLRTM_Panel_Write( panel,FileId )
!      IF ( err_stat /= SUCCESS ) THEN
!        WRITE(msg,'("Error writing spectral chunk #",i0)' ) j
!        CALL Write_Cleanup(); RETURN
!      END IF
!      
!      ! Update the begin index
!      l1 = l2 + 1
!      
!      ! Cleanup
!      CALL LBLRTM_Phdr_Destroy( phdr )
!      CALL LBLRTM_Panel_Destroy( panel )
!
!    END DO Write_Chunk_loop
!
!
!    ! Write an End-of-Layer marker
!    IF ( write_eol ) THEN
!      err_stat = LBLRTM_Write_EoL( FileId )
!      IF ( err_stat /= SUCCESS ) THEN
!        msg = 'Error writing EoL marker'
!        CALL Write_Cleanup(); RETURN
!      END IF
!    END IF
!    
!  CONTAINS
!
!    SUBROUTINE Write_CleanUp()
!      IF ( File_Open(FileId) ) THEN
!        CLOSE( FileId,IOSTAT=io_stat,IOMSG=io_msg )
!        IF ( io_stat /= 0 ) &
!          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
!      END IF
!      err_stat = FAILURE
!      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
!    END SUBROUTINE Write_CleanUp
!
!  END FUNCTION LBLRTM_File_Write



!!------------------------------------------------------------------------------
!!
!! NAME:
!!       Read_LBLRTM_File
!!
!! PURPOSE:
!!       Function to read a layer of data from an LBLRTM format file.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Read_LBLRTM_File ( FileID,                                &  ! Input
!!                                          Panel_Type,                            &  ! Input
!!                                          LBLRTM_File,                          &  ! Output
!!                                          EOF,                                   &  ! Output
!!                                          Panel_Request     = Panel_Request,     &  ! Optional input
!!                                          Diagnostic_Output = Diagnostic_Output, &  ! Optional input
!!                                          RCS_Id            = RCS_Id,            &  ! Revision control
!!                                          Message_Log       = Message_Log        )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       FileID:             Logical unit number associated with LBLRTM file.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN)
!!
!!       Panel_Type:         Integer specifying the LBLRTM file type, i.e. single
!!                           or double panel. Valid input values are defined in
!!                           the LBLRTM_Parameters module.
!!                             = LBLRTM_SINGLE_PANEL_TYPE:  Single panel file
!!                             = LBLRTM_DOUBLE_PANEL_TYPE:  Double panel file
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Panel_Request:      Integer flag specifying which panels from a double
!!                           panel file is wanted.
!!                             = 1     : The first panel is returned
!!                             = 2     : The second panel is returned
!!                             = other : All panels are returned [DEFAULT]
!!                           If not specified, ALL the data is returned.
!!                           If specified with a single panel type, it is ignored.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Diagnostic_Output:  Integer flag specifying the level of diagnostic 
!!                           output required.
!!                             = 1     : Only the file header is output.
!!                             = 2     : Both the file header and panel header(s)
!!                                       are output.
!!                             = other : No output is generated [DEFAULT]
!!                           If not specified no output is generated.
!!                           If the MESSAGE_LOG argument is also specified, the
!!                           output is written to the log file.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Message_Log:        Character string specifying a filename in which any
!!                           messages will be logged. If not specified, or if an
!!                           error occurs opening the log file, the default action
!!                           is to output messages to standard output.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OUTPUT ARGUMENTS:
!!       LBLRTM_File:       LBLRTM_File structure containing the layer data.
!!                           UNITS:      N/A
!!                           TYPE:       LBLRTM_File_type
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT)
!!
!!       EOF:                Flag indicating end-of-file status for the LBLRTM
!!                           format file after the read. Valid return values are
!!                           defined in the LBLRTM_Parameters module.
!!                             = LBLRTM_FILE_PTR_EOF:   End-of-file has been reached.
!!                                                      The file is then closed.
!!                             = LBLRTM_FILE_PTR_OK:    No EOF or EOL condition. File
!!                                                      is positioned for further
!!                                                      reading.
!!                             = LBLRTM_FILE_PTR_UNDEF: An error occurred. The file is
!!                                                      closed.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT)
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       RCS_Id:             Character string containing the Revision Control
!!                           System Id field for the module.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:       The return value is an integer defining the error status.
!!                           The error codes are defined in the Message_Handler module.
!!                           If == SUCCESS the LBLRTM layer data read was successful
!!                              == FAILURE an error occurred
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!
!! COMMENTS:
!!       Note the INTENT on the output LBLRTM_File argument is IN OUT rather
!!       than just OUT. This is necessary because the argument may be defined on
!!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!!
!! CREATION HISTORY:
!!       Written by:     Paul van Delst, CIMSS/SSEC 29-Oct-2001
!!                       paul.vandelst@ssec.wisc.edu
!!
!!------------------------------------------------------------------------------
!
!  FUNCTION Read_LBLRTM_File ( FileID,            &  ! Input
!                               Panel_Type,        &  ! Input
!                               LBLRTM_File,      &  ! Output
!                               EOF,               &  ! Output
!                               Panel_Request,     &  ! Optional input
!                               Diagnostic_Output, &  ! Optional input
!                               RCS_Id,            &  ! Revision control
!                               Message_Log )      &  ! Error messaging
!                             RESULT ( Error_Status )
!    ! Arguments
!    INTEGER,                 INTENT(IN)     :: FileID
!    INTEGER,                 INTENT(IN)     :: Panel_Type
!    TYPE(LBLRTM_File_type), INTENT(IN OUT) :: LBLRTM_File
!    INTEGER,                 INTENT(OUT)    :: EOF
!    INTEGER,      OPTIONAL,  INTENT(IN)     :: Panel_Request
!    INTEGER,      OPTIONAL,  INTENT(IN)     :: Diagnostic_Output
!    CHARACTER(*), OPTIONAL,  INTENT(OUT)    :: RCS_Id
!    CHARACTER(*), OPTIONAL,  INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_LBLRTM_File'
!    ! Local variables
!    CHARACTER( 256 ) :: Message
!    LOGICAL :: Fhdr_Output
!    LOGICAL :: Phdr_Output
!    INTEGER :: n_Panels_to_Read
!    INTEGER :: n_Panels_to_Return
!    INTEGER :: i_Panel, i_Save, i
!    INTEGER :: n_Points, l1, l2
!    INTEGER :: Panel_Chunk_Count
!    TYPE(LBLRTM_Phdr_type) :: Phdr
!    REAL( LBLRTM_FP_KIND ), DIMENSION( LBLRTM_MAX_PANEL_POINTS ) :: Panel
!
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! Check if file is open
!    IF ( .NOT. File_Open( FileID ) ) THEN
!      Error_Status = FAILURE
!      EOF          = LBLRTM_FILE_PTR_UNDEF
!      CALL Display_Message( ROUTINE_NAME, &
!                            'LBLRTM file is not open.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!
!    ! Check the panel type
!    ! --------------------
!    IF ( Panel_Type /= LBLRTM_SINGLE_PANEL_TYPE .AND. &
!         Panel_Type /= LBLRTM_DOUBLE_PANEL_TYPE       ) THEN
!      Error_Status = FAILURE
!      EOF          = LBLRTM_FILE_PTR_UNDEF
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Invalid LBLRTM panel type.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      CLOSE( FileID )
!      RETURN
!    END IF
!
!
!    ! Check the panel request argument
!    ! --------------------------------
!    ! Default is to read all data
!    n_Panels_to_Read   = LBLRTM_N_PANELS( Panel_Type )  ! Used for reading
!    n_Panels_to_Return = LBLRTM_N_PANELS( Panel_Type )  ! Used for allocating
!    i_Panel            = 3
!
!    ! Otherwise, for double panel files, determine what
!    ! data to return. Note that the PANEL_REQUEST argument
!    ! is ignored if specified for a single panel file.
!    IF ( PRESENT( Panel_Request ) .AND. Panel_Type == LBLRTM_DOUBLE_PANEL_TYPE ) THEN
!
!      SELECT CASE ( Panel_Request )
!        CASE ( 1 )
!          ! Just want the first panel returned
!          n_Panels_to_Return = 1
!          i_Panel            = 1
!        CASE ( 2 )
!          ! Just want the second panel returned
!          n_Panels_to_Return = 1
!          i_Panel            = 2
!        CASE DEFAULT
!          ! Everything else returns BOTH
!          n_Panels_to_Return = 2
!          i_Panel            = 3
!      END SELECT
!
!    END IF
!
!
!    ! Check the diagnostic output argument
!    ! ------------------------------------
!    ! Default is no diagnostic output...
!    Fhdr_Output = .FALSE.
!    Phdr_Output = .FALSE.
!    ! ...unless the keyword is correctly set
!    IF ( PRESENT( Diagnostic_Output ) ) THEN
!      SELECT CASE ( Diagnostic_Output )
!        CASE ( 1 )
!          Fhdr_Output = .TRUE.
!        CASE ( 2 )
!          Fhdr_Output = .TRUE.
!          Phdr_Output = .TRUE.
!        CASE DEFAULT
!          Fhdr_Output = .FALSE.
!          Phdr_Output = .FALSE.
!      END SELECT
!    END IF
!
!
!    ! Read the file header
!    ! --------------------
!    Error_Status = Read_LBLRTM_Fhdr( FileID, &
!                                     LBLRTM_File%Fhdr, &
!                                     EOF, &
!                                     Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      EOF = LBLRTM_FILE_PTR_UNDEF
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error occurred reading file header.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Check for EOF. Remember that an EOL is only
!    ! flagged during a PANEL HEADER read.
!    ! Error_Status returned from Read_LBLRTM_Fhdr,
!    IF ( EOF == LBLRTM_FILE_PTR_EOF ) RETURN
!
!
!    ! Output diagnostic output if required
!    IF ( Fhdr_Output )  THEN
!      CALL Print_LBLRTM_Fhdr( LBLRTM_File%Fhdr,        &
!                              Message_Log = Message_Log )
!    END IF
!
!
!    ! Compute the number of spectral points in the layer
!    ! --------------------------------------------------
!    n_Points = Compute_n_Points( LBLRTM_File%Fhdr%Begin_Frequency,   &
!                                 LBLRTM_File%Fhdr%End_Frequency,     &
!                                 LBLRTM_File%Fhdr%Frequency_Interval )
!
!
!    ! Allocate the layer structure
!    ! ----------------------------
!    Error_Status = Allocate_LBLRTM_File( n_Points,                 &
!                                          n_Panels_to_Return,       &
!                                          LBLRTM_File,             &
!                                          Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      EOF = LBLRTM_FILE_PTR_UNDEF
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error occurred allocating LBLRTM_File.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      CLOSE( FileID )
!      RETURN
!    END IF
!
!
!    !#--------------------------------------------------------------------------#
!    !#                          -- READ PANEL DATA --                           #
!    !#                                                                          #
!    !# Here it should be pointed out that the term "panel" refers to two sorts  #
!    !# of different things.                                                     #
!    !#                                                                          #
!    !# A "single panel" or "double panel" file refers to an LBLRTM format file  #
!    !# that contains one or two spectrally coincident data set(s) respectively. #
!    !# E.g. a single panel file usually contains optical depths, and a double   #
!    !# panel file usually contains radiances (first panel) and transmittances   #
!    !# (second panel). So in this respect how many "panels" a file contains is  #
!    !# indicative of how much data is in the file.                              #
!    !#                                                                          #
!    !# The other definition relates to how data is stored within a layer. Each  #
!    !# layer spectrum (or spectra if it's a double panel file) is stored in a   #
!    !# series of "chunks", also referred to as (you guessed it) "panels".       #
!    !#                                                                          #
!    !# Over the years of working with FASCODE/LBLRTM format data files I have   #
!    !# conflated these two meanings. Sorry 'bout that. So, to be clear, the     #
!    !# following "Read_Panel_Chunk" loop is for reading the data "chunk" panels.#
!    !# These chunks are then transferred to either the first or second          #
!    !# spectrum (panel) in the LBLRTM_File%Spectrum pointer array. Phew.       # 
!    !#--------------------------------------------------------------------------#
!
!    ! Initialise counters
!    ! -------------------
!    Panel_Chunk_Count = 0
!    l1 = 1                ! Begin index of current panel chunk in output array
!
!
!    ! Begin open loop
!    ! ---------------
!    Read_Panel_Chunk: DO
!
!      ! Read a panel header
!      ! -------------------
!      
!      ! Increment panel chunk count
!      Panel_Chunk_Count = Panel_Chunk_Count + 1
!
!      ! Read the panel chunk header
!      Error_Status = Read_LBLRTM_Phdr( FileID, &
!                                       Phdr, &
!                                       EOF, &
!                                       Message_Log = Message_Log )
!      IF ( Error_Status /= SUCCESS ) THEN
!        EOF = LBLRTM_FILE_PTR_UNDEF
!        WRITE( Message, '( "Error reading panel chunk #", i4, " header." )' ) &
!                        Panel_Chunk_Count
!        CALL Display_Message( ROUTINE_NAME,    &
!                              TRIM( Message ), &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!        RETURN
!      END IF
!
!      ! Check for EOL or EOF
!      IF ( EOF == LBLRTM_FILE_PTR_EOL .OR. &
!           EOF == LBLRTM_FILE_PTR_EOF      ) EXIT Read_Panel_Chunk
!      
!      ! Output diagnostic output if required
!      IF ( Phdr_Output )  THEN
!        CALL Print_LBLRTM_Phdr( Phdr, &
!                                Panel_Chunk_Count, &
!                                Message_Log = Message_Log )
!      END IF
!
!      ! Determine end index of current panel chunk in output array
!      l2 = l1 + Phdr%n_Points - 1
!      IF ( l2 > n_Points ) THEN
!        Error_Status = FAILURE
!        EOF          = LBLRTM_FILE_PTR_UNDEF
!        WRITE( Message, '( "End point index for panel chunk #", i4, &
!                          &" (",i10,") exceeds the output array bounds (",i10,")." )' ) &
!                        Panel_Chunk_Count, l2, n_Points
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                              Message_Log = Message_Log )
!        CLOSE( FileID )
!        RETURN
!      END IF
!
!
!      ! Read the required number of panel chunks
!      ! for single or double panel format file
!      ! ----------------------------------------
!
!      ! But first initialise a little counter
!      ! for the LBLRTM_File panel index
!      i_Save = 0
!
!      ! Now loop over panel chunks
!      DO i = 1, n_Panels_to_Read
!
!        ! Read the actual panel chunk data. The INT is required if
!        ! the LBLRTM data was output in "double precision mode"
!        Error_Status = Read_LBLRTM_Panel( FileID, &
!                                          Panel,   &
!                                          EOF,     &
!                                          n_Points    = INT( Phdr%n_Points ), & 
!                                          Message_Log = Message_Log    )
!
!        IF ( Error_Status /= SUCCESS ) THEN
!          EOF = LBLRTM_FILE_PTR_UNDEF
!          WRITE( Message, '( "Error reading panel chunk #", i4, &
!                            &" data in panel ", i1, "." )' ) &
!                          Panel_Chunk_Count, i
!          CALL Display_Message( ROUTINE_NAME, &
!                                message, &
!                                Error_Status, &
!                                Message_Log = Message_Log )
!          CLOSE( FileID )
!          RETURN
!        END IF
!
!        ! Test for end-of-file
!        IF ( EOF == LBLRTM_FILE_PTR_EOF ) EXIT Read_Panel_Chunk
!
!        ! Save the data if required
!        IF ( IAND( i, i_Panel ) /= 0 ) THEN
!          i_Save = i_Save + 1
!          LBLRTM_File%Spectrum(l1:l2, i_Save) = Panel( 1:Phdr%n_Points )
!        END IF
!
!      END DO
!
!      ! Update the begin index for the output arrays
!      l1 = l2 + 1
!
!    END DO Read_Panel_Chunk
!
!
!    ! Check the number of points read
!    ! -------------------------------
!    IF ( l2 /= n_Points ) THEN
!
!      ! Issue warning if the difference is more than one point.
!      ! A 1-point difference can be expected due to rounding
!      ! in the utility function COMPUTE_N_POINTS().
!      IF ( ABS( l2 - n_Points ) > 1 ) THEN
!        Error_Status = WARNING
!        WRITE( Message, '( i10, " points read, ", i10, " points expected." )' ) &
!                        l2, n_Points
!        CALL Display_Message( ROUTINE_NAME, &
!                              TRIM( Message ), &
!                              Error_Status, &
!                              Message_Log = Message_Log )
!      END IF
!
!      ! Ensure structure n_Points is correct
!      LBLRTM_File%n_Points = l2
!
!    END IF
!
!
!    ! Copy frequency range
!    ! --------------------
!    LBLRTM_File%Begin_Frequency    = LBLRTM_File%Fhdr%Begin_Frequency
!    LBLRTM_File%End_Frequency      = LBLRTM_File%Fhdr%Begin_Frequency + &
!                                      ( REAL( l2 - 1, Double ) * &
!                                        REAL( LBLRTM_File%Fhdr%Frequency_Interval, Double ) )
!    LBLRTM_File%Frequency_Interval = REAL( LBLRTM_File%Fhdr%Frequency_Interval, Double )
!
!  END FUNCTION Read_LBLRTM_File
!
!
!!------------------------------------------------------------------------------
!!
!! NAME:
!!       Write_LBLRTM_File
!!
!! PURPOSE:
!!       Function to write a layer of data to an LBLRTM format file.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Write_LBLRTM_File ( FileID,                        &  ! Input
!!                                           LBLRTM_File,                  &  ! Input
!!                                           Panel_Request = Panel_Request, &  ! Optional input
!!                                           Write_EOL     = Write_EOL,     &  ! Optional input
!!                                           RCS_Id        = RCS_Id,        &  ! Revision control
!!                                           Message_Log   = Message_Log    )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       FileID:             Logical unit number associated with LBLRTM file.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN)
!!
!!       LBLRTM_File:       LBLRTM_File structure containing the layer data
!!                           to write.
!!                           UNITS:      N/A
!!                           TYPE:       LBLRTM_File_type
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Panel_Request:      Integer flag specifying which panels from a double
!!                           panel file are to be written.
!!                             = 1     : The first panel is written
!!                             = 2     : The second panel is written
!!                             = other : Both panels are written. [DEFAULT]
!!                           If not specified, ALL the data is written.
!!                           If specified with a single panel LBLRTM layer type,
!!                           it is ignored.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Write_EOL:          Set this keyword to write an End-Of-Level (EOL) marker
!!                           to teh output LBLRTM file.
!!                             = 0     : No EOL marker written
!!                             = 1     : EOL marker is written
!!                             = other : No EOL marker written [DEFAULT]
!!                           If not specified, no EOL marker is written.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Message_Log:        Character string specifying a filename in which any
!!                           messages will be logged. If not specified, or if an
!!                           error occurs opening the log file, the default action
!!                           is to output messages to standard output.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       RCS_Id:             Character string containing the Revision Control
!!                           System Id field for the module.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:       The return value is an integer defining the error status.
!!                           The error codes are defined in the Message_Handler module.
!!                           If == SUCCESS the LBLRTM layer data write was successful
!!                              == FAILURE an error occurred
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!
!! CREATION HISTORY:
!!       Written by:     Paul van Delst, CIMSS/SSEC 29-Oct-2001
!!                       paul.vandelst@ssec.wisc.edu
!!
!!------------------------------------------------------------------------------
!
!  FUNCTION Write_LBLRTM_File ( FileID,        &  ! Input
!                                LBLRTM_File,  &  ! Input
!                                Panel_Request, &  ! Optional input
!                                Write_EOL,     &  ! Optional input
!                                RCS_Id,        &  ! Revision control
!                                Message_Log )  &  ! Error messaging
!                              RESULT ( Error_Status )
!    ! Arguments
!    INTEGER,                   INTENT(IN)  :: FileID
!    TYPE(LBLRTM_File_type), INTENT(IN)  :: LBLRTM_File
!    INTEGER,        OPTIONAL,  INTENT(IN)  :: Panel_Request
!    INTEGER,        OPTIONAL,  INTENT(IN)  :: Write_EOL
!    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: RCS_Id
!    CHARACTER(*), OPTIONAL,  INTENT(IN)  :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_File'
!    ! Local variables
!    CHARACTER( 256 ) :: Message
!    LOGICAL :: EOL_Output
!    INTEGER :: io_status
!    INTEGER :: n_Panel_Chunks, j
!    INTEGER :: i_Panel_Begin, i_Panel_End, i
!    INTEGER :: l1, l2
!    INTEGER :: Phdr_i_count
!    REAL( Double ) :: Begin_Frequency
!    REAL( Double ) ::   End_Frequency
!    TYPE(LBLRTM_Phdr_type) :: Phdr
!
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
!
!    ! Check if file is open
!    IF ( .NOT. File_Open( FileID ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'LBLRTM file is not open.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!    ! Default is to write all data...
!    i_Panel_Begin = 1
!    i_Panel_End   = LBLRTM_File%n_Panels
!    ! ...otherwise determine what data to write.
!    IF ( PRESENT( Panel_Request ) ) THEN
!      SELECT CASE ( Panel_Request )
!        CASE ( 1 )
!          ! Just write the first panel
!          i_Panel_Begin = 1
!          i_Panel_End   = 1
!        CASE ( 2 )
!          ! Just write the second panel
!          i_Panel_Begin = 2
!          i_Panel_End   = 2
!        CASE DEFAULT
!          ! Everything else writes everything
!          i_Panel_Begin = 1
!          i_Panel_End   = LBLRTM_File%n_Panels
!      END SELECT
!    END IF
!
!    ! Default is no EOL output...
!    EOL_Output = .FALSE.
!    ! ...unless the keyword is correctly set
!    IF ( PRESENT( Write_EOL ) ) THEN
!      IF ( Write_EOL == SET ) EOL_Output = .TRUE.
!    END IF
!
!
!    ! Write the file header
!    ! ---------------------
!    Error_Status = Write_LBLRTM_Fhdr( FileID, &
!                                      LBLRTM_File%Fhdr, &
!                                      Message_Log = Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error occurred writing file header.', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF
!
!
!    !#--------------------------------------------------------------------------#
!    !#                         -- WRITE PANEL DATA --                           #
!    !#                                                                          #
!    !# Here it should be pointed out that the term "panel" refers to two sort   #
!    !# of different things.                                                     #
!    !#                                                                          #
!    !# A "single panel" or "double panel" file refers to an LBLRTM format file  #
!    !# that contains one or two spectrally coincident data set(s) respectively. #
!    !# E.g. a single panel file usually contains optical depths, and a double   #
!    !# panel file usually contains radiances (first panel) and transmittances   #
!    !# (second panel). So in this respect how many "panels" a file contains is  #
!    !# indicative of how much data is in the file.                              #
!    !#                                                                          #
!    !# The other definition relates to how data is stored within a layer. Each  #
!    !# layer spectrum (or spectra if it's a double panel file) is stored in a   #
!    !# series of "chunks", also referred to as (you guessed it) "panels".       #
!    !#                                                                          #
!    !# Over the years of working with FASCODE/LBLRTM format data files I have   #
!    !# conflated these two meanings. Sorry 'bout that. So, to be clear, the     #
!    !# following "Write_Panel_Chunk" loop is for writing the data "chunk"       #
!    !# panels.                                                                  # 
!    !#--------------------------------------------------------------------------#
!
!    ! The number of complete panels
!    n_Panel_Chunks = ( LBLRTM_File%n_Points / LBLRTM_MAX_PANEL_POINTS )
!
!    ! The left overs
!    IF ( MOD( LBLRTM_File%n_Points, LBLRTM_MAX_PANEL_POINTS ) /= 0 ) THEN
!      n_Panel_Chunks = n_Panel_Chunks + 1
!    END IF
!
!    ! Initialise panel begin index
!    l1 = 1
!
!
!    ! Begin panel write loop
!    ! ----------------------
!    Write_Panel_Chunk: DO j = 1, n_Panel_Chunks
!
!      ! Write the pnale chunk header
!      ! ----------------------------
!      ! Calculate the panel end index
!      l2 = MIN( l1 + LBLRTM_MAX_PANEL_POINTS - 1, LBLRTM_File%n_Points )
!
!      ! Construct panel chunk header
!      Begin_Frequency = LBLRTM_File%Begin_Frequency + &
!                        ( REAL( l1 - 1, Double ) * LBLRTM_File%Frequency_Interval )
!      End_Frequency   = LBLRTM_File%Begin_Frequency + &
!                        ( REAL( l2 - 1, Double ) * LBLRTM_File%Frequency_Interval )
!      Phdr = LBLRTM_Phdr_type( Begin_Frequency,                 &
!                               End_Frequency,                   &
!                               LBLRTM_File%Frequency_Interval, &
!                               l2 - l1 + 1                      )
!
!      ! Write the header
!      Error_Status = Write_LBLRTM_Phdr( FileID, &
!                                        Phdr, &
!                                        Message_Log = Message_Log )
!      IF ( Error_Status /= SUCCESS ) THEN
!        WRITE( Message, '( "Error writing panel chunk #", i4, " header." )' ) j
!        CALL Display_Message( ROUTINE_NAME,    &
!                              TRIM( Message ), &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!        RETURN
!      END IF
!
!
!      ! Write the required number of panel chunks
!      ! -----------------------------------------
!      ! Now loop over panel chunks
!      DO i = i_Panel_Begin, i_Panel_End
!
!        ! Write the actual panel chunk data
!        Error_Status = Write_LBLRTM_Panel( FileID, &
!                                           REAL( LBLRTM_File%Spectrum(l1:l2,i), LBLRTM_FP_KIND ), &
!                                           Message_Log = Message_Log    )
!        IF ( Error_Status /= SUCCESS ) THEN
!          WRITE( Message, '( "Error writing panel chunk #", i4, &
!                            &" data for panel ", i1, "." )' ) j, i
!          CALL Display_Message( ROUTINE_NAME, &
!                                message, &
!                                Error_Status, &
!                                Message_Log = Message_Log )
!          CLOSE( FileID )
!          RETURN
!        END IF
!
!      END DO
!
!      ! Update the begin index for the input arrays
!      l1 = l2 + 1
!
!    END DO Write_Panel_Chunk
!
!
!    ! Write an end-of-layer marker
!    ! ----------------------------
!    IF ( EOL_Output ) THEN
!      Error_Status = Write_LBLRTM_EOL( FileID, &
!                                       Message_Log = Message_Log )
!      IF ( Error_Status /= SUCCESS ) THEN
!        Error_Status = FAILURE
!        CALL Display_Message( ROUTINE_NAME,    &
!                              'Error writing EOL marker.', &
!                              Error_Status,    &
!                              Message_Log = Message_Log )
!        CLOSE( FileID )
!        RETURN
!      END IF
!    END IF
!
!  END FUNCTION Write_LBLRTM_File

END MODULE LBLRTM_File_IO
