!
! SpcCoeff_Binary_IO
!
! Module containing routines to read and write Binary format
! SpcCoeff files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE SpcCoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE SpcCoeff_Define    , ONLY: SpcCoeff_type, &
                                 Associated_SpcCoeff, &
                                 Allocate_SpcCoeff, &
                                 Destroy_SpcCoeff, &
                                 CheckRelease_SpcCoeff, &
                                 Info_SpcCoeff
  USE AntCorr_Binary_IO  , ONLY: Read_AntCorr_Binary, Write_AntCorr_Binary

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_SpcCoeff_Binary
  PUBLIC :: Write_SpcCoeff_Binary
  PUBLIC :: Read_SpcCoeff_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  ! Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS



!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_SpcCoeff_Binary
!
! PURPOSE:
!       Function to inquire a Binary format SpcCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_SpcCoeff_Binary( Filename               , &  ! Input
!                                               n_Channels =n_Channels , &  ! Optional Output
!                                               Release    =Release    , &  ! Optional Output
!                                               Version    =Version    , &  ! Optional Output
!                                               RCS_Id     =RCS_Id     , &  ! Revision control
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:              Character string specifying the name of an SpcCoeff
!                              format data file.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER(*)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:           Character string specifying a filename in which any
!                              messages will be logged. If not specified, or if an
!                              error occurs opening the log file, the default action
!                              is to output messages to standard output.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER(*)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Channels:            The number of channels dimension of the 
!                              SpcCoeff data.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:               The coefficient file release number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:               The coefficient file version number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:                Character string containing the Revision Control
!                              System Id field for the module.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER(*)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:          The return value is an integer defining the error status.
!                              The error codes are defined in the Message_Handler module.
!                              If == SUCCESS the Binary file inquiry was successful
!                                 == FAILURE an unrecoverable error occurred.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_SpcCoeff_Binary( Filename   , &  ! Input
                                    n_Channels , &  ! Optional Output
                                    n_FOVs     , &  ! Optional Output
                                    Release    , &  ! Optional Output
                                    Version    , &  ! Optional Output
                                    RCS_Id     , &  ! Revision control
                                    Message_Log) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_FOVs
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_SpcCoeff_Binary'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER(Long) :: Rel, Ver
    INTEGER(Long) :: File_n_Channels, File_n_FOVs
 

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening SpcCoeff file '//TRIM(Filename), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Read the Release/Version information
    ! ------------------------------------
    READ( FileID, IOSTAT=IO_Status ) Rel, Ver
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading SpcCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CLOSE(FileID)
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) File_n_Channels, File_n_FOVs
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading SpcCoeff dimension values from ", a, &
                        &". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CLOSE(FileID)
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the return arguments
    ! ---------------------------
    ! Dimensions
    IF ( PRESENT(n_Channels) ) n_Channels = File_n_Channels
    IF ( PRESENT(n_FOVs    ) ) n_FOVs     = File_n_FOVs

    ! Release/Version information
    IF ( PRESENT(Release) ) Release = Rel
    IF ( PRESENT(Version) ) Version = Ver


    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error closing ", a, ". IOSTAT = ", i0 )' ) &
                    TRIM(Filename), IO_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Inquire_SpcCoeff_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       Read_SpcCoeff_Binary
!
! PURPOSE:
!       Function to read Binary format SpcCoeff files.
!
! CALLING SEQUENCE:
!       Error_Status = Read_SpcCoeff_Binary( Filename                           , &  ! Input
!                                            SpcCoeff                           , &  ! Output
!                                            Quiet            =Quiet            , &  ! Optional input
!                                            Process_ID       =Process_ID       , &  ! Optional input
!                                            Output_Process_ID=Output_Process_ID, &  ! Optional input
!                                            RCS_Id           =RCS_Id           , &  ! Revision control
!                                            Message_Log      =Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the
!                           input binary format SpcCoeff data file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SpcCoeff:           Structure to contain the spectral coefficient
!                           data read from the file.
!                           UNITS:      N/A
!                           TYPE:       SpcCoeff_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:              Set this argument to suppress Information messages
!                           being printed to standard output (or the Message
!                           log file if the Message_Log optional argument is
!                           used.) By default, Information messages are printed.
!                           If QUIET = 0, Information messages are OUTPUT.
!                              QUIET = 1, Information messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn Message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all Information messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the Information messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Binary file read was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_SpcCoeff_Binary( Filename         , &  ! Input
                                 SpcCoeff         , &  ! Output
                                 Quiet            , &  ! Optional input
                                 Process_ID       , &  ! Optional input
                                 Output_Process_ID, &  ! Optional input
                                 RCS_Id           , &  ! Revision control
                                 Message_Log      ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    TYPE(SpcCoeff_type)   , INTENT(IN OUT) :: SpcCoeff
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER     , OPTIONAL, INTENT(IN)     :: Process_ID
    INTEGER     , OPTIONAL, INTENT(IN)     :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SpcCoeff_Binary'
    ! Local variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Process_ID_Tag
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER(Long) :: n_Channels, n_FOVs
 

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational Messages....
    Noisy = .TRUE.
    ! ....unless....
    IF ( PRESENT(Quiet) ) THEN
      ! ....the QUIET keyword is set.
      IF ( Quiet == SET ) Noisy = .FALSE.
    ELSE
      ! ....the Process_ID is not selected for output
      IF ( PRESENT(Process_ID) .AND. PRESENT(Output_Process_ID) ) THEN
        IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
      END IF
    END IF

    ! Create a process ID Message tag for
    ! WARNING and FAILURE Messages
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '(";  MPI Prcess ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID,   &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)//TRIM(Process_ID_Tag)
      GOTO 2000  ! Clean up
    END IF


    ! Read the release/version information
    ! ------------------------------------
    READ( FileID, IOSTAT=IO_Status ) SpcCoeff%Release, SpcCoeff%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading SpcCoeff file Release/Version values from ",a, &
                       &". IOSTAT = ",i0,a)' ) &
                      TRIM(Filename), IO_Status, TRIM(Process_ID_Tag)
      GOTO 1000  ! Clean up
    END IF

    ! Check the release
    Error_Status = CheckRelease_SpcCoeff( SpcCoeff, &
                                          Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'SpcCoeff Release check failed for '//TRIM(Filename)//TRIM(Process_ID_Tag)
      GOTO 1000  ! Clean up
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) n_Channels, n_FOVs
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading SpcCoeff dimensions from ",a,". IOSTAT = ",i0,a)' ) &
                      TRIM(Filename), IO_Status, TRIM(Process_ID_Tag)
      GOTO 1000  ! Clean up
    END IF


    ! Allocate the SpcCoeff structure
    ! -------------------------------
    Error_Status = Allocate_SpcCoeff( n_Channels, &
                                      SpcCoeff, &
                                      n_FOVs     =n_FOVs, &
                                      Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating SpcCoeff structure.'//TRIM(Process_ID_Tag)
      GOTO 1000  ! Clean up
    END IF


    ! Read the sensor id information
    ! ------------------------------
    READ(FileID, IOSTAT=IO_Status) SpcCoeff%Sensor_Id       , &
                                   SpcCoeff%Sensor_Type     , &
                                   SpcCoeff%WMO_Satellite_Id, &
                                   SpcCoeff%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading SpcCoeff sensor information from ",a,". IOSTAT = ",i0,a)' ) &
                      TRIM(Filename), IO_Status, TRIM(Process_ID_Tag)
      GOTO 1000  ! Clean up
    END IF
    
    
    ! Read the channel data
    ! ---------------------
    READ(FileID, IOSTAT=IO_Status) SpcCoeff%Sensor_Channel            , &
                                   SpcCoeff%Polarization              , &
                                   SpcCoeff%Channel_Flag              , &
                                   SpcCoeff%Frequency                 , &
                                   SpcCoeff%Wavenumber                , &
                                   SpcCoeff%Planck_C1                 , &
                                   SpcCoeff%Planck_C2                 , &
                                   SpcCoeff%Band_C1                   , &
                                   SpcCoeff%Band_C2                   , &
                                   SpcCoeff%Cosmic_Background_Radiance, &
                                   SpcCoeff%Solar_Irradiance
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading SpcCoeff channel data from ",a,". IOSTAT = ",i0,a)' ) &
                      TRIM(Filename), IO_Status, TRIM(Process_ID_Tag)
      GOTO 1000  ! Clean up
    END IF
    
    
    ! Read the antenna correction data if necessary
    ! ---------------------------------------------
    IF ( SpcCoeff%AC_Present ) THEN
    
      ! Read the data
      Error_Status = Read_AntCorr_Binary( Filename, &
                                          SpcCoeff%AC, &
                                          No_File_Close=SET, &
                                          No_Allocate  =SET, &
                                          Quiet        =SET, &
                                          Message_Log  =Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '("Error reading SpcCoeff antenna correction data from ",a,&
                         &". IOSTAT = ",i0,a)' ) &
                        TRIM(Filename), IO_Status, TRIM(Process_ID_Tag)
        GOTO 1000  ! Clean up
      END IF
      
      ! Check that the AC data is for the same sensor
      IF ( SpcCoeff%Sensor_Id           /= SpcCoeff%AC%Sensor_Id        .OR. &
           SpcCoeff%WMO_Satellite_Id    /= SpcCoeff%AC%WMO_Satellite_Id .OR. &
           SpcCoeff%WMO_Sensor_Id       /= SpcCoeff%AC%WMO_Sensor_Id    .OR. &
           ANY( SpcCoeff%Sensor_Channel /= SpcCoeff%AC%Sensor_Channel )      ) THEN
        Message = 'Antenna correction sensor information is inconsistent with SpcCoeff'//&
                  TRIM(Process_ID_Tag)
        GOTO 1000  ! Clean up
      END IF
           
    END IF


    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error closing ",a,". IOSTAT = ",i0,a )' ) &
                      TRIM(Filename), IO_Status, TRIM(Process_ID_Tag)
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE( FileID, IOSTAT=IO_Status )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
    Destroy_Status = Destroy_SpcCoeff( SpcCoeff, Message_Log=Message_Log )
    
  END FUNCTION Read_SpcCoeff_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       Write_SpcCoeff_Binary
!
! PURPOSE:
!       Function to write Binary format SpcCoeff files.
!
! CALLING SEQUENCE:
!       Error_Status = Write_SpcCoeff_Binary( Filename               , &  ! Input
!                                             SpcCoeff               , &  ! Input
!                                             Quiet      = Quiet     , &  ! Optional input
!                                             RCS_Id     = RCS_Id    , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     SpcCoeff format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       SpcCoeff:     Structure containing the spectral coefficient data.
!                     UNITS:      N/A
!                     TYPE:       SpcCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE - the input SpcCoeff structure contains
!                                     unassociated pointer members, or
!                                   - a unrecoverable write error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
!------------------------------------------------------------------------------

  FUNCTION Write_SpcCoeff_Binary( Filename   , &  ! Input
                                  SpcCoeff   , &  ! Input
                                  Quiet      , &  ! Optional input
                                  RCS_Id     , &  ! Revision control
                                  Message_Log) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    TYPE(SpcCoeff_type)   , INTENT(IN)  :: SpcCoeff
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SpcCoeff_Binary'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
 

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check structure pointer association status
    ! ALL structure pointers must be associated
    IF ( .NOT. Associated_SpcCoeff( SpcCoeff ) ) THEN
      Message = 'Some or all INPUT SpcCoeff pointer '//&
                'members are NOT associated.'
      GOTO 2000  ! Clean up
    END IF
    
    ! Check the SpcCoeff structure Release
    Error_Status = CheckRelease_SpcCoeff( SpcCoeff, &
                                          Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'SpcCoeff Release check failed.'
      GOTO 2000  ! Clean up
    END IF

    ! Check the SpcCoeff structure dimensions
    IF ( SpcCoeff%n_Channels < 1 ) THEN
      Message = 'Channel dimension of SpcCoeff structure is < or = 0.'
      GOTO 2000  ! Clean up
    END IF

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      GOTO 2000  ! Clean up
    END IF


    ! Write the release/version information
    ! -------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) SpcCoeff%Release, SpcCoeff%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing SpcCoeff file Release/Version values to ",a, &
                       &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE( FileID, IOSTAT=IO_Status ) SpcCoeff%n_Channels, SpcCoeff%AC%n_FOVs
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing SpcCoeff dimensions to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the sensor id information
    ! -------------------------------
    WRITE(FileID, IOSTAT=IO_Status) SpcCoeff%Sensor_Id       , &
                                    SpcCoeff%Sensor_Type     , &
                                    SpcCoeff%WMO_Satellite_Id, &
                                    SpcCoeff%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing SpcCoeff sensor information to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF
    
    
    ! Read the channel data
    ! ---------------------
    WRITE(FileID, IOSTAT=IO_Status) SpcCoeff%Sensor_Channel            , &
                                    SpcCoeff%Polarization              , &
                                    SpcCoeff%Channel_Flag              , &
                                    SpcCoeff%Frequency                 , &
                                    SpcCoeff%Wavenumber                , &
                                    SpcCoeff%Planck_C1                 , &
                                    SpcCoeff%Planck_C2                 , &
                                    SpcCoeff%Band_C1                   , &
                                    SpcCoeff%Band_C2                   , &
                                    SpcCoeff%Cosmic_Background_Radiance, &
                                    SpcCoeff%Solar_Irradiance
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing SpcCoeff channel data to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF
    
    
    ! Write the antenna correction data if necessary
    ! ----------------------------------------------
    IF ( SpcCoeff%AC_Present ) THEN
      Error_Status = Write_AntCorr_Binary( Filename, &
                                           SpcCoeff%AC, &
                                           No_File_Close=SET, &
                                           Quiet        =SET, &
                                           Message_Log  =Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '("Error writing SpcCoeff antenna correction data to ",a,&
                         &". IOSTAT = ",i0)' ) &
                        TRIM(Filename), IO_Status
        GOTO 2000  ! Clean up
      END IF
    END IF
      

    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error closing ",a,". IOSTAT = ",i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE( FileID, IOSTAT=IO_Status, STATUS='DELETE' )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_SpcCoeff_Binary

END MODULE SpcCoeff_Binary_IO
