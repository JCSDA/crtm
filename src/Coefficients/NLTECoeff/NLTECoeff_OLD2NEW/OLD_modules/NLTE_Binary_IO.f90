!
! NLTE_Binary_IO
!
! Module containing routines to read and write Binary format
! NLTE files.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, 08-05-2010
!

MODULE NLTE_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE NLTE_Define        , ONLY: NLTE_Type          , &
                                 NLTE_Associated    , &
                                 NLTE_Create        , &
                                 NLTE_Destroy       , &
                                 CheckRelease_NLTE  , &
                                 Info_NLTE

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure procedures
  PUBLIC :: Inquire_NLTE_Binary
  PUBLIC :: Read_NLTE_Binary
  PUBLIC :: Write_NLTE_Binary

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Message character length
  INTEGER, PARAMETER :: ML = 512


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_NLTE_Binary
!
! PURPOSE:
!       Function to inquire a Binary format NLTE file.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_NLTE_Binary( Filename                           , &  ! Input
!                                           n_Predictors     = n_Predictors    , &  ! Optional output
!                                           n_Sensor_Zangles = n_Sensor_Zangles, &  ! Optional output
!                                           n_Solar_Zangles  = n_Solar_Zangles , &  ! Optional output
!                                           n_NLTE_Channels  = n_NLTE_Channels , &  ! Optional output
!                                           n_Channels       = n_Channels      , &  ! Optional output
!                                           Release          = Release         , &  ! Optional Output
!                                           Version          = Version         , &  ! Optional Output
!                                           Sensor_Id        = Sensor_Id       , &  ! Optional output
!                                           WMO_Satellite_Id = WMO_Satellite_Id, &  ! Optional output
!                                           WMO_Sensor_Id    = WMO_Sensor_Id   , &  ! Optional output
!                                           RCS_Id           = RCS_Id          , &  ! Revision control
!                                           Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           NLTE data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Predictors:       The number of predictor functions used in generating
!                           the NLTE data.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Predictors, where the 0'th
!                                 term is the offset. Therefore the actual number
!                                 of array elements along this dimension is
!                                 n_Predictors+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!    n_Sensor_Zangles:      Number of sensor zenith angles.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!     n_Solar_Zangles:      Number of solar zenith angles.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!    n_NLTE_Channels:       Number of NLTE channels.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!         n_Channels:       Number of channels of the sensor.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Release:            The NLTE data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The NLTE data/file version number. Used for
!                           purposes only in identifying the dataset for
!                           a particular release.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_NLTE_Binary( Filename        , &  ! Input
                                n_Predictors    , &  ! Optional output
                                n_Sensor_Zangles, &  ! Optional output
                                n_Solar_Zangles , &  ! Optional output
                                n_NLTE_Channels , &  ! Optional output  
                                n_Channels      , &  ! Optional output  
                                Release         , &  ! Optional Output
                                Version         , &  ! Optional Output
                                Sensor_Id       , &  ! Optional Output
                                WMO_Satellite_Id, &  ! Optional Output
                                WMO_Sensor_Id   , &  ! Optional Output
                                RCS_Id          , &  ! Revision control
                                Message_Log     ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Predictors
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Sensor_Zangles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Solar_Zangles
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_NLTE_Channels
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_NLTE_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    TYPE(NLTE_type) :: NLTE  

 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( Filename, &
                                     FileID)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening NLTE Binary file '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) NLTE%Release, NLTE%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Release/Version values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) NLTE%n_Predictors     , &
                                     NLTE%n_Sensor_Zangles , &
                                     NLTE%n_Solar_Zangles  , &
                                     NLTE%n_NLTE_Channels  , &
                                     NLTE%n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading dimension values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the sensor ids
    ! -------------------
    READ( FileID, IOSTAT=IO_Status ) NLTE%Sensor_Id       , &
                                     NLTE%WMO_Satellite_Id, &
                                     NLTE%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading sensor information from ",a,&
                       &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF

    
    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error closing ",a,". IOSTAT = ",i0)' ) &
                    TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    ! ---------------------------
    ! Dimensions
    IF ( PRESENT(n_Predictors) )      n_Predictors      = NLTE%n_Predictors
    IF ( PRESENT(n_Sensor_Zangles ) ) n_Sensor_Zangles  = NLTE%n_Sensor_Zangles 
    IF ( PRESENT(n_Solar_Zangles  ) ) n_Solar_Zangles   = NLTE%n_Solar_Zangles
    IF ( PRESENT(n_NLTE_Channels  ) ) n_NLTE_Channels   = NLTE%n_NLTE_Channels    
    IF ( PRESENT(n_Channels       ) ) n_Channels        = NLTE%n_Channels    

    ! Release/Version information
    IF ( PRESENT(Release) ) Release = NLTE%Release
    IF ( PRESENT(Version) ) Version = NLTE%Version

    ! Sensor ids
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = NLTE%Sensor_Id(1:MIN(LEN(Sensor_Id),LEN_TRIM(NLTE%Sensor_Id)))
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = NLTE%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = NLTE%WMO_Sensor_Id   
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      CHARACTER(256) :: Close_Message
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          CLOSE( FileID, IOSTAT=IO_Status )
          IF ( IO_Status /= 0 ) THEN
            WRITE( Close_Message,'("; Error closing input file during error cleanup. IOSTAT=",i0)') &
                                 IO_Status
            Message = TRIM(Message)//TRIM(Close_Message)
          END IF
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_NLTE_Binary


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_NLTE_Binary
!
! PURPOSE:
!       Function to read data into an NLTE structure from a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_NLTE_Binary( Filename                             , &  ! Input
!                                        NLTE                                 , &  ! Output
!                                        Quiet             = Quiet            , &  ! Optional input
!                                        Process_ID        = Process_ID       , &  ! Optional input
!                                        Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                        RCS_Id            = RCS_Id           , &  ! Revision control
!                                        Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           format NLTE data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       NLTE:               Structure containing the NLTE correction coefficient
!                           data read from the file.
!                           UNITS:      N/A
!                           TYPE:       NLTE_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:              Set this argument to suppress INFORMATION messages
!                           being printed to standard output (or the message
!                           log file if the Message_Log optional argument is
!                           used.) By default, INFORMATION messages are printed.
!                           If QUIET = 0, INFORMATION messages are OUTPUT.
!                              QUIET = 1, INFORMATION messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID, specified
!                           via the Process_ID argument, in which all INFORMATION
!                           messages are to be output. If the passed Process_ID
!                           value agrees with this value the INFORMATION messages
!                           are output. If MPI is not being used, ignore this
!                           argument.
!                           This argument is ignored if:
!                             - the optional Process_ID argument is not present.
!                             - the optional Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
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
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Binary file read was successful
!                              == FAILURE an unrecoverable read error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If the NLTE argument is defined upon input, it is redefined (or
!       reinitialised) at output.
!
! COMMENTS:
!       Note the INTENT on the output NLTE argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_NLTE_Binary( Filename         , &  ! Input
                             NLTE             , &  ! Output
                             Quiet            , &  ! Optional input
                             Process_ID       , &  ! Optional input
                             Output_Process_ID, &  ! Optional input
                             RCS_Id           , &  ! Revision control
                             Message_Log      ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    TYPE(NLTE_type)       , INTENT(IN OUT) :: NLTE
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER     , OPTIONAL, INTENT(IN)     :: Process_ID
    INTEGER     , OPTIONAL, INTENT(IN)     :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_NLTE_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    CHARACTER(ML) :: Process_ID_Tag
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER(Long) :: Version
    INTEGER(Long) :: n_Predictors
    INTEGER(Long) :: n_Sensor_Zangles
    INTEGER(Long) :: n_Solar_Zangles
    INTEGER(Long) :: n_NLTE_Channels
    INTEGER(Long) :: n_Channels
 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file is present
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      Error_Status = FAILURE
      RETURN
    END IF 

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless....
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    IF ( Noisy .AND. PRESENT(Process_ID) .AND. PRESENT(Output_Process_ID) ) THEN
      IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
    END IF

    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT(Process_ID) ) THEN
      WRITE( Process_ID_Tag,'(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Open the NLTE file
    ! ------------------
    Error_Status = Open_Binary_File( Filename, &
                                     FileID   )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      Error_Status = FAILURE
      RETURN
    END IF

    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) NLTE%Release, Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Release/Version values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Check the release
    Error_Status = CheckRelease_NLTE( NLTE,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'NLTE Release check failed for '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) n_Predictors     , &
                                     n_Sensor_Zangles , &
                                     n_Solar_Zangles  , &
                                     n_NLTE_Channels  , &
                                     n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading dimension values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    ! -----------------------------
    CALL NLTE_Create( NLTE             , &
                      n_Predictors      , &             
                      n_Sensor_Zangles , &               
                      n_Solar_Zangles  , &               
                      n_NLTE_Channels  , &               
                      n_Channels )                       
    IF ( .NOT. NLTE_Associated(NLTE) ) THEN
      Message = 'NLTE allocation failed'
      CALL Read_Cleanup(); RETURN
    END IF
    
    ! Assign the version number (which may be different)
    NLTE%Version = Version


    ! Read the sensor info
    ! --------------------
    READ( FileID, IOSTAT=IO_Status ) NLTE%Sensor_Id       , &
                                     NLTE%WMO_Satellite_Id, &
                                     NLTE%WMO_Sensor_Id 
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading sensor information from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the ensembel min, max and mean temperatures
    ! -------------------------------
    READ( FileID, IOSTAT=IO_Status ) NLTE%Tm                                     
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading ensemble temperatures from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the channel numbers
    ! -------------------------------
    READ( FileID, IOSTAT=IO_Status ) NLTE%NLTE_Channel_Index, &
                                     NLTE%NLTE_Channel                                     
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading channel data from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read solar angles and sensor zenith angle
    ! ----------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) NLTE%Sensor_Zangle, &
                                     NLTE%Solar_Zangle

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Angle information from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF


    ! read coefficient data
    ! --------------------------------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) NLTE%C
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading coefficient data from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup(); RETURN
    END IF

    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error closing ",a," after read. IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM(Process_ID_Tag), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_NLTE( NLTE, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      CHARACTER(ML) :: Close_Message
      INTEGER :: Destroy_Status
      ! Close file if necessary
      IF ( File_Exists( Filename ) ) THEN
        IF ( File_Open( Filename ) ) THEN
          CLOSE( FileID, IOSTAT=IO_Status )
          IF ( IO_Status /= 0 ) THEN
            WRITE( Close_Message,'("; Error closing ",a," during error cleanup. IOSTAT=",i0)') &
                                 TRIM(Filename), IO_Status
            Message = TRIM(Message)//TRIM(Close_Message)
          END IF
        END IF
      END IF
      ! Destroy the structure
      CALL NLTE_Destroy( NLTE )
    END SUBROUTINE Read_CleanUp
  


  END FUNCTION Read_NLTE_Binary


!--------------------------------------------------------------------------------
!
! NAME:
!       Write_NLTE_Binary
!
! PURPOSE:
!       Function to write an NLTE structure to a Binary format file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_NLTE_Binary( Filename                 , &  ! Input
!                                         NLTE                     , &  ! Input
!                                         Quiet       = Quiet      , &  ! Optional input
!                                         RCS_Id      = RCS_Id     , &  ! Revision control
!                                         Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     NLTE format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       NLTE:         Structure containing the NLTE correction coefficient
!                     data to write to the file.
!                     UNITS:      N/A
!                     TYPE:       NLTE_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information Messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
!                     information Messages are printed.
!                     If QUIET = 0, information Messages are OUTPUT.
!                        QUIET = 1, information Messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
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
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE - the input NLTE structure contains
!                                     unassociated pointer members, or
!                                   - a unrecoverable write error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs, the output file is deleted.
!
!--------------------------------------------------------------------------------

  FUNCTION Write_NLTE_Binary( Filename   , &  ! Input
                              NLTE       , &  ! Input
                              Quiet      , &  ! Optional input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    TYPE(NLTE_type)       , INTENT(IN)  :: NLTE
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_NLTE_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Open the NLTE data file
    ! -----------------------
    Error_Status = Open_Binary_File( Filename, &
                                     FileID,   &
                                     For_Output =.true. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      Error_Status = FAILURE
      RETURN
    END IF

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Check structure association status
    IF ( .NOT. NLTE_Associated( NLTE ) ) THEN
      Message = 'Some or all INPUT NLTE pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF
    
    ! Check the release
    Error_Status = CheckRelease_NLTE( NLTE, Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'NLTE structure Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Check the NLTE structure dimensions
    IF ( NLTE%n_Predictors      < 1 .OR. &
         NLTE%n_Sensor_Zangles  < 1 .OR. &
         NLTE%n_Solar_Zangles   < 1 .OR. &
         NLTE%n_NLTE_Channels   < 1 .OR. &
         NLTE%n_Channels        < 1  ) THEN
      Message = 'One or more dimensions of NLTE structure are < or = 0.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the Release and Version information
    ! -----------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) NLTE%Release, NLTE%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing Release/Version values to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the data dimensions
    ! -------------------------
    WRITE( FileID, IOSTAT=IO_Status ) NLTE%n_Predictors     , &
                                      NLTE%n_Sensor_Zangles , &
                                      NLTE%n_Solar_Zangles  , &
                                      NLTE%n_NLTE_Channels  , &
                                      NLTE%n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing dimension values to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the sensor info
    ! ---------------------
    WRITE( FileID, IOSTAT=IO_Status ) NLTE%Sensor_Id       , &
                                      NLTE%WMO_Satellite_Id, &
                                      NLTE%WMO_Sensor_Id 
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing sensor information to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the ensembel min, max and mean temperatures
    ! -------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) NLTE%Tm                                     
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing ensemble temperatures from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the channel numbers
    ! --------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) NLTE%NLTE_Channel_Index, &
                                      NLTE%NLTE_Channel
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing channel data to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write the data to the angle arrays
    ! -----------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) NLTE%Sensor_Zangle, &
                                      NLTE%Solar_Zangle
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing absorber information to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Write coefficient data
    ! ----------------------------------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) NLTE%C
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing coefficient data to ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup(); RETURN
    END IF

    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error closing ",a," after write. IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_NLTE( NLTE, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      CHARACTER(ML) :: Close_Message
      ! Close file if necessary
      IF ( File_Exists( Filename ) ) THEN
        IF ( File_Open( Filename ) ) THEN
          CLOSE( FileID, IOSTAT=IO_Status, STATUS='DELETE' )
          IF ( IO_Status /= 0 ) THEN
            WRITE( Close_Message,'("; Error deleting ",a," during error cleanup. IOSTAT=",i0)') &
                                 TRIM(Filename), IO_Status
            Message = TRIM(Message)//TRIM(Close_Message)
          END IF
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  

  END FUNCTION Write_NLTE_Binary

END MODULE NLTE_Binary_IO
