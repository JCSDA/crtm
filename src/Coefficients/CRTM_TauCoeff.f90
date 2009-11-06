!
! CRTM_TauCoeff
!
! Module containing the shared CRTM absorption coefficients (TauCoeff)
! and their load/destruction routines. 
!
! PUBLIC DATA:
!       TC:  Data structure containing the transmittance model
!            coefficient data for one or multiple transmittance 
!            algorithms for the requested sensors.
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure TC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Yong Han, JCSDA, NOAA/NESDIS 20-Jun-2008
!       Modified by:    David Groff, SAIC 5-Nov-2009
MODULE CRTM_TauCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds          , ONLY: Long
  USE File_Utility        , ONLY: File_Exists
  USE Binary_File_Utility , ONLY: Open_Binary_File
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters     , ONLY: MAX_N_SENSORS, &
                                  TAU_ODAS, TAU_ODPS, TAU_ODSSU
  USE ODAS_TauCoeff       , ONLY: ODAS_Load_TauCoeff    => Load_TauCoeff   , &
                                  ODAS_Destroy_TauCoeff => Destroy_TauCoeff, &
                                  ODAS_TC => TC
  USE ODAS_Define         , ONLY: ODAS_type
  USE ODPS_TauCoeff       , ONLY: ODPS_Load_TauCoeff    => Load_TauCoeff   , &
                                  ODPS_Destroy_TauCoeff => Destroy_TauCoeff, &
                                  ODPS_TC => TC
  USE ODPS_Define         , ONLY: ODPS_type
  USE ODSSU_TauCoeff      , ONLY: ODSSU_Load_TauCoeff    => Load_TauCoeff   , &
                                  ODSSU_Destroy_TauCoeff => Destroy_TauCoeff, &
                                  ODSSU_TC => TC
  USE ODSSU_Define        , ONLY: ODSSU_type
  USE TauCoeff_Define     , ONLY: TauCoeff_type, &
                                  TauCoeff_Destroy, &
                                  TauCoeff_Create

 ! Disable all implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! The shared data
  PUBLIC :: TC

  ! Public routines in this module
  PUBLIC :: CRTM_Load_TauCoeff
  PUBLIC :: CRTM_Destroy_TauCoeff

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id$'

  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1

  ! --------------------------------------
  ! The shared data for the gas absorption
  ! (AtmAbsorption) model
  ! --------------------------------------
  TYPE(TauCoeff_type), SAVE :: TC

CONTAINS
!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Load_TauCoeff
!
! PURPOSE:
!       Function to load the TauCoeff transmittance coefficient data into
!       the shared data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_TauCoeff( Sensor_ID        =Sensor_ID,         &  ! Optional input
!                                          File_Path        =File_Path,         &  ! Optional input
!                                          Quiet            =Quiet,             &  ! Optional input
!                                          Process_ID       =Process_ID,        &  ! Optional input
!                                          Output_Process_ID=Output_Process_ID, &  ! Optional input
!                                          Message_Log      =Message_Log        )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Sensor_ID:          List of the sensor IDs (e.g. hirs3_n17, amsua_n18,
!                           ssmis_f16, etc) with which the CRTM is to be
!                           initialised. These Sensor ID are used to construct
!                           the sensor specific TauCoeff filenames containing
!                           the necessary coefficient data, i.e.
!                             <Sensor_ID>.TauCoeff.bin
!                           If this argument is not specified, the default
!                           TauCoeff filename is
!                             TauCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Rank-1
!                           ATTRIBUTES: INTENT(IN)
!
!       File_Path:          Character string specifying a file path for the
!                           input data files. If not specified, the current
!                           directory is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
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
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
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
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the TauCoeff data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                              == WARNING the number of channels read in differs
!                                         from that stored in the CRTM_Parameters
!                                         module.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structures
!       in this module.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Load_TauCoeff( Sensor_ID        , &  ! Input
                               File_Path        , &  ! Optional input
                               Quiet            , &  ! Optional input
                               Process_ID       , &  ! Optional input
                               Output_Process_ID, &  ! Optional input
                               Message_Log      ) &  ! Error messaging
                             RESULT( Error_Status )

    ! Arguments
    CHARACTER(*), DIMENSION(:), OPTIONAL, INTENT(IN) :: Sensor_ID
    CHARACTER(*),               OPTIONAL, INTENT(IN) :: File_Path
    INTEGER,                    OPTIONAL, INTENT(IN) :: Quiet
    INTEGER,                    OPTIONAL, INTENT(IN) :: Process_ID
    INTEGER,                    OPTIONAL, INTENT(IN) :: Output_Process_ID
    CHARACTER(*),               OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_TauCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Process_ID_Tag
    CHARACTER(256), DIMENSION(MAX_N_SENSORS) :: TauCoeff_File
    INTEGER :: Allocate_Status, Deallocate_Status
    INTEGER :: n, n_Sensors
    ! Local
    INTEGER, PARAMETER :: SL = 128
    INTEGER            :: Algorithm_ID
    CHARACTER(SL), ALLOCATABLE :: SensorIDs(:)

    ! Set up
    Error_Status = SUCCESS
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT(Process_ID) ) THEN
      WRITE( Process_ID_Tag, '(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Determine the number of sensors and construct their filenames
    IF ( PRESENT(Sensor_ID) ) THEN

      ! Construct filenames for specified sensors
      n_Sensors = SIZE(Sensor_ID)
      IF ( n_Sensors > MAX_N_SENSORS ) THEN
        Error_Status = FAILURE
        WRITE(Message,'("Too many sensors, ",i0," specified. Maximum of ",i0," sensors allowed.")') &
                      n_Sensors, MAX_N_SENSORS
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message)//TRIM(Process_ID_Tag), &
                              Error_Status, &
                              Message_Log=Message_Log)
        RETURN
      END IF
      DO n=1,n_Sensors
        TauCoeff_File(n) = TRIM(ADJUSTL(Sensor_ID(n)))//'.TauCoeff.bin'
      END DO
    ELSE
      ! No sensors specified. Use default filename.
      n_Sensors=1
      TauCoeff_File(1) = 'TauCoeff.bin'
    END IF
    
    ! Add the file path
    IF ( PRESENT(File_Path) ) THEN
      DO n=1,n_Sensors
        TauCoeff_File(n) = TRIM(ADJUSTL(File_Path))//TRIM(TauCoeff_File(n))
      END DO
    END IF

    ! set the sensor dimension for structure TC
    TC%n_Sensors = n_Sensors

    ! Allocate memory for the SensorIDs array
    IF ( PRESENT(Sensor_ID) ) THEN

      ! Check string length
      IF(SL < LEN(Sensor_ID(1)))THEN
        Error_Status = FAILURE
        WRITE(Message,'("The string length ", i4, &
                        &"for the string array SensorIDs is smaller than ", &
                        &i4, "of the input Sensor_ID.")') &
                        SL, LEN(Sensor_ID(1))
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message)//TRIM(Process_ID_Tag), &
                              Error_Status, &
                              Message_Log=Message_Log)
        RETURN
      END IF
      ALLOCATE( SensorIDs( n_Sensors ),   & 
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating SensorIDs with an n_Sensors dimension. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

    CALL TauCoeff_Create(TC, n_Sensors, Error_Status)
    IF ( Error_Status /= SUCCESS ) THEN 
      message = 'Error creating TC'
      CALL Display_Message( ROUTINE_NAME, TRIM(message), Error_Status)
      RETURN
    END IF

    !----------------------------------------------------  
    ! Determine algorithm IDs from the TauCoeff files    
    !----------------------------------------------------  
    Sensor_Loop: DO n = 1, n_Sensors

      ! set global sensor index
      TC%Sensor_Index(n) = n

      ! Get the transmittance algorithm ID
      Error_Status = Inquire_AlgorithmID( TRIM(TauCoeff_File(n)), &
                                          Algorithm_ID,           &
                                          Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'cannot obtain transmittance algorithm ID from file '//&
                              TRIM( TauCoeff_File(n) )//TRIM( Process_ID_Tag ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      TC%Algorithm_ID(n) = Algorithm_ID

      ! update the sensor counter and sensor (local) index for a specific algorithm
      SELECT CASE( Algorithm_ID )
        CASE ( TAU_ODAS )

          TC%n_ODAS = TC%n_ODAS + 1
          ! local sensor index, which is used within the algorithm
          TC%Sensor_LoIndex(n) = TC%n_ODAS

        CASE ( TAU_ODPS )

          TC%n_ODPS = TC%n_ODPS + 1
          ! local sensor index, which is used within the algorithm
          TC%Sensor_LoIndex(n) = TC%n_ODPS

        CASE ( TAU_ODSSU )

          TC%n_ODSSU = TC%n_ODSSU + 1
          ! local sensor index, which is used within the algorithm
          TC%Sensor_LoIndex(n) = TC%n_ODSSU
          
        CASE DEFAULT

          Error_Status = FAILURE
          
          IF(Algorithm_ID==10) THEN 
            Message='The algorithm ID does not exist, TauCoeff file need to be converted to new format'
          ELSE
            WRITE( Message, '( "The algorithm ID =  ", i5, " does not exist ")' ) &
                             Algorithm_ID

          ENDIF
          CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            FAILURE, &
                            Message_Log = Message_Log )
                            
          RETURN

      END SELECT

    END DO Sensor_Loop

    !-----------------------------------------------------------
    !  Load algorithm-specific coefficient data
    !-----------------------------------------------------------

    ! *** ODAS algorithm (Compact OPTRAN) ***

    n = TC%n_ODAS
    IF( n > 0 )THEN
      IF ( PRESENT(Sensor_ID) ) THEN
        SensorIDs(1:n) = PACK(Sensor_ID, MASK=TC%Algorithm_ID == TAU_ODAS)
        Error_Status = ODAS_Load_TauCoeff( &
                                       Sensor_ID        =SensorIDs(1:n)   , & 
                                       File_Path        =File_Path        , & 
                                       Quiet            =Quiet            , & 
                                       Process_ID       =Process_ID       , & 
                                       Output_Process_ID=Output_Process_ID, & 
                                       Message_Log      =Message_Log        ) 
      ELSE
        ! for the case that the Sensor_ID is not present (in this case, 1 sensor only)
        Error_Status = ODAS_Load_TauCoeff( &
                                       File_Path        =File_Path        , &
                                       Quiet            =Quiet            , &
                                       Process_ID       =Process_ID       , &
                                       Output_Process_ID=Output_Process_ID, &
                                       Message_Log      =Message_Log        )
      END IF

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error loading ODAS TauCoeff data', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF

      ! set the pointer pointing to the local (algorithm specific) TC array
      TC%ODAS => ODAS_TC
        
    END IF

    ! *** ODPS algorithm  ***

    n = TC%n_ODPS
    IF( n > 0 )THEN
      IF ( PRESENT(Sensor_ID) ) THEN
        SensorIDs(1:n) = PACK(Sensor_ID, MASK=TC%Algorithm_ID == TAU_ODPS)
        Error_Status = ODPS_Load_TauCoeff( &
                                       Sensor_ID        =SensorIDs(1:n)   , & 
                                       File_Path        =File_Path        , & 
                                       Quiet            =Quiet            , & 
                                       Process_ID       =Process_ID       , & 
                                       Output_Process_ID=Output_Process_ID, & 
                                       Message_Log      =Message_Log        ) 
      ELSE
        ! for the case that the Sensor_ID is not present (in this case, 1 sensor only)
        Error_Status = ODPS_Load_TauCoeff( &
                                       File_Path        =File_Path        , &
                                       Quiet            =Quiet            , &
                                       Process_ID       =Process_ID       , &
                                       Output_Process_ID=Output_Process_ID, &
                                       Message_Log      =Message_Log        )
      END IF

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error loading ODPS TauCoeff data', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF

      ! set the pointer pointing to the local (algorithm specific) TC array
      TC%ODPS => ODPS_TC
        
    END IF

    ! *** ODSSU algorithm  ***

    n = TC%n_ODSSU
    IF( n > 0 )THEN
      IF ( PRESENT(Sensor_ID) ) THEN
        SensorIDs(1:n) = PACK(Sensor_ID, MASK=TC%Algorithm_ID == TAU_ODSSU)
        Error_Status = ODSSU_Load_TauCoeff( &
                                       Sensor_ID        =SensorIDs(1:n)   , & 
                                       File_Path        =File_Path        , & 
                                       Quiet            =Quiet            , & 
                                       Process_ID       =Process_ID       , & 
                                       Output_Process_ID=Output_Process_ID, & 
                                       Message_Log      =Message_Log        ) 
      ELSE
        ! for the case that the Sensor_ID is not present (in this case, 1 sensor only)
        Error_Status = ODSSU_Load_TauCoeff( &
                                       File_Path        =File_Path        , &
                                       Quiet            =Quiet            , &
                                       Process_ID       =Process_ID       , &
                                       Output_Process_ID=Output_Process_ID, &
                                       Message_Log      =Message_Log        )
      END IF

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error loading ODSSU TauCoeff data', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF

      ! set the pointer pointing to the local (algorithm specific) TC array
      TC%ODSSU => ODSSU_TC
        
    END IF


    !----------------------------------------------
    ! deallocate local arrays
    !----------------------------------------------

    IF ( PRESENT(Sensor_ID) ) THEN
      DEALLOCATE(SensorIDs, STAT  = Deallocate_Status)
      IF ( Deallocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating the SensorIDs array', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    ENDIF
  
  END FUNCTION CRTM_Load_TauCoeff

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_TauCoeff
!
! PURPOSE:
!       Function to deallocate the public shared data structure containing
!       the CRTM TauCoeff transmittance coefficient data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_TauCoeff( Process_ID  = Process_ID, &  ! Optional input
!                                             Message_Log = Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:       Set this argument to the MPI process ID that this
!                         function call is running under. This value is used
!                         solely for controlling message output. If MPI is not
!                         being used, ignore this argument.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the deallocation of the public TC data
!                                       structure was successful
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structures
!       in this module.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_TauCoeff( Process_ID,   &  ! Optional input
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )

    ! Arguments
    INTEGER,      OPTIONAL, INTENT(IN)  :: Process_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_TauCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Process_ID_Tag
    INTEGER :: n, Destroy_Status, Allocate_Status
    
    ! Set up
    Error_Status = SUCCESS
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! ----------------------------------------------
    ! Destroy TauCoeff structures
    ! ---------------------------------------------- 

    IF( TC%n_ODAS > 0 )THEN

      ! disassociate the TC%ODAS pointer (which is pointing to TauCoeff_ODAS)
      NULLIFY( TC%ODAS )

      ! Destroy local TC, i.e TauCoeff_ODAS
      Destroy_Status = ODAS_Destroy_TauCoeff( Process_ID =Process_ID , &
                                              Message_Log=Message_Log  )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating shared TauCoeff_ODAS data structure', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF

      TC%n_ODAS     = 0    

    END IF

    IF( TC%n_ODPS > 0 )THEN

      ! disassociate the TC%ODPS pointer (which is pointing to TauCoeff_ODPS)
      NULLIFY( TC%ODPS )

      ! Destroy local TC, i.e TauCoeff_ODPS
      Destroy_Status = ODPS_Destroy_TauCoeff( Process_ID =Process_ID , &
                                              Message_Log=Message_Log  )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating shared TauCoeff_ODPS data structure', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF

      TC%n_ODPS     = 0    

    END IF

    IF( TC%n_ODSSU > 0 )THEN

      ! disassociate the TC%ODAS pointer (which is pointing to TauCoeff_ODAS)
      NULLIFY( TC%ODSSU )

      ! Destroy local TC, i.e TauCoeff_ODAS
      Destroy_Status = ODSSU_Destroy_TauCoeff( Process_ID =Process_ID , &
                                               Message_Log=Message_Log  )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating shared TauCoeff_SSU data structure', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF

      TC%n_ODSSU     = 0    

    END IF

    ! Destroy TC
    CALL TauCoeff_Destroy(TC, Error_Status)                                                                   
    IF ( Error_Status /= SUCCESS ) THEN 
      message = 'Error destroying TC'
      CALL Display_Message( ROUTINE_NAME, TRIM(message), Error_Status)
      RETURN
    END IF                          

    TC%n_Sensors    = 0       

  END FUNCTION CRTM_Destroy_TauCoeff

  FUNCTION Inquire_AlgorithmID(  Filename        , &  ! Input
                                 Algorithm_ID    , &  ! Output
                                 RCS_Id          , &  ! Revision control
                                 Message_Log     ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(OUT) :: Algorithm_ID
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_AlgorithmID'

    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER(Long) :: Algorithm_ID_in
    INTEGER(Long) :: Release_in
    INTEGER(Long) :: Version_in

 
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
                                     FileID, &
                                     Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening TauCoeff Binary file '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) Release_in, Version_in
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Release/Version values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the Alorithm ID
    ! --------------------
    READ( FileID, IOSTAT=IO_Status ) Algorithm_ID_in
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading Algorithm ID from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF

    ! Assign the return argument
    Algorithm_ID = Algorithm_ID_in

    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error closing ",a,". IOSTAT = ",i0)' ) &
                    TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(); RETURN
    END IF

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

  END FUNCTION Inquire_AlgorithmID
                               
END MODULE CRTM_TauCoeff
