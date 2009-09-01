!
! CRTM_LifeCycle
!
! Module containing CRTM life cycle functions to initialize and destroy
! the CRTM space.
!
! Written by:     Paul van Delst, CIMSS/SSEC 21-May-2004
!                 paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_LifeCycle

  
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler
  USE CRTM_SpcCoeff
  USE CRTM_TauCoeff
  USE CRTM_AerosolCoeff
  USE CRTM_CloudCoeff
  USE CRTM_EmisCoeff
  USE CRTM_ChannelInfo_Define
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public procedures
  PUBLIC :: CRTM_Init
  PUBLIC :: CRTM_Destroy


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Maximum string length for path+filenames
  INTEGER, PARAMETER :: SL = 2000


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Init
!
! PURPOSE:
!       Function to initialise the CRTM.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Init( ChannelInfo                        , &
!                                 Sensor_ID        =Sensor_ID        , &
!                                 CloudCoeff_File  =CloudCoeff_File  , &
!                                 AerosolCoeff_File=AerosolCoeff_File, &
!                                 EmisCoeff_File   =EmisCoeff_File   , &
!                                 File_Path        =File_Path        , &
!                                 Quiet            =Quiet            , &
!                                 Process_ID       =Process_ID       , &
!                                 Output_Process_ID=Output_Process_ID, &
!                                 RCS_Id           =RCS_Id           , &
!                                 Message_Log      =Message_Log        )
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:        ChannelInfo structure array populated based on
!                           the contents of the coefficient files and the
!                           user inputs.
!                           UNITS:      N/A
!                           TYPE:       CRTM_ChannelInfo_type
!                           DIMENSION:  Rank-1 (n_Sensors)
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Sensor_ID:          List of the sensor IDs (e.g. hirs3_n17, amsua_n18,
!                           ssmis_f16, etc) with which the CRTM is to be
!                           initialised. These Sensor ID are used to construct
!                           the sensor specific SpcCoeff and TauCoeff filenames
!                           containing the necessary coefficient data, i.e.
!                             <Sensor_ID>.SpcCoeff.bin
!                           and
!                             <Sensor_ID>.TauCoeff.bin
!                           for each sensor Id in the list. IF this argument is
!                           not specified, the default SpcCoeff and TauCoeff
!                           filenames are "SpcCoeff.bin" and "TauCoeff.bin"
!                           respectively.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Same as output ChannelInfo argument
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       CloudCoeff_File:    Name of the CRTM Binary format CloudCoeff file
!                           containing the scattering coefficient data. If not
!                           specified the default filename is "CloudCoeff.bin".
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       AerosolCoeff_File:  Name of the CRTM Binary format AerosolCoeff file
!                           containing the aerosol absorption and scattering
!                           coefficient data. If not specified the default
!                           filename is "AerosolCoeff.bin".
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       EmisCoeff_File:     Name of the CRTM Binary format EmisCoeff file
!                           containing the IRSSEM coefficient data. If not
!                           specified the default filename is "EmisCoeff.bin".
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
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
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to the screen.
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
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the CRTM initialisation was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       All public data arrays accessed by this module and its dependencies
!       are overwritten.
!
! RESTRICTIONS:
!       If specified, the length of the combined file path and filename strings
!       cannot exceed 2000 characters.
!
! COMMENTS:
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Init( ChannelInfo      , &  ! Output
                      Sensor_ID        , &  ! Optional input
                      SensorID         , &  ! Optional input ***OBSCELESCENT***
                      CloudCoeff_File  , &  ! Optional input
                      AerosolCoeff_File, &  ! Optional input
                      EmisCoeff_File   , &  ! Optional input
                      File_Path        , &  ! Optional input
                      Quiet            , &  ! Optional input
                      Process_ID       , &  ! Optional input
                      Output_Process_ID, &  ! Optional input
                      RCS_Id           , &  ! Revision control
                      Message_Log      ) &  ! Error messaging
                    RESULT( Error_Status )

    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo(:)
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Sensor_ID(:)
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: SensorID(:)
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: CloudCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: AerosolCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: EmisCoeff_File
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: File_Path
    INTEGER     ,      OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER     ,      OPTIONAL, INTENT(IN)     :: Process_ID
    INTEGER     ,      OPTIONAL, INTENT(IN)     :: Output_Process_ID
    CHARACTER(*),      OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Init'
    ! Local variables
    CHARACTER(SL) :: Default_CloudCoeff_File
    CHARACTER(SL) :: Default_AerosolCoeff_File
    CHARACTER(SL) :: Default_EmisCoeff_File
    INTEGER :: l, n, n_Sensors
    CHARACTER(20) :: Local_Sensor_ID(SIZE(ChannelInfo))


    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimensionality
    n_Sensors = SIZE(ChannelInfo)
    IF ( PRESENT(Sensor_ID) .OR. PRESENT(SensorID) ) THEN
      ! Check size of Sensor_Id array
      IF ( PRESENT(Sensor_ID) ) THEN
        IF ( SIZE(Sensor_ID) /= n_Sensors ) THEN
          Error_Status=FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Inconsistent ChannelInfo and Sensor_ID dimensions', &
                                Error_Status, &
                                Message_Log=Message_Log )
          RETURN
        END IF
        Local_Sensor_ID = Sensor_Id
      ! Check size of SensorId array
      ELSE IF ( PRESENT(SensorID) ) THEN
        IF ( SIZE(SensorID) /= n_Sensors ) THEN
          Error_Status=FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Inconsistent ChannelInfo and Sensor_ID dimensions', &
                                Error_Status, &
                                Message_Log=Message_Log )
          RETURN
        END IF
        Local_Sensor_ID = SensorId
      END IF
    ELSE
      ! No Sensor_ID specfied. ChannelInfo must only have one element.
      IF ( n_Sensors /= 1 ) THEN
        Error_Status=FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'ChannelInfo dimension > 1 without SensorID input', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Specify the default filenames
    Default_CloudCoeff_File   = 'CloudCoeff.bin'
    Default_AerosolCoeff_File = 'AerosolCoeff.bin'
    Default_EmisCoeff_File    = 'EmisCoeff.bin'

    ! Were other filenames specified?
    IF ( PRESENT(CloudCoeff_File) ) &
      Default_CloudCoeff_File = TRIM(ADJUSTL(CloudCoeff_File))

    IF ( PRESENT(AerosolCoeff_File) ) &
      Default_AerosolCoeff_File = TRIM(ADJUSTL(AerosolCoeff_File))

    IF ( PRESENT(EmisCoeff_File) ) &
      Default_EmisCoeff_File = TRIM(ADJUSTL(EmisCoeff_File))

    ! Was a path specified?
    IF ( PRESENT(File_Path) ) THEN
      Default_CloudCoeff_File   = TRIM(ADJUSTL(File_Path)) // TRIM(Default_CloudCoeff_File)
      Default_AerosolCoeff_File = TRIM(ADJUSTL(File_Path)) // TRIM(Default_AerosolCoeff_File)
      Default_EmisCoeff_File    = TRIM(ADJUSTL(File_Path)) // TRIM(Default_EmisCoeff_File)
    END IF


    ! Load the spectral coefficients
    ! ------------------------------
    Error_Status = CRTM_Load_SpcCoeff( Sensor_ID        =Local_Sensor_ID  , &
                                       File_Path        =File_Path        , &
                                       Quiet            =Quiet            , &
                                       Process_ID       =Process_ID       , &
                                       Output_Process_ID=Output_Process_ID, &
                                       Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading SpcCoeff data', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Load the gas absorption coefficients
    ! ------------------------------------
    Error_Status = CRTM_Load_TauCoeff( Sensor_ID        =Local_Sensor_ID  , &
                                       File_Path        =File_Path        , &
                                       Quiet            =Quiet            , &
                                       Process_ID       =Process_ID       , &
                                       Output_Process_ID=Output_Process_ID, &
                                       Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading TauCoeff data', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Load the cloud coefficients
    ! ---------------------------
    Error_Status = CRTM_Load_CloudCoeff( TRIM(Default_CloudCoeff_File)      , &
                                         Quiet            =Quiet            , &
                                         Process_ID       =Process_ID       , &
                                         Output_Process_ID=Output_Process_ID, &
                                         Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading CloudCoeff data from '//&
                            TRIM( Default_CloudCoeff_File ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Load the aerosol coefficients
    ! -----------------------------
    Error_Status = CRTM_Load_AerosolCoeff( TRIM(Default_AerosolCoeff_File)    , &
                                           Quiet            =Quiet            , &
                                           Process_ID       =Process_ID       , &
                                           Output_Process_ID=Output_Process_ID, &
                                           Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading AerosolCoeff data from '//&
                            TRIM( Default_AerosolCoeff_File ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Load the IRSSE coefficients
    ! ---------------------------
    Error_Status = CRTM_Load_EmisCoeff( TRIM( Default_EmisCoeff_File ), &
                                        Quiet            =Quiet            , &
                                        Process_ID       =Process_ID       , &
                                        Output_Process_ID=Output_Process_ID, &
                                        Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading EmisCoeff data from '//&
                            TRIM( Default_EmisCoeff_File ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Load the ChannelInfo structure
    ! ------------------------------
    ! **** THIS CODE ASSUMES USING ALL CHANNELS ****
    DO n = 1, n_Sensors

      ! Allocate the ChannelInfo structure
      Error_Status = CRTM_Allocate_ChannelInfo( SC(n)%n_Channels, &
                                                ChannelInfo(n), &
                                                Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Allocation of ChannelInfo structure failed.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF

      ! Set the Sensor_Index component
      ChannelInfo(n)%Sensor_Index = n
      
      ! Fill the Channel_Index component
      ! **** THIS IS WHERE CHANNEL SELECTION COULD OCCUR ****
      ChannelInfo(n)%Channel_Index = (/(l, l=1,SC(n)%n_Channels)/)
      ! Fill the rest of the ChannelInfo structure
      ChannelInfo(n)%Sensor_ID        = SC(n)%Sensor_Id
      ChannelInfo(n)%WMO_Satellite_ID = SC(n)%WMO_Satellite_ID
      ChannelInfo(n)%WMO_Sensor_ID    = SC(n)%WMO_Sensor_ID
      ChannelInfo(n)%Sensor_Channel   = SC(n)%Sensor_Channel
    END DO
    
  END FUNCTION CRTM_Init


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Destroy
!
! PURPOSE:
!       Function to deallocate all the shared data arrays allocated and
!       populated during the CRTM initialization.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy( ChannelInfo            , &
!                                    Process_ID =Process_ID , &
!                                    RCS_Id     =RCS_Id     , &
!                                    Message_Log=Message_Log  )
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:  Reinitialized ChannelInfo structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:   Set this argument to the MPI process ID that this
!                     function call is running under. This value is used
!                     solely for controlling message output. If MPI is not
!                     being used, ignore this argument.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to the screen.
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
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the CRTM deallocations were successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       All CRTM shared data arrays and structures are deallocated.
!
! COMMENTS:
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy( ChannelInfo, &  ! Output
                         Process_ID , &  ! Optional input
                         RCS_Id     , &  ! Revision control
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo(:)
    INTEGER     ,      OPTIONAL, INTENT(IN)     :: Process_ID
    CHARACTER(*),      OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy'
    ! Local variables
    INTEGER :: Destroy_Status
    INTEGER :: n, n_Sensors

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! The number of sensors
    n_Sensors = SIZE(ChannelInfo)


    ! Destroy all the structures
    ! --------------------------
    DO n = 1, n_Sensors
      Destroy_Status = CRTM_Destroy_ChannelInfo( ChannelInfo(n)         , &
                                                 Message_Log=Message_Log  )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying ChannelInfo structure.', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO

    Destroy_Status = CRTM_Destroy_EmisCoeff( Process_ID =Process_ID , &
                                             Message_Log=Message_Log  )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared EmisCoeff data structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

    Destroy_Status = CRTM_Destroy_AerosolCoeff( Process_ID =Process_ID , &
                                                Message_Log=Message_Log  )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared AerosolCoeff data structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

    Destroy_Status = CRTM_Destroy_CloudCoeff( Process_ID =Process_ID , &
                                              Message_Log=Message_Log  )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared CloudCoeff data structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

    Destroy_Status = CRTM_Destroy_TauCoeff( Process_ID =Process_ID , &
                                            Message_Log=Message_Log  )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared TauCoeff data structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

    Destroy_Status = CRTM_Destroy_SpcCoeff( Process_ID =Process_ID , &  
                                            Message_Log=Message_Log  )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared SpcCoeff data structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION CRTM_Destroy

END MODULE CRTM_LifeCycle
