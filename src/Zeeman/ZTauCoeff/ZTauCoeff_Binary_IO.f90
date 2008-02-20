!
! ZTauCoeff_Binary_IO
!
! Module containing routines to read and write Binary format
! ZTauCoeff data files.
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, CIMSS/SSEC, 17-Jul-2007 
!                    paul.vandelst@noaa.gov
!                    Yong Han, NESDIS/STAR
!                    yong.han@noaa.gov
!

MODULE ZTauCoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE ZTauCoeff_Define   , ONLY: ZTauCoeff_type, &
                                 Associated_ZTauCoeff, &
                                 Destroy_ZTauCoeff, &
                                 Allocate_ZTauCoeff, &
                                 CheckRelease_ZTauCoeff, &
                                 Info_ZTauCoeff
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure procedures
  PUBLIC :: Inquire_ZTauCoeff_Binary
  PUBLIC :: Read_ZTauCoeff_Binary
  PUBLIC :: Write_ZTauCoeff_Binary
    

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
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
!       Inquire_ZTauCoeff_Binary
!
! PURPOSE:
!       Function to inquire a Binary ZTauCoeff format file to obtain the
!       dimensions and attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_ZTauCoeff_Binary( Filename                          , &  ! Input
!                                                n_Predictors     =n_Predictors    , &  ! Optional output
!                                                n_Layers         =n_Layers        , &  ! Optional output
!                                                n_Channels       =n_Channels      , &  ! Optional output
!                                                n_Sets           =n_Sets          , &  ! Optional output
!                                                Release          =Release         , &  ! Optional output
!                                                Version          =Version         , &  ! Optional output
!                                                Sensor_Id        =Sensor_Id       , &  ! Optional output
!                                                WMO_Satellite_Id =WMO_Satellite_Id, &  ! Optional output
!                                                WMO_Sensor_Id    =WMO_Sensor_Id   , &  ! Optional output
!                                                RCS_Id           =RCS_Id          , &  ! Revision control
!                                                Message_Log      =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the
!                           ZTauCoeff Binary format data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Predictors:       Number of predictors.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Layers:           Number of atmospheric layers.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         Number of sensor channels.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Sets:             Number of sets of ZTauCoeff data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the Binary ZTauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the Binary ZTauCoeff file.
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
!                     If == SUCCESS the Binary file inquiry was successful.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_ZTauCoeff_Binary( Filename        , &  ! Input
                                     n_Predictors    , &  ! Optional Output
                                     n_Layers        , &  ! Optional Output
                                     n_Channels      , &  ! Optional Output
                                     n_Sets          , &  ! Optional Output
                                     Release         , &  ! Optional Output
                                     Version         , &  ! Optional Output
                                     Sensor_Id       , &  ! Optional output
                                     WMO_Satellite_Id, &  ! Optional output
                                     WMO_Sensor_Id   , &  ! Optional output
                                     RCS_Id          , &  ! Revision Control
                                     Message_Log     ) &  ! Error Messaging
                                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Predictors
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers    
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels  
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Sets  
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_ZTauCoeff_Binary'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    TYPE(ZTauCoeff_type) :: Dummy  

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    
    ! Open the Binary format ZTauCoeff file
    ! -----------------------------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening ZTauCoeff file '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) Dummy%Release, Dummy%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading ZTauCoeff Release/Version values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) Dummy%n_Predictors, &
                                     Dummy%n_Layers    , &
                                     Dummy%n_Channels  , &   
                                     Dummy%n_Sets  
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading ZTauCoeff dimension values from ",a,&
                      &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Read the sensor ids
    ! -------------------
    READ( FileID, IOSTAT=IO_Status ) Dummy%Sensor_Id       , &
                                     Dummy%WMO_Satellite_Id, &
                                     Dummy%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading ZTauCoeff sensor information from ",a,&
                       &". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF
    
    
    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error closing ", a, ". IOSTAT = ", i0 )' ) &
                    TRIM(Filename), IO_Status
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    ! ---------------------------
    ! Dimensions
    IF ( PRESENT(n_Predictors) ) n_Predictors = Dummy%n_Predictors
    IF ( PRESENT(n_Layers    ) ) n_Layers     = Dummy%n_Layers    
    IF ( PRESENT(n_Channels  ) ) n_Channels   = Dummy%n_Channels  
    IF ( PRESENT(n_Sets  ) )     n_Sets       = Dummy%n_Sets  

    ! Release/Version information
    IF ( PRESENT(Release) ) Release = Dummy%Release
    IF ( PRESENT(Version) ) Version = Dummy%Version

    ! Sensor ids
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = Dummy%Sensor_Id       
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = Dummy%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = Dummy%WMO_Sensor_Id   
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      CHARACTER(256) :: Close_Message
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          CLOSE( FileID, IOSTAT=IO_Status )
          IF ( IO_Status /= 0 ) THEN
            WRITE( Close_Message,'("; Error closing ",a," during error cleanup. IOSTAT=",i0)') &
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
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_ZTauCoeff_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       Read_ZTauCoeff_Binary
!
! PURPOSE:
!       Function to read data from a Binary format ZTauCoeff file.
!
! CALLING SEQUENCE:
!     Error_Status = Read_ZTauCoeff_Binary( Filename               , &  ! Input
!                                           ZTauCoeff              , &  ! Output
!                                           Quiet      =Quiet      , &  ! Optional input
!                                           RCS_Id     =RCS_Id     , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of the
!                     Binary format ZTauCoeff data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ZTauCoeff:    Structure to contain the Zeeman TauCoeff data
!                     read from file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(ZTauCoeff_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the message log file if
!                     the MESSAGE_LOG optional argument is used.) By default,
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
!                     If == SUCCESS the Binary data read was successful.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       If specified as the output data type, the INTENT on the output ZTauCoeff
!       structure argument is IN OUT rather than just OUT. This is necessary
!       because the argument may be defined on input. To prevent memory leaks,
!       the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_ZTauCoeff_Binary( Filename     , &  ! Input
                                  ZTauCoeff    , &  ! Output
                                  No_File_Close, &  ! Optional input
                                  No_Allocate  , &  ! Optional input
                                  Quiet        , &  ! Optional input
                                  RCS_Id       , &  ! Revision Control
                                  Message_Log  ) &  ! Error Messaging
                                RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    TYPE(ZTauCoeff_type)  , INTENT(IN OUT) :: ZTauCoeff
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_File_Close
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_Allocate
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ZTauCoeff_Binary'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Destroy_Status
    LOGICAL :: Yes_File_Close
    LOGICAL :: Yes_Allocate
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER(Long) :: n_Predictors
    INTEGER(Long) :: n_Layers    
    INTEGER(Long) :: n_Channels  
    INTEGER(Long) :: n_Sets  

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file is open. If not, open it.
    ! Otherwise get its FileID.
    IF ( .NOT. File_Open( FileName ) ) THEN
      ! Check that the file exists
      IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
        Message = 'File '//TRIM(Filename)//' not found.'
        CALL Read_Cleanup( Destroy_Structure=SET ); RETURN
      END IF 
      ! Open the file
      Error_Status = Open_Binary_File( TRIM(Filename), &
                                       FileID, &
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM(Filename)
        CALL Read_Cleanup( Destroy_Structure=SET ); RETURN
      END IF
    ELSE
      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=FileID )
      ! Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Read_Cleanup( Destroy_Structure=SET ); RETURN
      END IF
    END IF

    ! Default action is to close the file on exit....
    Yes_File_Close = .TRUE.
    ! ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF
    
    ! Default action is to allocate the structure....
    Yes_Allocate = .TRUE.
    ! ...unless the No_Allocate optional argument is set.
    IF ( PRESENT( No_Allocate ) ) THEN
      IF ( No_Allocate == SET ) Yes_Allocate = .FALSE.
    END IF
    
    ! Output informational messages...
    Noisy = .TRUE.
    ! ...unless the Quiet keyword is set
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    
    
    ! Read the Release and Version information
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) ZTauCoeff%Release, ZTauCoeff%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading ZTauCoeff Release/Version values from ", a, &
                      &". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF

    ! Check the release
    Error_Status = CheckRelease_ZTauCoeff( ZTauCoeff, &
                                           Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ZTauCoeff Release check failed for '//TRIM(Filename)
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) n_Predictors, &
                                     n_Layers    , &
                                     n_Channels  , & 
                                     n_Sets  
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading ZTauCoeff dimension values from ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF


    ! Allocate the output structure if required
    ! -----------------------------------------
    IF ( Yes_Allocate ) THEN
      Error_Status = Allocate_ZTauCoeff( n_Predictors, &
                                         n_Layers    , &
                                         n_Channels  , &
                                         n_Sets      , &
                                         ZTauCoeff   , &
                                         Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'ZTauCoeff allocation failed'
        CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
      END IF
    END IF


    ! Read the sensor id information
    ! ------------------------------
    READ( FileID, IOSTAT=IO_Status ) ZTauCoeff%Sensor_Id       , &
                                     ZTauCoeff%WMO_Satellite_Id, &
                                     ZTauCoeff%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading ZTauCoeff sensor information from ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF


    ! Read the Zeeman data
    ! --------------------
    READ( FileID, IOSTAT=IO_Status ) ZTauCoeff%Sensor_Channel, &
                                     ZTauCoeff%Temperature   , &
                                     ZTauCoeff%Level_Pressure, &
                                     ZTauCoeff%Pressure      , &
                                     ZTauCoeff%ChannelIndex  , &
                                     ZTauCoeff%PredictorIndex, &
                                     ZTauCoeff%Secant_Zenith , &
                                     ZTauCoeff%C             
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading ZTauCoeff data from ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF


    ! Close the file
    ! --------------
    IF ( Yes_File_Close ) THEN
      CLOSE( FileID, IOSTAT=IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '("Error closing ",a," after read. IOSTAT = ",i0)' ) &
                        TRIM(Filename), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              WARNING, &
                              Message_Log=Message_Log )
      END IF
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ZTauCoeff( ZTauCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF
    
  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File, Destroy_Structure )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      INTEGER, OPTIONAL, INTENT(IN) :: Destroy_Structure
      CHARACTER(256) :: Close_Message
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          CLOSE( FileID, IOSTAT=IO_Status )
          IF ( IO_Status /= 0 ) THEN
            WRITE( Close_Message,'("; Error closing ",a," during error cleanup. IOSTAT=",i0)') &
                                 TRIM(Filename), IO_Status
            Message = TRIM(Message)//TRIM(Close_Message)
          END IF
        END IF
      END IF
      ! Destroy the structure if necessary
      IF ( PRESENT(Destroy_Structure) ) THEN
        IF ( Destroy_Structure == SET ) THEN
          Destroy_Status = Destroy_ZTauCoeff(ZTauCoeff, Message_Log=Message_Log)
          IF ( Destroy_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error destroying ZTauCoeff during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_ZTauCoeff_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       Write_ZTauCoeff_Binary
!
! PURPOSE:
!       Function to write ZTauCoeff data to a Binary format ZTauCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_ZTauCoeff_Binary( Filename               , &  ! Input
!                                              ZTauCoeff              , &  ! Input
!                                              Quiet      =Quiet      , &  ! Optional input
!                                              RCS_Id     =RCS_Id     , &  ! Revision control
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of the Binary
!                     format ZTauCoeff data file to write data into.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       ZTauCoeff:    Structure containing the Zeeman TauCoeff data
!                     to write to file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(ZTauCoeff_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the message log file if
!                     the MESSAGE_LOG optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       Integer
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
!                     If == SUCCESS the Binary data write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs, the output file is deleted.
!
!------------------------------------------------------------------------------

  FUNCTION Write_ZTauCoeff_Binary( Filename     , &  ! Input            
                                   ZTauCoeff    , &  ! Output           
                                   No_File_Close, &  ! Optional input
                                   Quiet        , &  ! Optional input   
                                   RCS_Id       , &  ! Revision Control 
                                   Message_Log  ) &  ! Error Messaging  
                                 RESULT( Error_Status )               
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    TYPE(ZTauCoeff_type)  , INTENT(IN)  :: ZTauCoeff
    INTEGER     , OPTIONAL, INTENT(IN)  :: No_File_Close
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ZTauCoeff_Binary'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Yes_File_Close
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file is open. If not, open it.
    ! Otherwise get the file ID.
    IF ( .NOT. File_Open( FileName ) ) THEN
      Error_Status = Open_Binary_File( TRIM(Filename), &
                                       FileID, &
                                       For_Output=SET, &
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    ELSE
      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=FileID )
      ! Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Write_Cleanup( Close_File=SET ); RETURN
      END IF
    END IF

    ! Check structure association status
    IF ( .NOT. Associated_ZTauCoeff( ZTauCoeff ) ) THEN
      Message = 'Some or all INPUT ZTauCoeff pointer members are NOT associated.'
      CALL Write_Cleanup( Close_File=SET ); RETURN
    END IF
    
    ! Check the release
    Error_Status = CheckRelease_ZTauCoeff( ZTauCoeff, &
                                           Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ZTauCoeff structure Release check failed.'
      CALL Write_Cleanup( Close_File=SET ); RETURN
    END IF

    ! Check the structure dimensions
    IF ( ZTauCoeff%n_Predictors < 1 .OR. &
         ZTauCoeff%n_Layers     < 1 .OR. &
         ZTauCoeff%n_Channels   < 1 .OR. &
         ZTauCoeff%n_Sets       < 1 ) THEN
      Message = 'Dimensions of ZTauCoeff structure are < or = 0.'
      CALL Write_Cleanup( Close_File=SET ); RETURN
    END IF

    ! Default action is to close the file on exit....
    Yes_File_Close = .TRUE.
    ! ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == 1 ) Yes_File_Close = .FALSE.
    END IF
    
    ! Output informational messages...
    Noisy = .TRUE.
    ! ...unless the Quiet keyword is set
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Write the Release and Version information
    ! -----------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) ZTauCoeff%Release, ZTauCoeff%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error writing ZTauCoeff Release/Version values to ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      CALL Write_Cleanup( Close_File=SET ); RETURN
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE(FileID, IOSTAT=IO_Status) ZTauCoeff%n_Predictors, &
                                    ZTauCoeff%n_Layers    , &
                                    ZTauCoeff%n_Channels  , &
                                    ZTauCoeff%n_Sets
  
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error writing ZTauCoeff dimension values to ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      CALL Write_Cleanup( Close_File=SET ); RETURN
    END IF


    ! Write the sensor id information
    ! -------------------------------
    WRITE(FileID, IOSTAT=IO_Status) ZTauCoeff%Sensor_Id       , &
                                    ZTauCoeff%WMO_Satellite_Id, &
                                    ZTauCoeff%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing ZTauCoeff sensor information to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup( Close_File=SET ); RETURN
    END IF


    ! Write the data
    ! ---------------------------------
    WRITE(FileID, IOSTAT=IO_Status) ZTauCoeff%Sensor_Channel, &
                                    ZTauCoeff%Temperature   , &
                                    ZTauCoeff%Level_Pressure, &
                                    ZTauCoeff%Pressure      , &
                                    ZTauCoeff%ChannelIndex  , &
                                    ZTauCoeff%PredictorIndex, &
                                    ZTauCoeff%Secant_Zenith , &
                                    ZTauCoeff%C             
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing ZTauCoeff data to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Write_Cleanup( Close_File=SET ); RETURN
    END IF

    
    ! Close the file
    ! --------------
    IF ( Yes_File_Close ) THEN
      CLOSE( FileID, IOSTAT=IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '("Error closing ",a,". IOSTAT = ",i0)' ) &
                        TRIM(Filename), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              WARNING, &
                              Message_Log=Message_Log )
      END IF
    END IF
    
    
    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ZTauCoeff( ZTauCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp( Close_File )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      CHARACTER(256) :: Close_Message
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          CLOSE( FileID, IOSTAT=IO_Status, STATUS='DELETE' )
          IF ( IO_Status /= 0 ) THEN
            WRITE( Close_Message,'("; Error closing ",a," during error cleanup. IOSTAT=",i0)') &
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

  END FUNCTION Write_ZTauCoeff_Binary


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################


END MODULE ZTauCoeff_Binary_IO
