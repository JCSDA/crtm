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


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  FUNCTION Inquire_ZTauCoeff_Binary( Filename    , &  ! Input
                                     n_Predictors, &  ! Optional Output
                                     n_Layers    , &  ! Optional Output
                                     n_Channels  , &  ! Optional Output
                                     Release     , &  ! Optional Output
                                     Version     , &  ! Optional Output
                                     RCS_Id      , &  ! Revision Control
                                     Message_Log ) &  ! Error Messaging
                                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Predictors
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers    
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels  
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_ZTauCoeff_Binary'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: Rel, Ver
    INTEGER :: n_File_Predictors
    INTEGER :: n_File_Layers    
    INTEGER :: n_File_Channels  

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM(Filename)//' not found.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Open the Binary format ZTauCoeff file
    ! -----------------------------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening ZTauCoeff file '//TRIM(Filename), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Read the Release and Version information
    ! ----------------------------------------
    READ(FileID, IOSTAT=IO_Status) Rel, Ver
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading ZTauCoeff Release/Version values from ", a, &
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
    READ(FileID, IOSTAT=IO_Status) n_File_Predictors, &
                                   n_File_Layers    , &
                                   n_File_Channels  
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading ZTauCoeff dimension values from ", a, &
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
    IF ( PRESENT(n_Predictors) ) n_Predictors = n_File_Predictors
    IF ( PRESENT(n_Layers    ) ) n_Layers     = n_File_Layers    
    IF ( PRESENT(n_Channels  ) ) n_Channels   = n_File_Channels  

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
    
  END FUNCTION Inquire_ZTauCoeff_Binary


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
    CHARACTER(256) :: Message
    INTEGER :: Destroy_Status
    LOGICAL :: Yes_File_Close
    LOGICAL :: Yes_Allocate
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Predictors
    INTEGER :: n_Layers    
    INTEGER :: n_Channels  

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
        GOTO 2000  ! Clean up
      END IF 
      ! Open the file
      Error_Status = Open_Binary_File( TRIM(Filename), &
                                       FileID, &
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM(Filename)
        GOTO 2000  ! Clean up
      END IF
    ELSE
      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=FileID )
      ! Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        GOTO 2000  ! Clean up
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
      WRITE(Message,'("Error reading ZTauCoeff Release/Version values from ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the release
    Error_Status = CheckRelease_ZTauCoeff( ZTauCoeff, &
                                           Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ZTauCoeff Release check failed for '//TRIM(Filename)
      GOTO 1000  ! Clean up
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ(FileID, IOSTAT=IO_Status) n_Predictors, &
                                   n_Layers    , &
                                   n_Channels  
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading ZTauCoeff dimension values from ",a,". IOSTAT = ",i0)' ) &
                     TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Allocate the output structure if required
    ! -----------------------------------------
    IF ( Yes_Allocate ) THEN
      Error_Status = Allocate_ZTauCoeff( n_Predictors, &
                                         n_Layers    , &
                                         n_Channels  , &
                                         ZTauCoeff, &
                                         Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'ZTauCoeff allocation failed'
        GOTO 1000  ! Clean up
      END IF
    END IF


    ! Read the sensor id information
    ! ------------------------------
    READ(FileID, IOSTAT=IO_Status) ZTauCoeff%Sensor_Id       , &
                                   ZTauCoeff%WMO_Satellite_Id, &
                                   ZTauCoeff%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading ZTauCoeff sensor information from ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Read the Zeeman data
    ! --------------------
    READ(FileID, IOSTAT=IO_Status) ZTauCoeff%Sensor_Channel, &
                                   ZTauCoeff%Level_Altitude, &
                                   ZTauCoeff%Level_Pressure, &
                                   ZTauCoeff%Pressure      , &
                                   ZTauCoeff%ChannelIndex  , &
                                   ZTauCoeff%PredictorIndex, &
                                   ZTauCoeff%Secant_Zenith , &
                                   ZTauCoeff%C             
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading ZTauCoeff data from ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Close the file
    ! --------------
    IF ( Yes_File_Close ) THEN
      CLOSE( FileID, IOSTAT=IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
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
    Destroy_Status = Destroy_ZTauCoeff( ZTauCoeff, Message_Log=Message_Log )
    
  END FUNCTION Read_ZTauCoeff_Binary


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
    CHARACTER(256) :: Message
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
        GOTO 2000  ! Clean up
      END IF
    ELSE
      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=FileID )
      ! Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        GOTO 2000  ! Clean up
      END IF
    END IF

    ! Check structure association status
    IF ( .NOT. Associated_ZTauCoeff( ZTauCoeff ) ) THEN
      Message = 'Some or all INPUT ZTauCoeff pointer members are NOT associated.'
      GOTO 1000  ! Clean up
    END IF
    
    ! Check the release
    Error_Status = CheckRelease_ZTauCoeff( ZTauCoeff, &
                                         Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ZTauCoeff structure Release check failed.'
      GOTO 1000  ! Clean up
    END IF

    ! Check the structure dimensions
    IF ( ZTauCoeff%n_Predictors < 1 .OR. &
         ZTauCoeff%n_Layers     < 1 .OR. &
         ZTauCoeff%n_Channels   < 1 ) THEN
      Message = 'Dimensions of ZTauCoeff structure are < or = 0.'
      GOTO 1000  ! Clean up
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
      GOTO 2000  ! Clean up
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE(FileID, IOSTAT=IO_Status) ZTauCoeff%n_Predictors, &
                                    ZTauCoeff%n_Layers    , &
                                    ZTauCoeff%n_Channels  
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error writing ZTauCoeff dimension values to ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      GOTO 2000  ! Clean up
    END IF


    ! Write the sensor id information
    ! -------------------------------
    WRITE(FileID, IOSTAT=IO_Status) ZTauCoeff%Sensor_Id       , &
                                    ZTauCoeff%WMO_Satellite_Id, &
                                    ZTauCoeff%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing ZTauCoeff sensor information to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 2000  ! Clean up
    END IF


    ! Write the antenna correction data
    ! ---------------------------------
    WRITE(FileID, IOSTAT=IO_Status) ZTauCoeff%Sensor_Channel, &
                                    ZTauCoeff%Level_Altitude, &
                                    ZTauCoeff%Level_Pressure, &
                                    ZTauCoeff%Pressure      , &
                                    ZTauCoeff%ChannelIndex  , &
                                    ZTauCoeff%PredictorIndex, &
                                    ZTauCoeff%Secant_Zenith , &
                                    ZTauCoeff%C             
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing ZTauCoeff data to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 2000  ! Clean up
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

  END FUNCTION Write_ZTauCoeff_Binary

END MODULE ZTauCoeff_Binary_IO
