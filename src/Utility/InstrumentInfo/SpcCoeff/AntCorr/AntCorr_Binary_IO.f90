!
! AntCorr_Binary_IO
!
! Module containing routines to read and write Binary format
! AntCorr data files.
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, CIMSS/SSEC, 08-Jun-2007
!                    paul.vandelst@ssec.wisc.edu
!

MODULE AntCorr_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE AntCorr_Define     , ONLY: AntCorr_type, &
                                 Associated_AntCorr, &
                                 Destroy_AntCorr, &
                                 Allocate_AntCorr, &
                                 CheckRelease_AntCorr, &
                                 Info_AntCorr
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure procedures
  PUBLIC :: Inquire_AntCorr_Binary
  PUBLIC :: Read_AntCorr_Binary
  PUBLIC :: Write_AntCorr_Binary
    

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

  FUNCTION Inquire_AntCorr_Binary( Filename   , &  ! Input
                                   n_FOVs     , &  ! Optional Output
                                   n_Channels , &  ! Optional Output
                                   Release    , &  ! Optional Output
                                   Version    , &  ! Optional Output
                                   RCS_Id     , &  ! Revision Control
                                   Message_Log) &  ! Error Messaging
                                 RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_FOVs
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_AntCorr_Binary'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: Rel, Ver
    INTEGER :: n_File_FOVs, n_File_Channels

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Open the Binary format AntCorr file
    ! -----------------------------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening AntCorr file '//TRIM(Filename), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Read the Release and Version information
    ! ----------------------------------------
    READ(FileID, IOSTAT=IO_Status) Rel, Ver
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading AntCorr Release/Version values from ", a, &
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
    READ(FileID, IOSTAT=IO_Status) n_File_FOVs, n_File_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading AntCorr dimension values from ", a, &
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
    IF ( PRESENT(n_FOVs    ) ) n_FOVs     = n_File_FOVs
    IF ( PRESENT(n_Channels) ) n_Channels = n_File_Channels

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
    
  END FUNCTION Inquire_AntCorr_Binary


  FUNCTION Read_AntCorr_Binary( Filename     , &  ! Input
                                AntCorr      , &  ! Output
                                No_File_Close, &  ! Optional input
                                No_Allocate  , &  ! Optional input
                                Quiet        , &  ! Optional input
                                RCS_Id       , &  ! Revision Control
                                Message_Log  ) &  ! Error Messaging
                              RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: Filename
    TYPE(AntCorr_type)    , INTENT(IN OUT) :: AntCorr
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_File_Close
    INTEGER     , OPTIONAL, INTENT(IN)     :: No_Allocate
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_AntCorr_Binary'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Destroy_Status
    LOGICAL :: Yes_File_Close
    LOGICAL :: Yes_Allocate
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Channels, n_FOVs

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file is open. If not, open it.
    ! Otherwise get its FileID.
    IF ( .NOT. File_Open( FileName ) ) THEN
      ! Check that the file exists
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Message = 'File '//TRIM( Filename )//' not found.'
        GOTO 2000  ! Clean up
      END IF 
      ! Open the file
      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF
    ELSE
      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=FileID )
      ! Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
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
    READ( FileID, IOSTAT=IO_Status ) AntCorr%Release, AntCorr%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading AntCorr Release/Version values from ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the release
    Error_Status = CheckRelease_AntCorr( AntCorr, &
                                         Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'AntCorr Release check failed for '//TRIM(Filename)
      GOTO 1000  ! Clean up
    END IF


    ! Read the data dimensions
    ! ------------------------
    READ(FileID, IOSTAT=IO_Status) n_FOVs, n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error reading AntCorr dimension values from ",a,". IOSTAT = ",i0)' ) &
                     TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Allocate the output structure if required
    ! -----------------------------------------
    IF ( Yes_Allocate ) THEN
      Error_Status = Allocate_AntCorr( n_FOVs, n_Channels, &
                                       AntCorr, &
                                       Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'AntCorr allocation failed'
        GOTO 1000  ! Clean up
      END IF
    END IF


    ! Read the sensor id information
    ! ------------------------------
    READ(FileID, IOSTAT=IO_Status) AntCorr%Sensor_Id       , &
                                   AntCorr%WMO_Satellite_Id, &
                                   AntCorr%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading AntCorr sensor information from ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Read the antenna correction data
    ! --------------------------------
    READ(FileID, IOSTAT=IO_Status) AntCorr%Sensor_Channel, &
                                   AntCorr%A_earth       , &
                                   AntCorr%A_space       , &
                                   AntCorr%A_platform
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading AntCorr data from ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Close the file
    ! --------------
    IF ( Yes_File_Close ) THEN
      CLOSE( FileID, IOSTAT=IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              WARNING, &
                              Message_Log=Message_Log )
      END IF
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_AntCorr( AntCorr, Message )
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
    Destroy_Status = Destroy_AntCorr( AntCorr, Message_Log=Message_Log )
    
  END FUNCTION Read_AntCorr_Binary


  FUNCTION Write_AntCorr_Binary( Filename     , &  ! Input            
                                 AntCorr      , &  ! Input           
                                 No_File_Close, &  ! Optional input
                                 Quiet        , &  ! Optional input   
                                 RCS_Id       , &  ! Revision Control 
                                 Message_Log  ) &  ! Error Messaging  
                               RESULT( Error_Status )               
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    TYPE(AntCorr_type)    , INTENT(IN)  :: AntCorr
    INTEGER,      OPTIONAL, INTENT(IN)  :: No_File_Close
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AntCorr_Binary'
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
      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       For_Output=SET, &
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF
    ELSE
      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=FileID )
      ! Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 2000  ! Clean up
      END IF
    END IF

    ! Check structure association status
    IF ( .NOT. Associated_AntCorr( AntCorr ) ) THEN
      Message = 'Some or all INPUT AntCorr pointer members are NOT associated.'
      GOTO 1000  ! Clean up
    END IF
    
    ! Check the release
    Error_Status = CheckRelease_AntCorr( AntCorr, &
                                         Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'AntCorr structure Release check failed.'
      GOTO 1000  ! Clean up
    END IF

    ! Check the structure dimensions
    IF ( AntCorr%n_FOVs < 1 .OR. AntCorr%n_Channels < 1 ) THEN
      Message = 'Dimensions of AntCorr structure are < or = 0.'
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
    WRITE( FileID, IOSTAT=IO_Status ) AntCorr%Release, AntCorr%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error writing AntCorr Release/Version values to ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      GOTO 2000  ! Clean up
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE(FileID, IOSTAT=IO_Status) AntCorr%n_FOVs, AntCorr%n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message,'("Error writing AntCorr dimension values to ", a, &
                     &". IOSTAT = ", i0 )' ) &
                     TRIM(Filename), IO_Status
      GOTO 2000  ! Clean up
    END IF


    ! Write the sensor id information
    ! -------------------------------
    WRITE(FileID, IOSTAT=IO_Status) AntCorr%Sensor_Id       , &
                                    AntCorr%WMO_Satellite_Id, &
                                    AntCorr%WMO_Sensor_Id    
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing AntCorr sensor information to ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      GOTO 2000  ! Clean up
    END IF


    ! Write the antenna correction data
    ! ---------------------------------
    WRITE(FileID, IOSTAT=IO_Status) AntCorr%Sensor_Channel, &
                                    AntCorr%A_earth       , &
                                    AntCorr%A_space       , &
                                    AntCorr%A_platform
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing AntCorr data to ",a,". IOSTAT = ",i0)' ) &
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
      CALL Info_AntCorr( AntCorr, Message )
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

  END FUNCTION Write_AntCorr_Binary

END MODULE AntCorr_Binary_IO
