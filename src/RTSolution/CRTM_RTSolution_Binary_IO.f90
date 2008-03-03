!
! CRTM_RTSolution_Binary_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_RTSolution files.
!
! This module is primarily used for testing purposes only. Eventually 
! it will be removed from the CRTM library.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-May-2007
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_RTSolution_Binary_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility          , ONLY: File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                    Display_Message
  USE Binary_File_Utility   , ONLY: Open_Binary_File
  USE CRTM_Parameters       , ONLY: ZERO, ONE, SET, YES
  USE CRTM_RTSolution_Define, ONLY: CRTM_RTSolution_type, &
                                    CRTM_Associated_RTSolution, &
                                    CRTM_Allocate_RTSolution, &
                                    CRTM_Destroy_RTSolution
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Only the module routines are public
  PUBLIC :: CRTM_Inquire_RTSolution_Binary
  PUBLIC :: CRTM_Read_RTSolution_Binary
  PUBLIC :: CRTM_Write_RTSolution_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'


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
!       CRTM_Inquire_RTSolution_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM RTSolution structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_RTSolution_Binary( Filename               , &  ! Input
!                                                      n_Channels =n_Channels , &  ! Optional output
!                                                      n_Profiles =n_Profiles , &  ! Optional output
!                                                      RCS_Id     =RCS_Id     , &  ! Revision control
!                                                      Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     RTSolution format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
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
!       n_Channels:   The number of spectral channels for which there is
!                     data in the file. Note that this value will always
!                     be 0 for a profile-only RTSolution dataset-- it only
!                     has meaning for K-matrix RTSolution data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Profiles:   The number of atmospheric profiles in the data file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:       Character string containing the version control Id
!                     field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file inquire was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_RTSolution_Binary( Filename   , &  ! Input
                                           n_Channels , &  ! Optional output
                                           n_Profiles , &  ! Optional output
                                           RCS_Id     , &  ! Revision control
                                           Message_Log) &  ! Error messaging
                                         RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_RTSolution_Binary'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Channels_in_File
    INTEGER :: n_Profiles_in_File
 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      GOTO 2000  ! Clean up
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      GOTO 2000
    END IF


    ! Read the number of profiles
    ! ---------------------------
    CALL Read_Dimensions( Filename, FileID, &
                          n_Channels_in_File, n_Profiles_in_File, &
                          IO_Status, Message )
    IF ( IO_Status /= 0 ) GOTO 1000


    ! Save optional return arguments
    ! ------------------------------
    IF ( PRESENT( n_Channels ) ) n_Channels = n_Channels_in_File
    IF ( PRESENT( n_Profiles ) ) n_Profiles = n_Profiles_in_File


    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE( FileID )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION CRTM_Inquire_RTSolution_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Read_RTSolution_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM RTSolution structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_RTSolution_Binary( Filename               , &  ! Input
!                                                   RTSolution             , &  ! Output
!                                                   Quiet      =Quiet      , &  ! Optional input
!                                                   n_Channels =n_Channels , &  ! Optional output
!                                                   n_Profiles =n_Profiles , &  ! Optional output
!                                                   RCS_Id     =RCS_Id     , &  ! Revision control
!                                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     RTSolution format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       RTSolution:   Structure array containing the RTSolution data. Note 
!                     the rank is CHANNELS x PROFILES.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Rank-2 (L x M)
!                     ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this argument to suppress non-ERROR messages
!                     being printed to standard output (or the message
!                     log file if the Message_Log optional argument is
!                     used.) By default, INFORMATION and WARNING messages
!                     are printed.
!                     If QUIET = 0, All messages are output.
!                        QUIET = 1, INFORMATION messages are suppressed.
!                        QUIET = 2, WARNING and INFORMATION messages
!                                   are suppressed.
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
!       n_Channels:   The number of channels for which data was read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Profiles:   The number of profiles for which data was read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:       Character string containing the version control Id
!                     field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file read was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output RTSolution argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Read_RTSolution_Binary( Filename    , &  ! Input
                                        RTSolution  , &  ! Output
                                        Quiet       , &  ! Optional input
                                        n_Channels  , &  ! Optional output
                                        n_Profiles  , &  ! Optional output
                                        RCS_Id      , &  ! Revision control
                                        Message_Log ) &  ! Error messaging
                                      RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),               INTENT(IN)     :: Filename
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: RTSolution(:,:)  ! L x M
    INTEGER,          OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT)    :: n_Channels
    INTEGER,          OPTIONAL, INTENT(OUT)    :: n_Profiles
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_RTSolution_Binary(L x M)'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Output_Information, Output_Warning
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER :: l, n_File_Channels, n_Input_Channels
    INTEGER :: m, n_File_Profiles, n_Input_Profiles
    TYPE(CRTM_RTSolution_type) :: Dummy_RTSolution
 

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      GOTO 2000  ! Clean up
    END IF

    ! Check Quiet optional argument
    Output_Information = .TRUE.
    Output_Warning     = .TRUE.
    IF ( PRESENT(Quiet) ) THEN
      SELECT CASE (Quiet)
        CASE (1); Output_Information = .FALSE.
        CASE (2); Output_Information = .FALSE.
                  Output_Warning     = .FALSE.
        CASE DEFAULT
          ! Do nothing
      END SELECT
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      GOTO 2000  ! Clean up
    END IF


    ! Read the dimensions     
    ! -------------------
    CALL Read_Dimensions( Filename, FileID, &
                          n_File_Channels, n_File_Profiles, &
                          IO_Status, Message )
    IF ( IO_Status /= 0 ) GOTO 1000

    ! Check if n_Channels in file is > size of output array
    n_Input_Channels = SIZE(RTSolution,1)
    IF ( n_File_Channels > n_Input_Channels ) THEN
      WRITE( Message, '( "Number of channels, ",i0," > size of the output RTSolution ", &
                        &"structure array dimension, ",i0,". Only the first ",i0, &
                        &" channel RTSolution structures will be read." )' ) &
                      n_File_Channels, n_Input_Channels, n_Input_Channels
      IF ( Output_Warning ) CALL Display_Message( ROUTINE_NAME, &
                                                  TRIM(Message), &
                                                  WARNING, &
                                                  Message_Log=Message_Log )
    END IF
    n_Input_Channels = MIN(n_Input_Channels, n_File_Channels)
    
    ! Check if n_Profiles in file is > size of output array
    n_Input_Profiles = SIZE(RTSolution,2)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( Message, '( "Number of profiles, ",i0," > size of the output RTSolution ", &
                        &"structure array dimension, ",i0,". Only the first ",i0, &
                        &" profile RTSolution structures will be read." )' ) &
                      n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      IF ( Output_Warning ) CALL Display_Message( ROUTINE_NAME, &
                                                  TRIM(Message), &
                                                  WARNING, &
                                                  Message_Log=Message_Log )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles
    ! --------------------------
    Profile_Loop: DO m = 1, n_Input_Profiles
      Channel_Loop: DO l = 1, n_Input_Channels
  
        ! Read the structure
        Error_Status = Read_RTSolution_Record( FileID, &
                                               RTSolution(l,m), &
                                               Message_Log=Message_Log )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading RTSolution element (",i0,",",i0,") from ", a )' ) &
                          l, m, TRIM(Filename)
          GOTO 1000  ! Clean up
        END IF

      END DO Channel_Loop
    END DO Profile_Loop


    ! Save optional return arguments
    ! ------------------------------
    IF ( PRESENT(n_Channels) ) n_Channels = n_Input_Channels
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_Input_Profiles


    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP',   &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Output_Information ) THEN
      WRITE( Message, '("Number of channels and profiles read from ",a,": ",i0,1x,i0 )' ) &
                      TRIM(Filename), n_Input_Channels, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE( FileID )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
    Destroy_Status = CRTM_Destroy_RTSolution( RTSolution, &
                                              Message_Log=Message_Log )
    Destroy_Status = CRTM_Destroy_RTSolution( Dummy_RTSolution, &
                                              Message_Log=Message_Log )

  END FUNCTION CRTM_Read_RTSolution_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Write_RTSolution_Binary
!
! PURPOSE:
!       Function to write Binary format RTSolution files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_RTSolution_Binary( Filename               , &  ! Input
!                                                    RTSolution             , &  ! Input
!                                                    Quiet      =Quiet      , &  ! Optional input
!                                                    RCS_Id     =RCS_Id     , &  ! Revision control
!                                                    Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     RTSolution format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       RTSolution:   Structure containing the RTSolution data to write.
!                     Note the rank is CHANNELS x PROFILES.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Rank-2 (L x M)
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this argument to suppress INFORMATION messages
!                     being printed to standard output (or the message
!                     log file if the Message_Log optional argument is
!                     used.) By default, INFORMATION messages are printed.
!                     If QUIET = 0, INFORMATION messages are OUTPUT.
!                        QUIET = 1, INFORMATION messages are SUPPRESSED.
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
!       RCS_Id:       Character string containing the version control Id
!                     field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE an unrecoverable error occurred.
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

  FUNCTION CRTM_Write_RTSolution_Binary( Filename    , &  ! Input
                                         RTSolution  , &  ! Input
                                         Quiet       , &  ! Optional input
                                         RCS_Id      , &  ! Revision control
                                         Message_Log ) &  ! Error messaging
                                       RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_RTSolution_type), INTENT(IN)  :: RTSolution(:,:)  ! L x M
    INTEGER,          OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_RTSolution_Binary'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: l, n_Output_Channels
    INTEGER :: m, n_Output_Profiles
 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Set the allowed dimensions
    n_Output_Channels = SIZE(RTSolution,1)
    n_Output_Profiles = SIZE(RTSolution,2)

    ! Check Quiet optional argument
    Noisy = .TRUE.
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
      GOTO 1000
    END IF


    ! Write the dimensions
    ! --------------------
    CALL Write_Dimensions( Filename, FileID, n_Output_Channels, n_Output_Profiles, &
                           IO_Status, Message )
    IF ( IO_Status /= 0 ) GOTO 1000


    ! Loop over all the data
    ! ----------------------
    Profile_Loop: DO m = 1, n_Output_Profiles
      Channel_Loop: DO l = 1, n_Output_Channels

        ! Write the structure data
        Error_Status = Write_RTSolution_Record( FileID, &
                                                RTSolution(l,m), &
                                                Message_Log=Message_Log )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '("Error writing RTSolution element (",i0,",",i0,") to ",a)' ) &
                          l, m, TRIM(Filename)
          GOTO 1000
        END IF

      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP',   &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message, '("Number of channels and profiles written to ",a,": ",i0,1x,i0 )' ) &
                      TRIM(Filename), n_Output_Channels, n_Output_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION CRTM_Write_RTSolution_Binary



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! ------------------------------------------------
  ! Function to read a single RTSolution data record
  ! ------------------------------------------------
  FUNCTION Read_RTSolution_Record( FileID     , &  ! Input
                                   RTSolution , &  ! Output
                                   Message_Log) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    INTEGER,                    INTENT(IN)     :: FileID
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: RTSolution
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_RTSolution_Binary(Record)'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: n_Layers

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT=IO_Status ) n_Layers
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading RTSolution data dimensions. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Allocate the RTSolution structure
    ! ---------------------------------
    Error_Status = CRTM_Allocate_RTSolution( n_Layers, &
                                             RTSolution, &
                                             Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error allocating RTSolution data structure.'
      GOTO 1000  ! Clean up
    END IF


    ! Read the forward radiative transfer intermediate results
    ! --------------------------------------------------------
    READ( FileID, IOSTAT=IO_Status ) RTSolution%Surface_Emissivity     , &
                                     RTSolution%Up_Radiance            , &
                                     RTSolution%Down_Radiance          , &
                                     RTSolution%Down_Solar_Radiance    , &
                                     RTSolution%Surface_Planck_Radiance, &
                                     RTSolution%Layer_Optical_Depth
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading RTSolution intermediate results. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Read the radiative transfer results
    ! ---------------------------------
    READ( FileID, IOSTAT=IO_Status ) RTSolution%Radiance              , &
                                     RTSolution%Brightness_Temperature
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading RTSolution data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
    Destroy_Status = CRTM_Destroy_RTSolution( RTSolution, &
                                              Message_Log=Message_Log )
    CLOSE( FileID, IOSTAT=IO_Status )

  END FUNCTION Read_RTSolution_Record


  ! -------------------------------------------------
  ! Function to write a single RTSolution data record
  ! -------------------------------------------------
  FUNCTION Write_RTSolution_Record( FileID     , &  ! Input
                                    RTSolution , &  ! Input
                                    Message_Log) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    INTEGER,                    INTENT(IN)  :: FileID
    TYPE(CRTM_RTSolution_type), INTENT(IN)  :: RTSolution
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_RTSolution_Binary(Record)'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
 
    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check structure pointer association status
    IF ( .NOT. CRTM_Associated_RTSolution( RTSolution ) ) THEN
      Message = 'Some or all INPUT RTSolution pointer members are NOT associated.'
      GOTO 1000
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE( FileID, IOSTAT=IO_Status ) RTSolution%n_Layers
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing RTSolution data dimensions. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF
    
    
    ! Write the forward radiative transfer intermediate results
    ! ---------------------------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) RTSolution%Surface_Emissivity     , &
                                      RTSolution%Up_Radiance            , &
                                      RTSolution%Down_Radiance          , &
                                      RTSolution%Down_Solar_Radiance    , &
                                      RTSolution%Surface_Planck_Radiance, &
                                      RTSolution%Layer_Optical_Depth
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing RTSolution intermediate results. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the radiative transfer results
    ! ------------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) RTSolution%Radiance              , &
                                      RTSolution%Brightness_Temperature
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing RTSolution data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
    CLOSE( FileID, STATUS=WRITE_ERROR_STATUS, IOSTAT=IO_Status )

  END FUNCTION Write_RTSolution_Record


  ! -------------------------------------------
  ! Utility routine to read the file dimensions
  ! -------------------------------------------
  SUBROUTINE Read_Dimensions( Filename, FileID, &
                              n_Channels, n_Profiles, &
                              IO_Status, Message )
    ! Arguments
    CHARACTER(*), INTENT(IN)  :: Filename
    INTEGER,      INTENT(IN)  :: FileID
    INTEGER,      INTENT(OUT) :: n_Channels
    INTEGER,      INTENT(OUT) :: n_Profiles
    INTEGER,      INTENT(OUT) :: IO_Status
    CHARACTER(*), INTENT(OUT) :: Message
    ! Read the dimensions from file    
    READ( FileID, IOSTAT=IO_Status ) n_Channels, n_Profiles
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading RTSolution data dimensions from ",a,&
                      &". IOSTAT = ",i0)' ) TRIM(Filename), IO_Status
    END IF
  END SUBROUTINE Read_Dimensions


  ! --------------------------------------------
  ! Utility routine to write the file dimensions
  ! --------------------------------------------
  SUBROUTINE Write_Dimensions( Filename, FileID, &
                               n_Channels, n_Profiles, &
                               IO_Status, Message )
    ! Arguments
    CHARACTER(*), INTENT(IN)  :: Filename
    INTEGER,      INTENT(IN)  :: FileID
    INTEGER,      INTENT(IN)  :: n_Channels
    INTEGER,      INTENT(IN)  :: n_Profiles
    INTEGER,      INTENT(OUT) :: IO_Status
    CHARACTER(*), INTENT(OUT) :: Message
    ! Write the dimensions to file    
    WRITE( FileID, IOSTAT=IO_Status ) n_Channels, n_Profiles
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error writing RTSolution data dimensions to ",a, &
                      &". IOSTAT = ",i0)' ) TRIM(Filename), IO_Status
      CLOSE( FileID, STATUS=WRITE_ERROR_STATUS )
    END IF
  END SUBROUTINE Write_Dimensions

END MODULE CRTM_RTSolution_Binary_IO
