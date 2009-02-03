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
  ! Default message length
  INTEGER, PARAMETER :: ML = 256


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Inquire_RTSolution_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM RTSolution structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_RTSolution_Binary( Filename               , &
!                                                      n_Channels =n_Channels , &
!                                                      n_Profiles =n_Profiles , &
!                                                      RCS_Id     =RCS_Id     , &
!                                                      Message_Log=Message_Log  )
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
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_RTSolution_Binary( Filename   , &  ! Input
                                           n_Channels , &  ! Optional output
                                           n_Profiles , &  ! Optional output
                                           RCS_Id     , &  ! Revision control
                                           Message_Log) &  ! Error messaging
                                         RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_RTSolution_Binary'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, m
 
    ! Set up
    ! ------
    err_stat = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    ! -------------
    err_stat = Open_Binary_File( Filename, fid, Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the number of channels,profiles
    ! ------------------------------------
    READ( fid,IOSTAT=io_stat ) l, m
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions from ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Close the file
    ! --------------
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return arguments
    ! ------------------------
    IF ( PRESENT(n_Channels) ) n_Channels = l
    IF ( PRESENT(n_Profiles) ) n_Profiles = m

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( fid,IOSTAT=io_stat )
          IF ( io_stat /= SUCCESS ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup'
        END IF
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_Inquire_RTSolution_Binary


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Read_RTSolution_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM RTSolution structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_RTSolution_Binary( Filename               , &
!                                                   RTSolution             , &
!                                                   Quiet      =Quiet      , &
!                                                   n_Channels =n_Channels , &
!                                                   n_Profiles =n_Profiles , &
!                                                   RCS_Id     =RCS_Id     , &
!                                                   Message_Log=Message_Log  )
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
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Read_RTSolution_Binary( Filename    , &  ! Input
                                        RTSolution  , &  ! Output
                                        Quiet       , &  ! Optional input
                                        n_Channels  , &  ! Optional output
                                        n_Profiles  , &  ! Optional output
                                        RCS_Id      , &  ! Revision control
                                        Message_Log ) &  ! Error messaging
                                      RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)     :: Filename
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: RTSolution(:,:)  ! L x M
    INTEGER,          OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT)    :: n_Channels
    INTEGER,          OPTIONAL, INTENT(OUT)    :: n_Profiles
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_RTSolution_Binary(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, n_File_Channels, n_Input_Channels
    INTEGER :: m, n_File_Profiles, n_Input_Profiles
 

    ! Set up
    ! ------
    err_stat = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    ! Default action is to output info messages...
    Noisy = .TRUE.
    ! ...unless the Quiet optional argument is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file
    ! -------------
    err_stat = Open_Binary_File( Filename, fid, Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions     
    ! -------------------
    READ( fid,IOSTAT=io_stat ) n_File_Channels, n_File_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions from ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Check if n_Channels in file is > size of output array
    n_Input_Channels = SIZE(RTSolution,DIM=1)
    IF ( n_File_Channels > n_Input_Channels ) THEN
      WRITE( msg,'("Number of channels, ",i0," > size of the output RTSolution ", &
                  &"structure array dimension, ",i0,". Only the first ",i0, &
                  &" channel RTSolution structures will be read." )' ) &
                  n_File_Channels, n_Input_Channels, n_Input_Channels
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING, Message_Log=Message_Log )
    END IF
    n_Input_Channels = MIN(n_Input_Channels, n_File_Channels)
    ! Check if n_Profiles in file is > size of output array
    n_Input_Profiles = SIZE(RTSolution,DIM=2)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output RTSolution ", &
                  &"structure array dimension, ",i0,". Only the first ",i0, &
                  &" profile RTSolution structures will be read." )' ) &
                  n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING, Message_Log=Message_Log )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles and channels
    ! ---------------------------------------
    Profile_Loop: DO m = 1, n_Input_Profiles
      Channel_Loop: DO l = 1, n_Input_Channels
        err_stat = Read_Record( fid, RTSolution(l,m), Message_Log=Message_Log )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error reading RTSolution element (",i0,",",i0,") from ",a)' ) &
                     l, m, TRIM(Filename)
          CALL Read_Cleanup(Close_File=.TRUE.); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop

    
    ! Close the file
    ! --------------
    CLOSE( fid,IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the return values
    ! ---------------------
    IF ( PRESENT(n_Channels) ) n_Channels = n_Input_Channels
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_Input_Profiles


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of channels and profiles read from ",a,": ",i0,1x,i0)' ) &
                 TRIM(Filename), n_Input_Channels, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION, Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( fid,IOSTAT=io_stat )
          IF ( io_stat /= 0 ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Destroy the structure
      err_stat = CRTM_Destroy_RTSolution( RTSolution, Message_Log=Message_Log)
      IF ( err_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error destroying RTSolution structure during error cleanup.'
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CRTM_Read_RTSolution_Binary


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Write_RTSolution_Binary
!
! PURPOSE:
!       Function to write Binary format RTSolution files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_RTSolution_Binary( Filename               , &
!                                                    RTSolution             , &
!                                                    Quiet      =Quiet      , &
!                                                    RCS_Id     =RCS_Id     , &
!                                                    Message_Log=Message_Log  )
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
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Write_RTSolution_Binary( Filename    , &  ! Input
                                         RTSolution  , &  ! Input
                                         Quiet       , &  ! Optional input
                                         RCS_Id      , &  ! Revision control
                                         Message_Log ) &  ! Error messaging
                                       RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_RTSolution_type), INTENT(IN)  :: RTSolution(:,:)  ! L x M
    INTEGER,          OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_RTSolution_Binary(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, n_Output_Channels
    INTEGER :: m, n_Output_Profiles
 
    ! Set up
    ! ------
    err_stat = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    ! Default action is to output info messages...
    Noisy = .TRUE.
    ! ...unless the Quiet optional argument is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    ! Set the allowed dimensions
    n_Output_Channels = SIZE(RTSolution,1)
    n_Output_Profiles = SIZE(RTSolution,2)


    ! Open the file
    ! -------------
    err_stat = Open_Binary_File( TRIM(Filename), &
                                 fid, &
                                 For_Output  = SET, &
                                 Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    ! --------------------
    WRITE( fid,IOSTAT=io_stat ) n_Output_Channels, n_Output_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions to ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the data
    ! --------------
    Profile_Loop: DO m = 1, n_Output_Profiles
      Channel_Loop: DO l = 1, n_Output_Channels
        err_stat = Write_Record( fid, RTSolution(l,m), Message_Log=Message_Log )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error writing RTSolution element (",i0,",",i0,") to ",a)' ) &
                     l, m, TRIM(Filename)
          CALL Write_Cleanup(Close_File=.TRUE.); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    ! ------------------------------------
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of channels and profiles written to ",a,": ",i0,1x,i0 )' ) &
                 TRIM(Filename), n_Output_Channels, n_Output_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
          IF ( io_stat /= 0 ) &
            msg = TRIM(msg)//'; Error deleting output file during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_Write_RTSolution_Binary



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single RTSolution data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID                 , &
!                                   RTSolution             , &
!                                   Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       RTSolution:   CRTM RTSolution structure containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
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
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the record read was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Read_Record( fid        , &  ! Input
                        rts        , &  ! Output
                        Message_Log) &  ! Error messaging
                      RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)     :: fid
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_RTSolution_Binary(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: n_Layers

    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Read the data dimensions
    ! ------------------------
    READ( fid,IOSTAT=io_stat ) n_Layers
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'( "Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the RTSolution structure
    ! ---------------------------------
    err_stat = CRTM_Allocate_RTSolution( n_Layers, &
                                         rts, &
                                         Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error allocating data structure.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the forward radiative transfer intermediate results
    ! --------------------------------------------------------
    READ( fid,IOSTAT=io_stat ) rts%Surface_Emissivity     , &
                               rts%Up_Radiance            , &
                               rts%Down_Radiance          , &
                               rts%Down_Solar_Radiance    , &
                               rts%Surface_Planck_Radiance, &
                               rts%Layer_Optical_Depth
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading RTSolution intermediate results. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the radiative transfer results
    ! -----------------------------------
    READ( fid,IOSTAT=io_stat ) rts%Radiance              , &
                               rts%Brightness_Temperature
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading RTSolution data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      ! Deallocate RTSolution structure
      err_stat = CRTM_Destroy_RTSolution( rts, Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error destroying RTSolution structure during error cleanup'
      ! Close input file
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      ! Report error(s)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!----------------------------------------------------------------------------------
!
! NAME:
!       Write_Record
!
! PURPOSE:
!       Function to write a single RTSolution data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID                 , &
!                                    RTSolution             , &
!                                    Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       RTSolution:   CRTM RTSolution structure containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
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
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the record write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Write_Record( fid        , &  ! Input
                         rts        , &  ! Input
                         Message_Log) &  ! Error messaging
                       RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)  :: fid
    TYPE(CRTM_RTSolution_type), INTENT(IN)  :: rts
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_RTSolution_Binary(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
 
    ! Set up
    ! ------
    err_stat = SUCCESS
    ! Check structure pointer association status
    IF ( .NOT. CRTM_Associated_RTSolution( rts ) ) THEN
      msg = 'Some or all INPUT RTSolution pointer members are NOT associated.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE( fid,IOSTAT=io_stat ) rts%n_Layers
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing RTSolution data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF
    
    
    ! Write the forward radiative transfer intermediate results
    ! ---------------------------------------------------------
    WRITE( fid,IOSTAT=io_stat ) rts%Surface_Emissivity     , &
                                rts%Up_Radiance            , &
                                rts%Down_Radiance          , &
                                rts%Down_Solar_Radiance    , &
                                rts%Surface_Planck_Radiance, &
                                rts%Layer_Optical_Depth
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing RTSolution intermediate results. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the radiative transfer results
    ! ------------------------------------
    WRITE( fid,IOSTAT=io_stat ) rts%Radiance              , &
                                rts%Brightness_Temperature
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing RTSolution data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Write_Record_Cleanup()
      ! Close and delete output file
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      ! Report error(s)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Write_Record_Cleanup

  END FUNCTION Write_Record

END MODULE CRTM_RTSolution_Binary_IO
