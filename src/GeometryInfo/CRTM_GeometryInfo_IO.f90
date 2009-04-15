!
! CRTM_GeometryInfo_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_GeometryInfo files.
!
! This module is primarily used for testing purposes only.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 13-Apr-2009
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_GeometryInfo_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility            , ONLY: File_Exists, File_Open
  USE Message_Handler         , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility     , ONLY: Open_Binary_File
  USE CRTM_Parameters         , ONLY: ZERO, ONE, SET, NOT_SET, YES
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Only the module routines are public
  PUBLIC :: CRTM_Inquire_GeometryInfo
  PUBLIC :: CRTM_Read_GeometryInfo
  PUBLIC :: CRTM_Write_GeometryInfo


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
!       CRTM_Inquire_GeometryInfo
!
! PURPOSE:
!       Function to inquire Binary format CRTM GeometryInfo structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_GeometryInfo( &
!                        Filename               , &
!                        n_Profiles =n_Profiles , &
!                        RCS_Id     =RCS_Id     , &
!                        Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     GeometryInfo format data file to read.
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
!       n_Profiles:   The number of profiles in the data file.
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

  FUNCTION CRTM_Inquire_GeometryInfo( &
    Filename   , &  ! Input
    n_Profiles , &  ! Optional output
    RCS_Id     , &  ! Revision control
    Message_Log) &  ! Error messaging
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_GeometryInfo'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m
 
    ! Set up
    err_stat = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the number of profiles
    READ( fid,IOSTAT=io_stat ) m
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions from ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return arguments
    IF ( PRESENT(n_Profiles) ) n_Profiles = m

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup'
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_Inquire_GeometryInfo


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Read_GeometryInfo
!
! PURPOSE:
!       Function to read Binary format CRTM GeometryInfo structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_GeometryInfo(    &
!                        Filename               , &
!                        gInfo                  , &
!                        Quiet      =Quiet      , &
!                        n_Profiles =n_Profiles , &
!                        RCS_Id     =RCS_Id     , &
!                        Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     GeometryInfo format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       gInfo:        Structure containing the GeometryInfo data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_GeometryInfo_type
!                     DIMENSION:  Rank-1 (M)
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       n_Profiles:   The number of profiles (M dimensions) for which
!                     data was read.
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
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Read_GeometryInfo( &
    Filename   , &  ! Input
    gInfo      , &  ! Output
    Quiet      , &  ! Optional input
    n_Profiles , &  ! Optional output
    RCS_Id     , &  ! Revision control
    Message_Log) &  ! Error messaging
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),                 INTENT(IN)  :: Filename
    TYPE(CRTM_GeometryInfo_type), INTENT(OUT) :: gInfo(:)  ! M
    INTEGER,            OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,            OPTIONAL, INTENT(OUT) :: n_Profiles
    CHARACTER(*),       OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),       OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_GeometryInfo'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n_File_Profiles
    INTEGER :: m, n_Input_Profiles
 

    ! Set up
    err_stat = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    ! ...Default action is to output info messages...
    Noisy = .TRUE.
    ! ...unless the Quiet optional argument is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions     
    READ( fid,IOSTAT=io_stat ) n_File_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions from ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Profiles > size of output array
    n_Input_Profiles = SIZE(gInfo)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output GeometryInfo ", &
                  &"structure array, ",i0,". Only the first ",i0, &
                  &" GeometryInfo structures will be read.")' ) &
                  n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING, Message_Log=Message_Log )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles
    Profile_Loop: DO m = 1, n_Input_Profiles
      err_stat = Read_Record( &
        fid, &
        gInfo(m), &
        Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading GeometryInfo element (",i0,") from ",a)' ) &
                   m, TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_Input_Profiles


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of profiles read from ",a,": ",i0)' ) TRIM(Filename), n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION, Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      ! Close file if necessary
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CRTM_Read_GeometryInfo


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Write_GeometryInfo
!
! PURPOSE:
!       Function to write Binary format GeometryInfo files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_GeometryInfo(   &
!                        Filename               , &
!                        gInfo                  , &
!                        Quiet      =Quiet      , &
!                        RCS_Id     =RCS_Id     , &
!                        Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     GeometryInfo format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       gInfo:        Structure containing the GeometryInfo data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_GeometryInfo_type
!                     DIMENSION:  Rank-1 (M)
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

  FUNCTION CRTM_Write_GeometryInfo( &
    Filename   , &  ! Input
    gInfo      , &  ! Input
    Quiet      , &  ! Optional input
    RCS_Id     , &  ! Revision control
    Message_Log) &  ! Error messaging
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),                 INTENT(IN)  :: Filename
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)  :: gInfo(:)  ! M
    INTEGER,            OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),       OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),       OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_GeometryInfo'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, n_Output_Profiles
 
    ! Set up
    err_stat = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    ! ...Default action is to output info messages...
    Noisy = .TRUE.
    ! ...unless the Quiet optional argument is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    n_Output_Profiles = SIZE(gInfo)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output=SET, Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) n_Output_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions to ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF

    
    ! Write the data
    Profile_Loop: DO m = 1, n_Output_Profiles
      err_stat = Write_Record( &
        fid, &
        gInfo(m), &
        Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing GeometryInfo element (",i0,") to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of profiles written to ",a,": ",i0)' ) &
                 TRIM(Filename), n_Output_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION, Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      ! Close file if necessary
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup.'
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_Write_GeometryInfo


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
!       Utility function to read a single GeometryInfo data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID                 , &
!                                   gInfo                  , &
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
!       gInfo:        CRTM GeometryInfo structure containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_GeometryInfo_type
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

  FUNCTION Read_Record( &
    fid        , &  ! Input
    gInfo      , &  ! Output
    Message_Log) &  ! Error messaging
  RESULT( err_stat )
    ! Arguments
    INTEGER,                      INTENT(IN)     :: fid
    TYPE(CRTM_GeometryInfo_type), INTENT(IN OUT) :: gInfo
    CHARACTER(*),       OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_GeometryInfo(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    INTEGER :: io_stat

    ! Set up
    err_stat = SUCCESS


    ! Read the user input data
    READ( fid,IOSTAT=io_stat ) &
      gInfo%Longitude           , &
      gInfo%Latitude            , &
      gInfo%Surface_Altitude    , &
      gInfo%iFOV                , &
      gInfo%Sensor_Scan_Angle   , &
      gInfo%Sensor_Zenith_Angle , &
      gInfo%Sensor_Azimuth_Angle, &
      gInfo%Source_Zenith_Angle , &
      gInfo%Source_Azimuth_Angle, &
      gInfo%Flux_Zenith_Angle      
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading gInfo user input. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the derived input data
    READ( fid,IOSTAT=io_stat ) &
      gInfo%Distance_Ratio       , &
      gInfo%Sensor_Scan_Radian   , &
      gInfo%Sensor_Zenith_Radian , &
      gInfo%Sensor_Azimuth_Radian, &
      gInfo%Secant_Sensor_Zenith , &
      gInfo%Source_Zenith_Radian , &
      gInfo%Source_Azimuth_Radian, &
      gInfo%Secant_Source_Zenith , &
      gInfo%Flux_Zenith_Radian   , &
      gInfo%Secant_Flux_Zenith   
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading gInfo derived data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
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
!       Function to write a single GeometryInfo data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID                 , &
!                                    gInfo                  , &
!                                    Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       gInfo:        CRTM GeometryInfo structure containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_GeometryInfo_type
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

  FUNCTION Write_Record( &
    fid        , &  ! Input
    gInfo      , &  ! Input
    Message_Log) &  ! Error messaging
  RESULT( err_stat )
    ! Arguments
    INTEGER,                      INTENT(IN)  :: fid
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)  :: gInfo
    CHARACTER(*),       OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_GeometryInfo(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    INTEGER :: io_stat
 
    ! Set up
    err_stat = SUCCESS


    ! Write the user input data
    WRITE( fid,IOSTAT=io_stat ) &
      gInfo%Longitude           , &
      gInfo%Latitude            , &
      gInfo%Surface_Altitude    , &
      gInfo%iFOV                , &
      gInfo%Sensor_Scan_Angle   , &
      gInfo%Sensor_Zenith_Angle , &
      gInfo%Sensor_Azimuth_Angle, &
      gInfo%Source_Zenith_Angle , &
      gInfo%Source_Azimuth_Angle, &
      gInfo%Flux_Zenith_Angle      
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing gInfo user input. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the derived input data
    WRITE( fid,IOSTAT=io_stat ) &
      gInfo%Distance_Ratio       , &
      gInfo%Sensor_Scan_Radian   , &
      gInfo%Sensor_Zenith_Radian , &
      gInfo%Sensor_Azimuth_Radian, &
      gInfo%Secant_Sensor_Zenith , &
      gInfo%Source_Zenith_Radian , &
      gInfo%Source_Azimuth_Radian, &
      gInfo%Secant_Source_Zenith , &
      gInfo%Flux_Zenith_Radian   , &
      gInfo%Secant_Flux_Zenith   
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing gInfo derived data. IOSTAT = ",i0)' ) io_stat
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

END MODULE CRTM_GeometryInfo_IO
