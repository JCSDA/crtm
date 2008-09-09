!
! CRTM_Atmosphere_Binary_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_Atmosphere files.
!
! This module is primarily used for testing purposes only. Eventually 
! it will be removed from the CRTM library.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Atmosphere_Binary_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility          , ONLY: File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility   , ONLY: Open_Binary_File
  USE CRTM_Parameters       , ONLY: ZERO, ONE, SET, NOT_SET, YES
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type, &
                                    CRTM_Associated_Atmosphere, &
                                    CRTM_Destroy_Atmosphere, &
                                    CRTM_Allocate_Atmosphere
  USE CRTM_Cloud_Binary_IO  , ONLY: CRTM_Read_Cloud_Binary, CRTM_Write_Cloud_Binary
  USE CRTM_Aerosol_Binary_IO, ONLY: CRTM_Read_Aerosol_Binary, CRTM_Write_Aerosol_Binary
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Only the module routines are public
  PUBLIC :: CRTM_Inquire_Atmosphere_Binary
  PUBLIC :: CRTM_Read_Atmosphere_Binary
  PUBLIC :: CRTM_Write_Atmosphere_Binary


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE CRTM_Read_Atmosphere_Binary
    MODULE PROCEDURE Read_Atmosphere_Rank1
    MODULE PROCEDURE Read_Atmosphere_Rank2
  END INTERFACE CRTM_Read_Atmosphere_Binary
  
  INTERFACE CRTM_Write_Atmosphere_Binary
    MODULE PROCEDURE Write_Atmosphere_Rank1
    MODULE PROCEDURE Write_Atmosphere_Rank2
  END INTERFACE CRTM_Write_Atmosphere_Binary
  

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
!       CRTM_Inquire_Atmosphere_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM Atmosphere structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_Atmosphere_Binary( Filename               , &  ! Input
!                                                      n_Channels =n_Channels , &  ! Optional output
!                                                      n_Profiles =n_Profiles , &  ! Optional output
!                                                      RCS_Id     =RCS_Id     , &  ! Revision control
!                                                      Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Atmosphere format data file to read.
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
!                     be 0 for a profile-only dataset-- it only has meaning
!                     for K-matrix data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
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

  FUNCTION CRTM_Inquire_Atmosphere_Binary( Filename   , &  ! Input
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Atmosphere_Binary'
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

  END FUNCTION CRTM_Inquire_Atmosphere_Binary


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Read_Atmosphere_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Atmosphere structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Atmosphere_Binary( Filename               , &  ! Input
!                                                   Atmosphere             , &  ! Output
!                                                   Quiet      =Quiet      , &  ! Optional input
!                                                   n_Channels =n_Channels , &  ! Optional output
!                                                   n_Profiles =n_Profiles , &  ! Optional output
!                                                   RCS_Id     =RCS_Id     , &  ! Revision control
!                                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Atmosphere format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Atmosphere:   Structure containing the Atmosphere data. Note the
!                     following meanings attributed to the dimensions of
!                     the structure array:
!                     Rank-1: M profiles.
!                             Only profile data are to be read in. The file
!                             does not contain channel information. The
!                             dimension of the structure is understood to
!                             be the PROFILE dimension.
!                     Rank-2: L channels  x  M profiles
!                             Channel and profile data are to be read in.
!                             The file contains both channel and profile
!                             information. The first dimension of the 
!                             structure is the CHANNEL dimension, the second
!                             is the PROFILE dimension. This is to allow
!                             K-matrix structures to be read in with the
!                             same function.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Rank-1 (M) or Rank-2 (L x M)
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
!       n_Channels:   The number of channels for which data was read. Note that
!                     this value will always be 0 for a profile-only dataset--
!                     it only has meaning for K-matrix data.
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
!       Note the INTENT on the output Atmosphere argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_Atmosphere_Rank1( Filename   , &  ! Input
                                  Atmosphere , &  ! Output
                                  Quiet      , &  ! Optional input
                                  n_Channels , &  ! Optional output
                                  n_Profiles , &  ! Optional output
                                  RCS_Id     , &  ! Revision control
                                  Message_Log, &  ! Error messaging
                                  Debug      ) &  ! Debug output control
                                RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)     :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atmosphere(:)  ! M
    INTEGER,          OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT)    :: n_Channels
    INTEGER,          OPTIONAL, INTENT(OUT)    :: n_Profiles
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    INTEGER,          OPTIONAL, INTENT(IN)     :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Atmosphere_Binary(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: Debug_Quiet
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n_File_Channels, n_File_Profiles
    INTEGER :: m, n_Input_Profiles
 

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
    ! Default action is to NOT output debug messages...
    Debug_Quiet = SET
    ! ...unless the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Debug_Quiet = NOT_SET
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
    ! Check that n_Channels is zero
    IF ( n_File_Channels /= 0 ) THEN
      WRITE( msg,'("n_Channels dimensions in ",a," is not zero for a rank-1 ",&
                  &"(i.e. profiles only) Atmosphere structure read.")' ) TRIM(Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Check if n_Profiles > size of output array
    n_Input_Profiles = SIZE(Atmosphere)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output Atmosphere ", &
                  &"structure array, ",i0,". Only the first ",i0, &
                  &" Atmosphere structures will be read.")' ) &
                  n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING, Message_Log=Message_Log )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles
    ! --------------------------
    Profile_Loop: DO m = 1, n_Input_Profiles
      err_stat = Read_Record( fid, Atmosphere(m), Message_Log=Message_Log, Debug=Debug_Quiet )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Atmosphere element (",i0,") from ",a)' ) &
                   m, TRIM(Filename)
        CALL Read_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file
    ! --------------
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF

    ! Set the return values
    ! ---------------------
    IF ( PRESENT(n_Channels) ) n_Channels = 0
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_Input_Profiles


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of profiles read from ",a,": ",i0)' ) TRIM(Filename), n_Input_Profiles
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
      err_stat = CRTM_Destroy_Atmosphere( Atmosphere, Message_Log=Message_Log)
      IF ( err_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error destroying Atmosphere structure during error cleanup.'
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION Read_Atmosphere_Rank1


  FUNCTION Read_Atmosphere_Rank2( Filename   , &  ! Input
                                  Atmosphere , &  ! Output
                                  Quiet      , &  ! Optional input
                                  n_Channels , &  ! Optional output
                                  n_Profiles , &  ! Optional output
                                  RCS_Id     , &  ! Revision control
                                  Message_Log, &  ! Error messaging
                                  Debug      ) &  ! Debug output control
                                RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)     :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atmosphere(:,:)  ! L x M
    INTEGER,          OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT)    :: n_Channels
    INTEGER,          OPTIONAL, INTENT(OUT)    :: n_Profiles
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    INTEGER,          OPTIONAL, INTENT(IN)     :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Atmosphere_Binary(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: Debug_Quiet
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
    ! Default action is to NOT output debug messages...
    Debug_Quiet = SET
    ! ...unless the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Debug_Quiet = NOT_SET
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
    n_Input_Channels = SIZE(Atmosphere,DIM=1)
    IF ( n_File_Channels > n_Input_Channels ) THEN
      WRITE( msg,'("Number of channels, ",i0," > size of the output Atmosphere ", &
                  &"structure array dimension, ",i0,". Only the first ",i0, &
                  &" channel Atmosphere structures will be read.")' ) &
                  n_File_Channels, n_Input_Channels, n_Input_Channels
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING, Message_Log=Message_Log )
    END IF
    n_Input_Channels = MIN(n_Input_Channels, n_File_Channels)
    ! Check if n_Profiles in file is > size of output array
    n_Input_Profiles = SIZE(Atmosphere,DIM=2)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg, '( "Number of profiles, ",i0," > size of the output Atmosphere ", &
                    &"structure array dimension, ",i0,". Only the first ",i0, &
                    &" profile Atmosphere structures will be read.")' ) &
                    n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING, Message_Log=Message_Log )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles and channels
    ! ---------------------------------------
    Profile_Loop: DO m = 1, n_Input_Profiles
      Channel_Loop: DO l = 1, n_Input_Channels
        err_stat = Read_Record( fid, Atmosphere(l,m), Message_Log=Message_Log, Debug=Debug )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error reading Atmosphere element (",i0,",",i0,") from ",a)' ) &
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
      err_stat = CRTM_Destroy_Atmosphere( Atmosphere, Message_Log=Message_Log)
      IF ( err_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error destroying Atmosphere structure during error cleanup.'
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION Read_Atmosphere_Rank2


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Write_Atmosphere_Binary
!
! PURPOSE:
!       Function to write Binary format Atmosphere files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Atmosphere_Binary( Filename               , &  ! Input
!                                                    Atmosphere             , &  ! Input
!                                                    Quiet      =Quiet      , &  ! Optional input
!                                                    RCS_Id     =RCS_Id     , &  ! Revision control
!                                                    Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     Atmosphere format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:   Structure containing the Atmosphere data to write.
!                     Note the following meanings attributed to the
!                     dimensions of the structure array:
!                     Rank-1: M profiles.
!                             Only profile data are to be read in. The file
!                             does not contain channel information. The
!                             dimension of the structure is understood to
!                             be the PROFILE dimension.
!                     Rank-2: L channels  x  M profiles
!                             Channel and profile data are to be read in.
!                             The file contains both channel and profile
!                             information. The first dimension of the 
!                             structure is the CHANNEL dimension, the second
!                             is the PROFILE dimension. This is to allow
!                             K-matrix structures to be read in with the
!                             same function.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Rank-1 (M) or Rank-2 (L x M)
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

  FUNCTION Write_Atmosphere_Rank1( Filename   , &  ! Input
                                   Atmosphere , &  ! Input
                                   Quiet      , &  ! Optional input
                                   RCS_Id     , &  ! Revision control
                                   Message_Log, &  ! Error messaging
                                   Debug      ) &  ! Debug output control
                                 RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: Atmosphere(:)  ! M
    INTEGER,          OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
    INTEGER,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Atmosphere_Binary(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: Debug_Quiet
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, n_Output_Profiles
 
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
    ! Default action is to NOT output debug messages...
    Debug_Quiet = SET
    ! ...unless the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Debug_Quiet = NOT_SET
    END IF
    ! Any invalid profiles?
    IF ( ANY(Atmosphere%n_Layers    == 0 .OR. &
             Atmosphere%n_Absorbers == 0      ) ) THEN
      msg = 'Zero dimension profiles in input!'
      CALL Write_Cleanup(); RETURN
    END IF
    n_Output_Profiles = SIZE(Atmosphere)


    ! Open the file
    ! -------------
    err_stat = Open_Binary_File( Filename, fid, For_Output=SET, Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    ! --------------------
    WRITE( fid,IOSTAT=io_stat ) 0, n_Output_Profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions to ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    
    ! Write the data
    ! --------------
    Profile_Loop: DO m = 1, n_Output_Profiles
      err_stat = Write_Record( fid, Atmosphere(m), Message_Log=Message_Log, Debug=Debug_Quiet )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Atmosphere element (",i0,") to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(Close_File=.TRUE.); RETURN
      END IF
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
      WRITE( msg,'("Number of profiles written to ",a,": ",i0)' ) &
                 TRIM(Filename), n_Output_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION, Message_Log=Message_Log )
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

  END FUNCTION Write_Atmosphere_Rank1


  FUNCTION Write_Atmosphere_Rank2( Filename   , &  ! Input
                                   Atmosphere , &  ! Input
                                   Quiet      , &  ! Optional input
                                   RCS_Id     , &  ! Revision control
                                   Message_Log, &  ! Error messaging
                                   Debug      ) &  ! Debug output control
                                 RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: Atmosphere(:,:)  ! L x M
    INTEGER,          OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
    INTEGER,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Atmosphere_Binary(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: Debug_Quiet
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, n_Output_Channels
    INTEGER :: m, n_Output_Profiles
 
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
    ! Default action is to NOT output debug messages...
    Debug_Quiet = SET
    ! ...unless the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Debug_Quiet = NOT_SET
    END IF
    ! Any invalid profiles?
    IF ( ANY(Atmosphere%n_Layers    == 0 .OR. &
             Atmosphere%n_Absorbers == 0      ) ) THEN
      msg = 'Zero dimension profiles in input!'
      CALL Write_Cleanup(); RETURN
    END IF
    n_Output_Channels = SIZE(Atmosphere,DIM=1)
    n_Output_Profiles = SIZE(Atmosphere,DIM=2)


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
        err_stat = Write_Record( fid, Atmosphere(l,m), Message_Log=Message_Log, Debug=Debug_Quiet )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error writing Atmosphere element (",i0,",",i0,") to ",a)' ) &
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

  END FUNCTION Write_Atmosphere_Rank2


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
!       Utility function to read a single atmosphere data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID                 , &  ! Input
!                                   Atmosphere             , &  ! Output
!                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Atmosphere:   CRTM Atmosphere structure containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
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
                        atm        , &  ! Output
                        Message_Log, &  ! Error messaging
                        Debug      ) &  ! Debug output control
                      RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)     :: fid
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: atm
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    INTEGER,          OPTIONAL, INTENT(IN)     :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Atmosphere_Binary(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    CHARACTER(256) :: fname
    INTEGER :: io_stat
    INTEGER :: n_Layers 
    INTEGER :: n_Absorbers
    INTEGER :: n_Clouds
    INTEGER :: n_Aerosols

    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Read the data dimensions
    ! ------------------------
    READ( fid,IOSTAT=io_stat ) n_Layers, &
                               n_Absorbers, &
                               n_Clouds, &
                               n_Aerosols
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'( "Error reading atm data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the atm structure
    ! ---------------------------------
    err_stat = CRTM_Allocate_Atmosphere( n_Layers, &
                                         n_Absorbers, &
                                         n_Clouds, &
                                         n_Aerosols, &
                                         atm, &
                                         Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error allocating atm data structure.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the climatology model flag and absorber IDs
    ! ------------------------------------------------
    READ( fid,IOSTAT=io_stat ) atm%Climatology, &
                               atm%Absorber_ID, &
                               atm%Absorber_Units

    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading atm climatology and absorber IDs. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the atmospheric profile data
    ! ---------------------------------
    READ( fid,IOSTAT=io_stat ) atm%Level_Pressure, &
                               atm%Pressure, &
                               atm%Temperature, &
                               atm%Absorber
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading atmospheric profile data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

    ! Read the cloud data
    ! -------------------
    IF ( n_Clouds > 0 ) THEN
      ! Get the data filename
      INQUIRE( UNIT=fid,NAME=fname )
      ! Read the cloud data
      err_stat = CRTM_Read_Cloud_Binary( fname, &
                                         atm%Cloud, &
                                         Quiet        =Debug, &
                                         No_File_Close=SET, &
                                         No_Allocate  =SET, &
                                         Message_Log  =Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading atm Cloud(s)'
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Read the aerosol data
    ! ---------------------
    IF ( n_Aerosols > 0 ) THEN
      ! Get the data filename
      INQUIRE( UNIT=fid,NAME=fname )
      ! Read the aerosol data
      err_stat = CRTM_Read_Aerosol_Binary( fname, &
                                           atm%Aerosol, &
                                           Quiet        =Debug, &
                                           No_File_Close=SET, &
                                           No_Allocate  =SET, &
                                           Message_Log  =Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading atm Aerosol(s)'
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      ! Deallocate atmosphere structure
      err_stat = CRTM_Destroy_Atmosphere( atm, Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error destroying atmosphere structure during error cleanup'
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
!       Function to write a single atmosphere data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID                 , &  ! Input
!                                    Atmosphere             , &  ! Input
!                                    Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:   CRTM Atmosphere structure containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
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
                         atm        , &  ! Input
                         Message_Log, &  ! Error messaging
                         Debug      ) &  ! Debug output control
                       RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)  :: fid
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: atm
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
    INTEGER,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_CRTM_Atmosphere_Binary(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    CHARACTER(256) :: fname
    INTEGER :: io_stat
 
    ! Set up
    ! ------
    err_stat = SUCCESS
    ! Check structure pointer association status
    IF ( .NOT. CRTM_Associated_Atmosphere( atm, Skip_Cloud=SET, Skip_Aerosol=SET ) ) THEN
      msg = 'Some or all INPUT Atmosphere pointer members are NOT associated.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data dimensions
    ! -------------------------
    WRITE( fid,IOSTAT=io_stat ) atm%n_Layers, &
                                atm%n_Absorbers, &
                                atm%n_Clouds, &
                                atm%n_Aerosols
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Atmosphere data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the climatology model flag and absorber IDs
    ! -------------------------------------------------
    WRITE( fid,IOSTAT=io_stat ) atm%Climatology, &
                                atm%Absorber_ID, &
                                atm%Absorber_Units
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Atmosphere climatology and absorber IDs. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the atmospheric profile data
    ! ----------------------------------
    WRITE( fid,IOSTAT=io_stat ) atm%Level_Pressure, &
                                atm%Pressure, &
                                atm%Temperature, &
                                atm%Absorber
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing atmospheric profile data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the cloud data
    ! --------------------
    IF ( atm%n_Clouds > 0 ) THEN
      ! Get the data filename
      INQUIRE( UNIT=fid,NAME=fname )
      ! Write the cloud data
      err_stat = CRTM_Write_Cloud_Binary( fname, &
                                          atm%Cloud, &
                                          Quiet        =Debug, &
                                          No_File_Close=SET, &
                                          Message_Log  =Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Atmosphere Cloud(s)'
        CALL Write_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Write the aerosol data
    ! ----------------------
    IF ( atm%n_Aerosols > 0 ) THEN
      ! Get the data filename
      INQUIRE( UNIT=fid,NAME=fname )
      ! Write the aerosol data
      err_stat = CRTM_Write_Aerosol_Binary( fname, &
                                            atm%Aerosol, &
                                            Quiet        =Debug, &
                                            No_File_Close=SET, &
                                            Message_Log  =Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Atmosphere Aerosol(s)'
        CALL Write_Record_Cleanup(); RETURN
      END IF
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

END MODULE CRTM_Atmosphere_Binary_IO
