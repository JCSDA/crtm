!
! CRTM_Surface_Binary_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_Surface files.
!
! This module is primarily used for testing purposes only. Eventually 
! it will be removed from the CRTM library.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Surface_Binary_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: fp
  USE File_Utility       , ONLY: File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Parameters    , ONLY: ZERO, ONE, SET
  USE CRTM_Surface_Define, ONLY: CRTM_Surface_type, &
                                 LAND_SURFACE, &
                                 WATER_SURFACE, &
                                 SNOW_SURFACE, &
                                 ICE_SURFACE, &
                                 N_VALID_SURFACE_TYPES, &
                                 SURFACE_TYPE_NAME, &
                                 N_VALID_LAND_TYPES, &
                                 N_VALID_WATER_TYPES, &
                                 N_VALID_SNOW_TYPES, &
                                 N_VALID_ICE_TYPES, &
                                 CRTM_Destroy_SensorData, &
                                 CRTM_Allocate_SensorData, &
                                 CRTM_Destroy_Surface

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Only the module routines are public
  PUBLIC :: CRTM_Inquire_Surface_Binary
  PUBLIC :: CRTM_Read_Surface_Binary
  PUBLIC :: CRTM_Write_Surface_Binary


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Read_Surface_Binary
    MODULE PROCEDURE Read_Surface_Rank1
    MODULE PROCEDURE Read_Surface_Rank2
  END INTERFACE CRTM_Read_Surface_Binary

  INTERFACE CRTM_Write_Surface_Binary
    MODULE PROCEDURE Write_Surface_Rank1
    MODULE PROCEDURE Write_Surface_Rank2
  END INTERFACE CRTM_Write_Surface_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Message string length
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
!       CRTM_Inquire_Surface_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM Surface structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_Surface_Binary( Filename               , &
!                                                   n_Channels =n_Channels , &
!                                                   n_Profiles =n_Profiles , &
!                                                   RCS_Id     =RCS_Id     , &
!                                                   Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Surface format data file to read.
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
!                     If == SUCCESS the Binary inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_Surface_Binary( Filename   , &  ! Input
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Surface_Binary'
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

  END FUNCTION CRTM_Inquire_Surface_Binary


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Read_Surface_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Surface structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Surface_Binary( Filename               , &
!                                                Surface                , &
!                                                Quiet      =Quiet      , &
!                                                n_Channels =n_Channels , &
!                                                n_Profiles =n_Profiles , &
!                                                RCS_Id     =RCS_Id     , &
!                                                Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Surface format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Surface:      Structure containing the Surface data. Note the
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
!                     TYPE:       CRTM_Surface_type
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
!       Note the INTENT on the output Surface argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_Surface_Rank1( Filename   , &  ! Input
                               Surface    , &  ! Output
                               Quiet      , &  ! Optional input
                               n_Channels , &  ! Optional output
                               n_Profiles , &  ! Optional output
                               RCS_Id     , &  ! Revision control
                               Message_Log, &  ! Error messaging
                               Debug      ) &  ! Debug output control
                             RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)     :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Surface(:)  ! M
    INTEGER,       OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Channels
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Profiles
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    INTEGER,       OPTIONAL, INTENT(IN)     :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
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
    ! The Quiet optional argument is overridden
    ! if the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Noisy = .TRUE.
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
                  &"(i.e. profiles only) Surface structure read.")' ) TRIM(Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Check if n_Profiles > size of output array
    n_Input_Profiles = SIZE(Surface)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output Surface ", &
                  &"structure array, ",i0,". Only the first ",i0, &
                  &" Surface structures will be read.")' ) &
                  n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING, Message_Log=Message_Log )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles
    ! --------------------------
    Profile_Loop: DO m = 1, n_Input_Profiles
      err_stat = Read_Record( fid, Surface(m), Message_Log=Message_Log)
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Surface element (",i0,") from ",a)' ) &
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
      err_stat = CRTM_Destroy_Surface( Surface, Message_Log=Message_Log)
      IF ( err_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error destroying Surface structure during error cleanup.'
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_Surface_Rank1


  FUNCTION Read_Surface_Rank2( Filename   , &  ! Input
                               Surface    , &  ! Output
                               Quiet      , &  ! Optional input
                               n_Channels , &  ! Optional output
                               n_Profiles , &  ! Optional output
                               RCS_Id     , &  ! Revision control
                               Message_Log, &  ! Error messaging
                               Debug      ) &  ! Debug output control
                             RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)     :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Surface(:,:)  ! L x M
    INTEGER,       OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Channels
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Profiles
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    INTEGER,       OPTIONAL, INTENT(IN)     :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(L x M)'
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
    ! The Quiet optional argument is overridden
    ! if the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Noisy = .TRUE.
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
    n_Input_Channels = SIZE(Surface,DIM=1)
    IF ( n_File_Channels > n_Input_Channels ) THEN
      WRITE( msg,'("Number of channels, ",i0," > size of the output Surface ", &
                  &"structure array dimension, ",i0,". Only the first ",i0, &
                  &" channel Surface structures will be read.")' ) &
                  n_File_Channels, n_Input_Channels, n_Input_Channels
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING, Message_Log=Message_Log )
    END IF
    n_Input_Channels = MIN(n_Input_Channels, n_File_Channels)
    ! Check if n_Profiles in file is > size of output array
    n_Input_Profiles = SIZE(Surface,DIM=2)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( msg, '( "Number of profiles, ",i0," > size of the output Surface ", &
                    &"structure array dimension, ",i0,". Only the first ",i0, &
                    &" profile Surface structures will be read.")' ) &
                    n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING, Message_Log=Message_Log )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles and channels
    ! ---------------------------------------
    Profile_Loop: DO m = 1, n_Input_Profiles
      Channel_Loop: DO l = 1, n_Input_Channels
        err_stat = Read_Record( fid, Surface(l,m), Message_Log=Message_Log )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error reading Surface element (",i0,",",i0,") from ",a)' ) &
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
      err_stat = CRTM_Destroy_Surface( Surface, Message_Log=Message_Log)
      IF ( err_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error destroying Surface structure during error cleanup.'
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION Read_Surface_Rank2


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Write_Surface_Binary
!
! PURPOSE:
!       Function to write Binary format Surface files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Surface_Binary( Filename               , &
!                                                 Surface                , &
!                                                 Quiet      =Quiet      , &
!                                                 RCS_Id     =RCS_Id     , &
!                                                 Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     Surface format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Surface:      Structure containing the Surface data to write.
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
!                     TYPE:       CRTM_Surface_type
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

  FUNCTION Write_Surface_Rank1( Filename   , &  ! Input
                                Surface    , &  ! Input
                                Quiet      , &  ! Optional input
                                RCS_Id     , &  ! Revision control
                                Message_Log, &  ! Error messaging
                                Debug      ) &  ! Debug output control
                              RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN)  :: Surface(:)  ! M
    INTEGER,       OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    INTEGER,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
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
    ! The Quiet optional argument is overridden
    ! if the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Noisy = .TRUE.
    END IF
    ! Set dimensions
    n_Output_Profiles = SIZE(Surface)


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
      err_stat = Write_Record( fid, Surface(m), Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Surface element (",i0,") to ",a)' ) m, TRIM(Filename)
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

  END FUNCTION Write_Surface_Rank1


  FUNCTION Write_Surface_Rank2( Filename   , &  ! Input
                                Surface    , &  ! Input
                                Quiet      , &  ! Optional input
                                RCS_Id     , &  ! Revision control
                                Message_Log, &  ! Error messaging
                                Debug      ) &  ! Debug output control
                              RESULT ( err_stat )
                              
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN)  :: Surface(:,:)  ! L x M
    INTEGER,       OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    INTEGER,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(L x M)'
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
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    ! Default action is to output info messages...
    Noisy = .TRUE.
    ! ...unless the Quiet optional argument is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    ! The Quiet optional argument is overridden
    ! if the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Noisy = .TRUE.
    END IF
    ! Get dimensions
    n_Output_Channels = SIZE(Surface,DIM=1)
    n_Output_Profiles = SIZE(Surface,DIM=2)


    ! Open the file
    ! -------------
    err_stat = Open_Binary_File( Filename,fid,For_Output=SET,Message_Log=Message_Log )
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
        err_stat = Write_Record( fid, Surface(l,m), Message_Log=Message_Log )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error writing Surface element (",i0,",",i0,") to ",a)' ) &
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

  END FUNCTION Write_Surface_Rank2


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
!       Utility function to read a single surface data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID                 , &
!                                   Surface                , &
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
!       Surface:      CRTM Surface structure containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
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
                        sfc        , &  ! Output
                        Message_Log) &  ! Error messaging
                      RESULT( err_stat )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: fid
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: sfc
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    INTEGER :: io_stat
    INTEGER :: Type_in_File
    INTEGER :: Type_by_Coverage
    REAL(fp) :: Total_Coverage
    INTEGER :: n_Channels

    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Read the gross surface type and check it
    ! ----------------------------------------
    READ( fid,IOSTAT=io_stat ) Type_in_File, &
                               sfc%Land_Coverage, &
                               sfc%Water_Coverage, &
                               sfc%Snow_Coverage, &
                               sfc%Ice_Coverage
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'( "Error reading gross surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

    ! Check the total coverage
    sfc%Land_Coverage  = MAX(sfc%Land_Coverage,  ZERO)
    sfc%Water_Coverage = MAX(sfc%Water_Coverage, ZERO)
    sfc%Snow_Coverage  = MAX(sfc%Snow_Coverage,  ZERO)
    sfc%Ice_Coverage   = MAX(sfc%Ice_Coverage,   ZERO)
    Total_Coverage = sfc%Land_Coverage  + &
                     sfc%Water_Coverage + &
                     sfc%Snow_Coverage  + &
                     sfc%Ice_Coverage  
    IF ( Total_Coverage > ONE ) THEN
      WRITE( msg,'("Total coverage fraction sum, ",es13.6,", is > 1.0")' ) Total_Coverage
      CALL Read_Record_Cleanup(); RETURN
    END IF

    ! Compute the coverage type
    Type_by_Coverage = 0
    IF ( sfc%Land_Coverage  > ZERO ) Type_by_Coverage = LAND_SURFACE
    IF ( sfc%Water_Coverage > ZERO ) Type_by_Coverage = Type_by_Coverage + WATER_SURFACE
    IF ( sfc%Snow_Coverage  > ZERO ) Type_by_Coverage = Type_by_Coverage + SNOW_SURFACE
    IF ( sfc%Ice_Coverage   > ZERO ) Type_by_Coverage = Type_by_Coverage + ICE_SURFACE

    ! Check the file and coverge surfce types
    IF ( Type_in_File /= Type_by_Coverage ) THEN
      msg = 'Coverage surface type, '//TRIM(SURFACE_TYPE_NAME( Type_by_Coverage ))//&
            ', inconsistent with that specified in file.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the surface type independent data
    ! --------------------------------------
    READ( fid,IOSTAT=io_stat ) sfc%Wind_Speed
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading surface type independent data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the land surface type data
    ! -------------------------------
    READ( fid,IOSTAT=io_stat ) sfc%Land_Type, &
                               sfc%Land_Temperature, &
                               sfc%Soil_Moisture_Content, &
                               sfc%Canopy_Water_Content , &
                               sfc%Vegetation_Fraction, &
                               sfc%Soil_Temperature
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading land surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

    ! Check the type
    IF ( sfc%Land_Type < 0 .OR. sfc%Land_Type > N_VALID_LAND_TYPES ) THEN
      msg = 'Invalid land surface type'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the water surface type data
    ! --------------------------------
    READ( fid,IOSTAT=io_stat ) sfc%Water_Type, &
                               sfc%Water_Temperature, &
                               sfc%Wind_Direction, &
                               sfc%Salinity
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading water surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

    ! Check the type
    IF ( sfc%Water_Type < 0 .OR. sfc%Water_Type > N_VALID_WATER_TYPES ) THEN
      msg = 'Invalid water surface type'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the snow surface type data
    ! -------------------------------
    READ( fid,IOSTAT=io_stat ) sfc%Snow_Type, &
                               sfc%Snow_Temperature, &
                               sfc%Snow_Depth, &
                               sfc%Snow_Density, &
                               sfc%Snow_Grain_Size
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading snow surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

    ! Check the type
    IF ( sfc%Snow_Type < 0 .OR. sfc%Snow_Type > N_VALID_SNOW_TYPES ) THEN
      msg = 'Invalid snow surface type'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the ice surface type data
    ! ------------------------------
    READ( fid,IOSTAT=io_stat ) sfc%Ice_Type, &
                               sfc%Ice_Temperature, &
                               sfc%Ice_Thickness, &
                               sfc%Ice_Density, &
                               sfc%Ice_Roughness
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading ice surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

    ! Check the type
    IF ( sfc%Ice_Type < 0 .OR. sfc%Ice_Type > N_VALID_ICE_TYPES ) THEN
      msg = 'Invalid ice surface type'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Destroy the SensorData structure. This step will be taken
    ! care of in the calling routine, Read_Surface_Binary(), so
    ! the following is a belt-and-braces thing. :o)
    ! ---------------------------------------------------------
    err_stat = CRTM_Destroy_SensorData( sfc%SensorData,Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error destroying SensorData structure.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the SensorData dimensions
    ! ------------------------------
    READ( fid,IOSTAT=io_stat ) n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading SensorData dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the SensorData if required
    ! -------------------------------
    IF ( n_Channels > 0 ) THEN
      ! Allocate the structure
      err_stat = CRTM_Allocate_SensorData( n_Channels, &
                                           sfc%SensorData, &
                                           Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error allocating SensorData structure.'
        CALL Read_Record_Cleanup(); RETURN
      END IF
      ! Read the Sensor data
      READ( fid,IOSTAT=io_stat ) sfc%SensorData%Select_WMO_Sensor_ID, &  
                                 sfc%SensorData%Sensor_ID           , &  
                                 sfc%SensorData%WMO_Satellite_ID    , &
                                 sfc%SensorData%WMO_Sensor_ID       , &
                                 sfc%SensorData%Sensor_Channel      , &
                                 sfc%SensorData%Tb
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error reading SensorData. IOSTAT = ",i0)' ) io_stat
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      ! Deallocate Surface structure
      err_stat = CRTM_Destroy_Surface( sfc, Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error destroying Surface structure during error cleanup'
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
!       Function to write a single Surface data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID                 , &
!                                    Surface                , &
!                                    Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Surface:      CRTM Surface structure containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
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
                         sfc        , &  ! Input
                         Message_Log) &  ! Error messaging
                       RESULT( err_stat )
    ! Arguments
    INTEGER,                 INTENT(IN) :: fid
    TYPE(CRTM_Surface_type), INTENT(IN) :: sfc
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: Type_by_Coverage


    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Write the gross surface type
    ! ----------------------------
    ! Compute the coverage type
    Type_by_Coverage = 0
    IF ( sfc%Land_Coverage  > ZERO ) Type_by_Coverage = LAND_SURFACE
    IF ( sfc%Water_Coverage > ZERO ) Type_by_Coverage = Type_by_Coverage + WATER_SURFACE
    IF ( sfc%Snow_Coverage  > ZERO ) Type_by_Coverage = Type_by_Coverage + SNOW_SURFACE
    IF ( sfc%Ice_Coverage   > ZERO ) Type_by_Coverage = Type_by_Coverage + ICE_SURFACE
    WRITE( fid,IOSTAT=io_stat ) Type_by_Coverage, &
                                sfc%Land_Coverage, &
                                sfc%Water_Coverage, &
                                sfc%Snow_Coverage, &
                                sfc%Ice_Coverage
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing surface type and coverage fractions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the surface type independent data
    ! ---------------------------------------
    WRITE( fid,IOSTAT=io_stat ) sfc%Wind_Speed
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing surface type independent data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the land surface type data
    ! --------------------------------
    WRITE( fid,IOSTAT=io_stat ) sfc%Land_Type, &
                                sfc%Land_Temperature, &
                                sfc%Soil_Moisture_Content, &
                                sfc%Canopy_Water_Content, &
                                sfc%Vegetation_Fraction, &
                                sfc%Soil_Temperature
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing land surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the water surface type data
    ! ---------------------------------
    WRITE( fid,IOSTAT=io_stat ) sfc%Water_Type, &
                                sfc%Water_Temperature, &
                                sfc%Wind_Direction, &
                                sfc%Salinity
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing water surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the snow surface type data
    ! --------------------------------
    WRITE( fid,IOSTAT=io_stat ) sfc%Snow_Type, &
                                sfc%Snow_Temperature, &
                                sfc%Snow_Depth, &
                                sfc%Snow_Density, &
                                sfc%Snow_Grain_Size
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing snow surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the ice surface type data
    ! -------------------------------
    WRITE( fid,IOSTAT=io_stat ) sfc%Ice_Type, &
                                sfc%Ice_Temperature, &
                                sfc%Ice_Thickness, &
                                sfc%Ice_Density, &
                                sfc%Ice_Roughness
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing ice surface type data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the SensorData dimensions
    ! -------------------------------
    WRITE( fid,IOSTAT=io_stat ) sfc%SensorData%n_Channels
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing SensorData dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the SensorData if required
    ! --------------------------------
    IF ( sfc%SensorData%n_Channels > 0 ) THEN
      WRITE( fid,IOSTAT=io_stat ) sfc%SensorData%Select_WMO_Sensor_ID, &
                                  sfc%SensorData%Sensor_ID           , &
                                  sfc%SensorData%WMO_Satellite_ID    , &
                                  sfc%SensorData%WMO_Sensor_ID       , &
                                  sfc%SensorData%Sensor_Channel      , &
                                  sfc%SensorData%Tb
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error writing SensorData. IOSTAT = ",i0)' ) io_stat
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

END MODULE CRTM_Surface_Binary_IO
