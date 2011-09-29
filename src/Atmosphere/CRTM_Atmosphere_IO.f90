!
! CRTM_Atmosphere_IO
!
! Module containing routines to inquire, read, and write CRTM
! Atmosphere object datafiles.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 01-Jul-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Atmosphere_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility          , ONLY: File_Open, File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility   , ONLY: Open_Binary_File
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type, &
                                    CRTM_Atmosphere_Associated, &
                                    CRTM_Atmosphere_Destroy, &
                                    CRTM_Atmosphere_Create
  USE CRTM_Cloud_IO         , ONLY: CRTM_Cloud_ReadFile, &
                                    CRTM_Cloud_WriteFile
  USE CRTM_Aerosol_IO       , ONLY: CRTM_Aerosol_ReadFile, &
                                    CRTM_Aerosol_WriteFile
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Atmosphere_InquireFile
  PUBLIC :: CRTM_Atmosphere_ReadFile
  PUBLIC :: CRTM_Atmosphere_WriteFile
  PUBLIC :: CRTM_Atmosphere_IOVersion


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE CRTM_Atmosphere_ReadFile
    MODULE PROCEDURE Read_Atmosphere_Rank1
    MODULE PROCEDURE Read_Atmosphere_Rank2
  END INTERFACE CRTM_Atmosphere_ReadFile
  
  INTERFACE CRTM_Atmosphere_WriteFile
    MODULE PROCEDURE Write_Atmosphere_Rank1
    MODULE PROCEDURE Write_Atmosphere_Rank2
  END INTERFACE CRTM_Atmosphere_WriteFile
  

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256
  ! File status on close after write error
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
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM Atmosphere object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_InquireFile( Filename               , &
!                                                   n_Channels = n_Channels, &
!                                                   n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM Atmosphere data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Channels:     The number of spectral channels for which there is
!                       data in the file. Note that this value will always
!                       be 0 for a profile-only dataset-- it only has meaning
!                       for K-matrix data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Profiles:     The number of profiles in the data file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file inquire was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Atmosphere_InquireFile( &
    Filename   , &  ! Input
    n_Channels , &  ! Optional output
    n_Profiles ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, m
 
    ! Set up
    err_stat = SUCCESS
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the number of channels,profiles
    READ( fid, IOSTAT=io_stat,IOMSG=io_msg ) l, m
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the optional return arguments
    IF ( PRESENT(n_Channels) ) n_Channels = l
    IF ( PRESENT(n_Profiles) ) n_Profiles = m

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_Atmosphere_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_ReadFile
!
! PURPOSE:
!       Function to read CRTM Atmosphere object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_ReadFile( Filename                , &
!                                                Atmosphere              , &
!                                                Quiet      = Quiet      , &
!                                                n_Channels = n_Channels , &
!                                                n_Profiles = n_Profiles , &
!
! INPUTS:
!       Filename:     Character string specifying the name of an
!                     Atmosphere format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Atmosphere:   CRTM Atmosphere object array containing the Atmosphere
!                     data. Note the following meanings attributed to the
!                     dimensions of the object array:
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
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
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
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file read was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_Atmosphere_Rank1( &
    Filename   , &  ! Input
    Atmosphere , &  ! Output
    Quiet      , &  ! Optional input
    n_Channels , &  ! Optional output
    n_Profiles , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: Atmosphere(:)  ! M
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_ReadFile(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: n_file_channels, n_file_profiles
    INTEGER :: m, n_input_profiles
 

    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions     
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_file_channels, n_file_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check that n_Channels is zero
    IF ( n_file_channels /= 0 ) THEN
      msg = 'n_Channels dimensions in '//TRIM(Filename)//' is not zero for a rank-1 '//&
            '(i.e. profiles only) Atmosphere read.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Profiles in file is > size of output array
    n_input_profiles = SIZE(Atmosphere)
    IF ( n_file_profiles > n_input_profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output Atmosphere", &
                  &" array, ",i0,". Only the first ",i0, &
                  &" profiles will be read.")' ) &
                  n_file_profiles, n_input_profiles, n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF
    n_input_profiles = MIN(n_input_profiles, n_file_profiles)


    ! Loop over all the profiles
    Profile_Loop: DO m = 1, n_input_profiles
      err_stat = Read_Record( fid, Atmosphere(m), &
                              Quiet = Quiet, &
                              Debug = Debug  )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Atmosphere element (",i0,") from ",a)' ) m, TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the optional return values
    IF ( PRESENT(n_Channels) ) n_Channels = 0
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_input_profiles


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of profiles read from ",a,": ",i0)' ) TRIM(Filename), n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL CRTM_Atmosphere_Destroy( Atmosphere )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION Read_Atmosphere_Rank1


  FUNCTION Read_Atmosphere_Rank2( &
    Filename   , &  ! Input
    Atmosphere , &  ! Output
    Quiet      , &  ! Optional input
    n_Channels , &  ! Optional output
    n_Profiles , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: Atmosphere(:,:)  ! L x M
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,          OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_ReadFile(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: l, n_file_channels, n_input_channels
    INTEGER :: m, n_file_profiles, n_input_profiles
 

    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_file_channels, n_file_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Clouds data dimension from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Channels in file is > size of output array
    n_input_channels = SIZE(Atmosphere,DIM=1)
    IF ( n_file_channels > n_input_channels ) THEN
      WRITE( msg,'("Number of channels, ",i0," > size of the output Atmosphere", &
                  &" array dimension, ",i0,". Only the first ",i0, &
                  &" channels will be read.")' ) &
                  n_file_channels, n_input_channels, n_input_channels
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF
    n_input_channels = MIN(n_input_channels, n_file_channels)
    ! ...Check if n_Profiles in file is > size of output array
    n_input_profiles = SIZE(Atmosphere,DIM=2)
    IF ( n_file_profiles > n_input_profiles ) THEN
      WRITE( msg, '( "Number of profiles, ",i0," > size of the output Atmosphere", &
                    &" array dimension, ",i0,". Only the first ",i0, &
                    &" profiles will be read.")' ) &
                    n_file_profiles, n_input_profiles, n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF
    n_input_profiles = MIN(n_input_profiles, n_file_profiles)


    ! Loop over all the profiles and channels
    Profile_Loop: DO m = 1, n_input_profiles
      Channel_Loop: DO l = 1, n_input_channels
        err_stat = Read_Record( fid, Atmosphere(l,m), &
                                Quiet = Quiet, &
                                Debug = Debug )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error reading Atmosphere element (",i0,",",i0,") from ",a)' ) l, m, TRIM(Filename)
          CALL Read_Cleanup(); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the optional return values
    IF ( PRESENT(n_Channels) ) n_Channels = n_input_channels
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_input_profiles


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of channels and profiles read from ",a,": ",i0,1x,i0)' ) &
             TRIM(Filename), n_input_channels, n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL CRTM_Atmosphere_Destroy( Atmosphere )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION Read_Atmosphere_Rank2


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_WriteFile
!
! PURPOSE:
!       Function to write CRTM Atmosphere object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_WriteFile( Filename     , &
!                                                 Atmosphere   , &
!                                                 Quiet = Quiet  )
!
! INPUTS:
!       Filename:     Character string specifying the name of the
!                     Atmosphere format data file to write.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:   CRTM Atmosphere object array containing the Atmosphere
!                     data. Note the following meanings attributed to the
!                     dimensions of the Atmosphere array:
!                     Rank-1: M profiles.
!                             Only profile data are to be read in. The file
!                             does not contain channel information. The
!                             dimension of the array is understood to
!                             be the PROFILE dimension.
!                     Rank-2: L channels  x  M profiles
!                             Channel and profile data are to be read in.
!                             The file contains both channel and profile
!                             information. The first dimension of the 
!                             array is the CHANNEL dimension, the second
!                             is the PROFILE dimension. This is to allow
!                             K-matrix structures to be read in with the
!                             same function.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Rank-1 (M) or Rank-2 (L x M)
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file write was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Write_Atmosphere_Rank1( &
    Filename   , &  ! Input
    Atmosphere , &  ! Input
    Quiet      , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN) :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atmosphere(:)  ! M
    LOGICAL,          OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_WriteFile(M)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: m, n_output_profiles
 
    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug


    ! Any invalid profiles?
    IF ( ANY(Atmosphere%n_Layers    == 0 .OR. &
             Atmosphere%n_Absorbers == 0      ) ) THEN
      msg = 'Zero dimension profiles in input!'
      CALL Write_Cleanup(); RETURN
    END IF
    n_output_profiles = SIZE(Atmosphere)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) 0, n_output_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions to '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF

    
    ! Write the data
    Profile_Loop: DO m = 1, n_output_profiles
      err_stat = Write_Record( fid, Atmosphere(m), &
                               Quiet = Quiet, &
                               Debug = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Atmosphere element (",i0,") to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of profiles written to ",a,": ",i0)' ) TRIM(Filename), n_output_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_Atmosphere_Rank1


  FUNCTION Write_Atmosphere_Rank2( &
    Filename   , &  ! Input
    Atmosphere , &  ! Input
    Quiet      , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: Atmosphere(:,:)  ! L x M
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_WriteFile(L x M)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: l, n_output_channels
    INTEGER :: m, n_output_profiles
 
    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug


    ! Any invalid profiles?
    IF ( ANY(Atmosphere%n_Layers    == 0 .OR. &
             Atmosphere%n_Absorbers == 0      ) ) THEN
      msg = 'Zero dimension profiles in input!'
      CALL Write_Cleanup(); RETURN
    END IF
    n_output_channels = SIZE(Atmosphere,DIM=1)
    n_output_profiles = SIZE(Atmosphere,DIM=2)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_output_channels, n_output_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions to '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the data
    Profile_Loop: DO m = 1, n_output_profiles
      Channel_Loop: DO l = 1, n_output_channels
        err_stat = Write_Record( fid, Atmosphere(l,m), &
                                 Quiet = Quiet, &
                                 Debug = Debug )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error writing Atmosphere element (",i0,",",i0,") to ",a)' ) l, m, TRIM(Filename)
          CALL Write_Cleanup(); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of channels and profiles written to ",a,": ",i0,1x,i0 )' ) &
             TRIM(Filename), n_output_channels, n_output_profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_Atmosphere_Rank2


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Atmosphere_IOVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Atmosphere_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Atmosphere_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single atmosphere data record
!

  FUNCTION Read_Record( &
    fid        , &  ! Input
    atm        , &  ! Output
    Quiet      , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)  :: fid
    TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: atm
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML) :: fname
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: n_layers 
    INTEGER :: n_absorbers
    INTEGER :: n_clouds
    INTEGER :: n_aerosols

    ! Set up
    err_stat = SUCCESS


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      n_layers, &
      n_absorbers, &
      n_clouds, &
      n_aerosols
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the Atmosphere structure
    CALL CRTM_Atmosphere_Create( atm, &
                                 n_layers, &
                                 n_absorbers, &
                                 n_clouds, &
                                 n_aerosols )
    IF ( .NOT. CRTM_Atmosphere_Associated( atm ) ) THEN
      msg = 'Error creating output object.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the climatology model flag and absorber IDs
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%Climatology, &
      atm%Absorber_ID, &
      atm%Absorber_Units
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading atmosphere climatology and absorber IDs - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the atmospheric profile data
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%Level_Pressure, &
      atm%Pressure, &
      atm%Temperature, &
      atm%Absorber
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading atmospheric profile data - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the cloud data
    IF ( n_clouds > 0 ) THEN
      INQUIRE( UNIT=fid,NAME=fname )
      err_stat = CRTM_Cloud_ReadFile( fname, &
                                      atm%Cloud, &
                                      Quiet    = Quiet, &
                                      No_Close = .TRUE., &
                                      Debug    = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading cloud data'
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Read the aerosol data
    IF ( n_aerosols > 0 ) THEN
      INQUIRE( UNIT=fid,NAME=fname )
      err_stat = CRTM_Aerosol_ReadFile( fname, &
                                        atm%Aerosol, &
                                        Quiet    = Quiet, &
                                        No_Close = .TRUE., &
                                        Debug    = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading aerosol data'
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_Atmosphere_Destroy( atm )
      CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!
! NAME:
!       Write_Record
!
! PURPOSE:
!       Function to write a single atmosphere data record
!

  FUNCTION Write_Record( &
    fid  , &  ! Input
    atm  , &  ! Input
    Quiet, &  ! Optional input
    Debug) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN) :: fid
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: atm
    LOGICAL,          OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML) :: fname
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
 
    ! Set up
    err_stat = SUCCESS
    IF ( .NOT. CRTM_Atmosphere_Associated( atm ) ) THEN
      msg = 'Input Atmosphere object is not used.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%n_Layers, &
      atm%n_Absorbers, &
      atm%n_Clouds, &
      atm%n_Aerosols
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the climatology model flag and absorber IDs
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%Climatology, &
      atm%Absorber_ID, &
      atm%Absorber_Units
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing atmosphere climatology and absorber IDs - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the atmospheric profile data
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      atm%Level_Pressure, &
      atm%Pressure, &
      atm%Temperature, &
      atm%Absorber
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing atmospheric profile data - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the cloud data
    IF ( atm%n_Clouds > 0 ) THEN
      INQUIRE( UNIT=fid,NAME=fname )
      err_stat = CRTM_Cloud_WriteFile( fname, &
                                       atm%Cloud, &
                                       Quiet    = Quiet, &
                                       No_Close = .TRUE., &
                                       Debug    = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing cloud data'
        CALL Write_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Write the aerosol data
    IF ( atm%n_Aerosols > 0 ) THEN
      INQUIRE( UNIT=fid,NAME=fname )
      err_stat = CRTM_Aerosol_WriteFile( fname, &
                                         atm%Aerosol, &
                                         Quiet    = Quiet, &
                                         No_Close = .TRUE., &
                                         Debug    = Debug )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing aerosol data'
        CALL Write_Record_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE Write_Record_Cleanup()
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_Record_Cleanup
    
  END FUNCTION Write_Record

END MODULE CRTM_Atmosphere_IO
