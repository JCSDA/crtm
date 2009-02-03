!
! Test_RTSolution
!
! Program to test the CRTM RTSolution structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Jan-2009
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_RTSolution

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                       Display_Message, Program_Message
  USE CRTM_Parameters          , ONLY: SET
  USE CRTM_RTSolution_Define   , ONLY: CRTM_RTSolution_type     , &
                                       CRTM_Destroy_RTSolution  , &
                                       CRTM_Allocate_RTSolution , &
                                       CRTM_Assign_RTSolution   , &
                                       CRTM_Equal_RTSolution
  USE CRTM_RTSolution_Binary_IO, ONLY: CRTM_Inquire_RTSolution_Binary, &
                                       CRTM_Write_RTSolution_Binary, &
                                       CRTM_Read_RTSolution_Binary
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_RTSolution'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'

  ! Filenames
  CHARACTER(*), PARAMETER :: TEST_FILENAME = 'Test.RTSolution.bin'
  ! Test loops
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 100
  INTEGER, PARAMETER :: INFO_N_LOOPS = 10
  ! Structure dimensions
  INTEGER, PARAMETER :: N_CHANNELS  = 20
  INTEGER, PARAMETER :: N_PROFILES  = 6
  INTEGER, PARAMETER :: N_LAYERS    = 100


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: l, m, n
  TYPE(CRTM_RTSolution_type), DIMENSION(N_CHANNELS, N_PROFILES) :: rts, rts_copy

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the CRTM RTSolution structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the RTSolution structure
  ! ---------------------------------
  Error_Status = CRTM_Allocate_RTSolution( N_LAYERS, rts )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating RTSolution structure array.', &
                          FAILURE )
    STOP
  END IF


  ! Fill structure with pretend data
  ! --------------------------------
  DO m = 1, N_PROFILES
    DO l = 1, N_CHANNELS
      rts(l,m)%Surface_Emissivity      = 1.0_fp
      rts(l,m)%Up_Radiance             = 2.0_fp
      rts(l,m)%Down_Radiance           = 3.0_fp
      rts(l,m)%Down_Solar_Radiance     = 4.0_fp
      rts(l,m)%Surface_Planck_Radiance = 5.0_fp
      rts(l,m)%Upwelling_Radiance      = 6.0_fp
      rts(l,m)%Layer_Optical_Depth     = 7.0_fp
      rts(l,m)%Radiance                = 8.0_fp
      rts(l,m)%Brightness_Temperature  = 9.0_fp
    END DO
  END DO


  ! Test the RTSolution I/O functions
  ! ---------------------------------
  WRITE( *,'(/5x,"Testing RTSolution I/O functions ...")' )
  ! Write the test datafile
  WRITE( *,'(10x,"Writing test RTSolution datafile ...")' )
  Error_Status = CRTM_Write_RTSolution_Binary( TEST_FILENAME, rts )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing RTSolution data file.', &
                          FAILURE )
    STOP
  END IF
  ! Inquire the datafile
  Error_Status = CRTM_Inquire_RTSolution_Binary( TEST_FILENAME, &
                                                 n_Channels = l, &
                                                 n_Profiles = m )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring CRTM test RTSolution datafile', &
                          FAILURE )
    STOP
  END IF
  ! Check the dimensions
  IF ( l /= N_CHANNELS .OR. m /= N_PROFILES ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Inquired file dimensions are incorrect.', &
                          FAILURE )
    STOP
  END IF
  ! Test read the datafile
  WRITE( *,'(10x,"Looping for RTSolution Binary read memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = CRTM_Read_RTSolution_Binary( TEST_FILENAME, rts, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error reading RTSolution datafile on attempt # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME,TRIM(Message),FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME,TRIM(Message),INFORMATION )
    END IF
  END DO


  ! Loop for assign leak test
  ! -------------------------
  WRITE( *,'(/5x,"Looping for RTSolution structure copy memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    ! Copy the structure...
    Error_Status = CRTM_Assign_RTSolution( rts, rts_copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error copying RTSolution structure array on attempt # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME,TRIM(Message),FAILURE )
      STOP
    END IF
    ! Test for equality
    Error_Status = CRTM_Equal_RTSolution( rts, rts_copy, Check_Intermediate=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error comparing RTSolution structure arrays on attempt # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME,TRIM(Message),FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME,TRIM(Message),INFORMATION )
    END IF
  END DO


  ! Test structures for equality
  ! ----------------------------
  WRITE( *,'(/5x,"Comparing RTSolution structures ...")' )
  Error_Status = CRTM_Equal_RTSolution( rts, rts_copy, Check_Intermediate=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error comparing RTSolution structure arrays',FAILURE )
    STOP
  END IF
  WRITE( *,'(10x,"Structures are equal.")' )


  ! Destroy the structure arrays
  ! ----------------------------
  Error_Status = CRTM_Destroy_RTSolution( rts )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error destroying rts structure array.',FAILURE )
    STOP
  END IF
  Error_Status = CRTM_Destroy_RTSolution( rts_copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error destroying rts_copy structure array.',FAILURE )
    STOP
  END IF

END PROGRAM Test_RTSolution
