!
! Test_Options
!
! Program to test the CRTM Options structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Jan-2009
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_Options

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds         , ONLY: fp
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                 Display_Message, Program_Message
  USE CRTM_Parameters    , ONLY: SET
  USE CRTM_Options_Define, ONLY: CRTM_Options_type      , &
                                 CRTM_Associated_Options, &
                                 CRTM_Destroy_Options   , &
                                 CRTM_Allocate_Options  , &
                                 CRTM_Assign_Options    , &
                                 CRTM_Equal_Options
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_Options'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'

  ! Test loops
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 100
  INTEGER, PARAMETER :: INFO_N_LOOPS = 10
  ! Structure dimensions
  INTEGER, PARAMETER :: N_CHANNELS  = 20
  INTEGER, PARAMETER :: N_PROFILES  = 6


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  INTEGER :: err_stat
  INTEGER :: m, n
  TYPE(CRTM_Options_type), DIMENSION(N_PROFILES) :: opts, opts_copy

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the CRTM Options structure '//&
                        'manipulation procedures.', &
                        '$Revision$' )


  ! UN-associated test
  ! ------------------
  WRITE( *,'(/5x,"Initial association test ...")' )
  IF ( ANY(CRTM_Associated_Options(opts)) ) THEN
    CALL Display_Message( PROGRAM_NAME,'Initial association',FAILURE )
    STOP
  END IF


  ! Destroy and allocate test
  ! -------------------------
  WRITE( *,'(/5x,"Destroy and allocate test ...")' )
  ! Destroy
  err_stat = CRTM_Destroy_Options(opts)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Destroy failure',FAILURE )
    STOP
  END IF
  ! Allocate
  err_stat = CRTM_Allocate_Options(N_CHANNELS,opts)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Allocate failure',FAILURE )
    STOP
  END IF


  ! Associated test
  ! ---------------
  WRITE( *,'(/5x,"Association test ...")' )
  IF ( ANY(.NOT. CRTM_Associated_Options(opts)) ) THEN
    CALL Display_Message( PROGRAM_NAME,'Association failure',FAILURE )
    STOP
  END IF


  ! Fill structure with pretend data
  ! --------------------------------
  DO m = 1, N_PROFILES
    opts(m)%Channel                    = 1
    opts(m)%Emissivity_Switch          = SET
    opts(m)%Emissivity                 = 1.0_fp
    opts(m)%Direct_Reflectivity_Switch = SET
    opts(m)%Direct_Reflectivity        = 2.0_fp
    opts(m)%Antenna_Correction         = SET
  END DO


  ! Loop for assign leak test
  ! -------------------------
  WRITE( *,'(/5x,"Looping for assign memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    err_stat = CRTM_Assign_Options( opts, opts_copy )
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error copying for profile # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( msg,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),INFORMATION )
    END IF
  END DO


  ! Test structures for equality
  ! ----------------------------
  WRITE( *,'(/5x,"Comparing structures ...")' )
  err_stat = CRTM_Equal_Options( opts, opts_copy )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error comparing Options structure arrays',FAILURE )
    STOP
  END IF
  WRITE( *,'(10x,"Structures are equal.")' )


  ! Destroy the structure arrays
  ! ----------------------------
  err_stat = CRTM_Destroy_Options( opts )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error destroying opts structure array.',FAILURE )
    STOP
  END IF
  err_stat = CRTM_Destroy_Options( opts_copy )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error destroying opts_copy structure array.',FAILURE )
    STOP
  END IF

END PROGRAM Test_Options
