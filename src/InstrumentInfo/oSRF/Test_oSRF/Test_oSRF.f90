! Test_SRF
!
! Program to test the oSRF modules
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 22-Jul-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_oSRF

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE Unit_Test
  USE oSRF_Define, ONLY: oSRF_type, &
                         Allocated_oSRF, &
                         Create_oSRF   , &
                         Destroy_oSRF  , &
                         Assign_oSRF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_oSRF'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! Single band number of points
  INTEGER, PARAMETER :: N1_POINTS(1) = (/ 20 /)
  ! Multi-band number of points
  INTEGER, PARAMETER :: N2_POINTS(4) = (/10, 12, 15, 11/)
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_status
  INTEGER :: i
  TYPE(oSRF_type) :: osrf1, osrf2, osrf2_copy


  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the oSRF modules.', &
                        '$Revision$' )

    
  ! Test initial status
  WRITE( *,'( 5x,"oSRF allocation status: ",2(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2/))


  ! Test creation
  WRITE( *,'(/5x,"Testing creation routines...")' )
  WRITE( *,'( 7x,"Single band creation...")' )
  err_status = Create_oSRF(N1_POINTS, osrf1)
  CALL Display_Message('Create_oSRF','Single band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 7x,"Multiple band creation...")' )
  err_status = Create_oSRF(N2_POINTS, osrf2)
  CALL Display_Message('Create_oSRF','Multiple band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF allocation status: ",2(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2/))


  ! Test assignment
  WRITE( *,'(/5x,"Testing assignment routines...")' )
!  osrf2%f1 = 0.693147181_fp
!  osrf2%f2 = 3.141592653_fp
  DO i = 1, 5
    err_status = Assign_oSRF(osrf2, osrf2_copy)
    CALL Display_Message('Assign_oSRF','',err_status)
  END DO
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF allocation status:",2(1x,l5))' ) Allocated_oSRF((/osrf2,osrf2_copy/))


  ! Test destruction
  WRITE( *,'(/5x,"Testing destruction routines...")' )
  err_status = Destroy_oSRF(osrf1)
  CALL Display_Message('Destroy_oSRF','Single band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  err_status = Destroy_oSRF(osrf2)
  CALL Display_Message('Destroy_oSRF','Multiple band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  err_status = Destroy_oSRF(osrf2_copy)
  CALL Display_Message('Destroy_oSRF','Multiple band copy',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF allocation status: ",3(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2,osrf2_copy/))

END PROGRAM Test_oSRF
